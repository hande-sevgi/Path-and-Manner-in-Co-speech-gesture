# ================================================================
# 07_analyze_baseline_1.R
# Baseline Study 1: event interpretation without gesture
# Dissertation-reproduction analysis
# ================================================================
#
# Purpose
#   1. Document the complete 4 x 2 baseline design.
#   2. Reproduce the dissertation's beta-regression analysis of the
#      three motion-event types: conflated, manner, and path.
#   3. Test whether motion-event ratings differ by polarity.
#   4. Save auditable model tables, predictions, and diagnostics.
#
# The supplied analysis file contains the 40 analysed participants.
# The dissertation documents 41 recruited participants and one exclusion
# before the dataset represented here.
#
# Run this script from the repository root:
#   source("code/scripts/07_analyze_baseline_1.R")
#
# Required input:
#   data/processed/baseline1_no_gesture.csv
#
# Outputs:
#   output/figures/baseline1/
#   output/tables/baseline1/
#   output/models/baseline1/

# ----------------------------------------------------------------
# 1. Package checks
# ----------------------------------------------------------------

required_packages <- c("glmmTMB", "emmeans", "ggplot2", "DHARMa")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages first:\n\n",
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

# ----------------------------------------------------------------
# 2. File paths and output directories
# ----------------------------------------------------------------

input_file <- "data/processed/baseline1_no_gesture.csv"

figure_dir <- "output/figures/baseline1"
table_dir  <- "output/tables/baseline1"
model_dir  <- "output/models/baseline1"

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir,  recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop(
    "Input file not found: ", input_file, "\n",
    "Run code/scripts/02_prepare_processed_data.R first."
  )
}

# ----------------------------------------------------------------
# 3. Read and validate the processed data
# ----------------------------------------------------------------

baseline1 <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_columns <- c(
  "study_id",
  "participant_id",
  "trial",
  "rating",
  "polarity",
  "event_type",
  "scenario",
  "included"
)

missing_columns <- setdiff(required_columns, names(baseline1))

if (length(missing_columns) > 0L) {
  stop(
    "The processed dataset is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (nrow(baseline1) != 400L) {
  stop("Expected 400 processed rows but found ", nrow(baseline1), ".")
}

if (!identical(unique(baseline1$study_id), "baseline1")) {
  stop("The processed file does not contain study_id = 'baseline1'.")
}

to_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    return(x == 1)
  }

  tolower(trimws(as.character(x))) %in% c("true", "1", "yes")
}

baseline1$included <- to_logical(baseline1$included)
baseline1$rating   <- suppressWarnings(as.numeric(baseline1$rating))

if (anyNA(baseline1$rating)) {
  stop("The rating column contains missing or non-numeric values.")
}

if (any(baseline1$rating < 0 | baseline1$rating > 100)) {
  stop("Ratings must be between 0 and 100.")
}

if (!all(baseline1$included)) {
  stop(
    "The supplied baseline file should contain the 40 analysed ",
    "participants with every row marked as included."
  )
}

participant_count <- length(unique(baseline1$participant_id))

if (participant_count != 40L) {
  stop("Expected 40 analysed participants but found ", participant_count, ".")
}

if (anyDuplicated(baseline1[c("participant_id", "trial")]) > 0L) {
  stop("Duplicate participant-by-trial observations were detected.")
}

expected_polarities <- c("affirmative", "negative")
expected_events <- c("conflated", "manner", "path", "no_motion")

if (!setequal(unique(baseline1$polarity), expected_polarities)) {
  stop("Unexpected polarity values were detected.")
}

if (!setequal(unique(baseline1$event_type), expected_events)) {
  stop("Unexpected event-type values were detected.")
}

baseline1$participant_id <- factor(baseline1$participant_id)
baseline1$scenario <- factor(
  baseline1$scenario,
  levels = sort(unique(baseline1$scenario))
)
baseline1$polarity <- factor(
  baseline1$polarity,
  levels = expected_polarities
)
baseline1$event_type <- factor(
  baseline1$event_type,
  levels = expected_events
)

if (nlevels(baseline1$scenario) != 5L) {
  stop(
    "Expected five stimulus scenarios but found ",
    nlevels(baseline1$scenario),
    "."
  )
}

if (anyNA(baseline1$polarity) ||
    anyNA(baseline1$event_type) ||
    anyNA(baseline1$scenario)) {
  stop("Unexpected factor values were detected.")
}

full_cell_counts <- as.data.frame(
  stats::xtabs(~ polarity + event_type, data = baseline1)
)

names(full_cell_counts)[names(full_cell_counts) == "Freq"] <- "observations"

if (nrow(full_cell_counts) != 8L ||
    any(full_cell_counts$observations != 50L)) {
  stop("Expected 50 observations in each of the eight design cells.")
}

write.csv(
  full_cell_counts,
  file.path(table_dir, "baseline1_full_cell_counts.csv"),
  row.names = FALSE
)

# The no-motion condition is part of the complete descriptive design but
# is not part of the dissertation's comparison among motion-event types.
no_motion_rows <- sum(baseline1$event_type == "no_motion")

if (no_motion_rows != 100L) {
  stop("Expected 100 no-motion observations but found ", no_motion_rows, ".")
}

model_data <- droplevels(
  baseline1[baseline1$event_type != "no_motion", , drop = FALSE]
)

if (nrow(model_data) != 300L) {
  stop(
    "Expected 300 motion-event observations but found ",
    nrow(model_data),
    "."
  )
}

if (nlevels(model_data$participant_id) != 40L) {
  stop("All 40 analysed participants should contribute to the model.")
}

# Restore the dissertation factor order after dropping no-motion trials.
model_data$event_type <- factor(
  model_data$event_type,
  levels = c("conflated", "manner", "path")
)
model_data$polarity <- factor(
  model_data$polarity,
  levels = c("affirmative", "negative")
)

# Reproduce the original beta analysis by moving exact endpoints inward.
epsilon <- 1e-6
model_data$rating_scaled <-
  (model_data$rating / 100) * (1 - 2 * epsilon) + epsilon

# Sum contrasts reproduce the dissertation's comparisons with the overall
# mean rather than imposing one event type as the reference category.
contrasts(model_data$polarity) <- stats::contr.sum(2)
contrasts(model_data$event_type) <- stats::contr.sum(3)

cat(
  "Baseline Study 1 data validated:\n",
  "  Analysed participants: ", nlevels(model_data$participant_id), "\n",
  "  Complete observations: ", nrow(baseline1), "\n",
  "  No-motion observations excluded from model: ", no_motion_rows, "\n",
  "  Motion-event model observations: ", nrow(model_data), "\n",
  "  Scenarios: ", nlevels(model_data$scenario), "\n",
  "  Exact zero ratings in model data: ", sum(model_data$rating == 0), "\n",
  "  Exact 100 ratings in model data: ", sum(model_data$rating == 100),
  "\n\n",
  sep = ""
)

# ----------------------------------------------------------------
# 4. Sample flow and descriptive statistics
# ----------------------------------------------------------------

sample_flow <- data.frame(
  stage = c(
    "Recruited (documented in dissertation)",
    "Excluded before supplied analysis file",
    "Available and analysed"
  ),
  participants = c(41L, 1L, 40L),
  source = c(
    "Dissertation Chapter 3",
    "Dissertation Chapter 3",
    input_file
  ),
  stringsAsFactors = FALSE
)

write.csv(
  sample_flow,
  file.path(table_dir, "baseline1_sample_flow.csv"),
  row.names = FALSE
)

analysis_flow <- data.frame(
  stage = c(
    "Complete observations",
    "No-motion observations excluded",
    "Motion-event observations modelled"
  ),
  observations = c(nrow(baseline1), no_motion_rows, nrow(model_data)),
  stringsAsFactors = FALSE
)

write.csv(
  analysis_flow,
  file.path(table_dir, "baseline1_analysis_flow.csv"),
  row.names = FALSE
)

descriptive_grouping <- c("polarity", "event_type")

mean_table <- aggregate(
  baseline1["rating"],
  baseline1[descriptive_grouping],
  mean
)
names(mean_table)[names(mean_table) == "rating"] <- "mean_rating"

sd_table <- aggregate(
  baseline1["rating"],
  baseline1[descriptive_grouping],
  stats::sd
)
names(sd_table)[names(sd_table) == "rating"] <- "sd_rating"

count_table <- aggregate(
  baseline1["rating"],
  baseline1[descriptive_grouping],
  length
)
names(count_table)[names(count_table) == "rating"] <- "observations"

descriptive_summary <- Reduce(
  function(left, right) {
    merge(left, right, by = descriptive_grouping, all = TRUE)
  },
  list(mean_table, sd_table, count_table)
)

descriptive_summary$standard_error <-
  descriptive_summary$sd_rating / sqrt(descriptive_summary$observations)
descriptive_summary$ci_95 <- 1.96 * descriptive_summary$standard_error

descriptive_summary <- descriptive_summary[
  order(descriptive_summary$polarity, descriptive_summary$event_type),
  ,
  drop = FALSE
]

write.csv(
  descriptive_summary,
  file.path(table_dir, "baseline1_descriptive_summary.csv"),
  row.names = FALSE
)

# ----------------------------------------------------------------
# 5. Complete-design raw-rating figure
# ----------------------------------------------------------------

raw_plot <- ggplot2::ggplot(
  baseline1,
  ggplot2::aes(
    x = polarity,
    y = rating,
    colour = event_type,
    fill = event_type
  )
) +
  ggplot2::geom_boxplot(
    alpha = 0.35,
    outlier.alpha = 0.20,
    position = ggplot2::position_dodge(width = 0.75)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  ggplot2::labs(
    title = "Baseline Study 1: ratings without gesture",
    subtitle = "Complete 4 x 2 descriptive design",
    x = "Polarity",
    y = "Rating",
    colour = "Event type",
    fill = "Event type"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(figure_dir, "baseline1_raw_ratings.png"),
  plot = raw_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# ----------------------------------------------------------------
# 6. Reproduce the dissertation beta model
# ----------------------------------------------------------------

baseline1_model <- glmmTMB::glmmTMB(
  rating_scaled ~ polarity * event_type +
    (1 | participant_id) +
    (1 | scenario),
  family = glmmTMB::beta_family(link = "logit"),
  data = model_data
)

hessian_ok <- isTRUE(baseline1_model$sdr$pdHess)
convergence_code <- baseline1_model$fit$convergence

if (!hessian_ok) {
  stop(
    "The beta model has a non-positive-definite Hessian. ",
    "Do not interpret the results until the model has been diagnosed."
  )
}

if (convergence_code != 0L) {
  stop(
    "The beta model did not return convergence code 0. ",
    "Do not interpret the results until the model has been diagnosed."
  )
}

saveRDS(
  baseline1_model,
  file.path(model_dir, "baseline1_beta_model.rds")
)

# ----------------------------------------------------------------
# 7. Model tables and planned comparisons
# ----------------------------------------------------------------

fixed_effects <- as.data.frame(
  summary(baseline1_model)$coefficients$cond,
  check.names = FALSE
)

fixed_effects$term <- rownames(fixed_effects)
rownames(fixed_effects) <- NULL
fixed_effects <- fixed_effects[
  ,
  c("term", setdiff(names(fixed_effects), "term")),
  drop = FALSE
]

write.csv(
  fixed_effects,
  file.path(table_dir, "baseline1_fixed_effects.csv"),
  row.names = FALSE
)

capture.output(
  summary(baseline1_model),
  file = file.path(table_dir, "baseline1_model_summary.txt")
)

omnibus_tests <- as.data.frame(emmeans::joint_tests(baseline1_model))

write.csv(
  omnibus_tests,
  file.path(table_dir, "baseline1_omnibus_tests.csv"),
  row.names = FALSE
)

# Each event type compared with the equally weighted motion-event mean.
# These link-scale deviations correspond to the dissertation's sum-coded
# interpretation of conflated, manner, and path events.
event_marginal_means <- emmeans::emmeans(
  baseline1_model,
  ~ event_type,
  weights = "equal"
)

event_deviations <- as.data.frame(
  summary(
    emmeans::contrast(
      event_marginal_means,
      method = "eff",
      adjust = "none"
    ),
    infer = c(TRUE, TRUE)
  )
)

write.csv(
  event_deviations,
  file.path(table_dir, "baseline1_event_deviations_from_mean.csv"),
  row.names = FALSE
)

# Pairwise event comparisons within each polarity condition.
event_by_polarity <- emmeans::emmeans(
  baseline1_model,
  ~ event_type | polarity
)

event_contrasts <- as.data.frame(
  summary(
    pairs(event_by_polarity, adjust = "holm"),
    infer = c(TRUE, TRUE)
  )
)

write.csv(
  event_contrasts,
  file.path(table_dir, "baseline1_event_contrasts_within_polarity.csv"),
  row.names = FALSE
)

# Polarity comparison within each motion-event type.
polarity_by_event <- emmeans::emmeans(
  baseline1_model,
  ~ polarity | event_type
)

polarity_contrasts <- as.data.frame(
  summary(
    pairs(polarity_by_event, adjust = "holm"),
    infer = c(TRUE, TRUE)
  )
)

write.csv(
  polarity_contrasts,
  file.path(table_dir, "baseline1_polarity_contrasts_within_event.csv"),
  row.names = FALSE
)

# ----------------------------------------------------------------
# 8. Model predictions and prediction figure
# ----------------------------------------------------------------

prediction_emmeans <- emmeans::emmeans(
  baseline1_model,
  ~ event_type * polarity,
  type = "response"
)

prediction_table <- as.data.frame(
  summary(
    prediction_emmeans,
    infer = c(TRUE, FALSE),
    type = "response"
  )
)

if (!"response" %in% names(prediction_table)) {
  stop("emmeans did not return a response-scale column named 'response'.")
}

lower_column <- grep("LCL|lower.CL", names(prediction_table), value = TRUE)
upper_column <- grep("UCL|upper.CL", names(prediction_table), value = TRUE)

if (length(lower_column) != 1L || length(upper_column) != 1L) {
  stop("Could not identify the prediction confidence-interval columns.")
}

prediction_table$predicted_rating <- 100 * prediction_table$response
prediction_table$lower_rating <- 100 * prediction_table[[lower_column]]
prediction_table$upper_rating <- 100 * prediction_table[[upper_column]]

write.csv(
  prediction_table,
  file.path(table_dir, "baseline1_model_predictions.csv"),
  row.names = FALSE
)

prediction_plot <- ggplot2::ggplot(
  prediction_table,
  ggplot2::aes(
    x = event_type,
    y = predicted_rating,
    colour = polarity,
    group = polarity
  )
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lower_rating, ymax = upper_rating),
    width = 0.08,
    linewidth = 0.7
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  ggplot2::labs(
    title = "Baseline Study 1: beta-model predictions",
    subtitle = "Motion-event trials only; 95% confidence intervals",
    x = "Event type",
    y = "Model-predicted rating",
    colour = "Polarity"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(figure_dir, "baseline1_model_predictions.png"),
  plot = prediction_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ----------------------------------------------------------------
# 9. DHARMa diagnostics
# ----------------------------------------------------------------

set.seed(123)

simulated_residuals <- DHARMa::simulateResiduals(
  fittedModel = baseline1_model,
  n = 1000,
  refit = FALSE,
  plot = FALSE
)

grDevices::png(
  filename = file.path(figure_dir, "baseline1_diagnostics.png"),
  width = 2200,
  height = 1400,
  res = 200
)

plot(simulated_residuals)
grDevices::dev.off()

extract_test <- function(test_result, test_name) {
  statistic_value <- if (length(test_result$statistic) > 0L) {
    unname(test_result$statistic[[1]])
  } else {
    NA_real_
  }

  data.frame(
    test = test_name,
    statistic = statistic_value,
    p_value = test_result$p.value,
    method = test_result$method,
    stringsAsFactors = FALSE
  )
}

diagnostic_tests <- rbind(
  extract_test(
    DHARMa::testUniformity(simulated_residuals, plot = FALSE),
    "uniformity"
  ),
  extract_test(
    DHARMa::testDispersion(simulated_residuals, plot = FALSE),
    "dispersion"
  ),
  extract_test(
    DHARMa::testOutliers(simulated_residuals, plot = FALSE),
    "outliers"
  )
)

write.csv(
  diagnostic_tests,
  file.path(table_dir, "baseline1_diagnostic_tests.csv"),
  row.names = FALSE
)

# ----------------------------------------------------------------
# 10. Metadata and session information
# ----------------------------------------------------------------

metadata <- data.frame(
  field = c(
    "analysis",
    "input_file",
    "documented_recruited_participants",
    "documented_excluded_participants",
    "participants_in_supplied_file",
    "complete_observations",
    "no_motion_observations_excluded",
    "model_observations",
    "scenarios",
    "family",
    "link",
    "endpoint_transformation",
    "polarity_contrasts",
    "event_contrasts",
    "event_levels",
    "model_formula",
    "convergence_code",
    "positive_definite_hessian",
    "random_seed",
    "glmmTMB_version",
    "emmeans_version",
    "DHARMa_version"
  ),
  value = c(
    "Baseline Study 1 dissertation-reproduction analysis",
    input_file,
    41,
    1,
    participant_count,
    nrow(baseline1),
    no_motion_rows,
    nrow(model_data),
    nlevels(model_data$scenario),
    "beta",
    "logit",
    "epsilon = 1e-6",
    "sum",
    "sum",
    "conflated | manner | path",
    paste(deparse(stats::formula(baseline1_model)), collapse = " "),
    convergence_code,
    hessian_ok,
    123,
    as.character(utils::packageVersion("glmmTMB")),
    as.character(utils::packageVersion("emmeans")),
    as.character(utils::packageVersion("DHARMa"))
  ),
  stringsAsFactors = FALSE
)

write.csv(
  metadata,
  file.path(table_dir, "baseline1_model_metadata.csv"),
  row.names = FALSE
)

capture.output(
  sessionInfo(),
  file = file.path(table_dir, "baseline1_session_info.txt")
)

# ----------------------------------------------------------------
# 11. Completion message
# ----------------------------------------------------------------

cat("\n============================================\n")
cat("Baseline Study 1 analysis complete\n")
cat("============================================\n\n")
cat("Analysed participants: ", participant_count, "\n", sep = "")
cat("Complete observations: ", nrow(baseline1), "\n", sep = "")
cat("No-motion observations excluded: ", no_motion_rows, "\n", sep = "")
cat("Model observations: ", nrow(model_data), "\n", sep = "")
cat("Positive-definite Hessian: ", hessian_ok, "\n", sep = "")
cat("\nFigures written to: ", figure_dir, "\n", sep = "")
cat("Tables written to: ", table_dir, "\n", sep = "")
cat("Models written to: ", model_dir, "\n", sep = "")
