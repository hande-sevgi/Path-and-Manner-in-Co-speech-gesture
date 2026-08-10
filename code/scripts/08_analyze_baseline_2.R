# ================================================================
# 08_analyze_baseline_2.R
# Baseline Study 2: gesture interpretation without event context
# Dissertation-reproduction analysis
# ================================================================
#
# Purpose
#   1. Document the complete 4 x 2 baseline design.
#   2. Reproduce the dissertation beta-regression analysis using
#      conflated, manner, path, and no-gesture conditions.
#   3. Test gesture type, polarity, and their interaction.
#   4. Reproduce the descriptive Gesture versus No Gesture summary.
#   5. Save auditable tables, predictions, diagnostics, and metadata.
#
# The supplied analysis file contains all 40 recruited participants.
# No participants were excluded from this baseline study.
#
# Run this script from the repository root:
#   source("code/scripts/08_analyze_baseline_2.R")
#
# Required input:
#   data/processed/baseline2_no_event.csv
#
# Outputs:
#   output/figures/baseline2/
#   output/tables/baseline2/
#   output/models/baseline2/


# ----------------------------------------------------------------
# 1. Package checks
# ----------------------------------------------------------------

required_packages <- c(
  "glmmTMB",
  "emmeans",
  "ggplot2",
  "DHARMa"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages first:\n\n",
    "install.packages(c(",
    paste(
      sprintf('"%s"', missing_packages),
      collapse = ", "
    ),
    "))"
  )
}


# ----------------------------------------------------------------
# 2. File paths and output directories
# ----------------------------------------------------------------

input_file <- "data/processed/baseline2_no_event.csv"

figure_dir <- "output/figures/baseline2"
table_dir  <- "output/tables/baseline2"
model_dir  <- "output/models/baseline2"

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  model_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(input_file)) {
  stop(
    "Input file not found: ",
    input_file,
    "\nRun code/scripts/02_prepare_processed_data.R first."
  )
}


# ----------------------------------------------------------------
# 3. Read and validate the processed data
# ----------------------------------------------------------------

baseline2 <- read.csv(
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
  "gesture_type",
  "scenario",
  "included"
)

missing_columns <- setdiff(
  required_columns,
  names(baseline2)
)

if (length(missing_columns) > 0L) {
  stop(
    "The processed dataset is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (nrow(baseline2) != 400L) {
  stop(
    "Expected 400 processed rows but found ",
    nrow(baseline2),
    "."
  )
}

if (!identical(unique(baseline2$study_id), "baseline2")) {
  stop(
    "The processed file does not contain study_id = 'baseline2'."
  )
}

to_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    return(x == 1)
  }

  tolower(trimws(as.character(x))) %in%
    c("true", "1", "yes")
}

baseline2$included <- to_logical(
  baseline2$included
)

baseline2$rating <- suppressWarnings(
  as.numeric(baseline2$rating)
)

if (anyNA(baseline2$rating)) {
  stop(
    "The rating column contains missing or non-numeric values."
  )
}

if (any(baseline2$rating < 0 | baseline2$rating > 100)) {
  stop("Ratings must be between 0 and 100.")
}

if (!all(baseline2$included)) {
  stop(
    "All rows should be marked as included because no participants ",
    "were excluded from Baseline Study 2."
  )
}

participant_count <- length(
  unique(baseline2$participant_id)
)

if (participant_count != 40L) {
  stop(
    "Expected 40 participants but found ",
    participant_count,
    "."
  )
}

participant_observations <- table(
  baseline2$participant_id
)

if (any(participant_observations != 10L)) {
  stop(
    "Each participant should contribute exactly 10 observations."
  )
}

if (anyDuplicated(
  baseline2[c("participant_id", "trial")]
) > 0L) {
  stop(
    "Duplicate participant-by-trial observations were detected."
  )
}

expected_polarities <- c(
  "affirmative",
  "negative"
)

expected_gestures <- c(
  "conflated",
  "manner",
  "path",
  "none"
)

if (!setequal(
  unique(baseline2$polarity),
  expected_polarities
)) {
  stop("Unexpected polarity values were detected.")
}

if (!setequal(
  unique(baseline2$gesture_type),
  expected_gestures
)) {
  stop("Unexpected gesture-type values were detected.")
}

baseline2$participant_id <- factor(
  baseline2$participant_id
)

baseline2$scenario <- factor(
  baseline2$scenario,
  levels = c(
    "leaf",
    "car",
    "plank",
    "chair",
    "paper"
  )
)

baseline2$polarity <- factor(
  baseline2$polarity,
  levels = expected_polarities
)

baseline2$gesture_type <- factor(
  baseline2$gesture_type,
  levels = expected_gestures
)

if (
  anyNA(baseline2$polarity) ||
    anyNA(baseline2$gesture_type) ||
    anyNA(baseline2$scenario)
) {
  stop("Unexpected factor values were detected.")
}

if (nlevels(baseline2$scenario) != 5L) {
  stop(
    "Expected five stimulus scenarios but found ",
    nlevels(baseline2$scenario),
    "."
  )
}

scenario_observations <- table(
  baseline2$scenario
)

if (any(scenario_observations != 80L)) {
  stop(
    "Each scenario should contribute exactly 80 observations."
  )
}

full_cell_counts <- as.data.frame(
  stats::xtabs(
    ~ polarity + gesture_type,
    data = baseline2
  )
)

names(full_cell_counts)[
  names(full_cell_counts) == "Freq"
] <- "observations"

cell_keys <- paste(
  full_cell_counts$polarity,
  full_cell_counts$gesture_type,
  sep = "|"
)

expected_cell_counts <- c(
  "affirmative|conflated" = 54L,
  "affirmative|manner" = 50L,
  "affirmative|path" = 49L,
  "affirmative|none" = 47L,
  "negative|conflated" = 50L,
  "negative|manner" = 48L,
  "negative|path" = 51L,
  "negative|none" = 51L
)

if (
  nrow(full_cell_counts) != 8L ||
    any(
      full_cell_counts$observations !=
        unname(expected_cell_counts[cell_keys])
    )
) {
  stop(
    "The polarity-by-gesture cell counts do not match the ",
    "archived analysis-ready dataset."
  )
}

write.csv(
  full_cell_counts,
  file.path(
    table_dir,
    "baseline2_full_cell_counts.csv"
  ),
  row.names = FALSE
)


# ----------------------------------------------------------------
# 4. Factor coding and endpoint transformation
# ----------------------------------------------------------------

# The original dissertation analysis used sum contrasts.
# The fourth gesture level (none) is represented implicitly by
# the negative sum of the first three gesture coefficients.
contrasts(
  baseline2$polarity
) <- stats::contr.sum(2)

contrasts(
  baseline2$gesture_type
) <- stats::contr.sum(4)

# Reproduce the dissertation beta analysis by moving exact
# endpoint ratings slightly inside the open interval (0, 1).
epsilon <- 1e-6

baseline2$rating_scaled <-
  (baseline2$rating / 100) *
  (1 - 2 * epsilon) +
  epsilon

exact_zero_count <- sum(
  baseline2$rating == 0
)

exact_hundred_count <- sum(
  baseline2$rating == 100
)

if (exact_zero_count != 41L) {
  stop(
    "Expected 41 exact zero ratings but found ",
    exact_zero_count,
    "."
  )
}

if (exact_hundred_count != 31L) {
  stop(
    "Expected 31 exact 100 ratings but found ",
    exact_hundred_count,
    "."
  )
}

cat(
  "Baseline Study 2 data validated:\n",
  "  Participants: ", participant_count, "\n",
  "  Model observations: ", nrow(baseline2), "\n",
  "  Gesture conditions: ", nlevels(baseline2$gesture_type), "\n",
  "  Scenarios: ", nlevels(baseline2$scenario), "\n",
  "  Exact zero ratings: ", exact_zero_count, "\n",
  "  Exact 100 ratings: ", exact_hundred_count,
  "\n\n",
  sep = ""
)


# ----------------------------------------------------------------
# 5. Sample flow and descriptive statistics
# ----------------------------------------------------------------

sample_flow <- data.frame(
  stage = c(
    "Recruited",
    "Excluded",
    "Available and analysed"
  ),
  participants = c(
    40L,
    0L,
    40L
  ),
  source = c(
    "Dissertation Chapter 3",
    "Dissertation Chapter 3",
    input_file
  ),
  stringsAsFactors = FALSE
)

write.csv(
  sample_flow,
  file.path(
    table_dir,
    "baseline2_sample_flow.csv"
  ),
  row.names = FALSE
)

descriptive_grouping <- c(
  "polarity",
  "gesture_type"
)

mean_table <- aggregate(
  baseline2["rating"],
  baseline2[descriptive_grouping],
  mean
)

names(mean_table)[
  names(mean_table) == "rating"
] <- "mean_rating"

sd_table <- aggregate(
  baseline2["rating"],
  baseline2[descriptive_grouping],
  stats::sd
)

names(sd_table)[
  names(sd_table) == "rating"
] <- "sd_rating"

count_table <- aggregate(
  baseline2["rating"],
  baseline2[descriptive_grouping],
  length
)

names(count_table)[
  names(count_table) == "rating"
] <- "observations"

descriptive_summary <- Reduce(
  function(left, right) {
    merge(
      left,
      right,
      by = descriptive_grouping,
      all = TRUE
    )
  },
  list(
    mean_table,
    sd_table,
    count_table
  )
)

descriptive_summary$standard_error <-
  descriptive_summary$sd_rating /
  sqrt(descriptive_summary$observations)

descriptive_summary$ci_95 <-
  1.96 * descriptive_summary$standard_error

descriptive_summary <- descriptive_summary[
  order(
    descriptive_summary$polarity,
    descriptive_summary$gesture_type
  ),
  ,
  drop = FALSE
]

write.csv(
  descriptive_summary,
  file.path(
    table_dir,
    "baseline2_descriptive_summary.csv"
  ),
  row.names = FALSE
)


# ----------------------------------------------------------------
# 6. Descriptive figures
# ----------------------------------------------------------------

raw_plot <- ggplot2::ggplot(
  baseline2,
  ggplot2::aes(
    x = polarity,
    y = rating,
    colour = gesture_type,
    fill = gesture_type
  )
) +
  ggplot2::geom_boxplot(
    alpha = 0.35,
    outlier.alpha = 0.20,
    position = ggplot2::position_dodge(
      width = 0.75
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  ggplot2::labs(
    title = "Baseline Study 2: ratings without event context",
    subtitle = "Complete 4 x 2 descriptive design",
    x = "Polarity",
    y = "Rating",
    colour = "Gesture type",
    fill = "Gesture type"
  ) +
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figure_dir,
    "baseline2_raw_ratings.png"
  ),
  plot = raw_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# The dissertation additionally presents Gesture versus No Gesture
# as a descriptive collapse. This does not replace the four-level model.
baseline2$gesture_presence <- factor(
  ifelse(
    baseline2$gesture_type == "none",
    "no_gesture",
    "gesture"
  ),
  levels = c(
    "gesture",
    "no_gesture"
  )
)

collapsed_mean <- aggregate(
  baseline2["rating"],
  baseline2[c("polarity", "gesture_presence")],
  mean
)

names(collapsed_mean)[
  names(collapsed_mean) == "rating"
] <- "mean_rating"

collapsed_sd <- aggregate(
  baseline2["rating"],
  baseline2[c("polarity", "gesture_presence")],
  stats::sd
)

names(collapsed_sd)[
  names(collapsed_sd) == "rating"
] <- "sd_rating"

collapsed_count <- aggregate(
  baseline2["rating"],
  baseline2[c("polarity", "gesture_presence")],
  length
)

names(collapsed_count)[
  names(collapsed_count) == "rating"
] <- "observations"

collapsed_summary <- Reduce(
  function(left, right) {
    merge(
      left,
      right,
      by = c("polarity", "gesture_presence"),
      all = TRUE
    )
  },
  list(
    collapsed_mean,
    collapsed_sd,
    collapsed_count
  )
)

collapsed_summary$standard_error <-
  collapsed_summary$sd_rating /
  sqrt(collapsed_summary$observations)

collapsed_summary$ci_95 <-
  1.96 * collapsed_summary$standard_error

write.csv(
  collapsed_summary,
  file.path(
    table_dir,
    "baseline2_gesture_presence_summary.csv"
  ),
  row.names = FALSE
)

collapsed_plot <- ggplot2::ggplot(
  baseline2,
  ggplot2::aes(
    x = polarity,
    y = rating,
    colour = gesture_presence,
    fill = gesture_presence
  )
) +
  ggplot2::geom_boxplot(
    alpha = 0.35,
    outlier.alpha = 0.20,
    position = ggplot2::position_dodge(
      width = 0.70
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  ggplot2::labs(
    title = "Baseline Study 2: gesture presence",
    subtitle = "Descriptive Gesture versus No Gesture comparison",
    x = "Polarity",
    y = "Rating",
    colour = "Condition",
    fill = "Condition"
  ) +
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figure_dir,
    "baseline2_gesture_presence.png"
  ),
  plot = collapsed_plot,
  width = 9,
  height = 6,
  dpi = 300
)


# ----------------------------------------------------------------
# 7. Reproduce the dissertation beta model
# ----------------------------------------------------------------

baseline2_model <- glmmTMB::glmmTMB(
  rating_scaled ~
    polarity * gesture_type +
    (1 | participant_id) +
    (1 | scenario),
  family = glmmTMB::beta_family(
    link = "logit"
  ),
  data = baseline2
)

hessian_ok <- isTRUE(
  baseline2_model$sdr$pdHess
)

convergence_code <-
  baseline2_model$fit$convergence

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
  baseline2_model,
  file.path(
    model_dir,
    "baseline2_beta_model.rds"
  )
)


# ----------------------------------------------------------------
# 8. Model tables and planned comparisons
# ----------------------------------------------------------------

fixed_effects <- as.data.frame(
  summary(baseline2_model)$coefficients$cond,
  check.names = FALSE
)

fixed_effects$term <- rownames(
  fixed_effects
)

rownames(fixed_effects) <- NULL

fixed_effects <- fixed_effects[
  ,
  c(
    "term",
    setdiff(
      names(fixed_effects),
      "term"
    )
  ),
  drop = FALSE
]

write.csv(
  fixed_effects,
  file.path(
    table_dir,
    "baseline2_fixed_effects.csv"
  ),
  row.names = FALSE
)

capture.output(
  summary(baseline2_model),
  file = file.path(
    table_dir,
    "baseline2_model_summary.txt"
  )
)

omnibus_tests <- as.data.frame(
  emmeans::joint_tests(
    baseline2_model
  )
)

write.csv(
  omnibus_tests,
  file.path(
    table_dir,
    "baseline2_omnibus_tests.csv"
  ),
  row.names = FALSE
)

# Each gesture type compared with the equally weighted mean
# across the four gesture conditions.
gesture_marginal_means <- emmeans::emmeans(
  baseline2_model,
  ~ gesture_type,
  weights = "equal"
)

gesture_deviations <- as.data.frame(
  summary(
    emmeans::contrast(
      gesture_marginal_means,
      method = "eff",
      adjust = "none"
    ),
    infer = c(TRUE, TRUE)
  )
)

write.csv(
  gesture_deviations,
  file.path(
    table_dir,
    "baseline2_gesture_deviations_from_mean.csv"
  ),
  row.names = FALSE
)

# Pairwise gesture comparisons within each polarity condition.
gesture_by_polarity <- emmeans::emmeans(
  baseline2_model,
  ~ gesture_type | polarity
)

gesture_contrasts <- as.data.frame(
  summary(
    emmeans::contrast(
      gesture_by_polarity,
      method = "pairwise",
      adjust = "holm"
    ),
    infer = c(TRUE, TRUE)
  )
)

write.csv(
  gesture_contrasts,
  file.path(
    table_dir,
    "baseline2_gesture_contrasts_within_polarity.csv"
  ),
  row.names = FALSE
)

# Polarity comparison within each gesture condition.
polarity_by_gesture <- emmeans::emmeans(
  baseline2_model,
  ~ polarity | gesture_type
)

polarity_contrasts <- as.data.frame(
  summary(
    emmeans::contrast(
      polarity_by_gesture,
      method = "pairwise",
      adjust = "holm"
    ),
    infer = c(TRUE, TRUE)
  )
)

write.csv(
  polarity_contrasts,
  file.path(
    table_dir,
    "baseline2_polarity_contrasts_within_gesture.csv"
  ),
  row.names = FALSE
)


# ----------------------------------------------------------------
# 9. Model predictions and prediction figure
# ----------------------------------------------------------------

prediction_emmeans <- emmeans::emmeans(
  baseline2_model,
  ~ gesture_type * polarity,
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
  stop(
    "emmeans did not return a response-scale column named 'response'."
  )
}

lower_column <- grep(
  "LCL|lower.CL",
  names(prediction_table),
  value = TRUE
)

upper_column <- grep(
  "UCL|upper.CL",
  names(prediction_table),
  value = TRUE
)

if (
  length(lower_column) != 1L ||
    length(upper_column) != 1L
) {
  stop(
    "Could not identify the prediction confidence-interval columns."
  )
}

prediction_table$predicted_rating <-
  100 * prediction_table$response

prediction_table$lower_rating <-
  100 * prediction_table[[lower_column]]

prediction_table$upper_rating <-
  100 * prediction_table[[upper_column]]

write.csv(
  prediction_table,
  file.path(
    table_dir,
    "baseline2_model_predictions.csv"
  ),
  row.names = FALSE
)

prediction_plot_data <- prediction_table

prediction_plot_data$gesture_type <- factor(
  prediction_plot_data$gesture_type,
  levels = c(
    "conflated",
    "manner",
    "path",
    "none"
  ),
  labels = c(
    "Conflated gesture",
    "Manner gesture",
    "Path gesture",
    "No gesture"
  )
)

prediction_plot_data$polarity <- factor(
  prediction_plot_data$polarity,
  levels = c(
    "affirmative",
    "negative"
  ),
  labels = c(
    "Affirmative",
    "Negative"
  )
)

prediction_plot <- ggplot2::ggplot(
  prediction_plot_data,
  ggplot2::aes(
    x = gesture_type,
    y = predicted_rating,
    colour = polarity
  )
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lower_rating,
      ymax = upper_rating
    ),
    width = 0.10,
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    size = 3.5
  ) +
  ggplot2::facet_wrap(
    ~ polarity,
    nrow = 1
  ) +
  ggplot2::scale_colour_manual(
    values = c(
      "Affirmative" = "#D55E00",
      "Negative" = "#0072B2"
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  ggplot2::labs(
    title = "Baseline Study 2: beta-model predictions",
    subtitle = "Four gesture conditions; 95% confidence intervals",
    x = "Gesture type",
    y = "Model-predicted rating"
  ) +
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      face = "bold"
    )
  )

ggplot2::ggsave(
  filename = file.path(
    figure_dir,
    "baseline2_model_predictions.png"
  ),
  plot = prediction_plot,
  width = 10,
  height = 6,
  dpi = 300
)


# ----------------------------------------------------------------
# 10. DHARMa diagnostics
# ----------------------------------------------------------------

set.seed(123)

simulated_residuals <- DHARMa::simulateResiduals(
  fittedModel = baseline2_model,
  n = 1000,
  refit = FALSE,
  plot = FALSE
)

grDevices::png(
  filename = file.path(
    figure_dir,
    "baseline2_diagnostics.png"
  ),
  width = 2200,
  height = 1400,
  res = 200
)

plot(simulated_residuals)

grDevices::dev.off()

extract_test <- function(
  test_result,
  test_name
) {
  statistic_value <- if (
    length(test_result$statistic) > 0L
  ) {
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
    DHARMa::testUniformity(
      simulated_residuals,
      plot = FALSE
    ),
    "uniformity"
  ),
  extract_test(
    DHARMa::testDispersion(
      simulated_residuals,
      plot = FALSE
    ),
    "dispersion"
  ),
  extract_test(
    DHARMa::testOutliers(
      simulated_residuals,
      plot = FALSE
    ),
    "outliers"
  )
)

write.csv(
  diagnostic_tests,
  file.path(
    table_dir,
    "baseline2_diagnostic_tests.csv"
  ),
  row.names = FALSE
)


# ----------------------------------------------------------------
# 11. Metadata and session information
# ----------------------------------------------------------------

metadata <- data.frame(
  field = c(
    "analysis",
    "input_file",
    "recruited_participants",
    "excluded_participants",
    "analysed_participants",
    "model_observations",
    "gesture_conditions",
    "scenarios",
    "family",
    "link",
    "endpoint_transformation",
    "polarity_contrasts",
    "gesture_contrasts",
    "gesture_levels",
    "model_formula",
    "convergence_code",
    "positive_definite_hessian",
    "random_seed",
    "glmmTMB_version",
    "emmeans_version",
    "DHARMa_version"
  ),
  value = c(
    "Baseline Study 2 dissertation-reproduction analysis",
    input_file,
    40,
    0,
    participant_count,
    nrow(baseline2),
    nlevels(baseline2$gesture_type),
    nlevels(baseline2$scenario),
    "beta",
    "logit",
    "epsilon = 1e-6",
    "sum",
    "sum",
    "conflated | manner | path | none",
    paste(
      deparse(
        stats::formula(baseline2_model)
      ),
      collapse = " "
    ),
    convergence_code,
    hessian_ok,
    123,
    as.character(
      utils::packageVersion("glmmTMB")
    ),
    as.character(
      utils::packageVersion("emmeans")
    ),
    as.character(
      utils::packageVersion("DHARMa")
    )
  ),
  stringsAsFactors = FALSE
)

write.csv(
  metadata,
  file.path(
    table_dir,
    "baseline2_model_metadata.csv"
  ),
  row.names = FALSE
)

capture.output(
  sessionInfo(),
  file = file.path(
    table_dir,
    "baseline2_session_info.txt"
  )
)


# ----------------------------------------------------------------
# 12. Completion message
# ----------------------------------------------------------------

cat("\n============================================\n")
cat("Baseline Study 2 analysis complete\n")
cat("============================================\n\n")

cat(
  "Participants: ",
  participant_count,
  "\n",
  sep = ""
)

cat(
  "Model observations: ",
  nrow(baseline2),
  "\n",
  sep = ""
)

cat(
  "Gesture conditions: ",
  nlevels(baseline2$gesture_type),
  "\n",
  sep = ""
)

cat(
  "Positive-definite Hessian: ",
  hessian_ok,
  "\n",
  sep = ""
)

cat(
  "\nFigures written to: ",
  figure_dir,
  "\n",
  sep = ""
)

cat(
  "Tables written to: ",
  table_dir,
  "\n",
  sep = ""
)

cat(
  "Models written to: ",
  model_dir,
  "\n",
  sep = ""
)
