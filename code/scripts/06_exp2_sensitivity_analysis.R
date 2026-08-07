# ================================================================
# 06_exp2_sensitivity_analysis.R
# Experiment 2: endpoint-aware robustness analysis
# ================================================================
#
# Purpose
#   1. Preserve the dissertation-reproduction beta models in Script 05.
#   2. Model genuine ratings of 0 and 100 without moving them inward.
#   3. Test whether the sentence-type by event-type interaction differs
#      between affirmative and negative trials.
#   4. Treat the five stimulus scenarios as fixed controls because five
#      levels are too few for a stable random-effect distribution.
#
# This is a sensitivity analysis, not a replacement for Script 05.
#
# Run this script from the repository root:
#   source("code/scripts/06_exp2_sensitivity_analysis.R")
#
# Required input:
#   data/processed/exp2_written_manner_path.csv
#
# Outputs:
#   output/figures/exp2_sensitivity/
#   output/tables/exp2_sensitivity/
#   output/models/exp2_sensitivity/

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

input_file <- "data/processed/exp2_written_manner_path.csv"

figure_dir <- "output/figures/exp2_sensitivity"
table_dir  <- "output/tables/exp2_sensitivity"
model_dir  <- "output/models/exp2_sensitivity"

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

exp2 <- read.csv(
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
  "sentence_type",
  "event_type",
  "scenario",
  "modifier_order",
  "included"
)

missing_columns <- setdiff(required_columns, names(exp2))

if (length(missing_columns) > 0L) {
  stop(
    "The processed dataset is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (nrow(exp2) != 1440L) {
  stop("Expected 1,440 processed rows but found ", nrow(exp2), ".")
}

if (!identical(unique(exp2$study_id), "exp2")) {
  stop("The processed file does not contain study_id = 'exp2'.")
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

exp2$included <- to_logical(exp2$included)
exp2$rating   <- suppressWarnings(as.numeric(exp2$rating))

if (anyNA(exp2$rating)) {
  stop("The rating column contains missing or non-numeric values.")
}

if (any(exp2$rating < 0 | exp2$rating > 100)) {
  stop("Ratings must remain within the original 0-100 scale.")
}

included_participants <- length(unique(exp2$participant_id[exp2$included]))
excluded_participants <- length(unique(exp2$participant_id[!exp2$included]))

if (included_participants != 114L) {
  stop(
    "Expected 114 included participants but found ",
    included_participants,
    "."
  )
}

if (excluded_participants != 6L) {
  stop(
    "Expected 6 excluded participants but found ",
    excluded_participants,
    "."
  )
}

included_rows <- sum(exp2$included)

if (included_rows != 1368L) {
  stop("Expected 1,368 included rows but found ", included_rows, ".")
}

if (any(exp2$modifier_order[exp2$included] != "manner_before_path")) {
  stop("Unexpected modifier order detected in Experiment 2.")
}

# Match Script 05 and the dissertation by analysing the three motion-event
# types only. The no-motion trials remain documented but are not modelled.
included_no_motion_rows <- sum(
  exp2$included & exp2$event_type == "no_motion"
)

if (included_no_motion_rows != 342L) {
  stop(
    "Expected 342 included no-motion observations but found ",
    included_no_motion_rows,
    "."
  )
}

analysis_data <- exp2[
  exp2$included & exp2$event_type != "no_motion",
  ,
  drop = FALSE
]

if (nrow(analysis_data) != 1026L) {
  stop(
    "Expected 1,026 included motion-event observations but found ",
    nrow(analysis_data),
    "."
  )
}

if (anyDuplicated(analysis_data[c("participant_id", "trial")]) > 0L) {
  stop("Duplicate participant-by-trial observations were detected.")
}

# Ordered-beta regression accepts the closed interval [0, 1]. Exact zeros
# and ones are deliberately preserved; no epsilon transformation is used.
analysis_data$rating_prop <- analysis_data$rating / 100

analysis_data$participant_id <- factor(analysis_data$participant_id)
analysis_data$scenario <- factor(
  analysis_data$scenario,
  levels = sort(unique(analysis_data$scenario))
)
analysis_data$sentence_type <- factor(
  analysis_data$sentence_type,
  levels = c("conflated", "path", "manner")
)
analysis_data$event_type <- factor(
  analysis_data$event_type,
  levels = c("conflated", "path", "manner")
)
analysis_data$polarity <- factor(
  analysis_data$polarity,
  levels = c("affirmative", "negative")
)

factor_columns <- c("scenario", "sentence_type", "event_type", "polarity")

if (any(vapply(analysis_data[factor_columns], anyNA, logical(1)))) {
  stop("Unexpected factor values were detected after setting factor levels.")
}

if (nlevels(analysis_data$scenario) != 5L) {
  stop(
    "Expected five stimulus scenarios but found ",
    nlevels(analysis_data$scenario),
    "."
  )
}

# Explicit treatment contrasts make all reference categories reproducible.
contrasts(analysis_data$scenario) <- stats::contr.treatment(
  nlevels(analysis_data$scenario),
  base = 1
)
contrasts(analysis_data$sentence_type) <- stats::contr.treatment(3, base = 1)
contrasts(analysis_data$event_type)    <- stats::contr.treatment(3, base = 1)
contrasts(analysis_data$polarity)      <- stats::contr.treatment(2, base = 1)

cell_counts <- as.data.frame(
  stats::xtabs(
    ~ sentence_type + event_type + polarity,
    data = analysis_data
  )
)

names(cell_counts)[names(cell_counts) == "Freq"] <- "observations"

if (any(cell_counts$observations == 0L)) {
  stop("At least one experimental condition contains no observations.")
}

write.csv(
  cell_counts,
  file.path(table_dir, "exp2_sensitivity_cell_counts.csv"),
  row.names = FALSE
)

analysis_flow <- data.frame(
  stage = c(
    "Included observations before event exclusion",
    "No-motion observations excluded",
    "Motion-event observations modelled"
  ),
  observations = c(
    included_rows,
    included_no_motion_rows,
    nrow(analysis_data)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  analysis_flow,
  file.path(table_dir, "exp2_sensitivity_analysis_flow.csv"),
  row.names = FALSE
)

endpoint_counts <- data.frame(
  endpoint = c("rating_0", "rating_100", "interior_1_to_99"),
  observations = c(
    sum(analysis_data$rating == 0),
    sum(analysis_data$rating == 100),
    sum(analysis_data$rating > 0 & analysis_data$rating < 100)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  endpoint_counts,
  file.path(table_dir, "exp2_sensitivity_endpoint_counts.csv"),
  row.names = FALSE
)

cat(
  "Experiment 2 sensitivity data validated:\n",
  "  Participants: ", nlevels(analysis_data$participant_id), "\n",
  "  Observations: ", nrow(analysis_data), "\n",
  "  No-motion observations excluded: ", included_no_motion_rows, "\n",
  "  Scenarios: ", nlevels(analysis_data$scenario), "\n",
  "  Exact zero ratings: ", sum(analysis_data$rating == 0), "\n",
  "  Exact 100 ratings: ", sum(analysis_data$rating == 100), "\n\n",
  sep = ""
)

# ----------------------------------------------------------------
# 4. Fit the endpoint-aware models
# ----------------------------------------------------------------

# Full model: the three-way interaction directly tests whether polarity
# changes the sentence-type by event-type relationship.
full_model <- glmmTMB::glmmTMB(
  rating_prop ~ sentence_type * event_type * polarity +
    scenario +
    (1 | participant_id),
  data = analysis_data,
  family = glmmTMB::ordbeta(link = "logit")
)

# Reduced model: retain all main effects and two-way interactions while
# removing only the three-way interaction.
reduced_model <- update(
  full_model,
  . ~ . - sentence_type:event_type:polarity
)

full_hessian_ok    <- isTRUE(full_model$sdr$pdHess)
reduced_hessian_ok <- isTRUE(reduced_model$sdr$pdHess)

if (!full_hessian_ok || !reduced_hessian_ok) {
  stop(
    "At least one ordered-beta model has a non-positive-definite Hessian. ",
    "Do not interpret the results until the model has been diagnosed."
  )
}

if (full_model$fit$convergence != 0L ||
    reduced_model$fit$convergence != 0L) {
  stop(
    "At least one ordered-beta model did not return convergence code 0. ",
    "Do not interpret the results until the model has been diagnosed."
  )
}

saveRDS(
  full_model,
  file.path(model_dir, "exp2_ordered_beta_full_model.rds")
)

saveRDS(
  reduced_model,
  file.path(model_dir, "exp2_ordered_beta_reduced_model.rds")
)

# ----------------------------------------------------------------
# 5. Test the three-way interaction
# ----------------------------------------------------------------

three_way_lrt_raw <- stats::anova(reduced_model, full_model)

three_way_lrt <- cbind(
  model = rownames(three_way_lrt_raw),
  as.data.frame(three_way_lrt_raw, check.names = FALSE),
  row.names = NULL
)

write.csv(
  three_way_lrt,
  file.path(table_dir, "exp2_sensitivity_three_way_lrt.csv"),
  row.names = FALSE
)

p_column <- grep("^Pr\\(", names(three_way_lrt), value = TRUE)

three_way_p <- if (length(p_column) == 1L) {
  tail(three_way_lrt[[p_column]], 1)
} else {
  NA_real_
}

omnibus_tests <- as.data.frame(emmeans::joint_tests(full_model))

write.csv(
  omnibus_tests,
  file.path(table_dir, "exp2_sensitivity_omnibus_tests.csv"),
  row.names = FALSE
)

# ----------------------------------------------------------------
# 6. Save coefficients and participant random-effect variance
# ----------------------------------------------------------------

fixed_effects <- as.data.frame(
  summary(full_model)$coefficients$cond,
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
  file.path(table_dir, "exp2_sensitivity_fixed_effects.csv"),
  row.names = FALSE
)

participant_variance_matrix <- glmmTMB::VarCorr(full_model)$cond$participant_id

random_effects <- data.frame(
  group = "participant_id",
  term = "(Intercept)",
  variance = as.numeric(participant_variance_matrix[1, 1]),
  standard_deviation = as.numeric(
    attr(participant_variance_matrix, "stddev")[1]
  ),
  stringsAsFactors = FALSE
)

write.csv(
  random_effects,
  file.path(table_dir, "exp2_sensitivity_random_effects.csv"),
  row.names = FALSE
)

# ----------------------------------------------------------------
# 7. Model-predicted ratings
# ----------------------------------------------------------------

prediction_emmeans <- emmeans::emmeans(
  full_model,
  ~ sentence_type * event_type * polarity,
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
    "emmeans did not return a response-scale column named 'response'. ",
    "Inspect prediction_emmeans before plotting."
  )
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
  file.path(table_dir, "exp2_sensitivity_model_predictions.csv"),
  row.names = FALSE
)

# ----------------------------------------------------------------
# 8. Planned pairwise comparisons
# ----------------------------------------------------------------

# Compare modification types within every event-type by polarity condition.
sentence_emmeans <- emmeans::emmeans(
  full_model,
  ~ sentence_type | event_type * polarity
)

sentence_contrasts <- as.data.frame(
  summary(
    pairs(sentence_emmeans, adjust = "holm"),
    infer = c(TRUE, TRUE)
  )
)

write.csv(
  sentence_contrasts,
  file.path(
    table_dir,
    "exp2_sensitivity_sentence_contrasts_within_event.csv"
  ),
  row.names = FALSE
)

# Compare event types within every modification-type by polarity condition.
event_emmeans <- emmeans::emmeans(
  full_model,
  ~ event_type | sentence_type * polarity
)

event_contrasts <- as.data.frame(
  summary(
    pairs(event_emmeans, adjust = "holm"),
    infer = c(TRUE, TRUE)
  )
)

write.csv(
  event_contrasts,
  file.path(
    table_dir,
    "exp2_sensitivity_event_contrasts_within_sentence.csv"
  ),
  row.names = FALSE
)

# ----------------------------------------------------------------
# 9. Prediction figure
# ----------------------------------------------------------------

prediction_plot <- ggplot2::ggplot(
  prediction_table,
  ggplot2::aes(
    x = event_type,
    y = predicted_rating,
    colour = sentence_type,
    group = sentence_type
  )
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lower_rating, ymax = upper_rating),
    width = 0.08,
    linewidth = 0.7
  ) +
  ggplot2::facet_wrap(~ polarity) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  ggplot2::labs(
    title = "Experiment 2: ordered-beta model predictions",
    subtitle = "Point estimates and 95% confidence intervals",
    x = "Event type",
    y = "Model-predicted rating",
    colour = "Modification type"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figure_dir,
    "exp2_sensitivity_model_predictions.png"
  ),
  plot = prediction_plot,
  width = 12,
  height = 7,
  dpi = 300
)

# ----------------------------------------------------------------
# 10. DHARMa diagnostics
# ----------------------------------------------------------------

set.seed(123)

simulated_residuals <- DHARMa::simulateResiduals(
  fittedModel = full_model,
  n = 1000,
  refit = FALSE,
  plot = FALSE
)

grDevices::png(
  filename = file.path(
    figure_dir,
    "exp2_sensitivity_diagnostics.png"
  ),
  width = 2400,
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
  file.path(table_dir, "exp2_sensitivity_diagnostic_tests.csv"),
  row.names = FALSE
)

# ----------------------------------------------------------------
# 11. Metadata, model summary, and session information
# ----------------------------------------------------------------

metadata <- data.frame(
  field = c(
    "analysis",
    "input_file",
    "included_observations_before_event_exclusion",
    "no_motion_observations_excluded",
    "model_observations",
    "participants",
    "excluded_participants",
    "scenarios",
    "rating_zero_observations",
    "rating_100_observations",
    "family",
    "link",
    "modifier_order",
    "scenario_treatment",
    "sentence_reference",
    "sentence_level_2",
    "sentence_level_3",
    "event_reference",
    "event_level_2",
    "event_level_3",
    "polarity_reference",
    "full_model_formula",
    "full_model_convergence_code",
    "full_model_positive_definite_hessian",
    "three_way_lrt_p_value",
    "random_seed",
    "glmmTMB_version",
    "emmeans_version",
    "DHARMa_version"
  ),
  value = c(
    "Experiment 2 endpoint-aware sensitivity analysis",
    input_file,
    included_rows,
    included_no_motion_rows,
    nrow(analysis_data),
    nlevels(analysis_data$participant_id),
    excluded_participants,
    nlevels(analysis_data$scenario),
    sum(analysis_data$rating == 0),
    sum(analysis_data$rating == 100),
    "ordered beta",
    "logit",
    "manner_before_path",
    "fixed effect (five stimulus scenarios)",
    "conflated",
    "path",
    "manner",
    "conflated",
    "path",
    "manner",
    "affirmative",
    paste(deparse(stats::formula(full_model)), collapse = " "),
    full_model$fit$convergence,
    full_hessian_ok,
    three_way_p,
    123,
    as.character(utils::packageVersion("glmmTMB")),
    as.character(utils::packageVersion("emmeans")),
    as.character(utils::packageVersion("DHARMa"))
  ),
  stringsAsFactors = FALSE
)

write.csv(
  metadata,
  file.path(table_dir, "exp2_sensitivity_metadata.csv"),
  row.names = FALSE
)

capture.output(
  summary(full_model),
  file = file.path(table_dir, "exp2_sensitivity_model_summary.txt")
)

capture.output(
  sessionInfo(),
  file = file.path(table_dir, "exp2_sensitivity_session_info.txt")
)

# ----------------------------------------------------------------
# 12. Completion message
# ----------------------------------------------------------------

cat("\n============================================\n")
cat("Experiment 2 sensitivity analysis complete\n")
cat("============================================\n\n")
cat(
  "Included participants: ",
  nlevels(analysis_data$participant_id),
  "\n",
  sep = ""
)
cat("Model observations: ", nrow(analysis_data), "\n", sep = "")
cat(
  "No-motion observations excluded: ",
  included_no_motion_rows,
  "\n",
  sep = ""
)
cat(
  "Scenarios controlled as fixed effects: ",
  nlevels(analysis_data$scenario),
  "\n",
  sep = ""
)
cat("Positive-definite Hessian: ", full_hessian_ok, "\n", sep = "")
cat(
  "Three-way interaction LRT p-value: ",
  format.pval(three_way_p, digits = 4),
  "\n",
  sep = ""
)
cat("\nFigures written to: ", figure_dir, "\n", sep = "")
cat("Tables written to: ", table_dir, "\n", sep = "")
cat("Models written to: ", model_dir, "\n", sep = "")
