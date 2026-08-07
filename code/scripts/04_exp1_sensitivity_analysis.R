# ================================================================
# 04_exp1_sensitivity_analysis.R
# Experiment 1: endpoint-aware robustness analysis
# ================================================================
#
# Purpose
#   1. Preserve the beta-regression analysis in Script 03.
#   2. Model genuine ratings of 0 and 100 without moving them inward.
#   3. Test whether the sentence-type by event-type interaction differs
#      between affirmative and negative trials.
#   4. Treat the five scenarios as fixed controls because five levels are
#      too few for a stable estimate of a random-effect distribution.
#
# Run this script from the repository root:
#   source("code/scripts/04_exp1_sensitivity_analysis.R")
#
# Required input:
#   data/processed/exp1_written_path_manner.csv
#
# Outputs:
#   output/figures/exp1_sensitivity/
#   output/tables/exp1_sensitivity/
#   output/models/exp1_sensitivity/

# ----------------------------------------------------------------
# 1. Package checks
# ----------------------------------------------------------------

required_packages <- c("glmmTMB", "emmeans", "ggplot2", "DHARMa")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
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

input_file <- "data/processed/exp1_written_path_manner.csv"

figure_dir <- "output/figures/exp1_sensitivity"
table_dir  <- "output/tables/exp1_sensitivity"
model_dir  <- "output/models/exp1_sensitivity"

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

exp1 <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_columns <- c(
  "participant_id",
  "trial",
  "rating",
  "polarity",
  "sentence_type",
  "event_type",
  "scenario",
  "included"
)

missing_columns <- setdiff(required_columns, names(exp1))

if (length(missing_columns) > 0) {
  stop(
    "The processed dataset is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
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

exp1$included <- to_logical(exp1$included)
exp1$rating   <- suppressWarnings(as.numeric(exp1$rating))

if (anyNA(exp1$rating)) {
  stop("The rating column contains missing or non-numeric values.")
}

if (any(exp1$rating < 0 | exp1$rating > 100)) {
  stop("Ratings must remain within the original 0-100 scale.")
}

included_participants <- length(unique(exp1$participant_id[exp1$included]))
excluded_participants <- length(unique(exp1$participant_id[!exp1$included]))

if (included_participants != 140) {
  stop(
    "Expected 140 included participants but found ",
    included_participants, "."
  )
}

if (excluded_participants != 10) {
  stop(
    "Expected 10 excluded participants but found ",
    excluded_participants, "."
  )
}

# Match Script 03 by analyzing the three motion-event types only.
analysis_data <- exp1[
  exp1$included & exp1$event_type != "no_motion",
  ,
  drop = FALSE
]

if (nrow(analysis_data) != 1260) {
  stop(
    "Expected 1,260 included motion-event observations but found ",
    nrow(analysis_data), "."
  )
}

if (anyDuplicated(analysis_data[c("participant_id", "trial")]) > 0) {
  stop("Duplicate participant-by-trial observations were detected.")
}

# Ordered-beta regression accepts the closed interval [0, 1].
# Exact zeros and ones are deliberately preserved.
analysis_data$rating_prop <- analysis_data$rating / 100

analysis_data$participant_id <- factor(analysis_data$participant_id)
analysis_data$scenario <- factor(
  analysis_data$scenario,
  levels = sort(unique(analysis_data$scenario))
)
analysis_data$sentence_type <- factor(
  analysis_data$sentence_type,
  levels = c("conflated", "manner", "path")
)
analysis_data$event_type <- factor(
  analysis_data$event_type,
  levels = c("conflated", "manner", "path")
)
analysis_data$polarity <- factor(
  analysis_data$polarity,
  levels = c("affirmative", "negative")
)

factor_columns <- c("scenario", "sentence_type", "event_type", "polarity")

if (any(vapply(analysis_data[factor_columns], anyNA, logical(1)))) {
  stop("Unexpected factor values were detected after setting factor levels.")
}

# Explicit treatment contrasts make the reference categories reproducible.
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

if (any(cell_counts$observations == 0)) {
  stop("At least one experimental condition contains no observations.")
}

write.csv(
  cell_counts,
  file.path(table_dir, "exp1_sensitivity_cell_counts.csv"),
  row.names = FALSE
)

endpoint_counts <- data.frame(
  endpoint = c("rating_0", "rating_100", "interior_1_to_99"),
  observations = c(
    sum(analysis_data$rating == 0),
    sum(analysis_data$rating == 100),
    sum(analysis_data$rating > 0 & analysis_data$rating < 100)
  )
)

write.csv(
  endpoint_counts,
  file.path(table_dir, "exp1_sensitivity_endpoint_counts.csv"),
  row.names = FALSE
)

cat(
  "Experiment 1 sensitivity data validated:\n",
  "  Participants: ", nlevels(analysis_data$participant_id), "\n",
  "  Observations: ", nrow(analysis_data), "\n",
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

# Reduced model: retains every main effect and two-way interaction but
# removes only the three-way interaction.
reduced_model <- update(
  full_model,
  . ~ . - sentence_type:event_type:polarity
)

full_hessian_ok    <- isTRUE(full_model$sdr$pdHess)
reduced_hessian_ok <- isTRUE(reduced_model$sdr$pdHess)

if (!full_hessian_ok || !reduced_hessian_ok) {
  stop(
    "At least one ordered-beta model has a non-positive-definite Hessian. ",
    "Do not interpret the model until it has been diagnosed."
  )
}

if (full_model$fit$convergence != 0 ||
    reduced_model$fit$convergence != 0) {
  stop(
    "At least one ordered-beta model did not return convergence code 0. ",
    "Do not interpret the model until it has been diagnosed."
  )
}

saveRDS(
  full_model,
  file.path(model_dir, "exp1_ordered_beta_full_model.rds")
)

saveRDS(
  reduced_model,
  file.path(model_dir, "exp1_ordered_beta_reduced_model.rds")
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
  file.path(table_dir, "exp1_sensitivity_three_way_lrt.csv"),
  row.names = FALSE
)

p_column <- grep("^Pr\\(", names(three_way_lrt), value = TRUE)

three_way_p <- if (length(p_column) == 1) {
  tail(three_way_lrt[[p_column]], 1)
} else {
  NA_real_
}

# Omnibus Wald tests provide a complete table of model terms.
omnibus_tests <- as.data.frame(emmeans::joint_tests(full_model))

write.csv(
  omnibus_tests,
  file.path(table_dir, "exp1_sensitivity_omnibus_tests.csv"),
  row.names = FALSE
)

# ----------------------------------------------------------------
# 6. Save model coefficients and random-effect variance
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
  file.path(table_dir, "exp1_sensitivity_fixed_effects.csv"),
  row.names = FALSE
)

random_effects <- as.data.frame(glmmTMB::VarCorr(full_model))

write.csv(
  random_effects,
  file.path(table_dir, "exp1_sensitivity_random_effects.csv"),
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

if (length(lower_column) != 1 || length(upper_column) != 1) {
  stop("Could not identify the prediction confidence-interval columns.")
}

prediction_table$predicted_rating <- 100 * prediction_table$response
prediction_table$lower_rating <- 100 * prediction_table[[lower_column]]
prediction_table$upper_rating <- 100 * prediction_table[[upper_column]]

write.csv(
  prediction_table,
  file.path(table_dir, "exp1_sensitivity_model_predictions.csv"),
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
    "exp1_sensitivity_sentence_contrasts_within_event.csv"
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
    "exp1_sensitivity_event_contrasts_within_sentence.csv"
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
    title = "Experiment 1: ordered-beta model predictions",
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
    "exp1_sensitivity_model_predictions.png"
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
    "exp1_sensitivity_diagnostics.png"
  ),
  width = 2400,
  height = 1400,
  res = 200
)

plot(simulated_residuals)
grDevices::dev.off()

extract_test <- function(test_result, test_name) {
  statistic_value <- if (length(test_result$statistic) > 0) {
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
  file.path(table_dir, "exp1_sensitivity_diagnostic_tests.csv"),
  row.names = FALSE
)

# ----------------------------------------------------------------
# 11. Metadata, model summary, and session information
# ----------------------------------------------------------------

metadata <- data.frame(
  field = c(
    "analysis",
    "input_file",
    "observations",
    "participants",
    "scenarios",
    "rating_zero_observations",
    "rating_100_observations",
    "family",
    "link",
    "scenario_treatment",
    "sentence_reference",
    "event_reference",
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
    "Experiment 1 endpoint-aware sensitivity analysis",
    input_file,
    nrow(analysis_data),
    nlevels(analysis_data$participant_id),
    nlevels(analysis_data$scenario),
    sum(analysis_data$rating == 0),
    sum(analysis_data$rating == 100),
    "ordered beta",
    "logit",
    "fixed effect (five stimulus scenarios)",
    "conflated",
    "conflated",
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
  file.path(table_dir, "exp1_sensitivity_metadata.csv"),
  row.names = FALSE
)

capture.output(
  summary(full_model),
  file = file.path(table_dir, "exp1_sensitivity_model_summary.txt")
)

capture.output(
  sessionInfo(),
  file = file.path(table_dir, "exp1_sensitivity_session_info.txt")
)

# ----------------------------------------------------------------
# 12. Completion message
# ----------------------------------------------------------------

cat("\n============================================\n")
cat("Experiment 1 sensitivity analysis complete\n")
cat("============================================\n\n")
cat("Included participants: ", nlevels(analysis_data$participant_id), "\n", sep = "")
cat("Model observations: ", nrow(analysis_data), "\n", sep = "")
cat("Scenarios controlled as fixed effects: ", nlevels(analysis_data$scenario), "\n", sep = "")
cat("Positive-definite Hessian: ", full_hessian_ok, "\n", sep = "")
cat("Three-way interaction LRT p-value: ", format.pval(three_way_p, digits = 4), "\n", sep = "")
cat("\nFigures written to: ", figure_dir, "\n", sep = "")
cat("Tables written to: ", table_dir, "\n", sep = "")
cat("Models written to: ", model_dir, "\n", sep = "")
