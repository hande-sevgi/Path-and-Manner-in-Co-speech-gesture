# ================================================================
# 09_analyze_experiment_3.R
# Experiment 3: path and manner in co-speech gesture
# Dissertation-reproduction analysis
# ================================================================
#
# Run this script from the repository root:
#   source("code/scripts/09_analyze_experiment_3.R")
#
# Required input:
#   data/processed/exp3_cospeech_gesture.csv
#
# Analysis plan reproduced from the Chapter 3 legacy workflow:
#   1. Retain the 142 participants who passed the documented
#      low-variability exclusion criterion.
#   2. Describe the complete 3 gesture x 4 event x 2 polarity design.
#   3. Fit separate affirmative and negative beta mixed models to
#      the three motion-event conditions.
#   4. Analyse no-motion trials separately with gesture type and
#      polarity in the same beta mixed model.
#   5. Save model objects, diagnostics, tables, and figures.
#
# Exact ratings of 0 and 100 are moved slightly inside (0, 1) to
# reproduce the dissertation beta-regression workflow. A later
# ordered-beta sensitivity analysis can retain the endpoints exactly.


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
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}


# ----------------------------------------------------------------
# 2. Paths and output directories
# ----------------------------------------------------------------

input_file <- "data/processed/exp3_cospeech_gesture.csv"

figure_dir <- "output/figures/exp3"
table_dir  <- "output/tables/exp3"
model_dir  <- "output/models/exp3"

for (directory in c(figure_dir, table_dir, model_dir)) {
  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

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

experiment3 <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)

required_columns <- c(
  "study_id",
  "participant_id",
  "trial",
  "rating",
  "polarity",
  "gesture_type",
  "event_type",
  "scenario",
  "included",
  "exclusion_reason"
)

missing_columns <- setdiff(
  required_columns,
  names(experiment3)
)

if (length(missing_columns) > 0L) {
  stop(
    "The processed dataset is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if ("Response_ID" %in% names(experiment3)) {
  stop("Processed data unexpectedly contain Response_ID.")
}

if (nrow(experiment3) != 1800L) {
  stop(
    "Expected 1,800 processed rows but found ",
    nrow(experiment3),
    "."
  )
}

if (!identical(unique(experiment3$study_id), "exp3")) {
  stop("The processed file does not contain study_id = 'exp3'.")
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

experiment3$included <- to_logical(experiment3$included)
experiment3$rating <- suppressWarnings(
  as.numeric(experiment3$rating)
)

if (anyNA(experiment3$rating)) {
  stop("The rating column contains missing or non-numeric values.")
}

if (any(experiment3$rating < 0 | experiment3$rating > 100)) {
  stop("Ratings must be between 0 and 100.")
}

included_data <- experiment3[experiment3$included, , drop = FALSE]

included_participants <- length(
  unique(included_data$participant_id)
)

excluded_participants <- length(
  unique(experiment3$participant_id[!experiment3$included])
)

if (nrow(included_data) != 1704L) {
  stop(
    "Expected 1,704 included rows but found ",
    nrow(included_data),
    "."
  )
}

if (included_participants != 142L) {
  stop(
    "Expected 142 included participants but found ",
    included_participants,
    "."
  )
}

if (excluded_participants != 8L) {
  stop(
    "Expected 8 excluded participants but found ",
    excluded_participants,
    "."
  )
}

participant_observations <- table(included_data$participant_id)

if (any(participant_observations != 12L)) {
  stop("Each included participant should contribute 12 observations.")
}

if (anyDuplicated(
  included_data[c("participant_id", "trial")]
) > 0L) {
  stop("Duplicate participant-by-trial observations were detected.")
}

expected_polarities <- c("affirmative", "negative")
expected_gestures <- c("conflated", "manner", "path")
expected_events <- c("conflated", "manner", "path", "no_motion")
expected_scenarios <- c("leaf", "car", "plank", "chair", "paper")

if (!setequal(unique(included_data$polarity), expected_polarities)) {
  stop("Unexpected polarity values were detected.")
}

if (!setequal(unique(included_data$gesture_type), expected_gestures)) {
  stop("Unexpected gesture-type values were detected.")
}

if (!setequal(unique(included_data$event_type), expected_events)) {
  stop("Unexpected event-type values were detected.")
}

if (!setequal(unique(included_data$scenario), expected_scenarios)) {
  stop("Unexpected scenario values were detected.")
}

included_data$participant_id <- factor(included_data$participant_id)
included_data$polarity <- factor(
  included_data$polarity,
  levels = expected_polarities
)
included_data$gesture_type <- factor(
  included_data$gesture_type,
  levels = expected_gestures
)
included_data$event_type <- factor(
  included_data$event_type,
  levels = expected_events
)
included_data$scenario <- factor(
  included_data$scenario,
  levels = expected_scenarios
)

if (
  anyNA(included_data$polarity) ||
    anyNA(included_data$gesture_type) ||
    anyNA(included_data$event_type) ||
    anyNA(included_data$scenario)
) {
  stop("Unexpected factor values were detected.")
}

exact_zero_count <- sum(included_data$rating == 0)
exact_hundred_count <- sum(included_data$rating == 100)

if (exact_zero_count != 532L) {
  stop(
    "Expected 532 exact zero ratings but found ",
    exact_zero_count,
    "."
  )
}

if (exact_hundred_count != 292L) {
  stop(
    "Expected 292 exact 100 ratings but found ",
    exact_hundred_count,
    "."
  )
}

cat(
  "Experiment 3 data validated:\n",
  "  Included participants: ", included_participants, "\n",
  "  Excluded participants: ", excluded_participants, "\n",
  "  Included observations: ", nrow(included_data), "\n",
  "  Gesture conditions: ", nlevels(included_data$gesture_type), "\n",
  "  Event conditions: ", nlevels(included_data$event_type), "\n",
  "  Scenarios: ", nlevels(included_data$scenario), "\n",
  "  Exact zero ratings: ", exact_zero_count, "\n",
  "  Exact 100 ratings: ", exact_hundred_count,
  "\n\n",
  sep = ""
)


# ----------------------------------------------------------------
# 4. Sample flow, cell counts, and descriptive statistics
# ----------------------------------------------------------------

sample_flow <- data.frame(
  stage = c(
    "Recruited",
    "Excluded for low rating variability",
    "Included in analysis"
  ),
  participants = c(
    150L,
    8L,
    142L
  ),
  source = c(
    "Dissertation Chapter 3",
    "02_prepare_processed_data.R",
    input_file
  ),
  stringsAsFactors = FALSE
)

write.csv(
  sample_flow,
  file.path(table_dir, "exp3_sample_flow.csv"),
  row.names = FALSE
)

full_cell_counts <- as.data.frame(
  stats::xtabs(
    ~ polarity + gesture_type + event_type,
    data = included_data
  )
)

names(full_cell_counts)[
  names(full_cell_counts) == "Freq"
] <- "observations"

if (
  nrow(full_cell_counts) != 24L ||
    sum(full_cell_counts$observations) != 1704L
) {
  stop("The complete 3 x 4 x 2 cell-count table is invalid.")
}

write.csv(
  full_cell_counts,
  file.path(table_dir, "exp3_full_cell_counts.csv"),
  row.names = FALSE
)

grouping_variables <- c(
  "polarity",
  "gesture_type",
  "event_type"
)

mean_table <- aggregate(
  included_data["rating"],
  included_data[grouping_variables],
  mean
)
names(mean_table)[names(mean_table) == "rating"] <- "mean_rating"

sd_table <- aggregate(
  included_data["rating"],
  included_data[grouping_variables],
  stats::sd
)
names(sd_table)[names(sd_table) == "rating"] <- "sd_rating"

count_table <- aggregate(
  included_data["rating"],
  included_data[grouping_variables],
  length
)
names(count_table)[names(count_table) == "rating"] <- "observations"

descriptive_summary <- Reduce(
  function(left, right) {
    merge(
      left,
      right,
      by = grouping_variables,
      all = TRUE
    )
  },
  list(mean_table, sd_table, count_table)
)

descriptive_summary$standard_error <-
  descriptive_summary$sd_rating /
  sqrt(descriptive_summary$observations)

descriptive_summary$ci_95 <-
  1.96 * descriptive_summary$standard_error

write.csv(
  descriptive_summary,
  file.path(table_dir, "exp3_descriptive_summary.csv"),
  row.names = FALSE
)


# ----------------------------------------------------------------
# 5. Complete-design descriptive figure
# ----------------------------------------------------------------

raw_plot <- ggplot2::ggplot(
  included_data,
  ggplot2::aes(
    x = event_type,
    y = rating,
    colour = gesture_type,
    fill = gesture_type
  )
) +
  ggplot2::geom_boxplot(
    alpha = 0.35,
    outlier.alpha = 0.18,
    position = ggplot2::position_dodge(width = 0.80)
  ) +
  ggplot2::facet_wrap(~ polarity) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  ggplot2::labs(
    title = "Experiment 3: co-speech gesture ratings",
    subtitle = "Complete 3 gesture x 4 event x 2 polarity design",
    x = "Event type",
    y = "Rating",
    colour = "Gesture type",
    fill = "Gesture type"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(figure_dir, "exp3_raw_ratings.png"),
  plot = raw_plot,
  width = 11,
  height = 6.5,
  dpi = 300
)


# ----------------------------------------------------------------
# 6. Reusable model and output helpers
# ----------------------------------------------------------------

epsilon <- 1e-6

scale_beta_response <- function(rating) {
  (rating / 100) * (1 - 2 * epsilon) + epsilon
}

fixed_effect_table <- function(model) {
  output <- as.data.frame(
    summary(model)$coefficients$cond,
    check.names = FALSE
  )

  output$term <- rownames(output)
  rownames(output) <- NULL

  output[
    ,
    c("term", setdiff(names(output), "term")),
    drop = FALSE
  ]
}

extract_dharma_test <- function(test_result, test_name) {
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

run_diagnostics <- function(model, output_label) {
  set.seed(123)

  simulation <- DHARMa::simulateResiduals(
    fittedModel = model,
    n = 1000,
    refit = FALSE,
    plot = FALSE
  )

  diagnostic_tests <- rbind(
    extract_dharma_test(
      DHARMa::testUniformity(simulation, plot = FALSE),
      "uniformity"
    ),
    extract_dharma_test(
      DHARMa::testDispersion(simulation, plot = FALSE),
      "dispersion"
    ),
    extract_dharma_test(
      DHARMa::testOutliers(simulation, plot = FALSE),
      "outliers"
    )
  )

  write.csv(
    diagnostic_tests,
    file.path(
      table_dir,
      paste0("exp3_", output_label, "_diagnostic_tests.csv")
    ),
    row.names = FALSE
  )

  grDevices::png(
    filename = file.path(
      figure_dir,
      paste0("exp3_", output_label, "_diagnostics.png")
    ),
    width = 2200,
    height = 1400,
    res = 200
  )

  plot(simulation)
  grDevices::dev.off()

  diagnostic_tests
}

add_response_scale <- function(prediction_table) {
  if (!"response" %in% names(prediction_table)) {
    stop("emmeans did not return a response-scale column named 'response'.")
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

  if (length(lower_column) != 1L || length(upper_column) != 1L) {
    stop("Could not identify prediction confidence-interval columns.")
  }

  prediction_table$predicted_rating <-
    100 * prediction_table$response
  prediction_table$lower_rating <-
    100 * prediction_table[[lower_column]]
  prediction_table$upper_rating <-
    100 * prediction_table[[upper_column]]

  prediction_table
}


# ----------------------------------------------------------------
# 7. Affirmative and negative motion-event models
# ----------------------------------------------------------------

fit_motion_model <- function(
  analysis_data,
  polarity_value,
  expected_rows
) {
  model_data <- analysis_data[
    analysis_data$polarity == polarity_value &
      analysis_data$event_type != "no_motion",
    ,
    drop = FALSE
  ]

  model_data <- droplevels(model_data)

  # These level orders reproduce the Chapter 3 legacy script.
  model_data$gesture_type <- factor(
    model_data$gesture_type,
    levels = c("path", "manner", "conflated")
  )
  model_data$event_type <- factor(
    model_data$event_type,
    levels = c("path", "manner", "conflated")
  )
  model_data$participant_id <- factor(model_data$participant_id)
  model_data$scenario <- factor(
    model_data$scenario,
    levels = expected_scenarios
  )

  if (nrow(model_data) != expected_rows) {
    stop(
      "Expected ",
      expected_rows,
      " ",
      polarity_value,
      " motion-event observations but found ",
      nrow(model_data),
      "."
    )
  }

  if (nlevels(model_data$participant_id) != 142L) {
    stop(
      "The ",
      polarity_value,
      " model should contain all 142 included participants."
    )
  }

  contrasts(model_data$gesture_type) <- stats::contr.sum(3)
  contrasts(model_data$event_type) <- stats::contr.sum(3)

  model_data$rating_scaled <- scale_beta_response(model_data$rating)

  model <- glmmTMB::glmmTMB(
    rating_scaled ~
      gesture_type * event_type +
      (1 | participant_id) +
      (1 | scenario),
    family = glmmTMB::beta_family(link = "logit"),
    data = model_data
  )

  if (!isTRUE(model$sdr$pdHess)) {
    stop(
      "The ",
      polarity_value,
      " model has a non-positive-definite Hessian."
    )
  }

  if (model$fit$convergence != 0L) {
    stop(
      "The ",
      polarity_value,
      " model did not return convergence code 0."
    )
  }

  saveRDS(
    model,
    file.path(
      model_dir,
      paste0("exp3_", polarity_value, "_model.rds")
    )
  )

  write.csv(
    fixed_effect_table(model),
    file.path(
      table_dir,
      paste0("exp3_", polarity_value, "_fixed_effects.csv")
    ),
    row.names = FALSE
  )

  capture.output(
    summary(model),
    file = file.path(
      table_dir,
      paste0("exp3_", polarity_value, "_model_summary.txt")
    )
  )

  omnibus <- as.data.frame(emmeans::joint_tests(model))
  write.csv(
    omnibus,
    file.path(
      table_dir,
      paste0("exp3_", polarity_value, "_omnibus_tests.csv")
    ),
    row.names = FALSE
  )

  gesture_by_event <- emmeans::emmeans(
    model,
    ~ gesture_type | event_type
  )

  gesture_contrasts <- as.data.frame(
    summary(
      emmeans::contrast(
        gesture_by_event,
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
      paste0(
        "exp3_",
        polarity_value,
        "_gesture_contrasts_within_event.csv"
      )
    ),
    row.names = FALSE
  )

  event_by_gesture <- emmeans::emmeans(
    model,
    ~ event_type | gesture_type
  )

  event_contrasts <- as.data.frame(
    summary(
      emmeans::contrast(
        event_by_gesture,
        method = "pairwise",
        adjust = "holm"
      ),
      infer = c(TRUE, TRUE)
    )
  )

  write.csv(
    event_contrasts,
    file.path(
      table_dir,
      paste0(
        "exp3_",
        polarity_value,
        "_event_contrasts_within_gesture.csv"
      )
    ),
    row.names = FALSE
  )

  predictions <- as.data.frame(
    summary(
      emmeans::emmeans(
        model,
        ~ gesture_type * event_type,
        type = "response"
      ),
      infer = c(TRUE, FALSE),
      type = "response"
    )
  )

  predictions <- add_response_scale(predictions)
  predictions$polarity <- polarity_value

  diagnostic_tests <- run_diagnostics(model, polarity_value)

  list(
    model = model,
    predictions = predictions,
    diagnostics = diagnostic_tests,
    observations = nrow(model_data),
    participants = nlevels(model_data$participant_id),
    zero_ratings = sum(model_data$rating == 0),
    hundred_ratings = sum(model_data$rating == 100)
  )
}

set.seed(123)
affirmative_results <- fit_motion_model(
  included_data,
  polarity_value = "affirmative",
  expected_rows = 635L
)

set.seed(123)
negative_results <- fit_motion_model(
  included_data,
  polarity_value = "negative",
  expected_rows = 643L
)


# ----------------------------------------------------------------
# 8. No-motion model
# ----------------------------------------------------------------

no_motion_data <- included_data[
  included_data$event_type == "no_motion",
  ,
  drop = FALSE
]

no_motion_data <- droplevels(no_motion_data)

if (nrow(no_motion_data) != 426L) {
  stop(
    "Expected 426 no-motion observations but found ",
    nrow(no_motion_data),
    "."
  )
}

no_motion_data$participant_id <- factor(no_motion_data$participant_id)
no_motion_data$polarity <- factor(
  no_motion_data$polarity,
  levels = c("affirmative", "negative")
)

# This gesture order reproduces the no-motion model in the legacy script.
no_motion_data$gesture_type <- factor(
  no_motion_data$gesture_type,
  levels = c("conflated", "path", "manner")
)
no_motion_data$scenario <- factor(
  no_motion_data$scenario,
  levels = expected_scenarios
)

if (nlevels(no_motion_data$participant_id) != 142L) {
  stop("The no-motion model should contain all 142 participants.")
}

contrasts(no_motion_data$polarity) <- stats::contr.sum(2)
contrasts(no_motion_data$gesture_type) <- stats::contr.sum(3)

no_motion_data$rating_scaled <- scale_beta_response(no_motion_data$rating)

no_motion_model <- glmmTMB::glmmTMB(
  rating_scaled ~
    gesture_type * polarity +
    (1 | participant_id) +
    (1 | scenario),
  family = glmmTMB::beta_family(link = "logit"),
  data = no_motion_data
)

if (!isTRUE(no_motion_model$sdr$pdHess)) {
  stop("The no-motion model has a non-positive-definite Hessian.")
}

if (no_motion_model$fit$convergence != 0L) {
  stop("The no-motion model did not return convergence code 0.")
}

saveRDS(
  no_motion_model,
  file.path(model_dir, "exp3_no_motion_model.rds")
)

write.csv(
  fixed_effect_table(no_motion_model),
  file.path(table_dir, "exp3_no_motion_fixed_effects.csv"),
  row.names = FALSE
)

capture.output(
  summary(no_motion_model),
  file = file.path(table_dir, "exp3_no_motion_model_summary.txt")
)

write.csv(
  as.data.frame(emmeans::joint_tests(no_motion_model)),
  file.path(table_dir, "exp3_no_motion_omnibus_tests.csv"),
  row.names = FALSE
)

no_motion_gesture_means <- emmeans::emmeans(
  no_motion_model,
  ~ gesture_type | polarity
)

no_motion_gesture_contrasts <- as.data.frame(
  summary(
    emmeans::contrast(
      no_motion_gesture_means,
      method = "pairwise",
      adjust = "holm"
    ),
    infer = c(TRUE, TRUE)
  )
)

write.csv(
  no_motion_gesture_contrasts,
  file.path(
    table_dir,
    "exp3_no_motion_gesture_contrasts_within_polarity.csv"
  ),
  row.names = FALSE
)

no_motion_polarity_means <- emmeans::emmeans(
  no_motion_model,
  ~ polarity | gesture_type
)

no_motion_polarity_contrasts <- as.data.frame(
  summary(
    emmeans::contrast(
      no_motion_polarity_means,
      method = "pairwise",
      adjust = "holm"
    ),
    infer = c(TRUE, TRUE)
  )
)

write.csv(
  no_motion_polarity_contrasts,
  file.path(
    table_dir,
    "exp3_no_motion_polarity_contrasts_within_gesture.csv"
  ),
  row.names = FALSE
)

no_motion_predictions <- as.data.frame(
  summary(
    emmeans::emmeans(
      no_motion_model,
      ~ gesture_type * polarity,
      type = "response"
    ),
    infer = c(TRUE, FALSE),
    type = "response"
  )
)

no_motion_predictions <- add_response_scale(no_motion_predictions)

write.csv(
  no_motion_predictions,
  file.path(table_dir, "exp3_no_motion_model_predictions.csv"),
  row.names = FALSE
)

no_motion_diagnostics <- run_diagnostics(
  no_motion_model,
  "no_motion"
)


# ----------------------------------------------------------------
# 9. Save combined motion-event predictions and figures
# ----------------------------------------------------------------

motion_predictions <- rbind(
  affirmative_results$predictions,
  negative_results$predictions
)

write.csv(
  motion_predictions,
  file.path(table_dir, "exp3_motion_model_predictions.csv"),
  row.names = FALSE
)

motion_plot_data <- motion_predictions
motion_plot_data$polarity <- factor(
  motion_plot_data$polarity,
  levels = c("affirmative", "negative"),
  labels = c("Affirmative", "Negative")
)
motion_plot_data$event_type <- factor(
  motion_plot_data$event_type,
  levels = c("path", "manner", "conflated"),
  labels = c("Path event", "Manner event", "Conflated event")
)
motion_plot_data$gesture_type <- factor(
  motion_plot_data$gesture_type,
  levels = c("path", "manner", "conflated"),
  labels = c("Path gesture", "Manner gesture", "Conflated gesture")
)

motion_prediction_plot <- ggplot2::ggplot(
  motion_plot_data,
  ggplot2::aes(
    x = event_type,
    y = predicted_rating,
    colour = gesture_type,
    group = gesture_type
  )
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lower_rating,
      ymax = upper_rating
    ),
    width = 0.08,
    linewidth = 0.7
  ) +
  ggplot2::facet_wrap(~ polarity) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  ggplot2::labs(
    title = "Experiment 3: beta-model predictions",
    subtitle = "Motion-event trials; 95% confidence intervals",
    x = "Event type",
    y = "Model-predicted rating",
    colour = "Gesture type"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(figure_dir, "exp3_motion_model_predictions.png"),
  plot = motion_prediction_plot,
  width = 11,
  height = 6.5,
  dpi = 300
)

no_motion_plot_data <- no_motion_predictions
no_motion_plot_data$gesture_type <- factor(
  no_motion_plot_data$gesture_type,
  levels = c("conflated", "path", "manner"),
  labels = c("Conflated gesture", "Path gesture", "Manner gesture")
)
no_motion_plot_data$polarity <- factor(
  no_motion_plot_data$polarity,
  levels = c("affirmative", "negative"),
  labels = c("Affirmative", "Negative")
)

no_motion_prediction_plot <- ggplot2::ggplot(
  no_motion_plot_data,
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
    linewidth = 0.8,
    position = ggplot2::position_dodge(width = 0.25)
  ) +
  ggplot2::geom_point(
    size = 3.5,
    position = ggplot2::position_dodge(width = 0.25)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  ggplot2::labs(
    title = "Experiment 3: no-motion model predictions",
    subtitle = "All gesture types retained; 95% confidence intervals",
    x = "Gesture type",
    y = "Model-predicted rating",
    colour = "Polarity"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(figure_dir, "exp3_no_motion_model_predictions.png"),
  plot = no_motion_prediction_plot,
  width = 9,
  height = 6,
  dpi = 300
)


# ----------------------------------------------------------------
# 10. Metadata and session information
# ----------------------------------------------------------------

model_metadata <- data.frame(
  model = c(
    "affirmative_motion_events",
    "negative_motion_events",
    "no_motion"
  ),
  observations = c(
    affirmative_results$observations,
    negative_results$observations,
    nrow(no_motion_data)
  ),
  participants = c(
    affirmative_results$participants,
    negative_results$participants,
    nlevels(no_motion_data$participant_id)
  ),
  exact_zero_ratings = c(
    affirmative_results$zero_ratings,
    negative_results$zero_ratings,
    sum(no_motion_data$rating == 0)
  ),
  exact_hundred_ratings = c(
    affirmative_results$hundred_ratings,
    negative_results$hundred_ratings,
    sum(no_motion_data$rating == 100)
  ),
  positive_definite_hessian = c(
    isTRUE(affirmative_results$model$sdr$pdHess),
    isTRUE(negative_results$model$sdr$pdHess),
    isTRUE(no_motion_model$sdr$pdHess)
  ),
  convergence_code = c(
    affirmative_results$model$fit$convergence,
    negative_results$model$fit$convergence,
    no_motion_model$fit$convergence
  ),
  family = rep("beta", 3),
  link = rep("logit", 3),
  endpoint_transformation = rep("epsilon = 1e-6", 3),
  stringsAsFactors = FALSE
)

write.csv(
  model_metadata,
  file.path(table_dir, "exp3_model_metadata.csv"),
  row.names = FALSE
)

capture.output(
  sessionInfo(),
  file = file.path(table_dir, "exp3_session_info.txt")
)


# ----------------------------------------------------------------
# 11. Completion message
# ----------------------------------------------------------------

cat("\n============================================\n")
cat("Experiment 3 analysis complete\n")
cat("============================================\n\n")

cat("Included participants: ", included_participants, "\n", sep = "")
cat("Excluded participants: ", excluded_participants, "\n", sep = "")
cat(
  "Affirmative motion-event observations: ",
  affirmative_results$observations,
  "\n",
  sep = ""
)
cat(
  "Negative motion-event observations: ",
  negative_results$observations,
  "\n",
  sep = ""
)
cat("No-motion observations: ", nrow(no_motion_data), "\n", sep = "")
cat(
  "Positive-definite Hessians: ",
  all(model_metadata$positive_definite_hessian),
  "\n",
  sep = ""
)

cat("\nFigures written to: ", figure_dir, "\n", sep = "")
cat("Tables written to: ", table_dir, "\n", sep = "")
cat("Models written to: ", model_dir, "\n", sep = "")
