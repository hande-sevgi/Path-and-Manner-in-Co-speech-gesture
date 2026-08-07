# ============================================================
# Experiment 2 analysis: Written English
# Manner before path
# ============================================================
#
# Run from the repository root:
#
# source("code/scripts/05_analyze_experiment_2.R")
# ============================================================


# ------------------------------------------------------------
# 1. Required packages
# ------------------------------------------------------------

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
    paste(
      "install.packages(c(",
      paste(
        paste0('"', missing_packages, '"'),
        collapse = ", "
      ),
      "))"
    )
  )
}


# ------------------------------------------------------------
# 2. File paths
# ------------------------------------------------------------

input_file <- file.path(
  "data",
  "processed",
  "exp2_written_manner_path.csv"
)

figure_directory <- file.path(
  "output",
  "figures",
  "exp2"
)

table_directory <- file.path(
  "output",
  "tables",
  "exp2"
)

model_directory <- file.path(
  "output",
  "models",
  "exp2"
)

directories <- c(
  figure_directory,
  table_directory,
  model_directory
)

for (directory in directories) {
  if (!dir.exists(directory)) {
    dir.create(
      directory,
      recursive = TRUE
    )
  }
}


# ------------------------------------------------------------
# 3. Read and validate processed data
# ------------------------------------------------------------

if (!file.exists(input_file)) {
  stop(
    "Cannot find ",
    input_file,
    ". Run 02_prepare_processed_data.R first."
  )
}

experiment_data <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
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
  "included",
  "exclusion_reason"
)

missing_columns <- setdiff(
  required_columns,
  names(experiment_data)
)

if (length(missing_columns) > 0L) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if ("Response_ID" %in% names(experiment_data)) {
  stop(
    "Processed data unexpectedly contain Response_ID."
  )
}

if (nrow(experiment_data) != 1440L) {
  stop(
    "Expected 1,440 processed rows but found ",
    nrow(experiment_data),
    "."
  )
}

if (!identical(unique(experiment_data$study_id), "exp2")) {
  stop("The processed file does not contain study_id = 'exp2'.")
}

if (!is.logical(experiment_data$included)) {
  experiment_data$included <-
    tolower(
      as.character(experiment_data$included)
    ) == "true"
}

included_data <- experiment_data[
  experiment_data$included,
]

if (nrow(included_data) != 1368L) {
  stop(
    "Expected 1,368 included rows but found ",
    nrow(included_data),
    "."
  )
}

if (any(
  included_data$modifier_order != "manner_before_path"
)) {
  stop(
    "Unexpected modifier order detected in Experiment 2."
  )
}

included_participants <- length(
  unique(included_data$participant_id)
)

excluded_participants <- length(
  unique(
    experiment_data$participant_id[
      !experiment_data$included
    ]
  )
)

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

if (any(included_data$rating < 0 |
        included_data$rating > 100)) {
  stop("Ratings outside the 0–100 range detected.")
}

message(
  "Experiment 2 data validated: ",
  included_participants,
  " included and ",
  excluded_participants,
  " excluded participants."
)


# ------------------------------------------------------------
# 4. Prepare variables
# ------------------------------------------------------------

included_data$polarity <- factor(
  included_data$polarity,
  levels = c(
    "affirmative",
    "negative"
  )
)

included_data$sentence_type <- factor(
  included_data$sentence_type,
  levels = c(
    "conflated",
    "path",
    "manner"
  )
)

included_data$event_type <- factor(
  included_data$event_type,
  levels = c(
    "conflated",
    "path",
    "manner",
    "no_motion"
  )
)

included_data$scenario <- factor(
  included_data$scenario,
  levels = c(
    "leaf",
    "car",
    "plank",
    "chair",
    "paper"
  )
)

included_data$participant_id <- factor(
  included_data$participant_id
)

if (
  anyNA(included_data$polarity) ||
    anyNA(included_data$sentence_type) ||
    anyNA(included_data$event_type) ||
    anyNA(included_data$scenario)
) {
  stop("Unexpected factor values were detected.")
}

epsilon <- 1e-6

included_data$rating_scaled <-
  (included_data$rating / 100) *
  (1 - 2 * epsilon) +
  epsilon


# ------------------------------------------------------------
# 5. Sample-flow table
# ------------------------------------------------------------

sample_flow <- data.frame(
  stage = c(
    "Input sample",
    "Excluded",
    "Included in analysis"
  ),
  participants = c(
    length(unique(experiment_data$participant_id)),
    excluded_participants,
    included_participants
  ),
  stringsAsFactors = FALSE
)

write.csv(
  sample_flow,
  file.path(
    table_directory,
    "exp2_sample_flow.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 6. Descriptive statistics
# ------------------------------------------------------------

group_variables <- c(
  "polarity",
  "sentence_type",
  "event_type"
)

mean_table <- aggregate(
  included_data["rating"],
  included_data[group_variables],
  mean,
  na.rm = TRUE
)

names(mean_table)[
  names(mean_table) == "rating"
] <- "mean_rating"

sd_table <- aggregate(
  included_data["rating"],
  included_data[group_variables],
  sd,
  na.rm = TRUE
)

names(sd_table)[
  names(sd_table) == "rating"
] <- "sd_rating"

count_table <- aggregate(
  included_data["rating"],
  included_data[group_variables],
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
      by = group_variables,
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
  1.96 *
  descriptive_summary$standard_error

descriptive_summary <- descriptive_summary[
  order(
    descriptive_summary$polarity,
    descriptive_summary$sentence_type,
    descriptive_summary$event_type
  ),
]

write.csv(
  descriptive_summary,
  file.path(
    table_directory,
    "exp2_descriptive_summary.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 7. Raw-rating figure
# ------------------------------------------------------------

raw_plot <- ggplot2::ggplot(
  included_data,
  ggplot2::aes(
    x = event_type,
    y = rating,
    color = sentence_type,
    fill = sentence_type
  )
) +
  ggplot2::geom_boxplot(
    alpha = 0.35,
    outlier.alpha = 0.15,
    position = ggplot2::position_dodge(
      width = 0.75
    )
  ) +
  ggplot2::facet_wrap(
    ~ polarity
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  ggplot2::labs(
    x = "Event type",
    y = "Rating",
    color = "Modification type",
    fill = "Modification type"
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figure_directory,
    "exp2_raw_ratings.png"
  ),
  plot = raw_plot,
  width = 10,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------
# 8. Model helper functions
# ------------------------------------------------------------

save_fixed_effects <- function(
  model,
  output_path
) {

  coefficient_matrix <-
    summary(model)$coefficients$cond

  coefficient_table <- data.frame(
    term = rownames(coefficient_matrix),
    coefficient_matrix,
    row.names = NULL,
    check.names = FALSE
  )

  write.csv(
    coefficient_table,
    output_path,
    row.names = FALSE
  )
}


save_sentence_contrasts <- function(
  model,
  output_path
) {

  estimated_means <- emmeans::emmeans(
    model,
    ~ sentence_type | event_type
  )

  contrast_table <- as.data.frame(
    summary(
      pairs(
        estimated_means,
        adjust = "holm"
      ),
      infer = c(TRUE, TRUE)
    )
  )

  contrast_table$p_value_adjustment <- "Holm"
  contrast_table$confidence_interval_adjustment <-
    "Bonferroni"

  write.csv(
    contrast_table,
    output_path,
    row.names = FALSE
  )
}


extract_dharma_test <- function(
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


fit_polarity_model <- function(
  analysis_data,
  polarity_value,
  output_label
) {

  model_data <- analysis_data[
    analysis_data$polarity == polarity_value &
      analysis_data$event_type != "no_motion",
  ]

  model_data <- droplevels(
    model_data
  )

  model_data$sentence_type <- factor(
    model_data$sentence_type,
    levels = c(
      "conflated",
      "path",
      "manner"
    )
  )

  model_data$event_type <- factor(
    model_data$event_type,
    levels = c(
      "conflated",
      "path",
      "manner"
    )
  )

  contrasts(
    model_data$sentence_type
  ) <- contr.treatment(
    3,
    base = 1
  )

  contrasts(
    model_data$event_type
  ) <- contr.treatment(
    3,
    base = 1
  )

  model <- glmmTMB::glmmTMB(
    rating_scaled ~
      sentence_type * event_type +
      (1 | participant_id) +
      (1 | scenario),
    family = glmmTMB::beta_family(
      link = "logit"
    ),
    data = model_data
  )

  if (!isTRUE(model$sdr$pdHess)) {
    warning(
      output_label,
      " model has a non-positive-definite Hessian."
    )
  }

  saveRDS(
    model,
    file.path(
      model_directory,
      paste0(
        "exp2_",
        output_label,
        "_model.rds"
      )
    )
  )

  save_fixed_effects(
    model,
    file.path(
      table_directory,
      paste0(
        "exp2_",
        output_label,
        "_fixed_effects.csv"
      )
    )
  )

  capture.output(
    summary(model),
    file = file.path(
      table_directory,
      paste0(
        "exp2_",
        output_label,
        "_model_summary.txt"
      )
    )
  )

  save_sentence_contrasts(
    model,
    file.path(
      table_directory,
      paste0(
        "exp2_",
        output_label,
        "_sentence_contrasts.csv"
      )
    )
  )

  estimated_means <- emmeans::emmeans(
    model,
    ~ sentence_type * event_type,
    type = "response"
  )

  prediction_table <- as.data.frame(
    summary(
      estimated_means,
      type = "response"
    )
  )

  prediction_table$polarity <-
    polarity_value

  set.seed(123)

  simulation <- DHARMa::simulateResiduals(
    fittedModel = model,
    n = 1000,
    plot = FALSE
  )

  diagnostic_tests <- rbind(
    extract_dharma_test(
      DHARMa::testUniformity(
        simulation,
        plot = FALSE
      ),
      "uniformity"
    ),
    extract_dharma_test(
      DHARMa::testDispersion(
        simulation,
        plot = FALSE
      ),
      "dispersion"
    ),
    extract_dharma_test(
      DHARMa::testOutliers(
        simulation,
        plot = FALSE
      ),
      "outliers"
    )
  )

  write.csv(
    diagnostic_tests,
    file.path(
      table_directory,
      paste0(
        "exp2_",
        output_label,
        "_diagnostic_tests.csv"
      )
    ),
    row.names = FALSE
  )

  grDevices::png(
    filename = file.path(
      figure_directory,
      paste0(
        "exp2_",
        output_label,
        "_diagnostics.png"
      )
    ),
    width = 1800,
    height = 1400,
    res = 180
  )

  plot(simulation)

  grDevices::dev.off()

  list(
    model = model,
    predictions = prediction_table,
    model_rows = nrow(model_data),
    model_participants = length(
      unique(model_data$participant_id)
    )
  )
}


# ------------------------------------------------------------
# 9. Fit affirmative and negative models
# ------------------------------------------------------------

set.seed(123)

affirmative_results <- fit_polarity_model(
  analysis_data = included_data,
  polarity_value = "affirmative",
  output_label = "affirmative"
)

set.seed(123)

negative_results <- fit_polarity_model(
  analysis_data = included_data,
  polarity_value = "negative",
  output_label = "negative"
)

if (affirmative_results$model_rows != 517L) {
  stop(
    "Expected 517 affirmative motion-event observations but found ",
    affirmative_results$model_rows,
    "."
  )
}

if (negative_results$model_rows != 509L) {
  stop(
    "Expected 509 negative motion-event observations but found ",
    negative_results$model_rows,
    "."
  )
}

if (
  affirmative_results$model_participants != 114L ||
    negative_results$model_participants != 114L
) {
  stop(
    "Each polarity model should contain all 114 included participants."
  )
}


# ------------------------------------------------------------
# 10. Save model predictions
# ------------------------------------------------------------

prediction_table <- rbind(
  affirmative_results$predictions,
  negative_results$predictions
)

if (!"response" %in% names(prediction_table)) {
  stop(
    "Could not identify the model prediction column."
  )
}

lower_column <- grep(
  "LCL|lower.CL",
  names(prediction_table),
  value = TRUE
)[1]

upper_column <- grep(
  "UCL|upper.CL",
  names(prediction_table),
  value = TRUE
)[1]

if (
  is.na(lower_column) ||
  is.na(upper_column)
) {
  stop(
    "Could not identify confidence-interval columns."
  )
}

prediction_table$predicted_rating <-
  prediction_table$response * 100

prediction_table$lower_rating <-
  prediction_table[[lower_column]] * 100

prediction_table$upper_rating <-
  prediction_table[[upper_column]] * 100

write.csv(
  prediction_table,
  file.path(
    table_directory,
    "exp2_model_predictions.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 11. Model-prediction figure
# ------------------------------------------------------------

prediction_plot <- ggplot2::ggplot(
  prediction_table,
  ggplot2::aes(
    x = event_type,
    y = predicted_rating,
    color = sentence_type,
    group = sentence_type
  )
) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(
      width = 0.35
    ),
    size = 2.5
  ) +
  ggplot2::geom_line(
    position = ggplot2::position_dodge(
      width = 0.35
    )
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lower_rating,
      ymax = upper_rating
    ),
    position = ggplot2::position_dodge(
      width = 0.35
    ),
    width = 0.15
  ) +
  ggplot2::facet_wrap(
    ~ polarity
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  ggplot2::labs(
    x = "Event type",
    y = "Model-predicted rating",
    color = "Modification type"
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figure_directory,
    "exp2_model_predictions.png"
  ),
  plot = prediction_plot,
  width = 10,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------
# 12. Save model metadata and R environment
# ------------------------------------------------------------

model_metadata <- data.frame(
  model = c(
    "affirmative",
    "negative"
  ),
  observations = c(
    affirmative_results$model_rows,
    negative_results$model_rows
  ),
  participants = c(
    affirmative_results$model_participants,
    negative_results$model_participants
  ),
  positive_definite_hessian = c(
    isTRUE(
      affirmative_results$model$sdr$pdHess
    ),
    isTRUE(
      negative_results$model$sdr$pdHess
    )
  ),
  convergence_code = c(
    affirmative_results$model$fit$convergence,
    negative_results$model$fit$convergence
  ),
  family = c("beta", "beta"),
  link = c("logit", "logit"),
  modifier_order = c(
    "manner_before_path",
    "manner_before_path"
  ),
  sentence_reference = c(
    "conflated",
    "conflated"
  ),
  sentence_level_2 = c("path", "path"),
  sentence_level_3 = c("manner", "manner"),
  event_reference = c(
    "conflated",
    "conflated"
  ),
  event_level_2 = c("path", "path"),
  event_level_3 = c("manner", "manner"),
  stringsAsFactors = FALSE
)

write.csv(
  model_metadata,
  file.path(
    table_directory,
    "exp2_model_metadata.csv"
  ),
  row.names = FALSE
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    model_directory,
    "session-info.txt"
  )
)


# ------------------------------------------------------------
# 13. Final summary
# ------------------------------------------------------------

message("")
message("============================================")
message("Experiment 2 analysis complete")
message("============================================")
message("")
message(
  "Included participants: ",
  included_participants
)
message(
  "Excluded participants: ",
  excluded_participants
)
message(
  "Affirmative model observations: ",
  affirmative_results$model_rows
)
message(
  "Negative model observations: ",
  negative_results$model_rows
)
message("")
message(
  "Figures written to: ",
  figure_directory
)
message(
  "Tables written to: ",
  table_directory
)
message(
  "Models written to: ",
  model_directory
)
