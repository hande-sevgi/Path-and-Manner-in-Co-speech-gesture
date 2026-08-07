# ============================================================
# Prepare Chapter 3 processed datasets
# ============================================================
#
# This script:
#   1. Reads the five organized input datasets.
#   2. Decodes compact Trial labels.
#   3. Creates consistent experimental variables.
#   4. Replaces platform IDs with study-specific IDs.
#   5. Reproduces documented participant exclusions.
#   6. Writes processed datasets to data/processed/.
#
# Input files are never modified.
#
# Run from the repository root:
#
# source("code/scripts/02_prepare_processed_data.R")
# ============================================================


# ------------------------------------------------------------
# 1. Directories
# ------------------------------------------------------------

input_directory <- file.path("data", "input")
processed_directory <- file.path("data", "processed")

if (!dir.exists(input_directory)) {
  stop(
    "Cannot find data/input/. ",
    "Open the repository as an RStudio Project."
  )
}

if (!dir.exists(processed_directory)) {
  dir.create(
    processed_directory,
    recursive = TRUE
  )
}


# ------------------------------------------------------------
# 2. Code mappings
# ------------------------------------------------------------

polarity_map <- c(
  Aff = "affirmative",
  Neg = "negative"
)

modification_map <- c(
  P = "path",
  M = "manner",
  C = "conflated"
)

gesture_map <- c(
  P = "path",
  M = "manner",
  C = "conflated",
  `0` = "none"
)

event_map <- c(
  P = "path",
  M = "manner",
  C = "conflated",
  `0` = "no_motion"
)

scenario_map <- c(
  A = "leaf",
  B = "car",
  C = "paper",
  D = "plank",
  E = "chair"
)

order_map <- c(
  P_M = "path_before_manner",
  M_P = "manner_before_path"
)


# ------------------------------------------------------------
# 3. Helper: safely decode codes
# ------------------------------------------------------------

decode_codes <- function(values, mapping, variable_name, study_id) {

  decoded <- unname(mapping[values])

  if (any(is.na(decoded))) {
    invalid_values <- unique(values[is.na(decoded)])

    stop(
      "[", study_id, "] Invalid ",
      variable_name, " codes: ",
      paste(invalid_values, collapse = ", ")
    )
  }

  decoded
}


# ------------------------------------------------------------
# 4. Helper: add an exclusion reason
# ------------------------------------------------------------

add_exclusion_reason <- function(
  current_reason,
  condition,
  new_reason
) {

  current_reason[condition & current_reason == ""] <-
    new_reason

  current_reason[condition & current_reason != "" &
                   current_reason != new_reason] <-
    paste0(
      current_reason[
        condition &
          current_reason != "" &
          current_reason != new_reason
      ],
      ";",
      new_reason
    )

  current_reason
}


# ------------------------------------------------------------
# 5. Main preparation function
# ------------------------------------------------------------

prepare_study <- function(
  study_id,
  input_file,
  output_file,
  trial_schema,
  expected_included_participants,
  apply_sd_exclusion = FALSE,
  feedback_exclusion_ids = character(),
  fixed_modifier_order = NA_character_
) {

  input_path <- file.path(
    input_directory,
    input_file
  )

  output_path <- file.path(
    processed_directory,
    output_file
  )

  message("")
  message("Preparing ", study_id, ": ", input_file)

  if (!file.exists(input_path)) {
    stop(
      "[", study_id, "] Missing input file: ",
      input_path
    )
  }

  data <- read.csv(
    input_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  required_columns <- c(
    "Response_ID",
    "Trial",
    "Rating"
  )

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "[", study_id, "] Missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  # ----------------------------------------------------------
  # Parse the compact trial code
  # ----------------------------------------------------------

  trial_components <- strsplit(
    trimws(data$Trial),
    "\\s+"
  )

  expected_parts <- switch(
    trial_schema,
    written = 4L,
    baseline_no_gesture = 3L,
    baseline_no_event = 3L,
    gesture = 4L,
    stop("Unknown trial schema: ", trial_schema)
  )

  if (any(lengths(trial_components) != expected_parts)) {
    stop(
      "[", study_id,
      "] Unexpected Trial structure detected."
    )
  }

  trial_matrix <- do.call(
    rbind,
    trial_components
  )

  polarity_code <- trial_matrix[, 1]
  scenario_token <- trial_matrix[, expected_parts]

  polarity <- decode_codes(
    polarity_code,
    polarity_map,
    "polarity",
    study_id
  )

  scenario_code <- substr(
    scenario_token,
    1L,
    1L
  )

  scenario <- decode_codes(
    scenario_code,
    scenario_map,
    "scenario",
    study_id
  )

  # Preserve the complete suffix without assigning an
  # unverified scientific meaning to its components.
  scenario_suffix <- sub(
    "^[A-E]_?",
    "",
    scenario_token
  )

  sentence_type <- rep(
    NA_character_,
    nrow(data)
  )

  gesture_type <- rep(
    NA_character_,
    nrow(data)
  )

  event_type <- rep(
    NA_character_,
    nrow(data)
  )

  if (trial_schema == "written") {

    sentence_type <- decode_codes(
      trial_matrix[, 2],
      modification_map,
      "sentence type",
      study_id
    )

    event_type <- decode_codes(
      trial_matrix[, 3],
      event_map,
      "event type",
      study_id
    )
  }

  if (trial_schema == "baseline_no_gesture") {

    event_type <- decode_codes(
      trial_matrix[, 2],
      event_map,
      "event type",
      study_id
    )
  }

  if (trial_schema == "baseline_no_event") {

    gesture_type <- decode_codes(
      trial_matrix[, 2],
      gesture_map,
      "gesture type",
      study_id
    )
  }

  if (trial_schema == "gesture") {

    gesture_type <- decode_codes(
      trial_matrix[, 2],
      gesture_map,
      "gesture type",
      study_id
    )

    event_type <- decode_codes(
      trial_matrix[, 3],
      event_map,
      "event type",
      study_id
    )
  }


  # ----------------------------------------------------------
  # Decode modifier order
  # ----------------------------------------------------------

  modifier_order <- rep(
    fixed_modifier_order,
    nrow(data)
  )

  if (
    trial_schema == "written" &&
      "Order" %in% names(data)
  ) {

    modifier_order <- decode_codes(
      data$Order,
      order_map,
      "modifier order",
      study_id
    )
  }


  # ----------------------------------------------------------
  # Create study-specific participant IDs
  # ----------------------------------------------------------

  source_participant_ids <- sort(
    unique(data$Response_ID)
  )

  anonymous_participant_ids <- paste0(
    study_id,
    "_",
    sprintf(
      "%03d",
      seq_along(source_participant_ids)
    )
  )

  participant_lookup <- setNames(
    anonymous_participant_ids,
    source_participant_ids
  )

  participant_id <- unname(
    participant_lookup[data$Response_ID]
  )


  # ----------------------------------------------------------
  # Calculate participant response variability
  # ----------------------------------------------------------

  rating <- suppressWarnings(
    as.numeric(data$Rating)
  )

  if (any(is.na(rating))) {
    stop(
      "[", study_id,
      "] Missing or nonnumeric ratings detected."
    )
  }

  participant_sd_lookup <- tapply(
    rating,
    data$Response_ID,
    sd,
    na.rm = TRUE
  )

  participant_sd <- unname(
    participant_sd_lookup[data$Response_ID]
  )


  # ----------------------------------------------------------
  # Apply documented exclusions
  # ----------------------------------------------------------

  exclusion_reason <- rep(
    "",
    nrow(data)
  )

  exclusion_threshold <- NA_real_

  if (apply_sd_exclusion) {

    exclusion_threshold <- as.numeric(
      quantile(
        participant_sd_lookup,
        probabilities = 0.05,
        na.rm = TRUE,
        names = FALSE
      )
    )

    low_variability_ids <- names(
      participant_sd_lookup[
        participant_sd_lookup <
          exclusion_threshold
      ]
    )

    low_variability_rows <-
      data$Response_ID %in% low_variability_ids

    exclusion_reason <- add_exclusion_reason(
      current_reason = exclusion_reason,
      condition = low_variability_rows,
      new_reason = "low_response_variability"
    )
  }

  if (length(feedback_exclusion_ids) > 0L) {

    missing_feedback_ids <- setdiff(
      feedback_exclusion_ids,
      unique(data$Response_ID)
    )

    if (length(missing_feedback_ids) > 0L) {
      stop(
        "[", study_id,
        "] Feedback-exclusion IDs not found: ",
        paste(
          missing_feedback_ids,
          collapse = ", "
        )
      )
    }

    feedback_rows <-
      data$Response_ID %in%
      feedback_exclusion_ids

    exclusion_reason <- add_exclusion_reason(
      current_reason = exclusion_reason,
      condition = feedback_rows,
      new_reason = "reported_inattention"
    )
  }

  included <- exclusion_reason == ""

  exclusion_reason[exclusion_reason == ""] <- NA_character_


  # ----------------------------------------------------------
  # Construct the processed dataset
  # ----------------------------------------------------------

  processed_data <- data.frame(
    study_id = study_id,
    participant_id = participant_id,
    trial = data$Trial,
    rating = rating,
    polarity = polarity,
    sentence_type = sentence_type,
    gesture_type = gesture_type,
    event_type = event_type,
    scenario_code = scenario_code,
    scenario = scenario,
    scenario_suffix = scenario_suffix,
    modifier_order = modifier_order,
    participant_sd = participant_sd,
    exclusion_threshold = rep(
      exclusion_threshold,
      nrow(data)
    ),
    included = included,
    exclusion_reason = exclusion_reason,
    stringsAsFactors = FALSE
  )


  # ----------------------------------------------------------
  # Validate processed results
  # ----------------------------------------------------------

  if (nrow(processed_data) != nrow(data)) {
    stop(
      "[", study_id,
      "] Row count changed during processing."
    )
  }

  if (any(is.na(processed_data$participant_id))) {
    stop(
      "[", study_id,
      "] Missing anonymous participant IDs."
    )
  }

  if (any(is.na(processed_data$polarity))) {
    stop(
      "[", study_id,
      "] Missing decoded polarity values."
    )
  }

  if (any(is.na(processed_data$scenario))) {
    stop(
      "[", study_id,
      "] Missing decoded scenario values."
    )
  }

  included_participant_count <- length(
    unique(
      processed_data$participant_id[
        processed_data$included
      ]
    )
  )

  if (
    included_participant_count !=
      expected_included_participants
  ) {
    stop(
      "[", study_id, "] Expected ",
      expected_included_participants,
      " included participants but found ",
      included_participant_count,
      "."
    )
  }


  # ----------------------------------------------------------
  # Write the processed dataset
  # ----------------------------------------------------------

  write.csv(
    processed_data,
    output_path,
    row.names = FALSE,
    na = ""
  )

  message(
    "Written: ",
    output_path
  )

  data.frame(
    study = study_id,
    input_rows = nrow(data),
    input_participants = length(
      unique(data$Response_ID)
    ),
    included_participants =
      included_participant_count,
    excluded_participants =
      length(unique(data$Response_ID)) -
      included_participant_count,
    output_file = output_file,
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# 6. Prepare all five studies
# ------------------------------------------------------------

exp1_feedback_exclusions <- c(
  "R_6AHzPTToSASboUX",
  "R_55GfGQA10UMwSdj"
)

processing_results <- list(

  prepare_study(
    study_id = "exp1",
    input_file = "WrittenEnglish_PathManner.csv",
    output_file = "exp1_written_path_manner.csv",
    trial_schema = "written",
    expected_included_participants = 140L,
    apply_sd_exclusion = TRUE,
    feedback_exclusion_ids =
      exp1_feedback_exclusions
  ),

  prepare_study(
    study_id = "exp2",
    input_file = "WrittenEnglish_MannerPath.csv",
    output_file = "exp2_written_manner_path.csv",
    trial_schema = "written",
    expected_included_participants = 114L,
    apply_sd_exclusion = TRUE,
    fixed_modifier_order =
      "manner_before_path"
  ),

  prepare_study(
    study_id = "baseline1",
    input_file = "Prelim_Gesture_NoGesture.csv",
    output_file = "baseline1_no_gesture.csv",
    trial_schema = "baseline_no_gesture",
    expected_included_participants = 40L,
    apply_sd_exclusion = FALSE
  ),

  prepare_study(
    study_id = "baseline2",
    input_file = "Prelim_Gesture_NoEvent.csv",
    output_file = "baseline2_no_event.csv",
    trial_schema = "baseline_no_event",
    expected_included_participants = 40L,
    apply_sd_exclusion = FALSE
  ),

  prepare_study(
    study_id = "exp3",
    input_file = "Gesture_2025.csv",
    output_file = "exp3_cospeech_gesture.csv",
    trial_schema = "gesture",
    expected_included_participants = 142L,
    apply_sd_exclusion = TRUE
  )
)


# ------------------------------------------------------------
# 7. Print processing summary
# ------------------------------------------------------------

processing_summary <- do.call(
  rbind,
  processing_results
)

rownames(processing_summary) <- NULL

message("")
message("============================================")
message("Processing summary")
message("============================================")
message("")

print(
  processing_summary,
  row.names = FALSE
)

message("")
message("All five processed datasets were created.")
message("The input files were not modified.")
