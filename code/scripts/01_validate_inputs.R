# Validate the five Chapter 3 input datasets
#
# Run this script from the repository root:
# source("code/scripts/01_validate_inputs.R")

input_directory <- file.path("data", "input")

if (!dir.exists(input_directory)) {
  stop(
    "Cannot find data/input/. ",
    "Open the repository as an RStudio project and run the script again."
  )
}

studies <- list(
  exp1 = list(
    file = "WrittenEnglish_PathManner.csv",
    required_columns = c("Response_ID", "Trial", "Rating", "Order"),
    expected_participants = 150L,
    trial_parts = 4L,
    second_codes = c("P", "M", "C"),
    third_codes = c("P", "M", "C", "0")
  ),
  exp2 = list(
    file = "WrittenEnglish_MannerPath.csv",
    required_columns = c("Response_ID", "Trial", "Rating"),
    expected_participants = 120L,
    trial_parts = 4L,
    second_codes = c("P", "M", "C"),
    third_codes = c("P", "M", "C", "0")
  ),
  baseline1 = list(
    file = "Prelim_Gesture_NoGesture.csv",
    required_columns = c("Response_ID", "Trial", "Rating"),
    expected_participants = 41L,
    trial_parts = 3L,
    second_codes = c("P", "M", "C", "0"),
    third_codes = NULL
  ),
  baseline2 = list(
    file = "Prelim_Gesture_NoEvent.csv",
    required_columns = c("Response_ID", "Trial", "Rating"),
    expected_participants = 40L,
    trial_parts = 3L,
    second_codes = c("P", "M", "C", "0"),
    third_codes = NULL
  ),
  exp3 = list(
    file = "Gesture_2025.csv",
    required_columns = c("Response_ID", "Trial", "Rating"),
    expected_participants = 150L,
    trial_parts = 4L,
    second_codes = c("P", "M", "C"),
    third_codes = c("P", "M", "C", "0")
  )
)

validate_study <- function(study_id, specification) {
  path <- file.path(input_directory, specification$file)

  if (!file.exists(path)) {
    stop("[", study_id, "] Missing file: ", path)
  }

  data <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  missing_columns <- setdiff(
    specification$required_columns,
    names(data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "[", study_id, "] Missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (any(is.na(data$Response_ID)) || any(data$Response_ID == "")) {
    stop("[", study_id, "] Missing participant identifiers detected.")
  }

  numeric_ratings <- suppressWarnings(as.numeric(data$Rating))

  if (any(is.na(numeric_ratings))) {
    stop("[", study_id, "] Missing or nonnumeric ratings detected.")
  }

  if (any(numeric_ratings < 0 | numeric_ratings > 100)) {
    stop("[", study_id, "] Ratings outside the 0–100 range detected.")
  }

  trials <- strsplit(trimws(data$Trial), "\\s+")

  incorrect_lengths <- lengths(trials) != specification$trial_parts

  if (any(incorrect_lengths)) {
    bad_examples <- unique(data$Trial[incorrect_lengths])

    stop(
      "[", study_id, "] Unexpected trial structure. Examples: ",
      paste(head(bad_examples, 5L), collapse = "; ")
    )
  }

  trial_matrix <- do.call(rbind, trials)

  invalid_polarity <- !trial_matrix[, 1] %in% c("Aff", "Neg")

  if (any(invalid_polarity)) {
    stop("[", study_id, "] Invalid polarity codes detected.")
  }

  invalid_second <- !trial_matrix[, 2] %in% specification$second_codes

  if (any(invalid_second)) {
    stop("[", study_id, "] Invalid second-position codes detected.")
  }

  if (!is.null(specification$third_codes)) {
    invalid_third <- !trial_matrix[, 3] %in% specification$third_codes

    if (any(invalid_third)) {
      stop("[", study_id, "] Invalid third-position codes detected.")
    }
  }

  scenario_position <- specification$trial_parts
  scenario_codes <- trial_matrix[, scenario_position]

  invalid_scenarios <- !grepl("^[A-E]_[0-9]+$", scenario_codes)

  if (any(invalid_scenarios)) {
    stop(
      "[", study_id, "] Unexpected scenario codes detected: ",
      paste(
        head(unique(scenario_codes[invalid_scenarios]), 5L),
        collapse = ", "
      )
    )
  }

  participant_count <- length(unique(data$Response_ID))

  if (participant_count != specification$expected_participants) {
    stop(
      "[", study_id, "] Expected ",
      specification$expected_participants,
      " participants but found ",
      participant_count,
      "."
    )
  }

  data.frame(
    study = study_id,
    file = specification$file,
    rows = nrow(data),
    participants = participant_count,
    rating_min = min(numeric_ratings),
    rating_max = max(numeric_ratings),
    stringsAsFactors = FALSE
  )
}

validation_summary <- do.call(
  rbind,
  Map(validate_study, names(studies), studies)
)

rownames(validation_summary) <- NULL

print(validation_summary, row.names = FALSE)
message("All five input datasets passed validation.")
