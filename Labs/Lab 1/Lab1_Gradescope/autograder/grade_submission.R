#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(ggplot2))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5L) {
  stop("Usage: grade_submission.R OBJECTS_RDS RESULTS_JSON STUDENT_LOG STATUS TIPS_CSV")
}

objects_file <- args[[1L]]
results_file <- args[[2L]]
student_log <- args[[3L]]
collect_status <- suppressWarnings(as.integer(args[[4L]]))
tips_file <- args[[5L]]
if (is.na(collect_status)) collect_status <- 1L

tests <- list()
add_test <- function(name, score, max_score, output) {
  tests[[length(tests) + 1L]] <<- list(
    name = name,
    score = max(0, min(as.numeric(score), as.numeric(max_score))),
    max_score = as.numeric(max_score),
    output = as.character(output),
    visibility = "visible"
  )
}

safe_log <- function(path, limit = 3000L) {
  if (!file.exists(path)) return("")
  value <- paste(readLines(path, warn = FALSE), collapse = "\n")
  if (nchar(value) > limit) value <- paste0(substr(value, 1L, limit), "\n[output truncated]")
  value
}

finite_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
}

near <- function(x, target, tolerance = 1e-6) {
  finite_scalar(x) && abs(as.numeric(x) - target) <= tolerance
}

normalize_names <- function(x) {
  if (!is.atomic(x) || anyNA(x)) return(character())
  sort(unique(tolower(trimws(as.character(x)))))
}

normalize_text <- function(x) {
  if (!is.atomic(x) || length(x) != 1L || is.na(x)) return("")
  tolower(trimws(as.character(x)))
}

normalize_days <- function(x) {
  value <- normalize_names(x)
  value <- sub("^thurs.*$", "thu", value)
  value <- sub("^thu.*$", "thu", value)
  value <- sub("^fri.*$", "fri", value)
  value <- sub("^sat.*$", "sat", value)
  value <- sub("^sun.*$", "sun", value)
  sort(unique(value))
}

same_set <- function(x, expected) identical(normalize_names(x), sort(tolower(expected)))

tips_ref <- read.csv(tips_file, stringsAsFactors = TRUE)
model_ref <- lm(tip ~ totbill, data = tips_ref)
coef_ref <- unname(coef(model_ref))
prediction_ref <- unname(predict(model_ref, newdata = data.frame(totbill = 45)))

payload <- if (file.exists(objects_file)) {
  tryCatch(readRDS(objects_file), error = function(e) NULL)
} else NULL

zero_remaining <- function(reason) {
  add_test("Exercise 1: arithmetic objects", 0, 20, reason)
  add_test("Load the tips data", 0, 10, reason)
  add_test("Exercise 2: cases and variables", 0, 10, reason)
  add_test("Exercise 2: variable classifications", 0, 10, reason)
  add_test("Exercise 2: collection days", 0, 5, reason)
  add_test("Exercise 3: scatterplot", 0, 5, reason)
  add_test("Exercise 3: direction", 0, 5, reason)
  add_test("Exercise 3: form", 0, 5, reason)
  add_test("Exercise 3: spread", 0, 5, reason)
  add_test("Exercise 4: fitted model", 0, 6, reason)
  add_test("Exercise 4: coefficients", 0, 8, reason)
  add_test("Exercise 4: prediction", 0, 4, reason)
  add_test("Exercise 4: slope percentage", 0, 2, reason)
}

if (is.null(payload)) {
  reason <- if (collect_status == 124L) {
    "The submission exceeded the 45-second limit."
  } else if (collect_status == 2L) {
    "The required file Lab1.Rmd was not found."
  } else {
    "The submission could not be evaluated."
  }
  log_text <- safe_log(student_log)
  if (nzchar(log_text)) reason <- paste(reason, log_text, sep = "\n")
  add_test("Submission runs", 0, 5, reason)
  zero_remaining(reason)
} else {
  present <- payload$present
  object <- payload$objects
  errors <- payload$errors

  if (!length(errors) && collect_status == 0L) {
    add_test("Submission runs", 5, 5, "Lab1.Rmd parsed and ran without errors.")
  } else {
    detail <- if (length(errors)) paste(errors, collapse = " | ") else safe_log(student_log)
    add_test("Submission runs", 0, 5, paste("The submission produced an error.", detail))
  }

  # Exercise 1: each sequential result earns its own portion of the section score.
  arithmetic_score <- 0
  arithmetic_notes <- character()
  start <- object$start_value
  if (isTRUE(present[["start_value"]]) && finite_scalar(start)) {
    arithmetic_score <- arithmetic_score + 2
    arithmetic_notes <- c(arithmetic_notes, "valid starting value")
    if (isTRUE(present[["after_add"]]) && near(object$after_add, start + 2)) {
      arithmetic_score <- arithmetic_score + 4
      arithmetic_notes <- c(arithmetic_notes, "addition correct")
    }
    if (isTRUE(present[["after_multiply"]]) && near(object$after_multiply, (start + 2) * 3)) {
      arithmetic_score <- arithmetic_score + 4
      arithmetic_notes <- c(arithmetic_notes, "multiplication correct")
    }
    if (isTRUE(present[["after_subtract"]]) && near(object$after_subtract, (start + 2) * 3 - 6)) {
      arithmetic_score <- arithmetic_score + 4
      arithmetic_notes <- c(arithmetic_notes, "subtraction correct")
    }
    if (isTRUE(present[["final_value"]]) && near(object$final_value, start)) {
      arithmetic_score <- arithmetic_score + 6
      arithmetic_notes <- c(arithmetic_notes, "final value returns to the start")
    }
  } else {
    arithmetic_notes <- "Expected start_value to be one finite number."
  }
  add_test("Exercise 1: arithmetic objects", arithmetic_score, 20, paste(arithmetic_notes, collapse = "; "))

  tips_actual <- if (isTRUE(present[["tips"]])) object$tips else NULL
  data_score <- 0
  data_note <- "Expected tips to be the data frame read from tips.csv."
  if (is.data.frame(tips_actual) && identical(dim(tips_actual), dim(tips_ref)) &&
      identical(names(tips_actual), names(tips_ref))) {
    data_score <- 4
    numeric_names <- c("obs", "totbill", "tip", "size")
    categorical_names <- c("sex", "smoker", "day", "time")
    numeric_ok <- isTRUE(all.equal(
      as.matrix(tips_actual[numeric_names]), as.matrix(tips_ref[numeric_names]),
      tolerance = 1e-10, check.attributes = FALSE
    ))
    categorical_ok <- all(vapply(
      categorical_names,
      function(n) identical(as.character(tips_actual[[n]]), as.character(tips_ref[[n]])),
      logical(1L)
    ))
    if (numeric_ok) data_score <- data_score + 3
    if (categorical_ok) data_score <- data_score + 3
    data_note <- if (data_score == 10) {
      "tips contains the correct 244 rows and eight columns."
    } else {
      "tips has the expected shape and names, but some values differ from the supplied file."
    }
  }
  add_test("Load the tips data", data_score, 10, data_note)

  dimension_score <- 0
  dimension_notes <- character()
  if (isTRUE(present[["n_cases"]]) && near(object$n_cases, 244)) {
    dimension_score <- dimension_score + 5
    dimension_notes <- c(dimension_notes, "244 cases")
  }
  if (isTRUE(present[["n_variables"]]) && near(object$n_variables, 7)) {
    dimension_score <- dimension_score + 5
    dimension_notes <- c(dimension_notes, "7 variables excluding obs")
  }
  if (!length(dimension_notes)) dimension_notes <- "Check n_cases and n_variables; obs is an identifier, not a variable."
  add_test("Exercise 2: cases and variables", dimension_score, 10, paste(dimension_notes, collapse = "; "))

  type_score <- 0
  type_notes <- character()
  if (isTRUE(present[["quantitative_variables"]]) &&
      same_set(object$quantitative_variables, c("totbill", "tip", "size"))) {
    type_score <- type_score + 5
    type_notes <- c(type_notes, "quantitative variables correct")
  }
  if (isTRUE(present[["categorical_variables"]]) &&
      same_set(object$categorical_variables, c("sex", "smoker", "day", "time"))) {
    type_score <- type_score + 5
    type_notes <- c(type_notes, "categorical variables correct")
  }
  if (!length(type_notes)) type_notes <- "Use the column names from tips; do not include obs."
  add_test("Exercise 2: variable classifications", type_score, 10, paste(type_notes, collapse = "; "))

  if (isTRUE(present[["collection_days"]]) &&
      identical(normalize_days(object$collection_days), c("fri", "sat", "sun", "thu"))) {
    add_test("Exercise 2: collection days", 5, 5, "Thursday, Friday, Saturday, and Sunday are all included; order is ignored.")
  } else {
    add_test("Exercise 2: collection days", 0, 5, "Expected all four days on which tips were collected.")
  }

  plot_actual <- if (isTRUE(present[["tips_plot"]])) object$tips_plot else NULL
  plot_score <- 0
  plot_note <- "Expected a ggplot2 point plot with totbill on x and tip on y."
  if (inherits(plot_actual, "ggplot")) {
    plot_score <- 2
    built <- tryCatch(ggplot_build(plot_actual), error = function(e) NULL)
    if (!is.null(built)) {
      correct_layer <- any(vapply(built$data, function(layer) {
        if (!all(c("x", "y") %in% names(layer)) || nrow(layer) != nrow(tips_ref)) return(FALSE)
        isTRUE(all.equal(as.numeric(layer$x), tips_ref$totbill, tolerance = 1e-8)) &&
          isTRUE(all.equal(as.numeric(layer$y), tips_ref$tip, tolerance = 1e-8))
      }, logical(1L)))
      if (correct_layer) {
        plot_score <- 5
        plot_note <- "The scatterplot uses total bill on x and tip on y."
      } else {
        plot_note <- "A ggplot object was found, but no layer contains the required x and y values."
      }
    }
  }
  add_test("Exercise 3: scatterplot", plot_score, 5, plot_note)

  direction <- if (isTRUE(present[["association_direction"]])) normalize_text(object$association_direction) else ""
  if (direction %in% c("positive", "increasing", "upward")) {
    add_test("Exercise 3: direction", 5, 5, "Correct: the association is positive.")
  } else {
    add_test("Exercise 3: direction", 0, 5, "Choose the direction shown by the scatterplot.")
  }

  form <- if (isTRUE(present[["relationship_form"]])) normalize_text(object$relationship_form) else ""
  if (form %in% c("approximately linear", "roughly linear", "linear")) {
    add_test("Exercise 3: form", 5, 5, "Correct: the relationship is approximately linear.")
  } else {
    add_test("Exercise 3: form", 0, 5, "Choose the form shown by the scatterplot.")
  }

  spread <- if (isTRUE(present[["spread_pattern"]])) normalize_text(object$spread_pattern) else ""
  spread_ok <- spread %in% c("increases", "increasing", "wider", "fan-shaped", "heteroscedastic") ||
    grepl("wider.*increase|increase.*wider|spread.*increase", spread)
  if (spread_ok) {
    add_test("Exercise 3: spread", 5, 5, "Correct: vertical variation tends to increase with total bill.")
  } else {
    add_test("Exercise 3: spread", 0, 5, "Choose how vertical variation changes as total bill increases.")
  }

  model_actual <- if (isTRUE(present[["tip_model"]])) object$tip_model else NULL
  if (inherits(model_actual, "lm") &&
      length(coef(model_actual)) == 2L &&
      isTRUE(all.equal(unname(coef(model_actual)), coef_ref, tolerance = 1e-6))) {
    add_test("Exercise 4: fitted model", 6, 6, "tip_model is the correct least-squares model for tip versus totbill.")
  } else if (inherits(model_actual, "lm")) {
    add_test("Exercise 4: fitted model", 2, 6, "An lm object was found, but its response, predictor, or data are not correct.")
  } else {
    add_test("Exercise 4: fitted model", 0, 6, "Expected tip_model to be created by lm(tip ~ totbill, data = tips).")
  }

  coefficient_score <- 0
  coefficient_notes <- character()
  if (isTRUE(present[["intercept"]]) && near(object$intercept, coef_ref[[1L]], tolerance = 0.001)) {
    coefficient_score <- coefficient_score + 4
    coefficient_notes <- c(coefficient_notes, "intercept correct")
  }
  if (isTRUE(present[["slope"]]) && near(object$slope, coef_ref[[2L]], tolerance = 0.001)) {
    coefficient_score <- coefficient_score + 4
    coefficient_notes <- c(coefficient_notes, "slope correct")
  }
  if (!length(coefficient_notes)) coefficient_notes <- "Extract the two coefficients from tip_model."
  add_test("Exercise 4: coefficients", coefficient_score, 8, paste(coefficient_notes, collapse = "; "))

  if (isTRUE(present[["predicted_tip_45"]]) && near(object$predicted_tip_45, prediction_ref, tolerance = 0.02)) {
    add_test("Exercise 4: prediction", 4, 4, "The predicted average tip for a $45 bill is correct.")
  } else {
    add_test("Exercise 4: prediction", 0, 4, "Use tip_model to predict the average tip when totbill is 45.")
  }

  if (isTRUE(present[["slope_percent"]]) && near(object$slope_percent, 100 * coef_ref[[2L]], tolerance = 0.1)) {
    add_test("Exercise 4: slope percentage", 2, 2, "The slope is correctly expressed as about 10.5%.")
  } else {
    add_test("Exercise 4: slope percentage", 0, 2, "Multiply the fitted slope by 100 to express it as a percentage.")
  }
}

# The rubric uses integer weights for simple partial-credit arithmetic, then
# converts those weights to the assignment's 10-point scale.
tests <- lapply(tests, function(test) {
  test$score <- test$score / 12.5
  test$max_score <- test$max_score / 12.5
  test
})

total_score <- sum(vapply(tests, function(x) x$score, numeric(1L)))
result <- list(
  score = total_score,
  max_score = 8,
  output = "Named R objects are graded by their statistical results, with rounding and answer order handled where appropriate.",
  visibility = "visible",
  stdout_visibility = "hidden",
  tests = tests
)

dir.create(dirname(results_file), recursive = TRUE, showWarnings = FALSE)
write_json(result, results_file, auto_unbox = TRUE, pretty = TRUE, digits = NA)
