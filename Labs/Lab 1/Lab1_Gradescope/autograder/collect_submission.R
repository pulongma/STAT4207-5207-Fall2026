#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: collect_submission.R STUDENT_FILE OUTPUT_RDS DATA_DIRECTORY")
}

student_file <- normalizePath(args[[1L]], mustWork = TRUE)
output_file <- args[[2L]]
data_directory <- normalizePath(args[[3L]], mustWork = TRUE)

requested <- c(
  "group_members",
  "start_value", "after_add", "after_multiply", "after_subtract", "final_value",
  "tips", "n_cases", "n_variables", "quantitative_variables",
  "categorical_variables", "collection_days", "tips_plot",
  "association_direction", "relationship_form", "spread_pattern",
  "tip_model", "intercept", "slope", "predicted_tip_45", "slope_percent"
)

student_env <- new.env(parent = globalenv())
errors <- character()
warnings <- character()
expressions <- NULL
code_file <- tempfile(fileext = ".R")

# The official data file is bundled with the grader, so student code using
# read.csv("tips.csv") does not need network access during grading.
old_directory <- getwd()
on.exit(setwd(old_directory), add = TRUE)
setwd(data_directory)

tryCatch({
  knitr::purl(student_file, output = code_file, documentation = 0, quiet = TRUE)
  expressions <- parse(file = code_file, keep.source = FALSE)
}, error = function(e) errors <<- c(errors, paste0("R Markdown extraction error: ", conditionMessage(e))))

# Evaluate top-level expressions independently so objects created before an
# error can still receive partial credit.
if (!is.null(expressions)) {
  for (i in seq_along(expressions)) {
    tryCatch(
      withCallingHandlers(
        eval(expressions[[i]], envir = student_env),
        warning = function(w) {
          warnings <<- c(warnings, paste0("Expression ", i, ": ", conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        errors <<- c(errors, paste0("Expression ", i, ": ", conditionMessage(e)))
      }
    )
  }
}

objects <- setNames(vector("list", length(requested)), requested)
present <- setNames(logical(length(requested)), requested)
for (name in requested) {
  present[[name]] <- exists(name, envir = student_env, inherits = FALSE)
  if (present[[name]]) objects[[name]] <- get(name, envir = student_env, inherits = FALSE)
}

saveRDS(
  list(present = present, objects = objects, errors = errors, warnings = warnings),
  file = output_file,
  compress = TRUE
)

if (length(errors)) message(paste(errors, collapse = "\n"))
if (length(warnings)) message(paste(warnings, collapse = "\n"))
