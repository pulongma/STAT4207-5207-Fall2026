#!/usr/bin/env bash
set -euo pipefail

project_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
autograder_dir="$project_dir/autograder"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

for case_name in solution equivalent partial; do
  if [[ "$case_name" == "solution" ]]; then
    submission_dir="$project_dir/solution"
  else
    submission_dir="$project_dir/sample_submissions/$case_name"
  fi
  results_dir="$test_root/$case_name"
  mkdir -p "$results_dir"
  AUTOGRADER_SOURCE_DIR="$autograder_dir" \
  AUTOGRADER_SUBMISSION_DIR="$submission_dir" \
  AUTOGRADER_RESULTS_DIR="$results_dir" \
    bash "$autograder_dir/run_autograder"
  printf '\n%s\n' "=== $case_name ==="
  Rscript --vanilla -e \
    'x <- jsonlite::read_json(commandArgs(TRUE)[1]); cat("score:", x$score, "/", x$max_score, "\n"); for (z in x$tests) cat("-", z$name, ":", z$score, "/", z$max_score, "\n")' \
    "$results_dir/results.json"
done
