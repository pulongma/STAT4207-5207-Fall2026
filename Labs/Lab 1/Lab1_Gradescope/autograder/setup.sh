#!/usr/bin/env bash
set -euo pipefail

apt-get update
DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
  r-base-core \
  r-cran-knitr \
  r-cran-ggplot2 \
  r-cran-jsonlite
apt-get clean
rm -rf /var/lib/apt/lists/*

chmod +x /autograder/run_autograder
