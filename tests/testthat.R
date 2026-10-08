# Test runner: execute from the repository root with
#   Rscript -e "testthat::test_dir('tests/testthat', reporter = 'summary')"
# Phase 1 suite: zero production-code changes. Tests extract pure helper
# functions textually out of server.R / global.R and evaluate them in
# isolated environments (see helper-app.R), so they always exercise the
# shipped implementations without sourcing the whole app.
library(testthat)
test_dir("tests/testthat", reporter = "summary")
