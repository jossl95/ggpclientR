#!/bin/sh
# run the tests: R_LIBS_USER may hold packages that are not installed system-wide
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_dir("tests/testthat", reporter = "summary", load_package = "none")' "$@"
