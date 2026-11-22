#!/bin/sh
export LC_ALL=${LC_ALL:-C.UTF-8}
# run the tests (the package is installed first: background workers load the installed one): R_LIBS_USER may hold packages that are not installed system-wide
R CMD INSTALL --no-docs . ${R_LIBS_USER:+-l "${R_LIBS_USER%%:*}"} >/dev/null 2>&1
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_dir("tests/testthat", reporter = "summary", load_package = "none")' "$@"
