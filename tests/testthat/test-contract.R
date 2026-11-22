# The fixtures shared with the Python package (tests/testthat/fixtures, copied from ggp_client/tests/fixtures):
# portal pages, an FFS dataset and a label case, with the results both packages must give.

fixture <- function(...) testthat::test_path("fixtures", ...)
read_expected <- function(name) jsonlite::read_json(fixture("expected", name))

sort_names <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  x <- lapply(x, sort_names)
  if (!is.null(names(x))) x <- x[order(names(x), method = "radix")]
  x
}

# what JSON makes of an R value, with sorted names: comparable with the expected files
canon <- function(x) sort_names(jsonlite::fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA),
                                                   simplifyVector = FALSE))

html <- function(name) paste(readLines(fixture("portal", paste0(name, ".html")), encoding = "UTF-8", warn = FALSE), collapse = "\n")

with_arrays <- function(d) { # a one-element vector is a JSON array in the expected files
  d$other_versions <- as.list(d$other_versions)
  d
}

test_that("parsing gives what the Python package gives", {
  want <- read_expected("parse.json")
  slugs <- lapply(stats::setNames(names(want$slugs), names(want$slugs)), \(s) {
    p <- parse_slug(s)
    c(p, naming(p, s, p$version))
  })
  expect_equal(canon(slugs), sort_names(want$slugs))
  expect_equal(parse_catalog(html("catalog")), unlist(want$catalog))
  got <- lapply(stats::setNames(names(want$datasets), names(want$datasets)), \(s) with_arrays(parse_dataset(html(s), s)))
  expect_equal(canon(got), sort_names(want$datasets))
  expect_equal(canon(lapply(parse_access(html("form")), with_arrays)), sort_names(want$access))
})

test_that("harmonized labels are what the Python package gives", {
  d <- withr::local_tempdir()
  case <- jsonlite::read_json(fixture("labels", "case.json"))
  files <- data.frame(file = character(), country = character(), src = character())
  for (i in seq_along(case$files)) {
    f <- file.path(d, paste0(i, ".parquet"))
    if (!is.null(case$files[[i]]$meta)) jsonlite::write_json(case$files[[i]]$meta, meta_of(f), auto_unbox = TRUE)
    files <- rbind(files, data.frame(file = f, country = case$files[[i]]$country, src = case$files[[i]]$src))
  }
  expect_equal(canon(merged_meta(files, unlist(case$columns))), sort_names(read_expected("labels.json")))
})

test_that("an FFS dataset is read as the Python package reads it", {
  want <- read_expected("ffs.json")
  d <- withr::local_tempdir()
  file.copy(c(fixture("ffs", "USA2.SPS"), fixture("ffs", "USA2.DAT")), d)
  x <- suppressMessages(read_ffs(file.path(d, "USA2.SPS"), file.path(d, "USA2.DAT")))
  expect_equal(names(x$df), unlist(want$columns))
  expect_equal(canon(lapply(as.list(x$df), as.list)), sort_names(want$data))
  expect_equal(canon(x$meta$column_labels), sort_names(want$column_labels))
  expect_equal(canon(lapply(x$meta$value_labels, as.list)), sort_names(want$value_labels))
  expect_equal(canon(x$meta$missing_ranges), sort_names(want$missing_ranges))
})
