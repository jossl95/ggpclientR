test_that("preprocessing writes lower-case names, in the parquet and its meta.json", {
  d <- withr::local_tempdir()
  root <- file.path(d, "raw")
  out <- file.path(d, "processed")
  labelled_dta(file.path(root, "at", "AT.dta"), data.frame(DEM01 = c(1, 2), Age = c(30, 40)),
               labels = list(DEM01 = "Gender", Age = "Age"), value_labels = list(DEM01 = c(Male = 1, Female = 2)))
  r <- row_of(local_path = "at")
  expect_equal(preprocess_rows(r, root, out)$done, 1L)
  pq <- file.path(out, "at", "AT.parquet")
  expect_equal(head(parquet_columns(pq), 2), c("dem01", "age"))
  m <- jsonlite::fromJSON(meta_of(pq), simplifyVector = FALSE)
  expect_equal(names(m$column_labels), c("dem01", "age"))
  expect_equal(m$value_labels, list(dem01 = list(`1` = "Male", `2` = "Female")))
  expect_setequal(names(m$original_types), c("dem01", "age"))
  expect_equal(preprocess_rows(r, root, out)$skipped, 1L) # lower case already: kept

  old <- arrow::read_parquet(pq)
  names(old)[1] <- "DEM01" # as an older version wrote it
  arrow::write_parquet(old, pq)
  expect_equal(preprocess_rows(r, root, out)$done, 1L) # upper-case names: made again
  expect_equal(parquet_columns(pq)[1], "dem01")
})

test_that("names that differ only in case are an error", {
  expect_equal(lower_names(c("A", "b"), "x"), c("a", "b"))
  expect_error(lower_names(c("V1", "v1"), "x"), "x: V1 and v1 differ only in case", class = "ggp_error")
})

META <- list(
  column_labels = list(edu = "Education", sex = "Sex", id = NULL),
  value_labels = list(edu = list(`1` = "Elementary school", `2` = "Elementary school", `3` = "University", a = "Refused"),
                      sex = list(M = "Male", F = "Female"))
)

test_that("ggp_label puts variable and value labels on the columns", {
  df <- data.frame(id = 1:4, edu = c(3, 1, NA, 7), sex = c("F", "M", "F", NA), n = c(1L, 2L, 3L, 4L))
  out <- ggp_label(df, META)
  expect_equal(attr(out$edu, "label"), "Education")
  expect_equal(attr(out$edu, "labels"), c(`Elementary school` = 1, `Elementary school` = 2, University = 3)) # not .a
  expect_equal(as.character(haven::as_factor(out$edu)), c("University", "Elementary school", NA, "7"))
  expect_equal(as.character(haven::as_factor(out$sex)), c("Female", "Male", "Female", NA))
  expect_identical(out$id, df$id) # no labels: left alone
  expect_identical(out$n, df$n)
  expect_identical(ggp_label(df, META, columns = "sex")$edu, df$edu)

  int <- ggp_label(data.frame(edu = c(1L, 3L)), META) # an integer column gets integer labels
  expect_equal(as.character(haven::as_factor(int$edu)), c("Elementary school", "University"))
})

test_that("ggp_meta reads the meta.json next to a parquet file", {
  d <- withr::local_tempdir()
  pq <- file.path(d, "x.parquet")
  arrow::write_parquet(data.frame(edu = 1), pq)
  expect_null(ggp_meta(pq)) # no meta.json
  write_json(META, meta_of(pq))
  out <- ggp_label(ggp_read(pq), ggp_meta(pq))
  expect_equal(as.character(haven::as_factor(out$edu)), "Elementary school")
})
