test_that("labels come out as JSON-ready lists with text codes", {
  d <- withr::local_tempdir()
  x <- data.frame(sex = haven::labelled(c(1, 2, 100000), c(Male = 1, Female = 2, Big = 100000), label = "Gender"), n = 1:3)
  haven::write_dta(x, file.path(d, "a.dta"))
  m <- ggp_meta(file.path(d, "a.dta"))
  expect_equal(m$column_labels, list(sex = "Gender", n = NA_character_))
  expect_equal(m$value_labels, list(sex = list(`1` = "Male", `2` = "Female", `100000` = "Big")))  # "100000", not "1e+05"
  expect_equal(names(m), c("file_label", "column_labels", "value_labels", "missing_ranges", "original_types"))
  expect_null(ggp_meta(file.path(d, "a.csv")))
  json <- jsonlite::toJSON(m, auto_unbox = TRUE, na = "null")
  expect_match(json, '"missing_ranges":{}', fixed = TRUE)  # an empty object, as in the Python package
  expect_match(json, '"value_labels":{"sex":{"1":"Male"', fixed = TRUE)

  s <- data.frame(q = haven::labelled_spss(c(1, 98), c(Yes = 1), na_values = 98, label = "Q"))
  haven::write_sav(s, file.path(d, "a.sav"))
  expect_equal(ggp_meta(file.path(d, "a.sav"))$missing_ranges, list(q = list(98)))
  expect_equal(ggp_read(file.path(d, "a.sav"))$q, c(1, NA))  # user-defined missing is NA when reading
})

test_that("a variable with value labels but no variable label has no label; extended missing values are letters", {
  d <- withr::local_tempdir()
  x <- data.frame(a = haven::labelled(c(1, 2), c(Yes = 1, No = 2)),
                  b = haven::labelled(c(1, haven::tagged_na("a")), c(Yes = 1, "not asked" = haven::tagged_na("a")), label = "B"))
  haven::write_dta(x, file.path(d, "a.dta"))
  m <- ggp_meta(file.path(d, "a.dta"))
  expect_equal(m$column_labels, list(a = NA_character_, b = "B"))  # "label" must not match "labels"
  expect_equal(m$value_labels$b, list(`1` = "Yes", a = "not asked"))  # as pyreadstat names them
})

test_that("text that is not UTF-8 is read as latin1", {
  d <- withr::local_tempdir()
  x <- data.frame(w = haven::labelled(c(1, 2), c("Café" = 1), label = "Café"), t = c("café", "b"))
  f <- file.path(d, "a.dta")
  haven::write_dta(x, f)
  b <- readBin(f, "raw", file.size(f))
  at <- which(b[-length(b)] == as.raw(0xc3) & b[-1] == as.raw(0xa9))  # every UTF-8 e-acute ...
  expect_gt(length(at), 2)
  for (i in at) b[i:(i + 1)] <- as.raw(c(0xe9, 0x00))  # ... as latin1 plus a padding NUL
  writeBin(b, f)
  expect_equal(ggp_meta(f)$column_labels$w, "Café")
  expect_equal(ggp_meta(f)$value_labels$w[["1"]], "Café")
  expect_equal(ggp_read(f)$t[1], "café")
  expect_no_error(jsonlite::toJSON(ggp_meta(f)))
})

test_that("a file type that cannot be read says so", {
  expect_error(ggp_read("x.xlsx"), "cannot read", class = "ggp_error")
})
