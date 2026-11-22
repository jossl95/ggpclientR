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
  expect_equal(ggp_read(file.path(d, "a.sav"), labelled = FALSE)$q, c(1, NA))  # user-defined missing is NA when reading
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
  expect_equal(ggp_read(f, labelled = FALSE)$t[1], "café")
  expect_no_error(jsonlite::toJSON(ggp_meta(f)))
})

test_that("a file type that cannot be read says so", {
  expect_error(ggp_read("x.xlsx"), "cannot read", class = "ggp_error")
})

test_that("dta and sav are read with haven: labelled by default, codes on request", {
  d <- withr::local_tempdir()
  labelled_dta(file.path(d, "a.dta"), data.frame(sex = c(1, 2)), labels = list(sex = "Gender"), value_labels = list(sex = c(Male = 1, Female = 2)))
  x <- ggp_read(file.path(d, "a.dta"))
  expect_s3_class(x$sex, "haven_labelled")
  expect_equal(as.character(haven::as_factor(x$sex)), c("Male", "Female"))
  expect_equal(attr(x$sex, "label"), "Gender")
  expect_equal(ggp_read(file.path(d, "a.dta"), labelled = FALSE)$sex, c(1, 2))
})

test_that("rds files: read, labelled, in a 7z or zip, and preprocessed like dta", {
  d <- withr::local_tempdir()
  x <- data.frame(age = haven::labelled(c(30, 40), c(thirty = 30), label = "Age"), t = c("a", "b"))
  saveRDS(x, file.path(d, "a.rds"))
  y <- ggp_read(file.path(d, "a.rds"))
  expect_equal(attr(y$age, "label"), "Age")
  expect_equal(ggp_meta(file.path(d, "a.rds"))$value_labels, list(age = list(`30` = "thirty")))
  expect_equal(ggp_read(file.path(d, "a.rds"), labelled = FALSE)$age, c(30, 40))
  saveRDS(1:3, file.path(d, "bad.rds"))
  expect_error(ggp_read(file.path(d, "bad.rds")), "not a data frame", class = "ggp_error")

  seven <- save_raw(make_7z(list("a.rds" = readBin(file.path(d, "a.rds"), "raw", file.size(file.path(d, "a.rds"))))), file.path(d, "a.rds.7z"))
  expect_equal(attr(ggp_read(seven)$age, "label"), "Age")  # compressed with 7z
  expect_equal(ggp_read(file.path(d, "a.rds"), labelled = FALSE)$t, ggp_read(seven, labelled = FALSE)$t)

  root <- file.path(d, "raw"); pre <- file.path(d, "pre")
  dir.create(file.path(root, "x"), recursive = TRUE)
  file.copy(file.path(d, "a.rds"), file.path(root, "x/a.rds"))
  r <- preprocess_rows(row_of(local_path = "x"), root, pre)
  expect_equal(r$done, 1L)
  expect_equal(ggp_read(file.path(pre, "x/a.parquet"))$age, c(30, 40))
  expect_equal(jsonlite::read_json(file.path(pre, "x/a.meta.json"))$value_labels$age[["30"]], "thirty")
})

test_that("a compressed file with several datafiles is refused, zip or 7z", {
  d <- withr::local_tempdir()
  both <- list("a.rds" = "1", "b.rds" = "2")
  expect_error(ggp_read(save_raw(make_7z(both), file.path(d, "x.7z"))), "holds 2 datafiles", class = "ggp_error")
  expect_error(ggp_read(save_raw(make_zip(both), file.path(d, "x.zip"))), "holds 2 datafiles", class = "ggp_error")
})
