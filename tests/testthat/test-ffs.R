test_that("an FFS dataset is read from its .dat with the labels of its .sps", {
  d <- withr::local_tempdir()
  writeBin(charToRaw(FFS_SPS), file.path(d, "USA2.SPS"))
  writeBin(charToRaw(FFS_DAT), file.path(d, "USA2.DAT"))
  expect_message(n <- convert_ffs(d, c("sav", "dta", "csv"), "ffs_us_v1.0"), "1 cells were not numbers")
  expect_equal(n, 3L)

  sav <- haven::read_sav(file.path(d, "ffs_us_v1.0.sav"), user_na = TRUE)
  expect_equal(names(sav), c("COUNTRY", "HHNUM", "WEIGHT", "V004_01", "V005_01"))  # $ is not allowed in Stata
  expect_equal(as.vector(sav$HHNUM), c("A0001", "A0002"))
  expect_equal(as.vector(sav$WEIGHT), c(0.7035, 1.2))
  expect_equal(as.numeric(sav$V004_01), c(1, 98))
  expect_equal(is.na(sav$V005_01), c(FALSE, TRUE))  # * is missing
  expect_equal(attr(sav$V004_01, "label"), 'Born in Nieder"sterreich')  # the stray quote does not end it
  expect_equal(attr(sav$WEIGHT, "label"), "Caseweight in US$")
  expect_equal(attr(sav$V005_01, "labels"), attr(sav$V004_01, "labels"))
  expect_equal(attr(sav$V004_01, "labels"), c(One = 1, `Missing value` = 98))
  expect_equal(attr(sav$COUNTRY, "labels"), c(`United States` = 23))
  expect_equal(attr(sav$V004_01, "na_values"), 98)

  dta <- haven::read_dta(file.path(d, "ffs_us_v1.0.dta"))
  expect_equal(attr(dta$V004_01, "labels"), c(One = 1, `Missing value` = 98))
  expect_equal(attr(dta$COUNTRY, "label"), "ISO country code")
  expect_equal(readLines(file.path(d, "ffs_us_v1.0.csv"))[1], '"COUNTRY","HHNUM","WEIGHT","V004_01","V005_01"')

  writeLines("mine", file.path(d, "ffs_us_v1.0.csv"))
  expect_equal(convert_ffs(d, "csv", "ffs_us_v1.0"), 1L)  # exists: kept
  expect_equal(readLines(file.path(d, "ffs_us_v1.0.csv")), "mine")
  suppressMessages(convert_ffs(d, "csv", "ffs_us_v1.0", force = TRUE))
  expect_false(identical(readLines(file.path(d, "ffs_us_v1.0.csv")), "mine"))
})

test_that("rewrite_sps changes names, not labels", {
  out <- rewrite_sps("V004$01  \"Caseweight in US$\"\nDATA LIST FILE='C:\\X\\USA2.DAT'", "ffs_us_v1.0.dat")
  expect_equal(out, "V004_01  \"Caseweight in US$\"\nDATA LIST FILE='ffs_us_v1.0.dat'")
})

test_that("the .sps parser reads layout, labels and missing values", {
  p <- parse_sps(FFS_SPS)
  expect_equal(p$records, 2L)
  expect_equal(p$layout$name, c("COUNTRY", "HHNUM", "WEIGHT", "V004$01", "V005$01"))
  expect_equal(p$layout$text, c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(p$missing[["V004$01"]], list(98))
  expect_equal(parse_sps(sub("(98)", "(1 THRU HI)", FFS_SPS, fixed = TRUE))$missing[["V004$01"]], list(list(lo = 1, hi = 1e300)))
})

test_that("a label whose code is not a number (Stata's .a) is left out of Stata files", {
  df <- data.frame(q = c(1, 2))
  meta <- list(column_labels = list(q = "Q"), value_labels = list(q = list(`1` = "Yes", a = "not asked")))
  f <- tempfile(fileext = ".dta")
  write_data(df, meta, f)
  expect_equal(attr(haven::read_dta(f)$q, "labels"), c(Yes = 1))
})
