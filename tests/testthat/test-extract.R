test_that("extension values", {
  expect_setequal(norm_ext(NULL), c("dta", "sav", "csv"))  # rds only on request
  expect_setequal(norm_ext(character()), c("dta", "sav", "csv"))  # rds only on request
  expect_equal(norm_ext(".SAV"), "sav")
  expect_setequal(norm_ext(c("dta", "CSV")), c("dta", "csv"))
})

test_that("unpack filters, mirrors nested zips and stays inside", {
  z <- make_zip(list(
    "A/x.dta" = "1", "A/x.sav" = "2", "A/README" = "r",
    "A/Sub.ZIP" = make_zip(list("y.csv" = "3", "doc.pdf" = "4")),
    "../evil.dta" = "x", "/abs.dta" = "x", "__MACOSX/A/._x.dta" = "x"
  ))
  root <- withr::local_tempdir()
  zip_file <- save_raw(z, file.path(root, "in.zip"))
  for (case in list(list("d", c("dta", "csv"), FALSE, c("a/sub/y.csv", "a/x.dta")),  # nested zip: lowercase folder
                    list("m", "dta", TRUE, c("a/README", "a/sub/doc.pdf", "a/x.dta")))) {  # metadata: non-data too
    expect_equal(unpack(zip_file, file.path(root, case[[1]]), case[[2]], case[[3]]), length(case[[4]]))
    expect_equal(tree(file.path(root, case[[1]])), case[[4]])
  }
  expect_false(file.exists(file.path(root, "evil.dta")))
  expect_true(all(grepl("^(d/|m/|in\\.zip)", tree(root))))  # nothing escaped
})

test_that("7z archives like the portal serves them", {
  seven <- make_7z(list("Data/AT.dta" = "1", "Docs/cb.pdf" = "2", "More.zip" = make_zip(list("y.sav" = "3"))))
  root <- withr::local_tempdir()
  f <- save_raw(seven, file.path(root, "dl.part"))
  expect_equal(unpack_file(f, "GGS_Wave1_Austria_V.4.4.7z", file.path(root, "a"), c("dta", "sav"), FALSE), 2L)
  expect_equal(tree(file.path(root, "a")), c("data/at.dta", "more/y.sav"))  # datafile names lower case
  f <- save_raw(make_zip(list("Inner.7z" = seven)), f)  # a 7z inside a zip
  expect_equal(unpack_file(f, "x.zip", file.path(root, "b"), "dta", TRUE), 2L)
  expect_equal(tree(file.path(root, "b")), c("inner/data/at.dta", "inner/docs/cb.pdf"))
  f <- save_raw(make_zip(list("Fake.7z" = make_zip(list("z.dta" = "5")))), f)  # content decides, not the extension
  expect_equal(unpack_file(f, "x.zip", file.path(root, "c"), "dta", FALSE), 1L)
  expect_equal(tree(file.path(root, "c")), "fake/z.dta")
})

test_that("a plain file is kept only if it matches", {
  root <- withr::local_tempdir()
  f <- save_raw(as.raw(100), file.path(root, "raw.part"))
  expect_equal(unpack_file(f, "AT.dta", file.path(root, "o"), "dta", FALSE), 1L)
  expect_equal(tree(file.path(root, "o")), "at.dta")
  expect_equal(unpack_file(f, "AT.dta", file.path(root, "o2"), "sav", FALSE), 0L)
  expect_equal(unpack_file(f, "..", file.path(root, "o3"), "dta", TRUE), 1L)  # an unsafe name is replaced
  expect_equal(tree(file.path(root, "o3")), "download")
  f <- save_raw(charToRaw("new"), f)
  expect_equal(unpack_file(f, "AT.dta", file.path(root, "o"), "dta", FALSE), 1L)  # matches, but exists: kept
  expect_equal(readBin(file.path(root, "o/at.dta"), "raw", 10), as.raw(100))
  expect_equal(unpack_file(f, "AT.dta", file.path(root, "o"), "dta", FALSE, force = TRUE), 1L)
  expect_equal(rawToChar(readBin(file.path(root, "o/at.dta"), "raw", 10)), "new")
  writeBin(as.raw(1), file.path(root, "o/AT.dta"))  # downloaded before names were lower-cased
  unpack_file(f, "AT.dta", file.path(root, "o"), "dta", FALSE)
  expect_equal(tree(file.path(root, "o")), c("AT.dta", "at.dta"))  # kept without force
  unpack_file(f, "AT.dta", file.path(root, "o"), "dta", FALSE, force = TRUE)
  expect_equal(tree(file.path(root, "o")), "at.dta")
})

test_that("rds is kept only when asked for", {
  expect_equal(norm_ext("rds"), "rds")
  z <- make_zip(list("a.rds" = "1", "a.dta" = "2"))
  root <- withr::local_tempdir()
  f <- save_raw(z, file.path(root, "in.zip"))
  expect_equal(tree({unpack(f, file.path(root, "d"), norm_ext(NULL), FALSE); file.path(root, "d")}), "a.dta")
  expect_equal(tree({unpack(f, file.path(root, "r"), norm_ext("rds"), FALSE); file.path(root, "r")}), "a.rds")
})
