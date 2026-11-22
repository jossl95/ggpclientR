test_that("read, preprocess and consolidate", {
  d <- withr::local_tempdir()
  root <- file.path(d, "download/raw")
  pre <- file.path(d, "download/processed")
  out <- file.path(d, "download/consolidated")
  at <- file.path(root, "ggs-ii/w1/at/ggs2_w1_at_v1.1/[raw data]")  # brackets in a folder name must not matter
  nl <- file.path(root, "ggs-ii/w1/nl/ggs2_w1_nl_v1.0")
  ffs <- file.path(root, "ffs/us")
  labelled_dta(file.path(at, "AT.dta"), data.frame(id = c(1, 2), age = c(30, 40), aut01_2101 = c(1, 1)),
               labels = list(id = "id", age = "Age", aut01_2101 = "AT only"), value_labels = list(age = c(thirty = 30)))
  dir.create(dirname(file.path(at, "AT.sav")), recursive = TRUE, showWarnings = FALSE)
  haven::write_sav(data.frame(id = 9, age = 99), file.path(at, "AT.sav"))  # the same file: dta is read
  dir.create(nl, recursive = TRUE)
  utils::write.csv(data.frame(id = 3, age = "n/a", x = 1.5, dem07_1801 = 5, att12_2101 = 7), file.path(nl, "NL.csv"), row.names = FALSE)
  dir.create(ffs, recursive = TRUE)
  writeLines("raw", file.path(ffs, "ffs_us.sps"))
  labelled_dta(file.path(pre, "ffs/us/converted_data.dta"), data.frame(V1 = 1), labels = list(V1 = "Age"))

  ffs_row <- row_of(project = "FFS", round = NA_integer_, series = "FFS", wave = "", country = "US", local_path = "ffs/us")
  s <- sources(ffs_row, root, pre)  # the labelled copy in processed, not the raw .sps
  expect_equal(c(basename(s$path), s$rel), c("converted_data.dta", "ffs/us/converted_data.dta"))
  r <- preprocess_rows(ffs_row, root, pre)  # parquet and meta.json next to the dta
  expect_equal(c(r$done, r$skipped, nrow(r$failed)), c(1, 0, 0))
  expect_equal(tree(file.path(pre, "ffs/us")), c("converted_data.dta", "converted_data.meta.json", "converted_data.parquet"))
  r <- preprocess_rows(ffs_row, root, pre)  # its own parquet is not a source
  expect_equal(c(r$done, r$skipped), c(0, 1))

  rows <- rbind(row_of(local_path = "ggs-ii/w1/at/ggs2_w1_at_v1.1"),
                row_of(country = "NL", local_path = "ggs-ii/w1/nl/ggs2_w1_nl_v1.0"),
                row_of(project = "", series = "", wave = "", country = "", version = ""))  # not downloaded: ignored
  expect_equal(basename(datafiles(at)), "AT.dta")
  expect_equal(ggp_read(file.path(at, "AT.sav"), labelled = FALSE)$age, 99)
  expect_equal(ggp_read(file.path(nl, "NL.csv"))$age, "n/a")

  r <- preprocess_rows(rows[1, ], root, pre)
  expect_equal(c(r$done, r$skipped), c(1, 0))
  r <- preprocess_rows(rows, root, pre, concurrency = 2)  # AT is already there
  expect_equal(c(r$done, r$skipped, nrow(r$failed)), c(1, 1, 0))
  at_parquet <- list.files(pre, "^AT\\.parquet$", recursive = TRUE, full.names = TRUE)
  df <- ggp_read(at_parquet)
  expect_equal(df$age, c(30, 40))
  expect_equal(unique(df$ggp_country), "AT")
  expect_equal(df$ggp_file[1], "ggs-ii/w1/at/ggs2_w1_at_v1.1/[raw data]/AT.dta")
  expect_equal(df$ggp_round[1], 2L)
  meta_file <- list.files(pre, "^AT\\.meta\\.json$", recursive = TRUE, full.names = TRUE)
  m <- jsonlite::read_json(meta_file)
  expect_equal(c(m$column_labels$age, m$value_labels$age[["30"]]), c("Age", "thirty"))
  expect_length(list.files(pre, "^NL\\.meta\\.json$", recursive = TRUE), 0)  # csv has no labels
  unlink(meta_file)
  r <- preprocess_rows(rows, root, pre)  # a missing meta.json is made again
  expect_equal(c(r$done, r$skipped), c(1, 1))

  r <- consolidate_rows(rows, root, pre, out)
  expect_equal(c(r$done, r$skipped, nrow(r$failed)), c(1, 0, 0))
  w1 <- ggp_read(file.path(out, "ggs-ii_w1.parquet"))
  expect_equal(sort(w1$age), sort(c("30", "40", "n/a")))  # a number and a text column become text
  expect_equal(sum(is.na(w1$x)), 2L)
  expect_false(any(c("aut01_2101", "dem07_1801") %in% names(w1)))  # country-specific: left out
  expect_equal(w1$att12_2101[w1$ggp_country == "NL"], 7)  # Austria's number, not NL's
  r <- consolidate_rows(rows, root, pre, out)  # up to date: kept
  expect_equal(c(r$done, r$skipped), c(0, 1))
  r <- consolidate_rows(rows[2, ], root, pre, out)  # other files: made again
  expect_equal(c(r$done, r$skipped), c(1, 0))
  expect_equal(ggp_read(file.path(out, "ggs-ii_w1.parquet"))$ggp_country, "NL")
  r <- consolidate_rows(rows, root, pre, out)  # AT back in, not "already present"
  expect_equal(c(r$done, r$skipped), c(1, 0))
  expect_equal(sort(unique(ggp_read(file.path(out, "ggs-ii_w1.parquet"))$ggp_country)), c("AT", "NL"))
  nl_parquet <- list.files(pre, "^NL\\.parquet$", recursive = TRUE, full.names = TRUE)
  Sys.setFileTime(file.path(out, "ggs-ii_w1.parquet"), file.mtime(nl_parquet) - 10)  # as if NL was preprocessed again after it
  expect_equal(consolidate_rows(rows, root, pre, out)$done, 1L)  # an input newer than the result
  expect_equal(consolidate_rows(rows, root, pre, out)$skipped, 1L)

  # a country not in COUNTRY_NUMBERS: its number from its own `country` variable, if the file has one country
  f <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data.frame(country = c(55, 55), v1_5501 = 1:2, v1_2101 = 1:2), f)
  expect_equal(country_columns(f, "XX"), "v1_5501")
  arrow::write_parquet(data.frame(country = c(55, 56), v1_5501 = 1:2), f)
  expect_equal(country_columns(f, ""), character())
  expect_length(list.files(d, "\\.part", recursive = TRUE, all.files = TRUE), 0)
})

test_that("merged labels are harmonized over the countries", {
  d <- withr::local_tempdir()
  files <- data.frame(file = character(), country = character(), src = character())
  for (x in list(
    list("AT", "Country", c("203" = "Czechia", "1" = "East Austria", "3" = "elementary school", "-1" = "Don't know")),
    list("HR", "Country", c("203" = "czechia ", "1" = "Pannonian Croatia", "4" = "Elementary school", "-1" = "Don't know")),
    list("GB", "Land", c("203" = "Czech Republic", "1" = "East Midlands", "-1" = "don't know")),
    list("GB_RU", "Страна", c("203" = "Чехия", "-1" = "Не знаю")))) {  # GB's second file: no vote
    f <- file.path(d, paste0(x[[1]], ".parquet"))
    write_json(list(column_labels = list(cc = x[[2]], gone = "x"), value_labels = list(cc = as.list(x[[3]]))), meta_of(f))
    files <- rbind(files, data.frame(file = f, country = substr(x[[1]], 1, 2), src = sprintf("ggs/%s/%s.dta", tolower(substr(x[[1]], 1, 2)), x[[1]])))
  }
  files <- rbind(files, data.frame(file = file.path(d, "NL.parquet"), country = "NL", src = "NL.csv"))  # csv: no meta.json, skipped
  m <- merged_meta(files, "cc")
  expect_equal(m$column_labels, list(cc = "Country"))  # the label most files use; dropped columns left out
  v <- m$value_labels$cc
  expect_equal(v[["203"]], "Czechia")  # majority (spelling and case variants count as one)
  expect_equal(v[["1"]], "East Austria / Pannonian Croatia / East Midlands")  # no majority: all of them
  expect_equal(v[["-1"]], "Don't know")  # 3 of 3 countries: the Russian file does not make it a minority
  expect_equal(c(v[["3"]], v[["4"]]), c("elementary school", "Elementary school"))  # two values may share a label
  expect_true(all(vapply(v, \(x) is.character(x) && length(x) == 1, logical(1))))  # every value has exactly one label
  expect_equal(m$original_labels$cc[["1"]], list(`East Austria` = list("ggs/at/AT.dta"), `Pannonian Croatia` = list("ggs/hr/HR.dta"),
                                                 `East Midlands` = list("ggs/gb/GB.dta")))
  expect_equal(m$original_labels$cc[["-1"]][["Не знаю"]], list("ggs/gb/GB_RU.dta"))  # every file's wording is kept
  expect_null(m$original_labels$cc[["3"]])  # labelled one way only
})

test_that("the GGS pipeline: download, preprocess, consolidate, Stata", {
  d <- withr::local_tempdir()
  dta <- file.path(d, "AT_w1.dta")  # a real labelled file in the fake AT download
  labelled_dta(dta, data.frame(age = c(1, 2, NA), aut01_2101 = c(1, 1, 1), note = c("a", "b", strrep("x", 3000))),
               labels = list(age = "Age group", aut01_2101 = "AT only", note = "Note"), value_labels = list(age = c(young = 1, old = 2)))
  local_portal(at_zip = make_zip(list("Data/AT_w1.dta" = readBin(dta, "raw", file.size(dta)), "Data/AT_w1.sav" = "sav")))
  unlink(dta)
  data <- file.path(d, "data")
  run <- function() {
    p <- ggp_client(data, token(), project = "ggs")
    ggp_sync(p)
    ggp_login(p)
    list(download = ggp_download(p, ext = "dta"), preprocess = ggp_preprocess(p), consolidate = ggp_consolidate(p),
         stata = ggp_stata(p))
  }
  steps <- suppressMessages(run())
  expect_equal(steps$download$failed$name, "ggs2-nl-wave1")  # the fake NL file is a web page: reported
  expect_equal(c(steps$download$done, steps$stata$done), c(1, 1))
  expect_equal(tree(file.path(data, "download/raw")), "ggs-ii/w1/at/ggs2_w1_at_v1.1/data/at_w1.dta")  # GGS only, dta only
  cons <- file.path(data, "download/consolidated")
  expect_equal(tree(cons), c("ggs-ii_w1.dta", "ggs-ii_w1.meta.json", "ggs-ii_w1.parquet"))
  meta <- jsonlite::read_json(file.path(cons, "ggs-ii_w1.meta.json"))
  expect_equal(meta$value_labels$age, list(`1` = "young", `2` = "old"))
  expect_false("aut01_2101" %in% names(meta$column_labels))
  expect_false("aut01_2101" %in% parquet_columns(file.path(cons, "ggs-ii_w1.parquet")))  # country-specific: left out
  st <- haven::read_dta(file.path(cons, "ggs-ii_w1.dta"))
  expect_equal(attr(st$age, "labels"), c(young = 1, old = 2))  # not the "a" of an extended missing value
  expect_equal(attr(st$age, "label"), "Age group")
  expect_equal(as.vector(st$ggp_country), rep("AT", 3))
  expect_equal(nchar(as.vector(st$note)[3]), 3000)  # haven writes long text whole
  again <- suppressMessages(run())
  expect_equal(c(again$stata$done, again$stata$skipped), c(0, 1))  # up to date: kept
  expect_equal(c(again$preprocess$done, again$consolidate$done), c(0, 0))
})

test_that("steps can run in the background", {
  skip_if_not_installed("mirai")
  skip_if_not(requireNamespace("ggpclient", quietly = TRUE) && "async" %in% names(formals(ggpclient::ggp_preprocess)),
              "the installed ggpclient is not this version")
  d <- withr::local_tempdir()
  labelled_dta(file.path(d, "download/raw/ggs-ii/w1/at/ggs2_w1_at_v1.1/a.dta"), data.frame(age = c(30, 40)))
  con <- db_connect(file.path(d, "catalogue.sqlite"))
  db_upsert(con, "ggs2-at-wave1", parse_dataset(AT, "ggs2-at-wave1"))
  db_mark_downloaded(con, 95L, "ggs-ii/w1/at/ggs2_w1_at_v1.1", "dta")
  DBI::dbDisconnect(con)
  p <- ggp_client(d, project = "ggs")
  m <- ggp_preprocess(p, async = TRUE)  # returns at once
  expect_true(inherits(m, "mirai"))
  r <- m[]
  expect_s3_class(r, "ggp_result")
  expect_equal(r$done, 1L)
  m2 <- ggp_consolidate(p, async = TRUE)
  expect_equal(m2[]$done, 1L)
  expect_true(file.exists(file.path(d, "download/consolidated/ggs-ii_w1.parquet")))
})

test_that("a background step that fails returns an error value", {
  skip_if_not_installed("mirai")
  skip_if_not(requireNamespace("ggpclient", quietly = TRUE) && "async" %in% names(formals(ggpclient::ggp_preprocess)),
              "the installed ggpclient is not this version")
  p <- ggp_client(withr::local_tempdir())
  p$dir <- "/proc/no/such/dir"  # the background process cannot create it
  expect_true(mirai::is_error_value(ggp_preprocess(p, async = TRUE)[]))
})
