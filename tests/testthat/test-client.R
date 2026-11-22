read_text <- function(path) rawToChar(readBin(path, "raw", file.size(path)))

test_that("a client holds the catalogue and the rows its filters select", {
  local_portal()
  d <- withr::local_tempdir()
  p <- ggp_client(d, token(), project = "ggs", round = 2, wave = "w1", country = c("at", "nl"))
  expect_equal(nrow(ggp_rows(p)), 0L)  # empty catalogue until the first sync
  s <- ggp_sync(p)
  expect_equal(ggp_rows(p)$slug, c("ggs2-at-wave1", "ggs2-nl-wave1"))  # refreshed by sync, filters applied
  expect_s3_class(s, "ggp_result")
  expect_equal(formals(ggp_client)$data_dir, "data")
  expect_output(print(p), "2 files")
  expect_error(ggp_client(d, concurrency = 0), "concurrency")
  expect_false(grepl("s3cr3t", paste(capture.output(str(p)), collapse = "\n")))  # printing never shows the link
})

test_that("sync indexes the catalogue, download unpacks it, and every step can be repeated", {
  local_portal()
  d <- withr::local_tempdir()
  p <- ggp_client(d, token())
  s <- ggp_sync(p)
  # v1.0 via "other versions" (v1.1 is the latest: one row), HH and NKPS via the access page
  expect_equal(c(s$done, nrow(s$failed)), c(7L, 0L))
  con <- p$con
  all <- db_select(con, all_versions = TRUE)  # ids follow project, round, wave, country: FFS first, then GGS-II
  ids <- stats::setNames(all$id, all$slug)
  expect_equal(ids[!duplicated(names(ids))],
               c("ffs-us-thefamilyandfertilitysurvey" = 1L, "ggs2-at-wave1" = 2L, "ggs2-at-wave1-v1-0" = 3L,
                 "upcoming-ggs2-fr-wave1" = 4L, "ggs2-nl-wave1" = 5L, "hh-harmonized-histories" = 6L, "other-nkps" = 7L))
  slugs <- \(...) db_select(con, ...)$slug
  expect_equal(slugs(), c("ffs-us-thefamilyandfertilitysurvey", "ggs2-at-wave1", "upcoming-ggs2-fr-wave1",
                          "ggs2-nl-wave1", "hh-harmonized-histories", "hh-harmonized-histories", "other-nkps"))
  expect_equal(slugs(country = "nl"), slugs(country = "NL"))  # one value, any case
  expect_equal(slugs(country = c("nl", "at")), c("ggs2-at-wave1", "ggs2-nl-wave1"))
  expect_equal(slugs(project = c("hh", "other")), c("hh-harmonized-histories", "hh-harmonized-histories", "other-nkps"))
  expect_equal(slugs(project = "other", wave = "NKPS"), "other-nkps")
  expect_equal(slugs(round = "ii"), slugs(round = 2))
  expect_error(db_select(con, round = 1), "unknown round")  # no GGS-I in this catalogue
  expect_error(db_select(con, country = c("nl", "xx")), "unknown country")

  ggp_login(p)
  dl <- file.path(d, "dl")
  r <- ggp_download(p, rows = db_select(con, downloadable = TRUE), root = dl)
  expect_equal(c(r$done, r$skipped), c(5L, 0L))
  expect_equal(r$failed$name, "ggs2-nl-wave1")
  expect_match(r$failed$error, "web page")

  at_dir <- file.path(dl, "ggs-ii/w1/at/ggs2_w1_at_v1.1")
  ffs_dir <- file.path(dl, "ffs/us")
  conv_dir <- file.path(d, "download/processed/ffs/us")
  expect_equal(tree(at_dir), c("data/at_w1.dta", "data/at_w1.sav"))
  expect_equal(tree(ffs_dir), c("ffs_us_v1.0.dat", "ffs_us_v1.0.sps", "ffs_us_v1.0.tbl"))  # FFS: raw here
  expect_equal(tree(conv_dir), "converted_data.dta")  # and converted (dta by default) in download/processed
  sps <- read_text(file.path(ffs_dir, "ffs_us_v1.0.sps"))  # rewritten: names with _, FILE= the new .dat
  expect_true(startsWith(sps, "DATA LIST FILE='ffs_us_v1.0.dat' records=2\r\n"))
  expect_true(grepl("V004_01  15-16", sps, fixed = TRUE) && grepl("\r\n V005_01\r\n", sps, fixed = TRUE))
  expect_true(grepl('"Caseweight in US$"', sps, fixed = TRUE) && !grepl("$01", sps, fixed = TRUE))
  expect_equal(read_text(file.path(ffs_dir, "ffs_us_v1.0.tbl")), "TABLE 4 BY IND01_01\r\nincome US$ 100\r\n")  # names only
  expect_equal(read_text(file.path(at_dir, "data/at_w1.dta")), "dta")
  got <- DBI::dbGetQuery(con, "SELECT local_path, fetched FROM file WHERE download_id = 95")
  expect_equal(unlist(got, use.names = FALSE), c("ggs-ii/w1/at/ggs2_w1_at_v1.1", "csv dta sav"))
  expect_length(list.files(d, pattern = "\\.part$", recursive = TRUE, all.files = TRUE), 0)
  expect_equal(format(file.mode(file.path(d, ".session.json"))), "600")
  expect_false(grepl("s3cr3t", read_text(file.path(d, ".session.json"))))

  again <- function(force = FALSE, ...) {  # a new client: no access url, the saved session is enough
    q <- ggp_client(d, force = force)
    ggp_login(q)
    r <- ggp_download(q, root = dl, ...)
    c(r$done, r$skipped)
  }
  expect_equal(tree(file.path(dl, "hh/harmonized-histories")),
               c("hh_harmonized-histories_1/x.dta", "hh_harmonized-histories_84/x.dta"))  # two files: a folder each
  expect_equal(tree(file.path(dl, "other/nkps")), "other_nkps/x.dta")
  expect_equal(again(), c(0, 5))  # same request: nothing to do
  expect_equal(again(ext = "dta"), c(0, 5))  # a subset for the others; FFS already has its dta
  expect_equal(again(ext = "csv"), c(0, 5))  # FFS converts to dta or sav only: csv means its default dta
  expect_equal(again(ext = "sav"), c(1, 4))  # FFS converted to sav as well
  expect_equal(tree(conv_dir), c("converted_data.dta", "converted_data.sav"))
  writeBin(charToRaw("my edits"), file.path(at_dir, "data/at_w1.dta"))
  expect_equal(again(metadata = TRUE), c(5, 0))
  expect_equal(read_text(file.path(at_dir, "data/at_w1.dta")), "my edits")  # fetched again for the docs: file kept
  expect_equal(tree(at_dir), c("data/at_w1.dta", "data/at_w1.sav", "docs/Codebook.pdf"))
  expect_equal(tree(ffs_dir), c("docs/readme.txt", "ffs_us_v1.0.dat", "ffs_us_v1.0.sps", "ffs_us_v1.0.tbl"))
  expect_length(list.files(dirname(ffs_dir), pattern = "^\\.unpack-", all.files = TRUE), 0)  # the temporary folder is gone
  expect_equal(again(metadata = TRUE), c(0, 5))
  expect_equal(again(force = TRUE), c(5, 0))  # force: everything again, existing files overwritten
  expect_equal(read_text(file.path(at_dir, "data/at_w1.dta")), "dta")
  unlink(ffs_dir, recursive = TRUE)  # raw files deleted by hand: downloaded again
  expect_equal(again(), c(1, 4))
  expect_equal(again(ext = "dta"), c(0, 5))  # no ext converts to dta too
  expect_true("ffs_us_v1.0.dat" %in% tree(ffs_dir) && "converted_data.dta" %in% tree(conv_dir))

  # every run is in the log next to the data, the access link never is
  text <- read_text(file.path(d, "ggpclient.log"))
  for (line in c("INFO sync: indexed 7 datasets, 0 failed", "INFO login: logged in with the access link",
                 "INFO login: saved session reused", "INFO downloaded ggs2-at-wave1: ", "files kept in ggs-ii/w1/at/ggs2_w1_at_v1.1",
                 "ERROR failed ggs2-nl-wave1: got a web page", "INFO skipped ffs-us-thefamilyandfertilitysurvey: already in ffs/us")) {
    expect_true(grepl(line, text, fixed = TRUE), info = line)
  }
  expect_false(grepl("s3cr3t", text, fixed = TRUE))
})

test_that("an ext without matches fails and leaves no folder", {
  local_portal()
  d <- withr::local_tempdir()
  p <- ggp_client(d, token(), country = "at")
  ggp_sync(p)
  r <- ggp_download(p, root = file.path(d, "dl"), ext = "csv")
  expect_equal(r$done, 0L)
  expect_match(r$failed$error, "looked for csv")
  expect_false(dir.exists(file.path(d, "dl/ggs-ii/w1/at/ggs2_w1_at_v1.1")))
  expect_true(is.na(DBI::dbGetQuery(p$con, "SELECT local_path FROM file WHERE download_id = 95")[[1]]))
})

test_that("login errors never leak the link", {
  local_portal()
  d <- withr::local_tempdir()
  local_mocked_bindings(askpass = \(prompt) NULL, .package = "askpass")  # nobody to ask
  expect_error(ggp_login(ggp_client(d)), "no valid session", class = "ggp_login_error")
  e <- expect_error(ggp_login(ggp_client(d, paste0(ggp_base(), "/form/accounts/login/email/wrong-t0k3n/"))),
                    class = "ggp_login_error")
  expect_false(grepl("wrong-t0k3n", conditionMessage(e)))
  e <- expect_error(ggp_login(ggp_client(d, "https://evil.example/t0k3n/")), "must start with", class = "ggp_login_error")
  expect_false(grepl("t0k3n", conditionMessage(e)))  # never request some other site
})

test_that("the link is asked for only when needed", {
  local_portal()
  d <- withr::local_tempdir()
  asked <- 0
  local_mocked_bindings(askpass = \(prompt) { asked <<- asked + 1; paste0("  ", token(), "\n") }, .package = "askpass")
  ggp_login(ggp_client(d))  # no argument, no environment variable, no session: asked once
  expect_equal(asked, 1)
  expect_true(file.size(file.path(d, ".session.json")) > 0)
  ggp_login(ggp_client(d))  # the saved session works: not asked again
  expect_equal(asked, 1)
})

test_that("an environment variable is not used: the link is asked for", {
  local_portal()
  d <- withr::local_tempdir()
  local_mocked_bindings(askpass = \(prompt) NULL, .package = "askpass")
  withr::local_envvar(GGP_ACCESS_URL = token())  # a link is valid for seven days: it is not kept in the environment
  expect_error(ggp_login(ggp_client(d)), "no valid session", class = "ggp_login_error")
})

test_that("a broken session file means log in again", {
  local_portal()
  d <- withr::local_tempdir()
  local_mocked_bindings(askpass = \(prompt) NULL, .package = "askpass")
  writeLines("this is not a cookie jar", file.path(d, ".session.json"))  # a write that was cut off
  Sys.chmod(file.path(d, ".session.json"), "644")
  ggp_login(ggp_client(d, token()))
  expect_equal(format(file.mode(file.path(d, ".session.json"))), "600")
  ggp_login(ggp_client(d))  # and the new one is valid
})

test_that("sync without a login still indexes the public pages", {
  local_portal()
  d <- withr::local_tempdir()
  local_mocked_bindings(askpass = \(prompt) NULL, .package = "askpass")
  s <- ggp_sync(ggp_client(d))
  expect_equal(s$done, 5L)
  expect_equal(s$failed$name, "data access page")
  expect_match(s$failed$error, "skipped")
})

test_that("Content-Disposition file names", {
  expect_equal(disposition_filename('attachment; filename="AT w1.zip"'), "AT w1.zip")
  expect_equal(disposition_filename("attachment; filename*=UTF-8''AT%20w1.zip"), "AT w1.zip")
  expect_equal(disposition_filename("attachment; filename=plain.zip; size=3"), "plain.zip")
  expect_null(disposition_filename(NA_character_))
})

test_that("the Python package's .session.json is a login for R, and the other way round", {
  local_portal()
  d <- withr::local_tempdir()
  local_mocked_bindings(askpass = \(prompt) NULL, .package = "askpass")
  host <- sub("^https?://", "", sub(":[0-9]+$", "", ggp_base()))
  jsonlite::write_json(list(list(name = "sessionid", value = "s1", domain = host, path = "/")),
                       file.path(d, ".session.json"), auto_unbox = TRUE)
  p <- ggp_client(d)  # no link, no R session: the Python one is imported
  expect_no_error(ggp_login(p))
  expect_match(read_text(file.path(d, "ggpclient.log")), "saved session reused")

  e <- withr::local_tempdir()  # a login with the link leaves a .session.json for Python
  ggp_login(ggp_client(e, token()))
  cookies <- jsonlite::read_json(file.path(e, ".session.json"))
  expect_equal(c(cookies[[1]]$name, cookies[[1]]$value), c("sessionid", "s1"))
  expect_equal(format(file.mode(file.path(e, ".session.json"))), "600")
})

test_that("every client logs to its own file", {
  a <- withr::local_tempdir()
  b <- withr::local_tempdir()
  pa <- ggp_client(a)
  pb <- ggp_client(b)
  ggp_preprocess(pa)
  ggp_log(pb$log, "info", "only b")
  expect_false(file.exists(pa$log) && grepl("only b", read_text(pa$log)))
  expect_match(read_text(pb$log), "only b")
})
