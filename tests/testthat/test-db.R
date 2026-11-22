fill <- function(con) {
  db_upsert(con, "ggs2-at-wave1", parse_dataset(AT, "ggs2-at-wave1"))
  db_upsert(con, "ggs2-at-wave1-v1-0", parse_dataset(AT10, "ggs2-at-wave1-v1-0"))
  db_upsert(con, "ggs2-nl-wave1", parse_dataset(NL, "ggs2-nl-wave1"))
  db_upsert(con, "ffs-us-thefamilyandfertilitysurvey", parse_dataset(FFS, "ffs-us-thefamilyandfertilitysurvey"))
}

test_that("upsert keeps ids and local paths; select filters", {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  withr::defer(DBI::dbDisconnect(con))
  fill(con)
  rows <- db_select(con)
  expect_equal(rows$slug, c("ggs2-at-wave1", "ggs2-nl-wave1", "ffs-us-thefamilyandfertilitysurvey"))  # latest only
  expect_equal(db_select(con, all_versions = TRUE)$slug[2], "ggs2-at-wave1-v1-0")
  expect_equal(db_select(con, project = "ggs", round = "ii", country = "nl")$download_id, 96L)
  expect_equal(db_select(con, wave = 1)$country, c("AT", "NL"))
  expect_equal(db_select(con, ids = "2-3")$slug, "ggs2-nl-wave1")  # id 2 is the superseded AT version
  expect_equal(db_select(con, ids = "2-3", all_versions = TRUE)$slug, c("ggs2-at-wave1-v1-0", "ggs2-nl-wave1"))
  expect_error(db_select(con, country = "xx"), "unknown country", class = "ggp_error")

  db_mark_downloaded(con, 95L, "ggs-ii/w1/at/ggs2_w1_at_v1.1", c("sav", "dta"))
  fill(con)  # a second sync
  r <- db_select(con, country = "AT")
  expect_equal(c(r$local_path, r$fetched, r$id), c("ggs-ii/w1/at/ggs2_w1_at_v1.1", "dta sav", "1"))
  expect_equal(r$n_files, 1L)
})

test_that("ids are read as ranges, in either direction", {
  expect_equal(parse_ids("3-7,10"), list(c(3L, 7L), c(10L, 10L)))
  expect_equal(parse_ids("7-3"), list(c(3L, 7L)))
})

test_that("a re-sync drops replaced links and keeps the old rows of a page with no link", {
  con <- db_connect(":memory:")
  withr::defer(DBI::dbDisconnect(con))
  page <- list(version = "1.0", country_name = "AT", title = "t", collected_from = NA_character_,
               collected_to = NA_character_, colectica_url = NA_character_)
  for (ids in list(95L, 97L, integer())) {  # link 95 replaced by 97, then a page where no link was found
    files <- lapply(ids, \(i) list(download_id = i, label = "a", size_mb = 1, url = paste0("u", i)))
    db_upsert(con, "ggs2-at-wave1", c(page, list(files = files)))
  }
  r <- db_select(con, downloadable = TRUE)
  expect_equal(c(r$download_id, r$n_files), c(97L, 1L))
})
