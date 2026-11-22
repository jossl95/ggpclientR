test_that("slugs and naming", {
  p <- parse_slug("ggs2-at-wave1")
  expect_equal(c(p$project, p$round, p$series, p$wave, p$country), c("GGS", "2", "GGS-II", "w1", "AT"))
  expect_equal(naming(p, "ggs2-at-wave1", "1.1"), list(rel_dir = "ggs-ii/w1/at", stem = "ggs2_w1_at_v1.1"))
  expect_equal(parse_slug("ggs2-at-wave1-v1-0")$version, "1.0")
  expect_equal(parse_slug("ggs2-se-wave1register")$wave, "w1register")
  expect_true(parse_slug("upcoming-ggs2-fr-wave1")$upcoming)
  ffs <- parse_slug("ffs-us-thefamilyandfertilitysurvey")
  expect_equal(naming(ffs, "x", "1.0"), list(rel_dir = "ffs/us", stem = "ffs_us_v1.0"))
  expect_equal(parse_slug("weird")$project, "OTHER")
  expect_equal(parse_slug("hh-harmonized-histories")$wave, "harmonized-histories")
})

test_that("the catalogue keeps only dataset pages", {
  expect_equal(parse_catalog(CATALOG), c("ggs2-at-wave1", "ggs2-nl-wave1", "upcoming-ggs2-fr-wave1",
                                         "ffs-us-thefamilyandfertilitysurvey"))
  expect_true(is.na(slug_from_href("/dataset/../x/")))
})

test_that("a dataset page", {
  d <- parse_dataset(AT, "ggs2-at-wave1")
  expect_equal(c(d$version, d$country_name), c("1.1", "Austria"))
  expect_equal(c(d$collected_from, d$collected_to), c("2022-10-31", "2023-03-12"))
  expect_match(d$colectica_url, "/c7fc$")
  expect_equal(d$other_versions, c("ggs2-at-wave1-v1-0", "ggs2-at-wave1-v1-1"))
  expect_equal(d$files, list(list(download_id = 95L, url = "https://www.ggp-i.org/form/download/95/",
                                  label = "Austria - Wave 1 (version 1.1) v1.1", size_mb = 15.3)))
  expect_equal(parse_dataset(NL, "ggs2-nl-wave1")$files[[1]]$size_mb, 1228.8)
  expect_length(parse_dataset(FR, "x")$files, 0)
})

test_that("the data access page lists the hidden sections once", {
  d <- parse_access(FORM)
  expect_equal(names(d), c("hh-harmonized-histories", "other-nkps"))
  expect_equal(vapply(d[["hh-harmonized-histories"]]$files, `[[`, integer(1), "download_id"), c(1L, 84L))
  expect_equal(d[["other-nkps"]]$files[[1]]$label, "NKPS")
})

test_that("the slug order is the catalogue order", {
  expect_equal(sort_slugs(c("ggs2-nl-wave1", "ffs-us-thefamilyandfertilitysurvey", "ggs2-at-wave1", "ggs1-at-wave1")),
               c("ffs-us-thefamilyandfertilitysurvey", "ggs1-at-wave1", "ggs2-at-wave1", "ggs2-nl-wave1"))
})
