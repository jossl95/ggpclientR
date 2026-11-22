# The R package and the Python package (ggp_client, used through reticulate, as in the docs:
# https://jossl95.github.io/ggp_client/r.html) process the same downloads: the results must be the same.
# Needs reticulate and a Python with ggp_client (RETICULATE_PYTHON); skipped otherwise.

test_that("R and Python preprocess and consolidate to the same data and labels", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_available(initialize = TRUE) && reticulate::py_module_available("ggp_client"),
              "Python package ggp_client not available")
  pyread <- reticulate::import("ggp_client.read")

  d <- withr::local_tempdir()
  r_dir <- file.path(d, "r")
  py_dir <- file.path(d, "py")
  dir.create(r_dir)
  con <- db_connect(file.path(r_dir, "catalogue.sqlite"))
  for (x in list(list("ggs2-at-wave1", AT, "at"), list("ggs2-nl-wave1", NL, "nl"))) {
    db_upsert(con, x[[1]], parse_dataset(x[[2]], x[[1]]))
  }
  rows <- db_select(con, downloadable = TRUE)
  raw <- file.path(r_dir, "download/raw")
  labelled_dta(file.path(raw, "ggs-ii/w1/at/ggs2_w1_at_v1.1/data/at.dta"),
               data.frame(id = c(1, 2), age = c(30, 40), aut01_2101 = c(1, 1), t = c("a", "b")),
               labels = list(age = "Age", aut01_2101 = "AT only"), value_labels = list(age = c(thirty = 30, forty = 40)))
  labelled_dta(file.path(raw, "ggs-ii/w1/nl/ggs2_w1_nl_v2.0/data/nl.dta"),
               data.frame(id = 3, age = 50, att12_2101 = 7, x = 1.5),
               labels = list(age = "Age in years"), value_labels = list(age = c(thirty = 30, `forty ` = 40, fifty = 50)))
  for (i in seq_len(nrow(rows))) db_mark_downloaded(con, rows$download_id[i], rows$rel_dir[i] |> paste0("/", rows$stem[i]), "dta")
  DBI::dbDisconnect(con)
  dir.create(py_dir)
  file.copy(list.files(r_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE), py_dir, recursive = TRUE)

  p <- ggp_client(r_dir, project = "ggs")
  expect_equal(nrow(ggp_rows(p)), 2L)
  ggp_preprocess(p)
  ggp_consolidate(p)

  py <- \(...) expect_equal(system2(reticulate::py_exe(), c("-m", "ggp_client.cli", "--data-dir", py_dir, ...),
                                    stdout = FALSE, stderr = FALSE), 0L)
  py("preprocess", "--project", "ggs")
  py("consolidate", "--project", "ggs")

  name <- "download/consolidated/ggs-ii_w1"
  r_df <- ggp_read(file.path(r_dir, paste0(name, ".parquet")))
  py_cols <- pyread$read(file.path(py_dir, paste0(name, ".parquet")))$to_dict(as_series = FALSE) # column -> values
  py_df <- lapply(py_cols, \(col) vapply(col, \(v) if (is.null(v)) NA_character_ else as.character(v), ""))
  expect_equal(names(r_df), names(py_df))
  expect_equal(nrow(r_df), length(py_df[[1]]))
  for (column in names(r_df)) {
    expect_equal(unname(py_df[[column]]), as.character(r_df[[column]]), info = column)
  }
  expect_false("aut01_2101" %in% names(r_df))  # Austria's variable; att12_2101 is 2101 too, but NL is 18, so it stays

  meta <- \(dir) jsonlite::read_json(file.path(dir, paste0(name, ".meta.json")))
  expect_equal(meta(r_dir), meta(py_dir))
  expect_equal(meta(r_dir)$value_labels$age[["40"]], "forty")  # majority: "forty" and "forty " are one label
})
