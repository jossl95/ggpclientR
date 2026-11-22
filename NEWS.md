# ggpclient 0.2.0

* Datafiles are saved in the country folder, named after their dataset: `download/raw/ggs-ii/w1/at/ggs2_w1_at_v1.1.dta`
  (and `processed/` likewise); documents go into a folder of that name. A wave that adds a name to a wave or the pilot
  gets a folder in it: `w2pilot` is `w2/pilot`, `pilotfollowup` is `pilot/followup`. Downloads in the old layout are
  still read.
* Variable names are lower case in everything written after a download (parquet, `meta.json`, FFS conversions,
  consolidated files); the raw downloads keep the portal's names.
* `ggp_label()` puts variable and value labels on a data frame; `ggp_meta()` also reads a parquet's `meta.json`.
* Country-specific variables: the file's own `country` variable is used before the table, as in the Python package.
* Downloads are tried again after a dropped connection, and finished downloads are recorded per batch.
* The login writes the cookie jar itself: a cookie jar written late by curl could overwrite the session.

# ggpclient 0.1.0

* Parquet is read and written with arrow. Consolidation uses DuckDB (`UNION ALL BY NAME`) for waves that fit in its memory
  and arrow, file by file, for wider ones (GGS-II wave 1: about 2.5 GB at most). `ggp_read(lazy = TRUE)` returns an arrow
  dataset.
* The login is one file, `.session.json`, shared with the Python package; curl's cookie jar is rebuilt from it.
* Background processes stay for the next step (`ggp_stop_workers()` ends them).
* Shared test fixtures with the Python package (`tests/testthat/fixtures`), checked in CI.
* A Quarto website (`docs/`), published to GitHub Pages.
* All concurrency is mirai (also on Windows): a download and its unpacking are one job, so archives do not pile up.
* Every client logs to its own file.

* First version: an R-native version of the Python package ggp_client. Sync, login, download, preprocess to parquet with
  `meta.json`, consolidate per round and wave with harmonized labels, Stata export, FFS conversion.
* Checked against the Python package on the live catalogue and on the 69 GGS files of both rounds: same rows, columns and
  labels.
* dta, sav and rds are read with haven and keep their labels (`labelled = TRUE`); rds is a datafile type, also as `.rds.7z`.
* `async = TRUE` on the download, preprocess, consolidate and Stata steps (mirai).
