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
