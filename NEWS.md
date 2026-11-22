# ggpclient 0.1.0

* Parquet is read and written with arrow (nanoparquet is gone). Consolidation streams file by file: about 2.5 GB at most
  for GGS-II wave 1. `ggp_read(lazy = TRUE)` returns an arrow dataset.
* All concurrency is mirai (also on Windows): a download and its unpacking are one job, so archives do not pile up.
* The login is shared with the Python package (`.session.json`). Every client logs to its own file.

* First version: an R-native version of the Python package ggp_client. Sync, login, download, preprocess to parquet with
  `meta.json`, consolidate per round and wave with harmonized labels, Stata export, FFS conversion.
* Checked against the Python package on the live catalogue and on the 69 GGS files of both rounds: same rows, columns and
  labels.
* dta, sav and rds are read with haven and keep their labels (`labelled = TRUE`); rds is a datafile type, also as `.rds.7z`.
* `async = TRUE` on the download, preprocess, consolidate and Stata steps (mirai).
