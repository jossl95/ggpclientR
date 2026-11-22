# ggpclient 0.1.0

* First version: an R-native version of the Python package ggp_client. Sync, login, download, preprocess to parquet with
  `meta.json`, consolidate per round and wave with harmonized labels, Stata export, FFS conversion.
* Checked against the Python package on the live catalogue and on the 69 GGS files of both rounds: same rows, columns and
  labels.
* dta, sav and rds are read with haven and keep their labels (`labelled = TRUE`); rds is a datafile type, also as `.rds.7z`.
* `async = TRUE` on the download, preprocess, consolidate and Stata steps (mirai).
