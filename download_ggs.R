#!/usr/bin/env Rscript
# Download all GGS data, rounds 1 and 2 (latest versions), as Stata files, and consolidate it per round and wave.
#
#   Rscript download_ggs.R              # into data/
#   Rscript download_ggs.R other_dir    # elsewhere
#
# 1. sync the catalogue and download every GGS dataset, keeping the .dta files   -> data/download/raw
# 2. preprocess each to parquet, with its labels in a .meta.json                 -> data/download/processed
# 3. consolidate per round and wave: <series>_<wave>.parquet and .meta.json      -> data/download/consolidated
# 4. the same as Stata, with the variable and value labels applied: <series>_<wave>.dta
#
# Asks for the emailed access link if there is no valid session (see the README). Work already done is kept.
# Progress is logged in <data_dir>/ggpclient.log. Exits with 1 if any step failed for any file.
library(ggpclient)

data_dir <- commandArgs(trailingOnly = TRUE)[1]
p <- ggp_client(if (is.na(data_dir)) "data" else data_dir, project = "ggs") # project GGS is rounds 1 and 2

p |> ggp_sync()
p |> ggp_login()
steps <- list(
  download = p |> ggp_download(ext = "dta"),
  preprocess = p |> ggp_preprocess(),
  consolidate = p |> ggp_consolidate(),
  stata = p |> ggp_stata()
)
for (step in steps) print(step)

quit(status = as.integer(any(vapply(steps, \(s) nrow(s$failed) > 0, logical(1)))))
