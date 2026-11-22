# ggpclient (R): project plan

An R-native version of the Python package [ggp_client](https://github.com/jossl95/ggp_client): download, label and
consolidate the data of the GGP data portal (GGS-I, GGS-II, FFS, Harmonized Histories, Additional Datasets).
No Python needed.

## Goal and rules

1. **Same workflow, same results.** A user of the Python package can switch to R and find the same steps, the same
   folders and the same files.
2. **R at home.** Functions, pipes (`|>`), S3 classes, `cli` messages, roxygen docs, testthat tests. No `$` method
   calls in the user-facing workflow, no Python idioms (`async`, context managers, tuples).
3. **No shortcuts on safety.** The access link works like a password: never logged, stored, committed or put in an
   error message. Files are written whole or not at all. Existing files are kept unless `force`.

## The Python workflow, and its R form

```python
async with ggpClient(project="ggs", round=2) as p:   # data_dir defaults to "data"
    await p.sync()
    print(len(p.rows), "files")
    print(await p.download(ext="dta"))               # (done, skipped, failed)
    print(await p.preprocess())
    print(await p.consolidate())
```

```r
library(ggpclient)

p <- ggp_client(project = "ggs", round = 2)          # data_dir defaults to "data"

p |> ggp_sync()
nrow(ggp_rows(p)) |> cat("files\n")
p |> ggp_download(ext = "dta")                       # done, skipped, failed
p |> ggp_preprocess()
p |> ggp_consolidate()
```

| Python | R | Notes |
|---|---|---|
| `ggpClient(...)` | `ggp_client(...)` | an S3 object that holds its state in an environment, so steps update it in place |
| `async with` | none | nothing to close: connections live only inside a call |
| `p.rows` | `ggp_rows(p)` | a data frame, one row per file; refreshed after sync and download |
| `await p.sync()` | `ggp_sync(p)` | returns a `ggp_result` (found, failed) |
| `await p.login()` | `ggp_login(p)` | |
| `await p.download(rows, root, ext, metadata)` | `ggp_download(p, ext, metadata, rows, root)` | `p` first, so it pipes |
| `await p.preprocess()` | `ggp_preprocess(p)` | |
| `await p.consolidate()` | `ggp_consolidate(p)` | |
| `consolidate.to_stata_async(...)` | `ggp_stata(p)` | |
| `(done, skipped, failed)` | `ggp_result`: `done`, `skipped`, `failed` (a data frame) | prints as `download: 5 done, 2 skipped, 0 failed`; autoprints at the prompt |
| `ggp-client convert x.SPS` | `ggp_convert_ffs("x.SPS", "dta")` | no command line tool: R users work in R |
| `download_ggs.py` | `inst/scripts/download_ggs.R`, also `ggp_ggs()` | |
| `force`, `concurrency`, filters | same names, as arguments of `ggp_client()` | |

Python's `await` and worker processes become: parallel downloads in one curl session (`httr2::req_perform_parallel`)
and parallel file processing with `parallel::mclapply` (forked workers on Linux and macOS, one worker on Windows).

## Data contract (identical to the Python package)

A file written by one package must be read by the other.

```
data/catalogue.sqlite              same schema (dataset, file)
data/.session                      cookie jar of the login (mode 600)   <- Python: .session.json; not shared
data/ggpclient.log
data/download/raw/<series>/<wave>/<cc>/<stem>/.../<file>.dta     lower-case folders and datafile names
data/download/processed/.../<file>.parquet  +  <file>.meta.json
data/download/consolidated/<series>_<wave>.parquet  +  .meta.json  +  .dta
```

- Provenance columns `ggp_project`, `ggp_round`, `ggp_series`, `ggp_wave`, `ggp_country`, `ggp_version`, `ggp_file`.
- `meta.json`: `file_label`, `column_labels`, `value_labels` (codes as text), `missing_ranges`, `original_types`;
  consolidated: harmonized `column_labels` and `value_labels` plus `original_labels`.
- Label harmonization: one vote per country; variable label by plurality; value label by majority, else all joined
  with " / "; codes never change; several codes may share a label.
- Country-specific variables (`..._<GGP country number><nn>`) left out of the consolidated files.
- The session file is not shared: a cookie jar is the natural format for httr2/curl.

## Modules

| Python | R file | Contents |
|---|---|---|
| `parse.py` | `R/parse.R` | slugs, naming, catalogue/dataset/access page parsing (`rvest`) |
| `db.py` | `R/db.R` | SQLite catalogue (`DBI`, `RSQLite`), `select` with the same filters |
| `client.py` | `R/client.R` | `ggp_client`, sync, login, download |
| `extract.py` | `R/extract.R` | unpack zip and 7z (`archive`), safe paths, lower-case names, nested archives |
| `read.py` | `R/read.R` | `ggp_read()`, `ggp_meta()` for parquet, dta, sav, csv, sps (`haven`, `nanoparquet`) |
| `processing.py` | `R/ffs.R`, `R/processing.R` | FFS `.sps`/`.dat` to labelled dta/sav; preprocess to parquet + meta |
| `consolidate.py` | `R/consolidate.R` | stack per series and wave, label harmonization, Stata export |
| `cli.py` | (none) | |

## Choices

- **Data frames:** base `data.frame`, no tibble: one dependency less. `haven` returns tibbles when reading; they are
  plain data frames for everything we do.
- **Parquet:** `nanoparquet`. Small, no system library, reads and writes data frames. `arrow` is the bigger engine and
  the upgrade path for data larger than memory (see risks).
- **Archives:** `archive` (libarchive reads zip and 7z, whatever the extension says; the portal serves zips named .7z).
- **HTTP:** `httr2`; cookies in a curl cookie jar; retries with `req_retry`.
- **Messages and logs:** `cli` on screen, the same lines appended to `data/ggpclient.log`. The access link is
  never part of a message; HTTP errors are rewritten before they are shown.
- **Tests:** `testthat` with `webfakes` serving the same synthetic portal as the Python tests (no network).
  Parity check: the R package and the Python package both process the same archive and the outputs are compared.

## Milestones

1. Skeleton, planning, parse + db + tests (no network). Parity with Python on the same HTML.
2. Client: sync, login, download, extract, against the fake portal.
3. Read, meta, FFS conversion, preprocess.
4. Consolidate, label harmonization, Stata export.
5. Docs: README, `?help` pages, a pkgdown site; `inst/scripts/download_ggs.R`.
6. Parity run on real GGS-II data; then CRAN-style checks (`R CMD check`).

## Risks and shortcuts

- **Memory.** polars stacks 20 countries lazily; in R a consolidated file (GGS-II wave 1: 185,660 rows x 4,001
  columns, about 6 GB as doubles) is held in memory. The consolidation is therefore one group at a time.
  Upgrade: DuckDB or arrow datasets, which stream. Not needed until someone hits the limit.
- **Encoding.** Some GGS `.dta` files carry broken text encoding; Python falls back to latin1. `haven` must be
  checked on those 16 files.
- **File label.** `haven` does not expose a Stata file label; `meta.json` keeps the key with an empty value.
- **Windows.** No forked workers there: `concurrency` applies to downloads only. Stated in the docs.
- **Stata text.** Python cuts text at 2,045 characters (pyreadstat has no strL). Whatever `haven` does is kept and
  documented.

## Open questions

- Repository: a private repo `jossl95/ggpclientR`. Creating it needs a person: the session may not create
  repositories. Until then the work is committed locally.
- Package name: `ggpclient` (R names allow no underscore); repo name `ggpclientR`.
- Licence and citation: follow the Python repo.
