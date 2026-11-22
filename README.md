# ggpclient

Download data from the [GGP data portal](https://www.ggp-i.org) (Generations and Gender Programme): GGS rounds 1 and 2,
FFS, Harmonized Histories and Additional Datasets. It indexes the portal, downloads and unpacks the files, converts
them to parquet with their labels, and stacks the countries of each round and wave into one dataset.

This is the R version of the Python package [ggp_client](https://github.com/jossl95/ggp_client): the same steps, the
same folders, the same files. No Python needed.

## Getting started

```r
# install.packages("pak"); pak::pak("jossl95/ggpclientR")
library(ggpclient)

p <- ggp_client(project = "ggs", round = 2)   # data_dir defaults to "data"
p |> ggp_sync()                               # index the portal into data/catalogue.sqlite
ggp_rows(p)                                   # what is there: one row per file
```

Downloading needs the access link the portal e-mails you. The first download asks you to paste it (the input is
hidden; this needs the askpass package), then the session is saved in `data/.session` and reused. You can also put
`GGP_ACCESS_URL=<link>` in your user-level `~/.Renviron` (`usethis::edit_r_environ()`). Treat the link like a password:
do not commit it or type it in a script, where it stays in your history.

## Use case: GGS-II in Stata format, consolidated to parquet

```r
library(ggpclient)

p <- ggp_client(project = "ggs", round = 2)

p |> ggp_sync()
p |> ggp_download(ext = "dta")   # every GGS-II country, .dta only: done, skipped, failed
p |> ggp_preprocess()            # each file to parquet + meta.json
p |> ggp_consolidate()           # one parquet per wave, all countries stacked
```

Each step returns a result that prints as `download: 5 done, 2 skipped, 0 failed`, with the failures listed.
Result, in `data/`:

```
download/raw/ggs-ii/w1/at/ggs2_w1_at_v1.1/.../ggsii_wave1_at_v_1_1.dta          as downloaded
download/processed/ggs-ii/w1/at/ggs2_w1_at_v1.1/.../ggsii_wave1_at_v_1_1.parquet one file as parquet
download/processed/.../ggsii_wave1_at_v_1_1.meta.json                           its labels
download/consolidated/ggs-ii_w1.parquet                                         all countries of wave 1
download/consolidated/ggs-ii_w1.meta.json                                       their harmonized labels
```

The columns `ggp_country`, `ggp_round`, `ggp_wave`, `ggp_version` and `ggp_file` say where each row came from.
Country-specific variables (named after the GGP country number, such as `aut01a_2101` for Austria) are left out of the
consolidated file, but kept in the per-country files. `p |> ggp_stata()` also writes each consolidated dataset as a
labelled Stata file (`ggs-ii_w1.dta`); `download_ggs.R` does all of this for both GGS rounds.

## Labels (meta.json)

The parquet files hold the codes. Their labels, as haven reads them from the `.dta` and `.sav` files, are in the
`.meta.json` next to them:

```r
df <- ggp_read("data/download/consolidated/ggs-ii_w1.parquet", col_select = c("ggp_country", "dem01"))
meta <- jsonlite::read_json("data/download/consolidated/ggs-ii_w1.meta.json")

meta$column_labels$dem01   # "Gender"                                       variable -> variable label
meta$value_labels$dem01    # list(`1` = "Male", `2` = "Female", ...)        variable -> value -> label

vl <- meta$value_labels$dem01
df$gender <- factor(df$dem01, levels = as.integer(names(vl)), labels = unlist(vl))
```

The consolidated labels are harmonized over the countries, each country voting once:

- every variable has one label: the one most countries use;
- every value has exactly one label: the one more than half of the countries use (ISO 203 is "Czechia", not "Czech
  Republic"); if there is no majority, all labels joined into one, most used first. Several values may share a label;
- codes are never changed;
- `original_labels` lists, for every value countries label differently, each wording and the files that use it.

## Other data

- **FFS** comes as fixed-width `.dat` with SPSS syntax (`.sps`) and frequency tables (`.tbl`), kept in
  `download/raw/ffs/<cc>/`. The download also converts it, with labels, to `download/processed/ffs/<cc>/converted_data.dta`
  (`ext = "sav"` for `.sav`). One dataset by hand: `ggp_convert_ffs("AUSTRIA.SPS", ext = "dta")`.
- **Harmonized Histories and Additional Datasets** are listed only after login; `ggp_sync()` logs in to index them
  (`project = "hh"` or `"other"`).
- **Filters** (`project`, `round`, `wave`, `country`, `ids`, `all_versions` in `ggp_client()`) apply to every step.
  Look at `ggp_rows(p)` first as a dry run.

## Good to know

- Nothing is downloaded or written twice: existing files are kept. `ggp_client(force = TRUE)` redoes everything,
  and replaces datafiles saved earlier under an uppercase name. A consolidated file is rebuilt by itself when its
  countries or input files change.
- Every step is logged to `data/ggpclient.log`; `options(ggpclient.verbose = TRUE)` shows every line on screen. The
  access link never is logged.
- Downloads run in one curl session; preprocessing and unpacking run in forked workers (`concurrency` at a time).
  Windows has no forked workers: there they run one at a time.
- Consolidating holds one wave in memory (GGS-II wave 1: 185,660 rows x 4,001 columns, about 6 GB).
- Three GGS-I files (Australia waves 1 and 2, Norway wave 1) give "404 Not Found" on the portal itself.
- GGS-II Belarus wave 1 comes in English and Russian: it is in the consolidated file twice. Filter on `ggp_file`.

Run the tests with `devtools::test()`.
