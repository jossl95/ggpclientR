# Read a downloaded datafile: parquet, dta, sav, csv, or an FFS .sps (with its .dat).
# Values are the codes; value labels are not applied. In sav, values SPSS marks as missing are NA.

FORMATS <- c(".parquet", ".dta", ".sav", ".rds", ".csv", ".sps") # also the preference when one file comes in several
LABELLED <- c(".dta", ".sav", ".rds") # the types that can carry variable and value labels
ARCHIVES <- c(".7z", ".zip") # a datafile compressed on its own: x.rds.7z, x.dta.zip

file_type <- function(path) tolower(paste0(".", tools::file_ext(path)))

# the .dat of the same name, or the only .dat next to the .sps
dat_of <- function(sps) {
  dats <- list.files(dirname(sps), pattern = "\\.dat$", ignore.case = TRUE, full.names = TRUE)
  same <- dats[tolower(tools::file_path_sans_ext(basename(dats))) == tolower(tools::file_path_sans_ext(basename(sps)))]
  if (length(same) || length(dats) == 1) {
    return((if (length(same)) same else dats)[1])
  }
  cli::cli_abort("no .dat for {.file {sps}}", class = "ggp_error")
}

# every piece of text in a Stata or SPSS read: names, labels, text columns
utf8_ok <- function(x) {
  text <- c(names(x), unlist(lapply(x, \(col) c(attr(col, "label", exact = TRUE), names(attr(col, "labels", exact = TRUE)),
                                                 if (is.character(col)) col))))
  all(validUTF8(text))
}

read_stat <- function(path, n_max = Inf, user_na = FALSE) {
  args <- list(path, n_max = n_max)
  f <- if (file_type(path) == ".dta") haven::read_dta else haven::read_sav
  if (file_type(path) == ".sav") args$user_na <- user_na
  x <- tryCatch(do.call(f, args), error = \(e) NULL)
  if (is.null(x) || !utf8_ok(x)) { # some GGS files mix encodings: read as their .sav twins are
    x <- do.call(f, c(args, encoding = "latin1"))
  }
  x
}

# a plain vector: the codes, without haven's label, format and missing-value attributes
plain <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  for (a in c("label", "labels", "format.stata", "format.spss", "display_width", "na_values", "na_range", "width")) {
    attr(x, a) <- NULL
  }
  x
}

#' Read a datafile
#'
#' Reads one datafile into a data frame: `.parquet`, `.dta`, `.sav`, `.rds`, `.csv`, or an FFS `.sps` (with its
#' `.dat`). A file compressed on its own as `.7z` or `.zip` (`data.rds.7z`) is unpacked and read when it holds one
#' datafile.
#'
#' Stata, SPSS and rds files are read with haven, so the data keeps its labels: `labelled = TRUE` returns haven
#' labelled columns (value labels and variable labels as attributes, see [haven::as_factor()]). With `FALSE` the
#' values are the codes, as in the parquet files. In `.sav` files, values that SPSS marks as missing are `NA`.
#'
#' @param path The file.
#' @param col_select For parquet files: the columns to read (default all). Wide files are large.
#' @param lazy For parquet files: return an arrow dataset instead of reading the file, to filter and select with dplyr
#'   and `dplyr::collect()` only what you need. For waves too big for memory.
#' @param labelled Keep the labels of dta, sav and rds data (default), or return plain codes.
#' @return A data frame.
#' @examples
#' \dontrun{
#' df <- ggp_read("ggsii_wave1_at_v_1_1.dta")           # labelled
#' df$dem01 |> haven::as_factor()
#' ggp_read("data/download/consolidated/ggs-ii_w1.parquet", col_select = c("ggp_country", "dem01"))
#' ggp_read("data/download/consolidated/ggs-ii_w1.parquet", lazy = TRUE) |>
#'   dplyr::filter(ggp_country == "NL") |> dplyr::select(dem01) |> dplyr::collect()
#' }
#' @export
ggp_read <- function(path, col_select = NULL, labelled = TRUE, lazy = FALSE) {
  type <- file_type(path)
  if (lazy) {
    if (type != ".parquet") cli::cli_abort("{.arg lazy} needs a parquet file", class = "ggp_error")
    return(arrow::open_dataset(path)) # nothing is read until dplyr::collect()
  }
  if (type %in% ARCHIVES) {
    return(read_compressed(path, col_select, labelled))
  }
  df <- switch(type,
    ".parquet" = as.data.frame(if (is.null(col_select)) arrow::read_parquet(path) else arrow::read_parquet(path, col_select = !!col_select)),
    ".csv" = utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    ".dta" = , ".sav" = as.data.frame(read_stat(path), check.names = FALSE, optional = TRUE),
    ".rds" = read_rds(path),
    ".sps" = read_ffs(path, dat_of(path))$df,
    cli::cli_abort(c("cannot read {.file {basename(path)}}", "i" = "choose from {.val {FORMATS}}"), class = "ggp_error")
  )
  if (labelled) df else as.data.frame(lapply(df, plain), check.names = FALSE, optional = TRUE)
}

read_rds <- function(path) {
  x <- readRDS(path)
  if (!is.data.frame(x)) {
    cli::cli_abort("{.file {basename(path)}} holds {.cls {class(x)[1]}}, not a data frame", class = "ggp_error")
  }
  as.data.frame(x, check.names = FALSE, optional = TRUE)
}

# a datafile in its own 7z or zip: unpacked next to nothing, read, removed
read_compressed <- function(path, col_select, labelled) {
  tmp <- tempfile("ggp-")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  unpack(path, tmp, DATA_EXT, FALSE)
  files <- list.files(tmp, recursive = TRUE, full.names = TRUE)
  if (length(files) != 1) {
    cli::cli_abort("{.file {basename(path)}} holds {length(files)} datafiles; unpack it with {.fn ggp_download} instead",
                   class = "ggp_error")
  }
  ggp_read(files, col_select, labelled)
}

# a value-label code as JSON text: 1 -> "1", never "1.0"
code <- function(k) vapply(k, \(v) format(v, scientific = FALSE, trim = TRUE, digits = 15), character(1), USE.NAMES = FALSE)

# the code of a value label as JSON text; Stata's extended missing values (.a, .b, ...) are "a", "b", ...
value_code <- function(k) ifelse(is.na(k), haven::na_tag(k), code(k))

#' The labels of a data file
#'
#' The variable labels, value labels and user-defined missing values of a Stata or SPSS file, as a list that
#' [jsonlite::toJSON()] turns into the `.meta.json` files this package writes. For a parquet file, its `.meta.json`.
#' [ggp_label()] puts them on the data.
#'
#' @param path A `.dta`, `.sav`, `.rds` or `.parquet` file.
#' @return `NULL` for other types (and a parquet file without `.meta.json`), else a list: `file_label`,
#'   `column_labels` (variable -> label), `value_labels` (variable -> code -> label; codes are text), `missing_ranges`
#'   (sav) and `original_types` (Stata or SPSS formats). A consolidated file's also has `original_labels`.
#' @examples
#' \dontrun{
#' m <- ggp_meta("ggsii_wave1_at_v_1_1.dta")
#' m$value_labels$dem01
#' m <- ggp_meta("data/download/consolidated/ggs-ii_w1.parquet")
#' }
#' @export
ggp_meta <- function(path) {
  if (file_type(path) == ".parquet") {
    json <- meta_of(path)
    return(if (file.exists(json)) jsonlite::fromJSON(json, simplifyVector = FALSE))
  }
  if (!file_type(path) %in% LABELLED) {
    return(NULL)
  }
  x <- if (file_type(path) == ".rds") read_rds(path) else read_stat(path, n_max = 0, user_na = TRUE)
  labels <- \(a) lapply(x, \(col) attr(col, a, exact = TRUE)) # exact: "label" must not match "labels"
  keep <- \(l) l[!vapply(l, is.null, logical(1))]
  vl <- keep(labels("labels"))
  missing <- Map(\(v, r) c(as.list(v), if (!is.null(r)) list(list(lo = r[1], hi = r[2]))),
                 labels("na_values"), labels("na_range"))
  missing <- missing[lengths(missing) > 0]
  list(
    file_label = "",
    column_labels = obj(lapply(labels("label"), \(l) l %||% NA_character_)),
    value_labels = obj(lapply(vl, \(l) as.list(stats::setNames(names(l), value_code(unname(l)))))),
    missing_ranges = obj(missing),
    original_types = obj(lapply(Map(\(a, b) a %||% b, labels("format.stata"), labels("format.spss")), \(l) l %||% NA_character_))
  )
}
