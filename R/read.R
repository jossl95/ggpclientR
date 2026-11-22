# Read a downloaded datafile: parquet, dta, sav, csv, or an FFS .sps (with its .dat).
# Values are the codes; value labels are not applied. In sav, values SPSS marks as missing are NA.

FORMATS <- c(".parquet", ".dta", ".sav", ".csv", ".sps") # also the preference when one file comes in several
LABELLED <- c(".dta", ".sav") # the types that carry variable and value labels

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

read_stat <- function(path, n_max = Inf, user_na = FALSE) {
  args <- list(path, n_max = n_max)
  f <- if (file_type(path) == ".dta") haven::read_dta else haven::read_sav
  if (file_type(path) == ".sav") args$user_na <- user_na
  # some GGS files mix encodings: read as their .sav twins are
  tryCatch(do.call(f, args), error = \(e) do.call(f, c(args, encoding = "latin1")))
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
#' Reads one datafile into a data frame: `.parquet`, `.dta`, `.sav`, `.csv`, or an FFS `.sps` (with its `.dat`).
#' The values are the codes; value labels are not applied (see [ggp_meta()]). In `.sav` files, values that SPSS
#' marks as missing are `NA`.
#'
#' @param path The file.
#' @param col_select For parquet files: the columns to read (default all). Wide files are large.
#' @return A data frame.
#' @examples
#' \dontrun{
#' df <- ggp_read("data/download/consolidated/ggs-ii_w1.parquet", col_select = c("ggp_country", "dem01"))
#' }
#' @export
ggp_read <- function(path, col_select = NULL) {
  type <- file_type(path)
  switch(type,
    ".parquet" = as.data.frame(nanoparquet::read_parquet(path, col_select = col_select)),
    ".csv" = utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    ".dta" = , ".sav" = as.data.frame(lapply(read_stat(path), plain), check.names = FALSE, optional = TRUE),
    ".sps" = read_ffs(path, dat_of(path))$df,
    cli::cli_abort(c("cannot read {.file {basename(path)}}", "i" = "choose from {.val {FORMATS}}"), class = "ggp_error")
  )
}

# a value-label code as JSON text: 1 -> "1", never "1.0"
code <- function(k) vapply(k, \(v) format(v, scientific = FALSE, trim = TRUE, digits = 15), character(1), USE.NAMES = FALSE)

#' The labels of a dta or sav file
#'
#' The variable labels, value labels and user-defined missing values of a Stata or SPSS file, as a list that
#' [jsonlite::toJSON()] turns into the `.meta.json` files this package writes.
#'
#' @param path A `.dta` or `.sav` file.
#' @return `NULL` for other types, else a list: `file_label`, `column_labels` (variable -> label), `value_labels`
#'   (variable -> code -> label; codes are text), `missing_ranges` (sav) and `original_types` (Stata or SPSS formats).
#' @examples
#' \dontrun{
#' m <- ggp_meta("ggsii_wave1_at_v_1_1.dta")
#' m$value_labels$dem01
#' }
#' @export
ggp_meta <- function(path) {
  if (!file_type(path) %in% LABELLED) {
    return(NULL)
  }
  x <- read_stat(path, n_max = 0, user_na = TRUE)
  labels <- \(a) lapply(x, \(col) attr(col, a))
  keep <- \(l) l[!vapply(l, is.null, logical(1))]
  vl <- keep(labels("labels"))
  missing <- Map(\(v, r) c(as.list(v), if (!is.null(r)) list(list(lo = r[1], hi = r[2]))),
                 labels("na_values"), labels("na_range"))
  missing <- missing[lengths(missing) > 0]
  list(
    file_label = "",
    column_labels = obj(lapply(labels("label"), \(l) l %||% NA_character_)),
    value_labels = obj(lapply(vl, \(l) as.list(stats::setNames(names(l), code(unname(l)))))),
    missing_ranges = obj(missing),
    original_types = obj(lapply(Map(\(a, b) a %||% b, labels("format.stata"), labels("format.spss")), \(l) l %||% NA_character_))
  )
}
