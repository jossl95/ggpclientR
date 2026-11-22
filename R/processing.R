# Processing downloaded data: FFS unpacking and conversion, and preprocessing to parquet + meta.json.
#
# Preprocessing saves downloaded datafiles as parquet under data_dir/download/processed, with columns that say where
# each row came from. One parquet per data file, at the same path as in download/raw, plus a .meta.json with its
# labels (dta and sav):
#   download/raw/ggs-ii/w1/at/ggs2_w1_at_v1.1.dta ->
#   download/processed/ggs-ii/w1/at/ggs2_w1_at_v1.1.parquet and ggs2_w1_at_v1.1.meta.json
# A file that comes in several types (dta, sav and csv of one dataset) is read once, in the order of FORMATS.
# FFS is read from its converted_data.dta/.sav, which the download puts in download/processed/ffs/<cc>/ already; a
# country without one is read from the raw .sps/.dat.
# Added columns: ggp_project, ggp_round, ggp_series, ggp_wave, ggp_country, ggp_version, ggp_file.
# An existing parquet (and meta.json) is kept unless force.

#' Unpack an FFS download into dest, the country folder (one dataset per country: no folder of its own)
#'
#' Raw files are named after the dataset: AUSTRIA.DAT becomes <stem>.dat, and .sps and .tbl likewise; when the archive
#' holds several of one type (Estonia) the original name is added: <stem>_est_f.dat. The .sps is rewritten: $ in
#' names becomes _, FILE= names the renamed .dat; in the .tbl a $ inside a name (IND01$01) becomes _ too.
#' Documents (metadata) keep their names. With convert_to the data is also saved as converted_data.<ext>, in
#' convert_dir (default: dest). Existing files are kept unless force. Returns the number of matching files.
#' @noRd
unpack_ffs <- function(archive, filename, dest, stem, ext, metadata, convert_to = character(), force = FALSE,
                       convert_dir = dest, log = NULL) {
  # shortcut: two versions of one country would share converted_data.*; the portal has one per country
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".unpack-", tmpdir = dirname(dest))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  n <- unpack_file(archive, filename, tmp, union(ext, RAW), metadata, force = TRUE)
  if (n && length(convert_to)) {
    dir.create(convert_dir, recursive = TRUE, showWarnings = FALSE)
    n <- n + convert_ffs(tmp, convert_to, CONVERTED, force, out_dir = convert_dir, log = log)
  }
  files <- sort(list.files(tmp, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE))
  suffix <- file_type(files)
  count <- table(suffix)
  as_latin1 <- \(path, f) writeBin(charToRaw(iconv(f(iconv(rawToChar(readBin(path, "raw", file.size(path))), "latin1", "UTF-8")),
                                                  "UTF-8", "latin1")), path)
  for (sps in files[suffix == ".sps"]) {
    dat <- if (count[".dat"] %in% 1L) paste0(stem, ".dat")
    as_latin1(sps, \(x) rewrite_sps(x, dat)) # keeps CRLF
  }
  for (tbl in files[suffix == ".tbl"]) { # report text: only a $ between name characters
    as_latin1(tbl, \(x) gsub("(?<=[A-Za-z0-9])\\$(?=[A-Za-z0-9])", "_", x, perl = TRUE))
  }
  place(tmp, dest, stem, dest, force)
  n
}

#' One file per dataset file in folder: the preferred type of each name
#'
#' FFS: the converted files (labelled) when there are any, else the raw ones; never both, as they hold the same data.
#' @noRd
datafiles <- function(folder, types = FORMATS) {
  files <- sort(list.files(folder, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE), method = "radix")
  files <- files[file_type(files) %in% types & !dir.exists(files) & !grepl(".part", basename(files), fixed = TRUE)]
  converted <- files[startsWith(tolower(basename(files)), CONVERTED)]
  pool <- if (length(converted)) converted else files
  key <- tolower(tools::file_path_sans_ext(pool))
  best <- vapply(split(seq_along(pool), factor(key, unique(key))), \(i) pool[i[which.min(match(file_type(pool[i]), FORMATS))]], "")
  sort(unname(best), method = "radix")
}

#' The datafiles of a download: lp is <country folder>/<file stem>, its files <stem>.dta, <stem>_x.dta, ...
#'
#' A folder of its own (downloads of older versions) holds all its files.
#' @noRd
own_files <- function(root, lp) {
  p <- file.path(root, lp)
  if (dir.exists(p)) {
    return(datafiles(p))
  }
  # shortcut: a stem without version (ggs2_w1_at) also takes ggs2_w1_at_v1.0.*; the portal gives every dataset one
  files <- datafiles(dirname(p))
  s <- tolower(tools::file_path_sans_ext(basename(files)))
  files[s == basename(p) | startsWith(s, paste0(basename(p), "_"))]
}

#' The data files of row r, as data.frame(path, rel): none if it is not downloaded
#'
#' rel is the path under root (or under out, for FFS: its converted dta/sav in download/processed when there are
#' any, else the raw files).
#' @noRd
sources <- function(r, root, out) {
  empty <- data.frame(path = character(), rel = character())
  lp <- r$local_path
  if (is.na(lp) || !nzchar(lp)) {
    return(empty)
  }
  folder <- if (dir.exists(file.path(root, lp))) file.path(out, lp) else dirname(file.path(out, lp))
  conv <- datafiles(folder, LABELLED)
  conv <- conv[startsWith(tolower(basename(conv)), CONVERTED)]
  paths <- if (length(conv)) conv else own_files(root, lp)
  base <- if (length(conv)) out else root
  data.frame(path = paths, rel = substring(paths, nchar(base) + 2L))
}

parquet_of <- function(out, rel) file.path(out, sub("\\.[^./]*$", ".parquet", rel))
meta_of <- function(parquet) sub("\\.parquet$", ".meta.json", parquet)

rows_of <- function(rows) lapply(seq_len(nrow(rows)), \(i) as.list(rows[i, , drop = FALSE]))

#' The files of rows whose parquet is missing (or all, if force), and how many were skipped
#' @noRd
jobs <- function(rows, root, out, force = FALSE) {
  todo <- list()
  skipped <- 0L
  for (r in rows_of(rows)) {
    s <- sources(r, root, out)
    for (i in seq_len(nrow(s))) {
      target <- parquet_of(out, s$rel[i])
      if (!force && file.exists(target) && has_lower_names(target) &&
          (!file_type(s$path[i]) %in% LABELLED || file.exists(meta_of(target)))) {
        skipped <- skipped + 1L
      } else {
        if (force) { # the parquet and meta of an older, differently cased name would stay behind
          drop_case_variants(target)
          drop_case_variants(meta_of(target))
        }
        todo[[length(todo) + 1]] <- list(src = s$path[i], rel = s$rel[i], target = target, r = r)
      }
    }
  }
  list(todo = todo, skipped = skipped)
}

# TRUE if every column name of the parquet is lower case; FALSE if it is unreadable (made by an older version: redo)
has_lower_names <- function(parquet) {
  tryCatch({
    n <- parquet_columns(parquet)
    all(n == tolower(n))
  }, error = \(e) FALSE)
}

#' The data frame of one file with the provenance columns; ggp_file is rel
#' @noRd
with_provenance <- function(df, r, rel) {
  df$ggp_project <- r$project
  df$ggp_round <- as.integer(r$round)
  df$ggp_series <- r$series
  df$ggp_wave <- if (is.na(r$wave) || !nzchar(r$wave)) NA_character_ else r$wave
  df$ggp_country <- if (is.na(r$country) || !nzchar(r$country)) NA_character_ else r$country
  df$ggp_version <- as.character(r$version)
  df$ggp_file <- rel
  df
}

write_json <- function(x, path) {
  atomic(path, \(part) writeLines(jsonlite::prettify(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null"), 1),
                                  part, useBytes = TRUE))
}

#' One file: parquet and meta.json, written whole or not at all
#' @noRd
to_parquet <- function(src, rel, target, r) {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  df <- ggp_read(src, labelled = FALSE)
  names(df) <- lower_names(names(df), rel)
  df <- with_provenance(df, r, rel)
  atomic(target, \(part) {
    arrow::write_parquet(df, part)
    m <- ggp_meta(src)
    if (!is.null(m)) {
      m <- lower_keys(m, c("column_labels", "value_labels", "missing_ranges", "original_types"))
      write_json(c(list(file = rel), m), meta_of(target))
    }
  })
}

# the job of one worker: one file to parquet
preprocess_one <- function(x) to_parquet(x$src, x$rel, x$target, x$r)

#' Save the downloaded data files of `rows` as parquet, `concurrency` at a time
#' @noRd
preprocess_rows <- function(rows, root, out, force = FALSE, concurrency = 4, log = NULL) {
  j <- jobs(rows, root, out, force)
  res <- parallel_map(j$todo, preprocess_one, concurrency)
  failed <- failures(character(), character())
  for (i in seq_along(res)) {
    if (is.null(res[[i]]$error)) {
      ggp_log(log, "info", "preprocessed %s", substring(j$todo[[i]]$target, nchar(out) + 2L))
    } else {
      failed <- rbind(failed, failures(j$todo[[i]]$rel, res[[i]]$error))
      ggp_log(log, "error", "preprocess failed %s: %s", j$todo[[i]]$rel, res[[i]]$error)
    }
  }
  new_result("preprocess", length(j$todo) - nrow(failed), j$skipped, failed)
}
