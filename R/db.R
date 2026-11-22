# SQLite catalogue: one row per dataset page (version), one row per download file.
# The same schema as the Python package: a catalogue.sqlite made by either is read by both.

SCHEMA <- c(
  "CREATE TABLE IF NOT EXISTS dataset (
     id INTEGER PRIMARY KEY AUTOINCREMENT,
     slug TEXT NOT NULL UNIQUE,
     project TEXT NOT NULL, round INTEGER, series TEXT NOT NULL, wave TEXT NOT NULL,
     country TEXT NOT NULL, country_name TEXT, version TEXT,
     is_latest INTEGER NOT NULL, upcoming INTEGER NOT NULL,
     title TEXT, collected_from TEXT, collected_to TEXT,
     colectica_url TEXT, page_url TEXT NOT NULL,
     rel_dir TEXT NOT NULL, stem TEXT NOT NULL, synced_at TEXT NOT NULL)",
  "CREATE TABLE IF NOT EXISTS file (
     download_id INTEGER PRIMARY KEY,
     dataset_id INTEGER NOT NULL REFERENCES dataset(id),
     label TEXT, size_mb REAL, url TEXT NOT NULL,
     local_path TEXT,
     fetched TEXT,
     downloaded_at TEXT)"
)

db_connect <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 10000") # background steps share the file: wait, do not fail
  for (s in SCHEMA) DBI::dbExecute(con, s)
  if (!"fetched" %in% DBI::dbGetQuery(con, "PRAGMA table_info(file)")$name) { # made by an older version
    DBI::dbExecute(con, "ALTER TABLE file ADD COLUMN fetched TEXT")
  }
  con
}

now_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S+00:00", tz = "UTC")

#' Insert or refresh a dataset (keeping its id) and its files (keeping local_path)
#' @noRd
db_upsert <- function(con, slug, d) {
  p <- parse_slug(slug)
  version <- if (is.na(d$version)) p$version else d$version
  nm <- naming(p, slug, version)
  row <- list(
    slug = slug, project = p$project, round = p$round, series = p$series, wave = p$wave,
    country = p$country, country_name = d$country_name, version = version,
    is_latest = as.integer(is.na(p$version)), upcoming = as.integer(p$upcoming), title = d$title,
    collected_from = d$collected_from, collected_to = d$collected_to,
    colectica_url = d$colectica_url, page_url = d$page_url %||% page_url(slug),
    rel_dir = nm$rel_dir, stem = nm$stem, synced_at = now_utc()
  )
  sets <- paste(sprintf("%s=excluded.%s", setdiff(names(row), "slug"), setdiff(names(row), "slug")), collapse = ", ")
  DBI::dbExecute(con, sprintf("INSERT INTO dataset(%s) VALUES(%s) ON CONFLICT(slug) DO UPDATE SET %s",
                              paste(names(row), collapse = ", "), paste0(":", names(row), collapse = ", "), sets),
                 params = row)
  id <- DBI::dbGetQuery(con, "SELECT id FROM dataset WHERE slug = ?", params = list(slug))$id
  for (f in d$files) {
    DBI::dbExecute(con, "INSERT INTO file(download_id, dataset_id, label, size_mb, url) VALUES(?,?,?,?,?)
                         ON CONFLICT(download_id) DO UPDATE SET dataset_id=excluded.dataset_id,
                         label=excluded.label, size_mb=excluded.size_mb, url=excluded.url",
                   params = list(f$download_id, id, f$label, f$size_mb, f$url))
  }
  if (length(d$files)) { # a link replaced on the page; a page with no link at all keeps its old rows
    keep <- vapply(d$files, \(f) f$download_id, integer(1))
    DBI::dbExecute(con, sprintf("DELETE FROM file WHERE dataset_id = ? AND download_id NOT IN (%s)",
                                paste(rep("?", length(keep)), collapse = ",")), params = c(list(id), as.list(keep)))
  }
  invisible()
}

#' Record the folder and what is in it: `kept` (datafile types, "raw", "meta")
#' @noRd
db_mark_downloaded <- function(con, download_id, rel_path, kept) {
  DBI::dbExecute(con, "UPDATE file SET local_path=?, fetched=?, downloaded_at=? WHERE download_id=?",
                 params = list(rel_path, paste(sort(unique(kept)), collapse = " "), now_utc(), download_id))
  invisible()
}

#' '3-7,10' as list(c(3, 7), c(10, 10)); a reversed range '7-3' is read as 3-7
#' @noRd
parse_ids <- function(spec) {
  lapply(strsplit(spec, ",")[[1]], \(part) {
    bounds <- as.integer(strsplit(trimws(part), "-")[[1]])
    sort(c(bounds[1], bounds[length(bounds)]))
  })
}

norm_round <- function(v) {
  v <- tolower(as.character(v))
  r <- c(i = 1L, ii = 2L)[v]
  as.integer(ifelse(is.na(r), suppressWarnings(as.integer(v)), r))
}
norm_wave <- function(v) {
  v <- tolower(as.character(v))
  ifelse(grepl("^[0-9]+$", v), paste0("w", v), v)
}

#' Rows (one per file; datasets without a file appear once) matching all given filters
#'
#' project, round, wave and country each take one value, several values, or NULL for all. Values are
#' case-insensitive and are the names used in the download folders: project "ggs", round 2 (or "ii"),
#' wave "w1" (or 1), country "nl". A value that matches nothing is an error.
#' @noRd
db_select <- function(con, project = NULL, round = NULL, wave = NULL, country = NULL, ids = NULL,
                      all_versions = FALSE, downloadable = FALSE) {
  where <- character()
  args <- list()
  isin <- function(name, norm, vals) {
    vals <- unique(norm(unlist(vals)))
    if (!length(vals)) {
      return()
    }
    known <- DBI::dbGetQuery(con, sprintf("SELECT DISTINCT %s AS v FROM dataset WHERE %s IS NOT NULL AND %s != ''",
                                          name, name, name))$v
    bad <- setdiff(as.character(vals), as.character(known))
    if (length(known) && length(bad)) { # a typo would otherwise silently select nothing
      cli::cli_abort(c("unknown {name} {.val {bad}}", "i" = "choose from {.val {sort(as.character(known))}}"),
                     class = "ggp_error")
    }
    where <<- c(where, sprintf("d.%s IN (%s)", name, paste(rep("?", length(vals)), collapse = ",")))
    args <<- c(args, as.list(vals))
  }
  isin("project", toupper, project)
  isin("round", norm_round, round)
  isin("wave", norm_wave, wave)
  isin("country", toupper, country)
  if (length(ids)) {
    ranges <- if (is.character(ids)) parse_ids(ids) else lapply(ids, \(i) c(i, i))
    where <- c(where, paste0("(", paste(rep("d.id BETWEEN ? AND ?", length(ranges)), collapse = " OR "), ")"))
    args <- c(args, as.list(unlist(ranges)))
  }
  if (!all_versions) where <- c(where, "d.is_latest = 1")
  if (downloadable) where <- c(where, "f.url IS NOT NULL")
  sql <- paste("SELECT d.*, f.download_id, f.label, f.size_mb, f.url, f.local_path, f.fetched,",
               "(SELECT COUNT(*) FROM file WHERE dataset_id = d.id) AS n_files",
               "FROM dataset d LEFT JOIN file f ON f.dataset_id = d.id")
  if (length(where)) sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  DBI::dbGetQuery(con, paste(sql, "ORDER BY d.id, f.download_id"), params = if (length(args)) args)
}
