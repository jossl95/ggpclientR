# Access to the GGP portal: sync the catalogue, log in, download and unpack files.

#' A client for the GGP data portal
#'
#' Holds the catalogue (`data_dir/catalogue.sqlite`), the rows your filters select, and the portal session. Pass it
#' to the steps with the pipe:
#'
#' ```r
#' p <- ggp_client(project = "ggs", round = 2)
#' p |> ggp_sync()
#' p |> ggp_download(ext = "dta")
#' p |> ggp_preprocess()
#' p |> ggp_consolidate()
#' ```
#'
#' The client changes in place: `ggp_rows(p)` shows the rows as they are after the last step. Steps are logged to
#' `data_dir/ggpclient.log`; set `options(ggpclient.verbose = TRUE)` to see every line on screen too. Downloads go to
#' `data_dir/download/raw`, FFS conversions and parquet files to `data_dir/download/processed`, stacked files to
#' `data_dir/download/consolidated`; the catalogue, session and log stay in `data_dir`.
#'
#' @param data_dir Where everything is kept.
#' @param access_url The access link the portal e-mails you. It works like a password: it is never logged, stored or
#'   shown. Better not to pass it here; see [ggp_login()].
#' @param concurrency How many downloads, and how many files processed, at a time.
#' @param project,round,wave,country Filters: one value, several, or `NULL` for all. Case does not matter:
#'   `project = "ggs"`, `round = 2` (or `"ii"`), `wave = "w1"` (or `1`), `country = c("nl", "at")`. A value that is
#'   not in the catalogue is an error.
#' @param ids Catalogue numbers, as `"3-12,20"` or a vector.
#' @param all_versions Also superseded versions (default: the latest only).
#' @param force Download and process again, overwriting files that already exist. By default existing files are never
#'   touched.
#' @return A `ggp_client`.
#' @examples
#' \dontrun{
#' p <- ggp_client(project = "ggs", round = 2)
#' p |> ggp_sync()
#' nrow(ggp_rows(p))
#' }
#' @export
ggp_client <- function(data_dir = "data", access_url = NULL, concurrency = 8, project = NULL, round = NULL,
                       wave = NULL, country = NULL, ids = NULL, all_versions = FALSE, force = FALSE) {
  if (concurrency < 1) {
    cli::cli_abort("{.arg concurrency} must be 1 or more", class = "ggp_error") # 0 would wait forever
  }
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  p <- new.env(parent = emptyenv())
  p$dir <- data_dir
  p$downloads <- file.path(data_dir, "download", "raw")
  p$processed <- file.path(data_dir, "download", "processed")
  p$consolidated <- file.path(data_dir, "download", "consolidated")
  url <- access_url %||% Sys.getenv("GGP_ACCESS_URL", NA_character_)
  p$access_url <- local({ # a function, so that printing or str() of the client never shows the link
    u <- url
    function() u
  })
  p$concurrency <- concurrency
  p$force <- force
  p$filters <- list(project = project, round = round, wave = wave, country = country, ids = ids,
                    all_versions = all_versions)
  p$con <- db_connect(file.path(data_dir, "catalogue.sqlite"))
  p$jar <- file.path(data_dir, ".session") # curl cookie jar: only you may read it
  if (!file.exists(p$jar)) file.create(p$jar)
  Sys.chmod(p$jar, "600")
  p$log <- file.path(data_dir, "ggpclient.log")
  .state$log <- p$log
  reg.finalizer(p, \(e) try(DBI::dbDisconnect(e$con), silent = TRUE), onexit = TRUE)
  class(p) <- "ggp_client"
  refresh(p)
  p
}

#' @export
print.ggp_client <- function(x, ...) {
  f <- Filter(Negate(is.null), x$filters[c("project", "round", "wave", "country", "ids")])
  cat(sprintf("<ggp_client> %s: %d files%s\n", x$dir, nrow(x$rows),
              if (length(f)) paste0(" (", paste(names(f), vapply(f, paste, "", collapse = ","), sep = " ", collapse = ", "), ")") else ""))
  invisible(x)
}

# the downloadable rows that match the client's filters
refresh <- function(p) {
  p$rows <- do.call(db_select, c(list(p$con), p$filters, list(downloadable = TRUE)))
  invisible(p)
}

#' The files a client works on
#'
#' @param p A [ggp_client()].
#' @return A data frame with one row per file: its dataset (`slug`, `project`, `round`, `series`, `wave`, `country`,
#'   `version`, ...), the file (`download_id`, `url`, `size_mb`) and where it is on disk (`local_path`, `fetched`).
#'   Refreshed after [ggp_sync()] and [ggp_download()].
#' @export
ggp_rows <- function(p) p$rows

portal_request <- function(url) {
  httr2::request(abs_url(url)) |>
    httr2::req_user_agent("ggpclient/0.1") |>
    httr2::req_timeout(60) |>
    httr2::req_error(is_error = \(resp) FALSE) # statuses are handled here
}

with_session <- function(req, p) httr2::req_cookie_preserve(req, p$jar)

#' The text of one page; transport errors and server errors are tried three times
#' @noRd
fetch_text <- function(p, url) {
  for (attempt in 1:3) {
    resp <- tryCatch(httr2::req_perform(with_session(portal_request(url), p)), error = \(e) e)
    if (inherits(resp, "error")) {
      ggp_log("warning", "%s: %s, attempt %d of 3", url, class(resp)[1], attempt)
    } else if (httr2::resp_status(resp) < 500) {
      if (httr2::resp_status(resp) >= 400) {
        cli::cli_abort("HTTP {httr2::resp_status(resp)} for {url}", class = "ggp_error")
      }
      return(httr2::resp_body_string(resp))
    } else {
      ggp_log("warning", "%s: HTTP %d, attempt %d of 3", url, httr2::resp_status(resp), attempt)
    }
    if (attempt < 3) Sys.sleep(2^(attempt - 1))
  }
  cli::cli_abort("giving up on {url}", class = "ggp_error")
}

# many pages at once (one curl session, `concurrency` connections); a page that fails is tried again by fetch_text
fetch_texts <- function(p, urls) {
  pool <- curl::new_pool(total_con = p$concurrency, host_con = p$concurrency)
  resps <- httr2::req_perform_parallel(lapply(urls, \(u) with_session(portal_request(u), p)), pool = pool,
                                       on_error = "continue", progress = FALSE)
  Map(\(url, resp) {
    ok <- inherits(resp, "httr2_response") && httr2::resp_status(resp) < 400
    if (ok) {
      return(list(value = httr2::resp_body_string(resp)))
    }
    tryCatch(list(value = fetch_text(p, url)), error = \(e) list(error = conditionMessage(e)))
  }, urls, resps)
}

#' Index the catalogue
#'
#' Reads the portal's public dataset pages and stores them in `data_dir/catalogue.sqlite`. Harmonized Histories and
#' Additional Datasets are listed only on the data access page, so this logs in (see [ggp_login()]) to add them;
#' without a login they are skipped and reported in `failed`, and the public part is still indexed.
#'
#' @param p A [ggp_client()].
#' @param dump A folder to save the pages in (for tests and bug reports).
#' @return A `ggp_result`: `done` is the number of datasets indexed, `failed` says which pages failed and why,
#'   `found` holds what was parsed.
#' @export
ggp_sync <- function(p, dump = NULL) {
  slugs <- parse_catalog(fetch_text(p, catalog_url()))
  seen <- slugs
  found <- list()
  failed <- failures(character(), character())
  todo <- slugs
  if (!is.null(dump)) dir.create(dump, recursive = TRUE, showWarnings = FALSE)
  while (length(todo)) { # each round also visits "other versions" discovered on the previous one
    pages <- fetch_texts(p, vapply(todo, page_url, ""))
    nxt <- character()
    for (i in seq_along(todo)) {
      slug <- todo[i]
      if (!is.null(pages[[i]]$error)) {
        failed <- rbind(failed, failures(slug, pages[[i]]$error))
        ggp_log("error", "sync: %s failed: %s", slug, pages[[i]]$error)
        next
      }
      if (!is.null(dump)) writeLines(pages[[i]]$value, file.path(dump, paste0(slug, ".html")), useBytes = TRUE)
      found[[slug]] <- parse_dataset(pages[[i]]$value, slug)
      new <- setdiff(found[[slug]]$other_versions, seen)
      nxt <- c(nxt, new)
      seen <- c(seen, new)
    }
    todo <- nxt
  }
  tryCatch({
    ggp_login(p)
    html <- fetch_text(p, "/form/")
    if (!is.null(dump)) writeLines(html, file.path(dump, "form.html"), useBytes = TRUE)
    access <- parse_access(html)
    found[names(access)] <- access
  }, error = \(e) {
    msg <- paste0("Harmonized Histories and Additional Datasets skipped: ", conditionMessage(e))
    failed <<- rbind(failed, failures("data access page", msg))
    ggp_log("warning", "sync: %s", msg)
  })
  for (slug in names(found)) { # the latest version often has its own -vX-Y page too: keep only the latest row
    base <- sub("-v\\d+-\\d+$", "", slug)
    if (base != slug && base %in% names(found) && identical(found[[base]]$version, parse_slug(slug)$version)) {
      found[[slug]] <- NULL
    }
  }
  DBI::dbWithTransaction(p$con, for (slug in sort_slugs(names(found))) db_upsert(p$con, slug, found[[slug]]))
  refresh(p)
  ggp_log("info", "sync: indexed %d datasets, %d failed", length(found), nrow(failed))
  new_result("sync", length(found), 0L, failed, found = found)
}

logged_in <- function(p) grepl("/logout/", fetch_text(p, "/form/"), fixed = TRUE) # only a logged-in page has the logout form

#' Log in to the portal
#'
#' Reuses the saved session (`data_dir/.session`) if it still works, else logs in with the access link. The link
#' comes from the `access_url` argument of [ggp_client()], then the `GGP_ACCESS_URL` environment variable, and if
#' neither is set you are asked for it (input hidden; needs the askpass package). The link works like a password: do
#' not put it in a script. A good place is your user-level `~/.Renviron` (`usethis::edit_r_environ()`):
#' `GGP_ACCESS_URL=<link>`. Syncing and downloading log in by themselves when needed.
#'
#' @param p A [ggp_client()].
#' @return `p`, invisibly. An error of class `ggp_login_error` when there is no valid session and no usable link.
#' @export
ggp_login <- function(p) {
  if (file.size(p$jar) > 0 && logged_in(p)) {
    ggp_log("info", "login: saved session reused")
    return(invisible(p))
  }
  url <- trimws(p$access_url())
  if (is.na(url) || !nzchar(url)) {
    url <- tryCatch(trimws(askpass::askpass("No valid session. Paste the emailed access link (input is hidden): ")),
                    error = \(e) "")
    if (!length(url) || is.na(url)) url <- ""
  }
  if (!nzchar(url)) {
    cli::cli_abort(c("no valid session",
                     "i" = "pass {.arg access_url}, set {.envvar GGP_ACCESS_URL}, or run in a terminal to be asked",
                     "i" = "request a fresh link by e-mail on the portal login page"), class = "ggp_login_error", call = NULL)
  }
  if (!startsWith(url, paste0(ggp_base(), "/"))) {
    cli::cli_abort("the access link must start with {.val {ggp_base()}/}", class = "ggp_login_error", call = NULL)
  }
  tryCatch(httr2::req_perform(with_session(portal_request(url), p)),
           error = \(e) cli::cli_abort("login request failed ({class(e)[1]})", class = "ggp_login_error", call = NULL))
  Sys.chmod(p$jar, "600")
  if (!logged_in(p)) {
    cli::cli_abort("the access link was not accepted (expired or already used?)", class = "ggp_login_error", call = NULL)
  }
  ggp_log("info", "login: logged in with the access link, session saved")
  invisible(p)
}

# the file name of a Content-Disposition header (filename*=UTF-8''... and quoting as well), or NULL
disposition_filename <- function(header) {
  if (is.null(header) || is.na(header)) {
    return(NULL)
  }
  m <- re_match("filename\\*\\s*=\\s*[^']*'[^']*'(?<n>[^;]+)", header, ignore.case = TRUE)
  if (!is.null(m)) {
    return(utils::URLdecode(trimws(m[["n"]])))
  }
  m <- re_match("filename\\s*=\\s*(?:\"(?<q>[^\"]*)\"|(?<t>[^;]+))", header, ignore.case = TRUE)
  if (is.null(m)) NULL else trimws(paste0(m[["q"]], m[["t"]]))
}

#' Download the files
#'
#' Downloads the files of the client's rows (`rows`, default `ggp_rows(p)`) and unpacks them under `root` (default
#' `data_dir/download/raw`). A file that was already unpacked for these options is skipped; asking for more later
#' (another `ext`, or `metadata`) downloads it again and adds the missing files to the folder. Files that already
#' exist are never overwritten, unless the client was made with `force = TRUE`.
#'
#' FFS: the `.dat`, `.sps` and `.tbl` files are kept under `root` as `ffs/<cc>/<stem>.dat` etc., and the data is
#' converted, with variable and value labels, to `data_dir/download/processed/ffs/<cc>/converted_data.<ext>`: dta
#' and/or sav as `ext` names them, else dta.
#'
#' @param p A [ggp_client()].
#' @param ext The datafile type(s) to keep: `"dta"`, `"sav"`, `"csv"`, several, or `NULL` for all three.
#' @param metadata Also keep the non-data files (documentation, codebooks) from the archives.
#' @param rows,root Other rows than the client's, another folder.
#' @return A `ggp_result` (done, skipped, failed).
#' @examples
#' \dontrun{
#' p |> ggp_download(ext = "dta")
#' }
#' @export
ggp_download <- function(p, ext = NULL, metadata = FALSE, rows = NULL, root = NULL) {
  rows <- rows %||% p$rows
  root <- root %||% p$downloads
  want <- norm_ext(ext)
  convert <- sort(intersect(want, c("dta", "sav")))  # FFS: labelled types only
  if (!length(unlist(ext)) || !length(convert)) convert <- "dta"
  meta <- if (metadata) "meta" else character()
  kept <- union(want, meta)
  ggp_log("info", "download: %d files, keeping %s", nrow(rows), paste(sort(kept), collapse = " "))

  todo <- list()
  skipped <- 0L
  for (r in rows_of(rows)) {
    k <- if (r$project == "FFS") union(c("raw", convert), meta) else kept
    there <- !is.na(r$local_path) && dir.exists(file.path(root, r$local_path))
    prev <- if (there) strsplit(r$fetched %|NA|% "", " ")[[1]] else character() # a deleted folder holds nothing
    if (!p$force && there && all(k %in% prev)) {
      skipped <- skipped + 1L
      ggp_log("info", "skipped %s: already in %s", r$slug, r$local_path)
    } else {
      todo[[length(todo) + 1]] <- list(r = r, k = k, prev = prev)
    }
  }

  failed <- failures(character(), character())
  fail <- \(slug, msg) {
    failed <<- rbind(failed, failures(slug, msg))
    ggp_log("error", "failed %s: %s", slug, msg)
  }
  jobs <- lapply(todo, \(t) {
    r <- t$r
    ffs <- r$project == "FFS"
    stem <- paste0(r$stem, if (r$n_files > 1) paste0("_", r$download_id) else "")
    rel <- if (ffs) r$rel_dir else paste0(r$rel_dir, "/", stem) # FFS: one dataset per country, no folder of its own
    dest <- file.path(root, rel)
    list(t = t, stem = stem, ffs = ffs, rel = rel, dest = dest, existed = dir.exists(dest),
         part = file.path(dirname(dest), paste0(stem, ".part")))
  })
  for (j in jobs) dir.create(dirname(j$dest), recursive = TRUE, showWarnings = FALSE)
  for (j in jobs) ggp_log("info", "downloading %s (file %d)", j$t$r$slug, j$t$r$download_id)

  # the downloads, `concurrency` at a time, straight to disk
  pool <- curl::new_pool(total_con = p$concurrency, host_con = p$concurrency)
  resps <- httr2::req_perform_parallel(lapply(jobs, \(j) with_session(portal_request(j$t$r$url), p)),
                                       paths = vapply(jobs, `[[`, "", "part"), pool = pool, on_error = "continue",
                                       progress = FALSE)
  cleanup <- \(j, ok) {
    unlink(j$part)
    if (!(ok || j$existed)) unlink(j$dest, recursive = TRUE) # a failed fetch must not delete what an earlier one unpacked
  }
  ready <- list()
  for (i in seq_along(jobs)) {
    j <- jobs[[i]]
    resp <- resps[[i]]
    msg <- if (!inherits(resp, "httr2_response")) {
      "download failed (no response)"
    } else if (httr2::resp_status(resp) >= 400) {
      sprintf("HTTP %d", httr2::resp_status(resp))
    } else if (grepl("text/html", httr2::resp_header(resp, "content-type") %||% "", fixed = TRUE)) {
      "got a web page instead of a file (no access to this dataset yet?)"
    }
    if (!is.null(msg)) {
      cleanup(j, FALSE)
      fail(j$t$r$slug, msg)
    } else {
      j$name <- disposition_filename(httr2::resp_header(resp, "content-disposition")) %||% j$stem
      j$size <- file.size(j$part)
      ready[[length(ready) + 1]] <- j
    }
  }

  # unpacking (and for FFS the conversion), in worker processes
  res <- parallel_map(ready, \(j) {
    if (j$ffs) {
      unpack_ffs(j$part, j$name, j$dest, j$stem, want, metadata, convert, p$force, file.path(p$processed, j$rel))
    } else {
      unpack_file(j$part, j$name, j$dest, want, metadata, p$force)
    }
  }, p$concurrency)
  done <- 0L
  for (i in seq_along(ready)) {
    j <- ready[[i]]
    n <- res[[i]]$value %||% 0L
    cleanup(j, n > 0)
    if (!is.null(res[[i]]$error)) {
      fail(j$t$r$slug, res[[i]]$error)
    } else if (n == 0) {
      fail(j$t$r$slug, sprintf("no matching files in the archive (looked for %s)", paste(sort(want), collapse = ", ")))
    } else {
      keep <- c(j$t$k, if (identical(j$rel, j$t$r$local_path)) j$t$prev)
      db_mark_downloaded(p$con, j$t$r$download_id, j$rel, keep)
      done <- done + 1L
      ggp_log("info", "downloaded %s: %.1f MB, %d files kept in %s", j$t$r$slug, j$size / 1e6, n, j$rel)
    }
  }
  ggp_log("info", "download: %d done, %d skipped, %d failed", done, skipped, nrow(failed))
  refresh(p) # local_path and fetched changed
  new_result("download", done, skipped, failed)
}

#' Save the downloaded data as parquet
#'
#' Saves the downloaded datafiles of the client's rows (`rows`, default `ggp_rows(p)`) as parquet in
#' `data_dir/download/processed`, one file per datafile at the same path as in `download/raw`, `concurrency` at a
#' time (forked workers; one worker on Windows). Next to each parquet of a Stata or SPSS file goes a `.meta.json` with
#' its variable and value labels. The columns `ggp_project`, `ggp_round`, `ggp_series`, `ggp_wave`, `ggp_country`,
#' `ggp_version` and `ggp_file` say where each row came from. Existing files are kept unless the client has
#' `force = TRUE`.
#'
#' @param p A [ggp_client()].
#' @param rows Other rows than the client's.
#' @return A `ggp_result`.
#' @export
ggp_preprocess <- function(p, rows = NULL) {
  preprocess_rows(rows %||% p$rows, p$downloads, p$processed, p$force, p$concurrency)
}

#' Stack the countries of each round and wave
#'
#' Stacks the preprocessed files of the client's rows per series and wave into
#' `data_dir/download/consolidated/<series>_<wave>.parquet` (for example `ggs-ii_w1.parquet`), with the harmonized
#' labels in `<series>_<wave>.meta.json`. Columns are matched by name; a column a country lacks is `NA` there, and
#' a column with different types gets the type that holds both. Variables named after one country (such as
#' `aut01a_2101` for Austria) are left out. An existing result is kept while it holds exactly these files and is
#' newer than all of them.
#'
#' The labels are harmonized over the countries, each country voting once: a variable gets the label most countries
#' use; a value gets the label more than half of the countries use, else all labels joined with `" / "`. Codes never
#' change. `original_labels` in the meta file lists every wording and the files that use it.
#'
#' One group is held in memory at a time: GGS-II wave 1 (185,660 rows, 4,001 columns) needs about 6 GB.
#'
#' @param p A [ggp_client()].
#' @param rows Other rows than the client's.
#' @return A `ggp_result`.
#' @export
ggp_consolidate <- function(p, rows = NULL) {
  consolidate_rows(rows %||% p$rows, p$downloads, p$processed, p$consolidated, p$force)
}

#' Write the consolidated data as Stata files
#'
#' Writes `<series>_<wave>.dta` next to each consolidated parquet, with the variable and value labels of its
#' `.meta.json` applied. Stata labels whole numbers only, so value labels go on whole-number columns. Kept while newer
#' than the parquet, unless the client has `force = TRUE`. Each file is held whole in memory while it is written:
#' keep `concurrency` low.
#'
#' @param p A [ggp_client()].
#' @param rows Other rows than the client's.
#' @param concurrency Files at a time.
#' @return A `ggp_result`.
#' @export
ggp_stata <- function(p, rows = NULL, concurrency = 2) {
  names <- names(consolidation_groups(rows %||% p$rows, p$downloads, p$processed))
  files <- file.path(p$consolidated, paste0(names, ".parquet"))
  stata_files(files[file.exists(files)], p$force, concurrency)
}

#' Convert one FFS dataset to Stata or SPSS
#'
#' Reads an FFS `.dat` with the layout and labels of its `.sps` and saves it, with variable and value labels, next
#' to the `.sps`. Every `$` in a variable name becomes `_` (Stata does not allow `$`). Missing value codes stay in the
#' data; sav also marks them as missing, as SPSS would.
#'
#' @param sps The `.sps` file; its `.dat` is the one of the same name, or the only one next to it.
#' @param ext `"dta"`, `"sav"`, `"csv"` (codes only), or several.
#' @param force Overwrite existing files.
#' @return The files written, invisibly.
#' @examples
#' \dontrun{
#' ggp_convert_ffs("AUSTRIA.SPS", ext = c("dta", "sav"))
#' }
#' @export
ggp_convert_ffs <- function(sps, ext = "dta", force = FALSE) {
  d <- read_ffs(sps, dat_of(sps))
  targets <- file.path(dirname(sps), paste0(tools::file_path_sans_ext(basename(sps)), ".", norm_ext(ext)))
  for (t in targets) write_data(d$df, d$meta, t, force)
  invisible(targets)
}
