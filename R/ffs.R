# FFS conversion: to labelled dta or sav.
#
# Each FFS archive holds COUNTRY.DAT (the data, `records` lines per respondent), COUNTRY.SPS (SPSS syntax: DATA LIST
# with the record and columns of every variable, VARIABLE LABELS, VALUE LABELS, MISSING VALUE) and COUNTRY.TBL (the
# published frequency tables). Every $ in a variable name becomes _ (Stata does not allow $): IND01$01 becomes
# IND01_01. Downloads also rewrite the raw .sps that way (see rewrite_sps). Missing value codes (e.g. 98) stay in the
# data; sav also marks them as missing, as SPSS would. csv has the codes only: it cannot hold labels.

FFS_FORMATS <- c("sav", "dta", "csv")
CONVERTED <- "converted_data"
RAW <- c("dat", "sps", "tbl", "sav") # always kept; .sav because Estonia ships those instead of a .sps
DATA_FILES <- c(".dat", ".sps", ".tbl", ".sav", ".dta", ".csv") # renamed after the dataset; documents keep their names
SECTION <- "^(DATA LIST|VARIABLE LABELS?|VALUE LABELS?|MISSING VALUES?)\\b(.*)$"
# a label runs to the last quote on its line: some files have stray quotes where umlauts were ("Nieder"sterreich")
TOKEN <- r"---("[^\n]*"|'[^\n]*'|\([^)]*\)|/|[^\s/()"']+)---"

quoted <- function(t) substr(t, 1, 1) %in% c('"', "'")
unquote <- function(t) {
  q <- substr(t, 1, 1)
  trimws(gsub(strrep(q, 2), q, substr(t, 2, nchar(t) - 1), fixed = TRUE))
}
number <- function(t) suppressWarnings(as.numeric(t)) # NA when t is not a number
var_name <- function(n) gsub("[^A-Za-z0-9_]", "_", n)

# list(DATA = list(head, tokens), VARIABLE = ..., VALUE = ..., MISSING = ...); a command ends with a period
sections <- function(text) {
  out <- list()
  cur <- NULL
  for (line in strsplit(text, "\r\n|\n|\r")[[1]]) {
    m <- regmatches(line, regexec(SECTION, line, perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(m)) {
      cur <- toupper(strsplit(m[2], " ")[[1]][1])
      out[[cur]] <- list(head = m[3], lines = character())
      next
    }
    if (is.null(cur)) next
    body <- sub("\\s+$", "", line)
    end <- endsWith(body, ".") # labels end in a quote, so a final period is the command terminator
    out[[cur]]$lines <- c(out[[cur]]$lines, if (end) substr(body, 1, nchar(body) - 1) else body)
    if (end) cur <- NULL
  }
  lapply(out, \(s) list(head = s$head, tokens = regmatches(paste(s$lines, collapse = "\n"),
                                                           gregexpr(TOKEN, paste(s$lines, collapse = "\n"), perl = TRUE))[[1]]))
}

# split on "/": the separator between variable groups in SPSS label commands
groups <- function(tokens) {
  out <- list()
  group <- character()
  for (t in tokens) {
    if (t == "/") {
      if (length(group)) out[[length(out) + 1]] <- group
      group <- character()
    } else {
      group <- c(group, t)
    }
  }
  if (length(group)) out[[length(out) + 1]] <- group
  out
}

# '(98)', '(8,9)', '(1 THRU 3)' as a list of values and list(lo, hi) ranges
missing_spec <- function(spec) {
  toks <- strsplit(trimws(gsub("[()]", "", gsub(",", " ", spec))), "\\s+")[[1]]
  out <- list()
  i <- 1
  while (i <= length(toks)) {
    if (i + 2 <= length(toks) && toupper(toks[i + 1]) == "THRU") {
      lo <- number(toks[i])
      hi <- number(toks[i + 2])
      out[[length(out) + 1]] <- list(lo = if (is.na(lo)) -1e300 else lo, hi = if (is.na(hi)) 1e300 else hi) # LO / HI
      i <- i + 3
    } else {
      v <- number(toks[i])
      if (!is.na(v)) out[[length(out) + 1]] <- v
      i <- i + 1
    }
  }
  out
}

#' Layout and labels from FFS SPSS syntax
#'
#' Returns `records`, `layout` (a data frame: name, record, first and last column, is_text), `var_labels`,
#' `value_labels` (variable -> code -> label) and `missing` (variable -> values and ranges).
#' @noRd
parse_sps <- function(text) {
  s <- sections(text)
  toks <- s$DATA$tokens
  records <- as.integer(regmatches(s$DATA$head, regexec("records\\s*=\\s*(\\d+)", s$DATA$head, ignore.case = TRUE))[[1]][2])
  layout <- list()
  rec <- 1L
  i <- 1
  while (i <= length(toks)) {
    if (toks[i] == "/") {
      rec <- as.integer(toks[i + 1])
      i <- i + 2
      next
    }
    name <- toks[i]
    cols <- toks[i + 1]
    fmt <- if (i + 2 <= length(toks) && startsWith(toks[i + 2], "(")) toupper(toks[i + 2]) else NA_character_
    if (!grepl("^\\d+(-\\d+)?$", cols) || !(is.na(fmt) || fmt == "(A)")) {
      cli::cli_abort("DATA LIST: unsupported entry {name} {cols} {fmt}", class = "ggp_error")
    }
    ab <- as.integer(strsplit(cols, "-")[[1]])
    layout[[length(layout) + 1]] <- data.frame(name = name, record = rec, a = ab[1], b = ab[length(ab)],
                                               text = identical(fmt, "(A)"))
    i <- i + if (is.na(fmt)) 2 else 3
  }
  var_labels <- list()
  for (g in groups(s$VARIABLE$tokens)) {
    names <- character()
    for (t in g) {
      if (quoted(t)) {
        for (n in names) var_labels[[n]] <- unquote(t)
        names <- character()
      } else {
        names <- c(names, t)
      }
    }
  }
  value_labels <- list()
  for (g in groups(s$VALUE$tokens)) {
    k <- which(!is.na(number(g)))[1]
    if (is.na(k)) k <- length(g) + 1
    idx <- seq(k, length(g) - 1, by = 2)
    idx <- idx[quoted(g[idx + 1])]
    labels <- stats::setNames(vapply(g[idx + 1], unquote, character(1), USE.NAMES = FALSE), code(number(g[idx])))
    for (n in g[seq_len(k - 1)]) {
      value_labels[[n]] <- c(value_labels[[n]][setdiff(names(value_labels[[n]]), names(labels))], labels)
    }
  }
  missing <- list()
  for (g in groups(s$MISSING$tokens)) {
    names <- character()
    for (t in g) {
      if (startsWith(t, "(")) {
        for (n in names) missing[[n]] <- missing_spec(t)
        names <- character()
      } else {
        names <- c(names, t)
      }
    }
  }
  list(records = records, layout = do.call(rbind, layout), var_labels = var_labels, value_labels = value_labels,
       missing = missing)
}

#' The data frame of an FFS dataset, with its labels
#'
#' Returns `df` and `meta` (`column_labels`, `value_labels` and `missing_ranges` by variable, with $ in names as _).
#' @noRd
read_ffs <- function(sps, dat, log = NULL) {
  p <- parse_sps(paste(readLines(sps, encoding = "latin1", warn = FALSE), collapse = "\n"))
  lines <- readLines(dat, encoding = "latin1", warn = FALSE)
  if (length(lines) %% p$records) {
    cli::cli_abort("{.file {dat}}: {length(lines)} lines is not a multiple of {p$records} records per case",
                   class = "ggp_error")
  }
  n <- length(lines) %/% p$records
  bad <- 0L
  cols <- lapply(seq_len(nrow(p$layout)), \(i) {
    l <- p$layout[i, ]
    s <- trimws(substr(lines[(seq_len(n) - 1) * p$records + l$record], l$a, l$b))
    s[!nzchar(s)] <- NA_character_
    if (l$text) {
      return(s)
    }
    num <- suppressWarnings(as.numeric(s)) # like SPSS: a cell that is not a number (the files use * for overflow) is missing
    bad <<- bad + sum(is.na(num) & !is.na(s))
    num
  })
  names(cols) <- var_name(p$layout$name)
  if (anyDuplicated(names(cols))) {
    cli::cli_abort("{.file {sps}}: two variable names collide once $ is replaced", class = "ggp_error")
  }
  if (bad) {
    ggp_log(log, "warning", "%s: %d cells were not numbers (such as *) and are missing, as SPSS reads them", dat, bad)
  }
  df <- as.data.frame(cols, check.names = FALSE, optional = TRUE)
  rename <- \(l) stats::setNames(l[names(l) %in% p$layout$name], var_name(names(l)[names(l) %in% p$layout$name]))
  numeric <- var_name(p$layout$name[!p$layout$text])
  missing <- rename(p$missing)
  list(df = df, meta = list(
    column_labels = lapply(rename(p$var_labels), substr, 1, 80), # Stata allows 80 chars
    value_labels = Filter(length, rename(p$value_labels)),
    missing_ranges = Filter(length, missing[names(missing) %in% numeric])
  ))
}

# df with its labels as haven wants them, ready to write
labelled_frame <- function(df, meta, kind) {
  for (column in names(df)) {
    x <- df[[column]]
    lab <- meta$column_labels[[column]]
    vl <- meta$value_labels[[column]]
    vl <- vl[!is.na(number(names(vl)))] # codes that are not numbers (Stata's .a, .b) cannot be labelled
    if (kind == "dta") { # Stata labels whole numbers only: drop labels on columns holding decimals
      whole <- is.numeric(x) && all(x[!is.na(x)] %% 1 == 0)
      vl <- if (whole) vl[number(names(vl)) %% 1 == 0] else NULL
    }
    labels <- if (length(vl)) stats::setNames(number(names(vl)), unlist(vl, use.names = FALSE))
    miss <- if (kind == "sav") meta$missing_ranges[[column]]
    if (length(miss)) {
      values <- unlist(Filter(is.numeric, miss))
      range <- Filter(is.list, miss)
      x <- haven::labelled_spss(x, labels = labels, na_values = values, na_range = if (length(range)) unlist(range[[1]]),
                                label = lab)
    } else if (length(labels)) {
      x <- haven::labelled(x, labels = labels, label = lab)
    } else if (!is.null(lab)) {
      attr(x, "label") <- lab
    }
    df[[column]] <- x
  }
  df
}

#' Save as target's type (sav, dta, csv) via a temporary name; an existing file is kept unless force
#'
#' Returns TRUE when the file was written.
#' @noRd
write_data <- function(df, meta, target, force = FALSE) {
  if (file.exists(target) && !force) {
    return(FALSE)
  }
  kind <- tolower(tools::file_ext(target))
  if (!kind %in% FFS_FORMATS) {
    cli::cli_abort("unknown format {.val {kind}}: choose from {.val {FFS_FORMATS}}", class = "ggp_error")
  }
  atomic(target, \(part) switch(kind,
    sav = haven::write_sav(labelled_frame(df, meta, "sav"), part),
    dta = haven::write_dta(labelled_frame(df, meta, "dta"), part),
    csv = utils::write.csv(df, part, row.names = FALSE, na = "")
  ))
  TRUE
}

# the sources of a folder: list(path, read) for each FFS dataset; the .sav files when there is no .sps (Estonia)
ffs_sources <- function(folder, log = NULL) {
  files <- sort(list.files(folder, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE))
  files <- files[!grepl(".part", basename(files), fixed = TRUE) & !dir.exists(files)]
  by_suffix <- \(s) files[file_type(files) == s]
  sources <- lapply(by_suffix(".sps"), \(sps) {
    dats <- by_suffix(".dat")
    dats <- dats[dirname(dats) == dirname(sps)]
    same <- dats[tolower(tools::file_path_sans_ext(basename(dats))) == tolower(tools::file_path_sans_ext(basename(sps)))]
    dat <- if (length(same)) same[1] else if (length(dats) == 1) dats else {
      cli::cli_abort("no .dat for {.file {basename(sps)}}", class = "ggp_error")
    }
    list(path = sps, read = \() read_ffs(sps, dat, log))
  })
  if (!length(sources)) {
    sources <- lapply(by_suffix(".sav"), \(sav) list(path = sav, read = \() {
      x <- read_stat(sav, user_na = TRUE)
      m <- ggp_meta(sav)
      ren <- \(l) stats::setNames(l, var_name(names(l)))
      list(df = stats::setNames(as.data.frame(lapply(x, plain), check.names = FALSE, optional = TRUE), var_name(names(x))),
           meta = list(column_labels = lapply(ren(m$column_labels), \(l) if (is.na(l)) NULL else l) |> Filter(f = Negate(is.null)),
                       value_labels = ren(m$value_labels), missing_ranges = ren(m$missing_ranges)))
    }))
  }
  if (!length(sources)) {
    cli::cli_abort("no .sps (or .sav) to convert", class = "ggp_error")
  }
  sources
}

#' Convert every FFS dataset unpacked in folder to `exts`, saved in out_dir (default: folder) as <name>.<ext>
#'
#' A dataset is a .sps with the .dat of the same name (or the only .dat next to it). A country shipped without
#' .sps but with .sav files (Estonia) is converted from those. Several datasets in one folder get the source name
#' added: <name>_<source>.<ext>. Returns the number of matching output files.
#' @noRd
convert_ffs <- function(folder, exts, name = CONVERTED, force = FALSE, out_dir = folder, log = NULL) {
  sources <- ffs_sources(folder, log)
  n <- 0L
  for (s in sources) {
    base <- if (length(sources) == 1) name else paste0(name, "_", tolower(tools::file_path_sans_ext(basename(s$path))))
    targets <- file.path(out_dir, paste0(base, ".", exts))
    todo <- targets[force | !file.exists(targets)]
    if (length(todo)) {
      d <- s$read()
      for (t in todo) write_data(d$df, d$meta, t, force)
    }
    n <- n + length(targets)
  }
  n
}

#' The .sps with every $ in a variable name as _, and FILE= pointing at dat_name (the .dat next to it)
#'
#' Quoted text (labels, the FILE path) is left alone, so a $ in a label stays.
#' @noRd
rewrite_sps <- function(text, dat_name = NULL) {
  m <- gregexpr("\"[^\n]*\"|'[^\n]*'|\\$", text, perl = TRUE)
  regmatches(text, m) <- lapply(regmatches(text, m), \(x) ifelse(x == "$", "_", x))
  if (!is.null(dat_name)) { # the original points at C:\\ISSA\\...\\COUNTRY.DAT; the .dat is renamed on download
    text <- sub("(?i)(\\bFILE\\s*=\\s*)(['\"])[^'\"\n]*\\2", paste0("\\1'", gsub("\\", "\\\\", dat_name, fixed = TRUE), "'"),
                text, perl = TRUE)
  }
  text
}
