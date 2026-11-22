# Stack the preprocessed parquet files per round and wave into one file:
# data_dir/download/consolidated/<series>_<wave>.parquet
#
#   download/consolidated/ggs-ii_w1.parquet   every GGS-II wave 1 country
#   download/consolidated/ggs-i_w2.parquet
#   download/consolidated/ffs.parquet         FFS has no waves
#
# Columns are matched by name; a column a country lacks is NA there, and a column with different types in different
# countries gets the type that holds both (a number and a text column become text). The ggp_country and ggp_file
# columns say where each row came from.
#
# Country-specific variables are left out: GGS names them after the GGP country number, <name>_<country><nn>
# (aut01a_2101 and covid_2201a are Austria's 21 and Estonia's 22; dem07_1401 is Germany's 14). The number comes from
# COUNTRY_NUMBERS, or else from the file's own `country` variable. The per-country files in download/processed keep
# them. Each result gets <name>.meta.json: the variable and value labels of its input files, harmonized (see
# merged_meta). to_stata() writes <name>.dta from the two, with the labels applied. An existing result is kept while
# it is up to date: it holds exactly the files of the rows given and is newer than all of them. Otherwise (a country
# added or left out, a file preprocessed again) it is made again.

# GGP country numbers, as in the `country` variable of the GGS-II files (Kazakhstan's file has none: from its names)
COUNTRY_NUMBERS <- c(AT = 21, CZ = 28, DE = 14, DK = 37, EE = 22, FI = 39, FR = 15, GB = 43, HK = 38, HR = 31, IS = 44,
                     KZ = 34, MD = 35, NL = 18, NO = 20, PL = 26, SE = 29, TW = 41, UY = 40)
CHUNK <- 250 # columns read together while stacking: the memory used besides the result

parquet_columns <- function(path) nanoparquet::read_parquet_schema(path)$name[-1]

#' list(name = data.frame(file, country, src)) for the preprocessed files of rows, per series and wave
#' @noRd
consolidation_groups <- function(rows, root, pre) {
  out <- list()
  for (r in rows_of(rows)) {
    s <- sources(r, root, pre)
    name <- tolower(if (is.na(r$wave) || !nzchar(r$wave)) r$series else paste0(r$series, "_", r$wave))
    for (i in seq_len(nrow(s))) {
      f <- parquet_of(pre, s$rel[i])
      if (file.exists(f)) {
        out[[name]] <- rbind(out[[name]], data.frame(file = f, country = r$country, src = posix(s$rel[i])))
      }
    }
  }
  out
}

#' The names in the file that end in this country's number: aut01a_2101 for Austria (21)
#' @noRd
country_columns <- function(path, country) {
  names <- parquet_columns(path)
  number <- COUNTRY_NUMBERS[toupper(country %||% "")]
  number <- if (is.na(number)) NULL else number
  own <- names[tolower(names) == "country"][1]
  if (is.null(number) && !is.na(own)) {
    vals <- unique(stats::na.omit(nanoparquet::read_parquet(path, col_select = own)[[1]]))
    # several countries in one file: no number of its own
    number <- if (length(vals) == 1 && is.numeric(vals) && vals >= 10 && vals < 100) as.integer(vals)
  }
  if (is.null(number)) character() else names[grepl(sprintf("_%d\\d\\d[a-z]*$", number), names, ignore.case = TRUE)]
}

#' The columns each file keeps, and the stacked data frame of them
#' @noRd
stack_files <- function(files) {
  keep <- lapply(seq_len(nrow(files)), \(i) {
    drop <- country_columns(files$file[i], files$country[i])
    if (length(drop)) {
      ggp_log("info", "consolidate: leaving out %d %s-specific variables of %s", length(drop), files$country[i],
              basename(files$file[i]))
    }
    setdiff(parquet_columns(files$file[i]), drop)
  })
  all <- unique(unlist(keep))
  n <- vapply(files$file, \(f) nanoparquet::read_parquet_info(f)$num_rows, numeric(1))
  out <- list()
  for (chunk in split(all, ceiling(seq_along(all) / CHUNK))) {
    pieces <- lapply(seq_len(nrow(files)), \(i) {
      have <- intersect(chunk, keep[[i]])
      if (length(have)) as.list(nanoparquet::read_parquet(files$file[i], col_select = have)) else list()
    })
    for (c in chunk) { # a column a file lacks is NA there; `c()` finds the type that holds both
      col <- lapply(pieces, `[[`, c)
      ref <- col[[which(!vapply(col, is.null, logical(1)))[1]]]
      col <- Map(\(x, k) x %||% ref[rep(NA_integer_, k)], col, n)
      if (any(vapply(col, is.character, logical(1)))) col <- lapply(col, as.character)
      out[[c]] <- do.call(c, unname(col))
    }
  }
  as.data.frame(out, check.names = FALSE, optional = TRUE)
}

#' True if target was made from exactly these files (its ggp_file column) and is newer than all of them
#' @noRd
is_current <- function(target, files) {
  tryCatch({
    if (!file.exists(target) || !file.exists(meta_of(target)) ||
        file.mtime(target) < max(file.mtime(files$file))) {
      return(FALSE)
    }
    have <- unique(nanoparquet::read_parquet(target, col_select = "ggp_file")[[1]])
    setequal(have, files$src)
  }, error = \(e) FALSE) # missing or unreadable: make it again
}

consolidation_todo <- function(rows, root, pre, out, force) {
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  all <- consolidation_groups(rows, root, pre)
  current <- !force & vapply(names(all), \(n) is_current(file.path(out, paste0(n, ".parquet")), all[[n]]), logical(1))
  list(todo = all[!current], skipped = sum(current))
}

norm_label <- function(label) tolower(gsub("\\s+", " ", trimws(label))) # "Czech  Republic" and "czech republic" are one

#' The label most voters use (by its normalized form), in its most common spelling; ties go to the first voter
#' @noRd
pick_label <- function(spellings) {
  norm <- norm_label(spellings)
  levels <- unique(norm)
  counts <- tabulate(match(norm, levels), length(levels))
  top <- which.max(counts)
  same <- spellings[norm == levels[top]]
  spell <- unique(same)
  list(label = spell[which.max(tabulate(match(same, spell), length(spell)))], levels = levels, counts = counts, top = top)
}

#' The harmonized labels of a consolidated file, from its input files' meta.json, for the columns it kept
#'
#' Each country has one vote, that of its first file: Belarus ships GGS-II wave 1 in English and in Russian.
#' Variable label: the one most countries use. Value labels, per variable: a code gets the label more than half of
#' the countries that label it use; with no such majority, all their labels joined by " / " (most used first). So
#' every value has exactly one label; several values may share one (2102 and 3103 "Elementary school"). Codes are
#' never changed. original_labels keeps, for every code labelled differently in any file, each wording and the source
#' files (under download/raw, or download/processed for FFS) that use it.
#' @noRd
merged_meta <- function(files, columns) {
  long <- list(names = list(), values = list())
  for (i in seq_len(nrow(files))) {
    m <- meta_of(files$file[i])
    if (!file.exists(m)) next # csv sources have none
    voter <- if (is.na(files$country[i]) || !nzchar(files$country[i])) files$src[i] else files$country[i] # a vote per file for HH and Additional Datasets
    j <- jsonlite::read_json(m, simplifyVector = FALSE)
    lab <- unlist(j$column_labels)
    lab <- lab[nzchar(lab) & names(lab) %in% columns]
    if (length(lab)) long$names[[i]] <- data.frame(var = names(lab), label = unname(lab), voter = voter)
    for (v in intersect(names(j$value_labels), columns)) {
      vl <- unlist(j$value_labels[[v]])
      vl <- vl[nzchar(vl)]
      if (length(vl)) {
        long$values[[length(long$values) + 1]] <- data.frame(var = v, code = names(vl), label = unname(vl),
                                                               src = files$src[i], voter = voter)
      }
    }
  }
  first_vote <- \(label, voter) label[!duplicated(voter)]
  names_long <- do.call(rbind, long$names)
  column_labels <- list()
  if (!is.null(names_long)) {
    for (v in unique(names_long$var)) {
      d <- names_long[names_long$var == v, ]
      column_labels[[v]] <- pick_label(first_vote(d$label, d$voter))$label
    }
  }
  values <- do.call(rbind, long$values)
  value_labels <- list()
  original <- list()
  if (!is.null(values)) {
    for (v in unique(values$var)) {
      dv <- values[values$var == v, ]
      d <- list()
      for (k in unique(dv$code)) {
        found <- dv[dv$code == k, ]
        votes <- first_vote(found$label, found$voter)
        p <- pick_label(votes)
        label <- p$label
        if (length(p$levels) > 1 && p$counts[p$top] * 2 <= length(votes)) { # no majority: say what each country means
          label <- paste(vapply(p$levels[order(-p$counts, seq_along(p$counts))],
                                \(n) pick_label(votes[norm_label(votes) == n])$label, ""), collapse = " / ")
        }
        if (length(unique(norm_label(found$label))) > 1) {
          orig <- lapply(split(found$src, factor(found$label, unique(found$label))), as.list)
          original[[v]] <- c(original[[v]], stats::setNames(list(orig), k))
        }
        d[[k]] <- label
      }
      value_labels[[v]] <- d
    }
  }
  list(column_labels = obj(column_labels), value_labels = obj(value_labels), original_labels = obj(original))
}

#' Stack one group and write <name>.parquet and <name>.meta.json (meta first: a parquet without it is never current)
#' @noRd
consolidate_one <- function(name, files, out) {
  df <- stack_files(files)
  target <- file.path(out, paste0(name, ".parquet"))
  atomic(target, \(part) {
    nanoparquet::write_parquet(df, part)
    write_json(merged_meta(files, names(df)), meta_of(target))
  })
  ggp_log("info", "consolidated %d files into %s.parquet", nrow(files), name)
}

#' Stack the preprocessed files of `rows` per series and wave. One group after the other: a group is held in memory
#' @noRd
consolidate_rows <- function(rows, root, pre, out, force = FALSE) {
  t <- consolidation_todo(rows, root, pre, out, force)
  failed <- failures(character(), character())
  for (name in names(t$todo)) {
    tryCatch(consolidate_one(name, t$todo[[name]], out), error = \(e) { # say, two columns whose types cannot be combined
      unlink(file.path(out, paste0(name, ".parquet.part")))
      failed <<- rbind(failed, failures(name, conditionMessage(e)))
      ggp_log("error", "consolidate failed %s: %s", name, conditionMessage(e))
    })
  }
  new_result("consolidate", length(t$todo) - nrow(failed), t$skipped, failed)
}

# the values a Stata value label can have
STATA_LONG <- c(-2147483647, 2147483620)

#' Write <name>.dta next to a consolidated <name>.parquet, with the labels of its meta.json applied
#'
#' Stata labels whole numbers only, so value labels go on integer columns and on decimal columns that hold only whole
#' numbers. Kept while newer than the parquet, unless force. Returns TRUE when written, FALSE when kept.
#' @noRd
to_stata <- function(parquet, force = FALSE) {
  target <- sub("\\.parquet$", ".dta", parquet)
  if (!force && file.exists(target) && file.mtime(target) >= file.mtime(parquet)) {
    return(FALSE)
  }
  m <- jsonlite::read_json(meta_of(parquet), simplifyVector = FALSE)
  df <- ggp_read(parquet)
  for (c in names(df)) { # whole numbers as 4-byte Stata longs, not 8-byte doubles: half the file
    x <- df[[c]]
    if (is.numeric(x) && !is.integer(x) && all(x[!is.na(x)] %% 1 == 0) && all(abs(x[!is.na(x)]) <= STATA_LONG[2])) {
      df[[c]] <- as.integer(x)
    }
    if (is.logical(x) && all(is.na(x))) df[[c]] <- as.integer(x)
  }
  m$column_labels <- lapply(m$column_labels, \(l) if (is.null(l)) NULL else substr(l, 1, 80)) # Stata allows 80 chars
  atomic(target, \(part) haven::write_dta(labelled_frame(df, m, "dta"), part))
  TRUE
}

#' Several groups at a time in forked workers; each file is held whole in memory while it is written
#' @noRd
stata_files <- function(parquets, force = FALSE, concurrency = 2) {
  res <- parallel_map(parquets, \(p) to_stata(p, force), concurrency)
  failed <- failures(character(), character())
  done <- 0L
  for (i in seq_along(res)) {
    if (!is.null(res[[i]]$error)) {
      failed <- rbind(failed, failures(basename(parquets[i]), res[[i]]$error))
      ggp_log("error", "to_stata failed %s: %s", basename(parquets[i]), res[[i]]$error)
    } else if (res[[i]]$value) {
      done <- done + 1L
      ggp_log("info", "to_stata: wrote %s", basename(sub("\\.parquet$", ".dta", parquets[i])))
    }
  }
  new_result("stata", done, length(parquets) - done - nrow(failed), failed)
}
