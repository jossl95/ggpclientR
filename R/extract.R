# Unpack downloaded archives: keep the datafiles asked for, mirror nested archives as nested folders.

DATA_EXT <- c("dta", "sav", "rds", "csv")
ARCHIVE_EXT <- c("zip", "7z") # the portal serves both, and some ".7z" files are really zips: content decides
MAX_DEPTH <- 5 # shortcut: no cap on unpacked size, fine for archives from the portal; add one if that changes

#' One extension, several, or NULL for all datafile types; any case, with or without a dot
#' @noRd
norm_ext <- function(ext) {
  ext <- unlist(ext)
  if (!length(ext)) {
    return(DATA_EXT)
  }
  unique(tolower(sub("^\\.", "", as.character(ext))))
}

#' Keep the requested datafiles; with metadata also everything that is not a datafile type
#' @noRd
wanted <- function(suffix, ext, metadata) suffix %in% ext || (metadata && !suffix %in% DATA_EXT)

suffix_of <- function(name) tolower(tools::file_ext(name))

# datafiles (dta, sav, csv) are saved in lower case, like the folders; documents keep their names
datafile_name <- function(name) if (suffix_of(name) %in% DATA_EXT) tolower(name) else name

#' Delete the files next to target that differ from it only in case: the old names of a datafile
#' @noRd
drop_case_variants <- function(target) {
  if (dir.exists(dirname(target))) {
    same <- list.files(dirname(target), all.files = TRUE, no.. = TRUE)
    same <- same[tolower(same) == tolower(basename(target)) & same != basename(target)]
    unlink(file.path(dirname(target), same))
  }
  invisible()
}

#' An archive member path as folder and file parts, or NULL if it could escape the target folder
#' @noRd
member_parts <- function(name) {
  name <- posix(name)
  parts <- strsplit(name, "/", fixed = TRUE)[[1]]
  if (startsWith(name, "/") || ".." %in% parts || grepl(":", parts[1] %||% "", fixed = TRUE)) {
    return(NULL)
  }
  parts <- parts[nzchar(parts)]
  if (!length(parts) || "__MACOSX" %in% parts || startsWith(parts[length(parts)], "._")) {
    return(NULL)
  }
  parts
}

is_archive <- function(path) {
  magic <- readBin(path, "raw", 6)
  identical(magic[1:2], as.raw(c(0x50, 0x4b))) || identical(magic, as.raw(c(0x37, 0x7a, 0xbc, 0xaf, 0x27, 0x1c)))
}

# member i of the archive, written whole or not at all
write_member <- function(path, i, target) {
  atomic(target, \(part) {
    src <- archive::archive_read(path, file = i)
    open(src, "rb")
    on.exit(close(src), add = TRUE)
    out <- file(part, "wb")
    on.exit(close(out), add = TRUE, after = FALSE)
    repeat {
      chunk <- readBin(src, "raw", 2^20)
      if (!length(chunk)) break
      writeBin(chunk, out)
    }
  })
}

#' Extract the wanted members of the archive at path into dest; a nested archive becomes a folder named after it
#'
#' Folder and datafile names are lowercased, other file names are kept. A file that already exists is left as it
#' is unless force, which also removes the same datafile saved under an old, differently cased name. Returns
#' the number of matching files (written or already there).
#' @noRd
unpack <- function(path, dest, ext, metadata, force = FALSE, depth = 0) {
  n <- 0L
  members <- archive::archive(path)
  for (i in seq_len(nrow(members))) {
    parts <- if (endsWith(members$path[i], "/")) NULL else member_parts(members$path[i])
    if (is.null(parts)) next
    fname <- parts[length(parts)]
    folder <- do.call(file.path, as.list(c(dest, tolower(parts[-length(parts)]))))
    suffix <- suffix_of(fname)
    if (suffix %in% ARCHIVE_EXT && depth < MAX_DEPTH) {
      inner <- tempfile()
      write_member(path, i, inner)
      if (is_archive(inner)) {
        n <- n + unpack(inner, file.path(folder, tolower(tools::file_path_sans_ext(fname))), ext, metadata, force,
                        depth + 1)
        unlink(inner)
        next
      }
      unlink(inner)
    }
    if (wanted(suffix, ext, metadata)) {
      target <- file.path(folder, datafile_name(fname))
      if (force || !file.exists(target)) {
        dir.create(folder, recursive = TRUE, showWarnings = FALSE)
        if (force && suffix %in% DATA_EXT) drop_case_variants(target)
        write_member(path, i, target)
      }
      n <- n + 1L
    }
  }
  n
}

#' Unpack a downloaded file: a zip or 7z is extracted, anything else is kept if it matches
#' @noRd
unpack_file <- function(path, name, dest, ext, metadata, force = FALSE) {
  if (is_archive(path)) {
    return(unpack(path, dest, ext, metadata, force))
  }
  parts <- member_parts(name)
  fname <- if (is.null(parts)) "download" else datafile_name(parts[length(parts)])
  if (!wanted(suffix_of(fname), ext, metadata)) {
    return(0L)
  }
  target <- file.path(dest, fname)
  if (force || !file.exists(target)) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    if (force) drop_case_variants(target)
    atomic(target, \(part) file.copy(path, part, overwrite = TRUE))
  }
  1L
}
