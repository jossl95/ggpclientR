# Archives built in the tests. A zip is written by hand (stored, no compression) so that any member name is possible,
# the unsafe ones too; 7z comes from the archive package.
crc32 <- local({
  xor32 <- function(a, b) bitwXor(a %/% 65536, b %/% 65536) * 65536 + bitwXor(a %% 65536, b %% 65536)
  table <- vapply(0:255, \(n) {
    for (k in 1:8) n <- if (n %% 2 == 1) xor32(n %/% 2, 3988292384) else n %/% 2
    n
  }, numeric(1))
  function(bytes) {
    crc <- 4294967295
    for (b in as.integer(bytes)) crc <- xor32(table[bitwXor(as.integer(crc %% 256), b) + 1], crc %/% 256)
    xor32(crc, 4294967295)
  }
})

le <- function(x, n) as.raw((x %/% 256^(0:(n - 1))) %% 256)

make_zip <- function(members) {
  out <- raw()
  central <- raw()
  for (name in names(members)) {
    data <- bytes(members[[name]])
    nm <- charToRaw(name)
    crc <- crc32(data)
    head <- c(le(20, 2), le(0, 2), le(0, 2), le(0, 2), le(33, 2), le(crc, 4), le(length(data), 4),
              le(length(data), 4), le(length(nm), 2), le(0, 2))
    central <- c(central, as.raw(c(0x50, 0x4b, 1, 2)), le(20, 2), head, le(0, 2), le(0, 2), le(0, 2), le(0, 4),
                 le(length(out), 4), nm)
    out <- c(out, as.raw(c(0x50, 0x4b, 3, 4)), head, nm, data)
  }
  c(out, central, as.raw(c(0x50, 0x4b, 5, 6)), le(0, 2), le(0, 2), le(length(members), 2), le(length(members), 2),
    le(length(central), 4), le(length(out), 4), le(0, 2))
}

make_7z <- function(members) {
  dir <- tempfile()
  dir.create(dir)
  for (name in names(members)) {
    dir.create(dirname(file.path(dir, name)), recursive = TRUE, showWarnings = FALSE)
    writeBin(bytes(members[[name]]), file.path(dir, name))
  }
  path <- tempfile(fileext = ".7z")
  withr::with_dir(dir, archive::archive_write_files(path, names(members), format = "7zip"))
  readBin(path, "raw", file.size(path))
}

bytes <- function(x) if (is.raw(x)) x else charToRaw(x)

# the files below dir, as paths relative to it
tree <- function(dir) sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))

save_raw <- function(x, path = tempfile()) {
  writeBin(x, path)
  path
}
