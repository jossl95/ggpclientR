`%||%` <- function(x, y) if (is.null(x)) y else x

# an absolute URL stays as it is; a path is joined to the portal
abs_url <- function(href) {
  ifelse(grepl("^https?://", href), href, paste0(ggp_base(), ifelse(startsWith(href, "/"), "", "/"), href))
}

# paths as folders and files use them: forward slashes, whatever the system
posix <- function(x) gsub("\\\\", "/", x)

# a file written whole or not at all: write to `part`, then rename
atomic <- function(target, write) {
  part <- paste0(target, ".part")
  on.exit(unlink(part), add = TRUE)
  write(part)
  file.rename(part, target) || stop("cannot write ", target)
  invisible(target)
}

# the log file of the client in use; set by ggp_client(), read here so that every function can log
.state <- new.env()

#' One log line: always to the log file, to the screen when it matters (warnings, errors) or when
#' `ggpclient.verbose` is TRUE. Never put an access link in a message: it works as a password.
#' @noRd
ggp_log <- function(level, msg, ...) {
  msg <- sprintf(msg, ...)
  if (!is.null(.state$log) && dir.exists(dirname(.state$log))) { # the folder of a client may be gone
    cat(sprintf("%s %s %s\n", format(Sys.time(), "%H:%M:%S"), toupper(level), msg), file = .state$log, append = TRUE)
  }
  if (level == "warning") cli::cli_alert_warning("{msg}")
  else if (level == "error") cli::cli_alert_danger("{msg}")
  else if (isTRUE(getOption("ggpclient.verbose", interactive()))) cli::cli_alert_info("{msg}")
  invisible(msg)
}

#' lapply over x in forked workers (one on Windows); a failure comes back as list(error = message), not as a stop
#' @noRd
parallel_map <- function(x, f, cores) {
  cores <- if (.Platform$OS.type == "windows") 1L else max(1L, min(as.integer(cores), length(x)))
  res <- parallel::mclapply(x, \(i) tryCatch(list(value = f(i)), error = \(e) list(error = conditionMessage(e))),
                            mc.cores = cores, mc.preschedule = FALSE)
  lapply(res, \(r) if (is.list(r) && ("value" %in% names(r) || "error" %in% names(r))) r else list(error = "the worker process stopped"))
}

# the result of a step: how many were done, skipped, and which failed (name, error)
new_result <- function(step, done = 0L, skipped = 0L, failed = data.frame(name = character(), error = character()), ...) {
  structure(list(step = step, done = as.integer(done), skipped = as.integer(skipped), failed = failed, ...),
            class = "ggp_result")
}

#' @export
print.ggp_result <- function(x, ...) {
  cat(sprintf("%s: %d done, %d skipped, %d failed\n", x$step, x$done, x$skipped, nrow(x$failed)))
  for (i in seq_len(nrow(x$failed))) cat(sprintf("  failed: %s: %s\n", x$failed$name[i], x$failed$error[i]))
  invisible(x)
}

failures <- function(names, errors) data.frame(name = as.character(names), error = as.character(errors))

# a list that JSON writes as an object, also when it is empty
obj <- function(x) if (length(x)) x else structure(list(), names = character())
