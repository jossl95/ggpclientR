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

#' One log line: to the client's log file (`log`, a path, or NULL for none), and to the screen when it matters
#' (warnings, errors) or when `ggpclient.verbose` is TRUE. Never put an access link in a message: it works as a password.
#' @noRd
ggp_log <- function(log, level, msg, ...) {
  msg <- sprintf(msg, ...)
  if (!is.null(log) && dir.exists(dirname(log))) { # the folder of a client may be gone
    cat(sprintf("%s %s %s\n", format(Sys.time(), "%H:%M:%S"), toupper(level), msg), file = log, append = TRUE)
  }
  if (level == "warning") cli::cli_alert_warning("{msg}")
  else if (level == "error") cli::cli_alert_danger("{msg}")
  else if (isTRUE(getOption("ggpclient.verbose", interactive()))) cli::cli_alert_info("{msg}")
  invisible(msg)
}

.workers <- new.env() # the number of background processes that are running, if any
WORKERS <- "ggpclient" # their mirai compute profile

#' Stop the background processes
#'
#' Downloading, unpacking, preprocessing and the Stata export run in background R processes (mirai). They start when
#' first needed and stay for the next step, so that a step does not pay for starting them. They end with your R session;
#' call this to stop them earlier, for example after installing a new version of the package.
#'
#' @return `NULL`, invisibly.
#' @export
ggp_stop_workers <- function() {
  mirai::daemons(0, .compute = WORKERS)
  .workers$n <- NULL
  invisible()
}

utils::globalVariables("fun") # the function the daemons call: sent along as `fun`

#' `f` over each item of x in `cores` background R processes (mirai daemons; also on Windows). A failure comes back as
#' list(error = message), not as a stop. `f` and what it calls must be in the package: the daemons load it.
#' @noRd
parallel_map <- function(x, f, cores) {
  if (!length(x)) {
    return(list())
  }
  n <- max(1L, min(as.integer(cores), length(x)))
  if (!identical(.workers$n, n) || mirai::status(.compute = WORKERS)$connections != n) { # not running, or for another count
    mirai::daemons(0, .compute = WORKERS)
    mirai::daemons(n, .compute = WORKERS)
    .workers$n <- n
  }
  res <- mirai::mirai_map(x, \(item) tryCatch(list(value = fun(item)), error = \(e) list(error = conditionMessage(e))),
                         fun = f, .compute = WORKERS)[]
  lapply(res, \(r) if (mirai::is_error_value(r)) list(error = paste("the worker process stopped:", as.character(r))) else r)
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
