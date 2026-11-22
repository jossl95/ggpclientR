labelled_dta <- function(path, df, labels = list(), value_labels = list()) {
  for (c in names(df)) {
    if (!is.null(value_labels[[c]])) {
      df[[c]] <- haven::labelled(df[[c]], value_labels[[c]], label = labels[[c]])
    } else if (!is.null(labels[[c]])) {
      attr(df[[c]], "label") <- labels[[c]]
    }
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  haven::write_dta(df, path)
}


row_of <- function(...) {
  r <- list(project = "GGS", round = 2L, series = "GGS-II", wave = "w1", country = "AT", version = "1", local_path = NA_character_)
  as.data.frame(utils::modifyList(r, list(...)))
}

