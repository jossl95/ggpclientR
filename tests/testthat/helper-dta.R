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

