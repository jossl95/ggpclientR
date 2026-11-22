# Putting variable and value labels on the data: the parquet files hold codes, their meta.json the labels.

#' Put the labels on a data frame
#'
#' Gives every column the variable label and value labels of `meta`, as haven labelled columns: the variable label in
#' the `label` attribute, the value labels in `labels`. [haven::as_factor()] then replaces the codes by their labels
#' (factors in the order of the codes), and RStudio's viewer shows the variable labels. Codes without a label keep
#' their value; Stata's extended missing codes (`.a`, `.b`) are not values of a numeric column and are left out.
#'
#' @param df A data frame, such as a parquet file read with [ggp_read()].
#' @param meta The labels: from [ggp_meta()], which reads the `.meta.json` next to a parquet file.
#' @param columns Only these columns (default: all).
#' @return `df`, with labelled columns.
#' @examples
#' \dontrun{
#' path <- "data/download/consolidated/ggs-ii_w1.parquet"
#' df <- ggp_label(ggp_read(path), ggp_meta(path))
#' attr(df$dem01, "label")   # "Gender"
#' haven::as_factor(df$dem01) # the labels instead of the codes
#' }
#' @export
ggp_label <- function(df, meta, columns = names(df)) {
  for (column in intersect(columns, names(df))) {
    lab <- meta$column_labels[[column]]
    codes <- meta$value_labels[[column]]
    if (is.null(lab) && !length(codes)) next
    x <- plain(df[[column]])
    labels <- NULL
    if (length(codes)) {
      keys <- if (is.numeric(x)) suppressWarnings(as.numeric(names(codes))) else names(codes)
      keep <- !is.na(keys) # Stata's .a, .b are not values of a numeric column
      labels <- stats::setNames(keys[keep], unlist(codes, use.names = FALSE)[keep])
      if (is.integer(x)) { # haven wants the labels in the type of the column
        if (all(labels %% 1 == 0)) labels <- stats::setNames(as.integer(labels), names(labels)) else x <- as.double(x)
      }
    }
    lab <- if (length(lab) && !is.na(lab)) as.character(lab)
    df[[column]] <- if (length(labels)) haven::labelled(x, labels = labels, label = lab) else structure(x, label = lab)
  }
  df
}
