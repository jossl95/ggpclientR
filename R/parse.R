# Pure parsing and naming helpers: no network, no database.

ggp_base <- function() sub("/+$", "", getOption("ggpclient.base", "https://www.ggp-i.org"))
catalog_url <- function() paste0(ggp_base(), "/data-catalog/")
page_url <- function(slug) paste0(ggp_base(), "/dataset/", slug, "/")

# slug key -> project, round, series folder name
SERIES <- list(
  ggs1 = list("GGS", 1L, "GGS-I"),
  ggs2 = list("GGS", 2L, "GGS-II"),
  ffs = list("FFS", NA_integer_, "FFS")
)
NO_WAVE <- "thefamilyandfertilitysurvey" # FFS is one survey per country
# sections listed only on the logged-in data access page, with no dataset page: heading -> slug key
ACCESS_SECTIONS <- c("Harmonized Histories" = "hh", "Additional Datasets" = "other")

# ggs2-at-wave1, ggs2-at-wave1-v1-0 (older version), upcoming-ggs2-fr-wave1
SLUG_RE <- paste0("^(?<upcoming>upcoming-)?(?<key>[a-z0-9]+)-(?<cc>[a-z]{2})-",
                  "(?<wave>[a-z0-9]+?)(?:-v(?<ver>\\d+-\\d+))?$")
DOWNLOAD_HREF <- "/form/download/(\\d+)"
SIZE_RE <- "(?s)(.+?),\\s*([\\d.]+)\\s*([KMG]B)\\b"
UNIT_MB <- c(KB = 1 / 1024, MB = 1, GB = 1024)

# the named capture groups of the first match of pattern in x, or NULL
re_match <- function(pattern, x, ignore.case = FALSE) {
  m <- regexpr(pattern, x, perl = TRUE, ignore.case = ignore.case)
  if (m == -1L) {
    return(NULL)
  }
  start <- attr(m, "capture.start")[1, ]
  len <- attr(m, "capture.length")[1, ]
  out <- substring(x, start, start + len - 1L)
  out[start == -1L] <- ""
  names(out) <- attr(m, "capture.names")
  out
}

squish <- function(x) trimws(gsub("\\s+", " ", x))

#' Dataset slug from a link, or NA
#'
#' The strict character set keeps slugs safe as path parts.
#' @noRd
slug_from_href <- function(href) {
  host <- ""
  path <- href
  m <- re_match("^(?:[a-zA-Z][a-zA-Z0-9+.-]*:)?//(?<host>[^/?#]*)(?<path>.*)$", href)
  if (!is.null(m)) {
    host <- tolower(m[["host"]])
    path <- m[["path"]]
  }
  path <- sub("[?#].*$", "", path)
  base_host <- tolower(sub("^[a-z]+://", "", ggp_base()))
  if (!host %in% c("", "www.ggp-i.org", "ggp-i.org", base_host)) {
    return(NA_character_)
  }
  m <- re_match("^/dataset/(?<slug>[A-Za-z0-9_-]+)/?$", path)
  if (is.null(m)) NA_character_ else tolower(m[["slug"]])
}

slugs_from_hrefs <- function(hrefs) {
  vapply(hrefs, slug_from_href, character(1), USE.NAMES = FALSE)
}

parse_slug <- function(slug) {
  key <- sub("-.*$", "", slug)
  name <- sub("^[^-]*-?", "", slug)
  if (key %in% ACCESS_SECTIONS) { # hh-..., other-...: no country or wave, the name stands in for the wave
    return(list(key = key, project = toupper(key), round = NA_integer_, series = toupper(key), country = "",
                wave = name, version = NA_character_, upcoming = FALSE))
  }
  m <- re_match(SLUG_RE, slug)
  if (is.null(m)) { # unknown pattern: still indexed, under OTHER
    return(list(key = NA_character_, project = "OTHER", round = NA_integer_, series = "OTHER", country = "",
                wave = slug, version = NA_character_, upcoming = FALSE))
  }
  s <- SERIES[[m[["key"]]]] %||% list(toupper(m[["key"]]), NA_integer_, toupper(m[["key"]]))
  list(key = m[["key"]], project = s[[1]], round = s[[2]], series = s[[3]], country = toupper(m[["cc"]]),
       wave = if (m[["wave"]] %in% NO_WAVE) "" else sub("^wave", "w", m[["wave"]]),
       version = if (nzchar(m[["ver"]])) sub("-", ".", m[["ver"]]) else NA_character_,
       upcoming = nzchar(m[["upcoming"]]))
}

#' Folder and file stem, both lowercase, e.g. "ggs-ii/w1/at" and "ggs2_w1_at_v1.1"
#' @noRd
naming <- function(p, slug, version) {
  v <- if (is.na(version)) "" else paste0("_v", version)
  if (is.na(p$key)) {
    return(list(rel_dir = "other", stem = paste0(slug, v)))
  }
  stem <- paste0(paste(Filter(nzchar, c(p$key, p$wave, tolower(p$country))), collapse = "_"), v)
  list(rel_dir = tolower(paste(Filter(nzchar, c(p$series, p$wave, p$country)), collapse = "/")), stem = stem)
}

#' Slugs in catalogue order: project, round, wave, country, slug
#' @noRd
sort_slugs <- function(slugs) {
  p <- lapply(slugs, parse_slug)
  pick <- function(f, na = "") vapply(p, \(x) as.character(x[[f]] %|NA|% na), character(1))
  round <- vapply(p, \(x) if (is.na(x$round)) 0L else x$round, integer(1))
  slugs[order(pick("project"), round, pick("wave"), pick("country"), slugs, method = "radix")]
}

`%|NA|%` <- function(x, y) if (length(x) == 0 || is.na(x)) y else x

html_text <- function(node, sep) {
  paste(xml2::xml_text(xml2::xml_find_all(node, ".//text()[not(ancestor::script or ancestor::style)]")),
        collapse = sep)
}

#' All dataset slugs linked from the catalogue page, in page order
#' @noRd
parse_catalog <- function(html) {
  hrefs <- xml2::xml_attr(xml2::xml_find_all(xml2::read_html(html), "//a[@href]"), "href")
  s <- slugs_from_hrefs(hrefs)
  unique(s[!is.na(s)])
}

field <- function(text, label) {
  m <- regexec(paste0("(?m)^[ \\t]*", label, ":\\s*([^\\n]+)"), text, perl = TRUE)
  m <- regmatches(text, m)[[1]]
  if (length(m)) trimws(m[2]) else NA_character_
}

# "Monday, October 31, 2022" and "October 31, 2022" as ISO dates; anything else is returned as it is
parse_date <- function(s) {
  m <- re_match("^(?:[A-Za-z]+,\\s*)?(?<month>[A-Za-z]+)\\s+(?<day>\\d{1,2}),\\s*(?<year>\\d{4})$", trimws(s))
  month <- if (is.null(m)) NA_integer_ else match(tolower(m[["month"]]), tolower(month.name))
  if (is.na(month)) {
    return(trimws(s))
  }
  sprintf("%s-%02d-%02d", m[["year"]], month, as.integer(m[["day"]]))
}

download_links <- function(node) {
  a <- xml2::xml_find_all(node, ".//a[contains(@href, '/form/download/')]")
  a[grepl(DOWNLOAD_HREF, xml2::xml_attr(a, "href"), perl = TRUE)]
}

download_id <- function(a) {
  as.integer(re_match(DOWNLOAD_HREF, xml2::xml_attr(a, "href"))[[1]])
}

downloads <- function(doc) {
  files <- list()
  for (a in as.list(download_links(doc))) {
    did <- download_id(a)
    label <- NA_character_
    size <- NA_real_
    # nearest ancestor that holds this one link plus a size
    for (el in rev(as.list(xml2::xml_find_all(a, "ancestor::*")))) {
      if (length(download_links(el)) > 1) {
        break
      }
      txt <- squish(html_text(el, " "))
      txt <- sub("^.*Downloads:", "", txt)
      m <- regexec(SIZE_RE, txt, perl = TRUE)
      m <- regmatches(txt, m)[[1]]
      if (length(m)) {
        label <- trimws(m[2])
        if (nchar(label) > 200) label <- NA_character_
        size <- round(as.numeric(m[3]) * UNIT_MB[[m[4]]], 2)
        break
      }
    }
    files[[as.character(did)]] <- list(download_id = did, url = abs_url(xml2::xml_attr(a, "href")),
                                       label = label, size_mb = size)
  }
  unname(files)
}

parse_dataset <- function(html, slug) {
  doc <- xml2::read_html(html)
  text <- html_text(doc, "\n")
  h1 <- xml2::xml_find_first(doc, "//h1")
  title <- if (inherits(h1, "xml_missing")) NA_character_ else squish(xml2::xml_text(h1))

  version <- re_match("^(?<v>\\d+(?:\\.\\d+)*)", field(text, "Version") %|NA|% "")[["v"]]
  if (is.null(version) && !is.na(title)) {
    version <- re_match("\\bv(?<v>\\d+(?:\\.\\d+)+)\\s*$", title)[["v"]]
  }
  version <- version %||% NA_character_

  dates <- strsplit(field(text, "Data Collection Date") %|NA|% "", "\\s+-\\s+", perl = TRUE)[[1]]
  dates <- if (length(dates)) dates else ""
  links <- xml2::xml_find_all(doc, "//a[@href]")
  links <- links[grepl("colectica\\.org/item", xml2::xml_attr(links, "href"), ignore.case = TRUE)]
  explore <- grepl("explore", xml2::xml_text(links), ignore.case = TRUE)
  colectica <- if (length(links)) xml2::xml_attr(links[[if (any(explore)) which(explore)[1] else 1]], "href") else NA_character_

  base <- sub("-v\\d+-\\d+$", "", slug) # other versions of the same dataset
  hrefs <- xml2::xml_attr(xml2::xml_find_all(doc, "//a[@href]"), "href")
  s <- slugs_from_hrefs(hrefs)
  same <- !is.na(s) & s != slug & startsWith(s, base) & grepl("^(-v\\d+-\\d+)?$", substring(s, nchar(base) + 1L))
  others <- sort(unique(s[same]), method = "radix")

  list(
    title = title,
    country_name = if (is.na(title)) NA_character_ else strsplit(title, "\\s+[\u2013-]\\s+", perl = TRUE)[[1]][1],
    version = version,
    collected_from = if (nzchar(dates[1])) parse_date(dates[1]) else NA_character_,
    collected_to = if (length(dates) > 1) parse_date(dates[2]) else NA_character_,
    colectica_url = colectica,
    files = downloads(doc),
    other_versions = others
  )
}

#' Datasets listed only on the logged-in data access page, by slug: one per heading, one file per row
#'
#' These sections are hidden behind a "Show N datasets" button, but the rows are in the HTML. Each list appears
#' twice (all, then split in columns), so rows are merged by download id.
#' @noRd
parse_access <- function(html) {
  doc <- xml2::read_html(html)
  found <- list()
  for (h2 in as.list(xml2::xml_find_all(doc, "//h2"))) {
    key <- ACCESS_SECTIONS[squish(xml2::xml_text(h2))]
    if (is.na(key)) {
      next
    }
    scope <- xml2::xml_find_first(h2, "ancestor::section[1]")
    if (inherits(scope, "xml_missing")) scope <- xml2::xml_parent(h2)
    for (h3 in as.list(xml2::xml_find_all(scope, ".//h3"))) {
      title <- squish(xml2::xml_text(h3))
      slug <- paste0(key, "-", gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", tolower(title))))
      d <- found[[slug]] %||% list(title = title, country_name = NA_character_, version = NA_character_,
                                    collected_from = NA_character_, collected_to = NA_character_,
                                    colectica_url = NA_character_, page_url = paste0(ggp_base(), "/form/"),
                                    files = list(), other_versions = character())
      for (li in as.list(xml2::xml_find_all(xml2::xml_find_first(h3, "following::ul[1]"), ".//li"))) {
        a <- download_links(li)
        if (length(a)) {
          did <- download_id(a[[1]])
          div <- xml2::xml_find_first(li, ".//div")
          d$files[[as.character(did)]] <- list(
            download_id = did, url = abs_url(xml2::xml_attr(a[[1]], "href")),
            label = if (inherits(div, "xml_missing")) NA_character_ else squish(xml2::xml_text(div)),
            size_mb = NA_real_)
        }
      }
      found[[slug]] <- d
    }
  }
  lapply(found, \(d) {
    d$files <- unname(d$files)
    d
  })
}
