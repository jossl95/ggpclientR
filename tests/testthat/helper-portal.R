# Synthetic portal pages, modelled on the text of the live catalogue and dataset pages (not raw markup).
CATALOG <- r"(<h2>GGS-II</h2>
<a href="https://www.ggp-i.org/dataset/ggs2-at-wave1/">Wave 1</a>
<a href="/dataset/ggs2-at-wave1/">View</a>
<a href="/dataset/ggs2-nl-wave1/">Explore Dataset</a>
<a href="/dataset/upcoming-ggs2-fr-wave1/">Wave 1</a>
<h2>FFS</h2><a href="/dataset/ffs-us-thefamilyandfertilitysurvey/">x</a>
<a href="/dataset/page/4/">older</a><a href="/dataset/">archive</a>
<a href="https://evil.example/dataset/ggs2-xx-wave1/">offsite</a>)"

AT <- r"(<h1>Austria – Wave 1 v1.1</h1>
<a href="/form/">Login</a>
<a href="https://ggp.colectica.org/item/int.ggp/c7fc">Explore Dataset on Colectica</a>
<p>Data Collection Date: Monday, October 31, 2022 - Sunday, March 12, 2023</p>
<p>Version: 1.1</p>
<div>Downloads: <span>Austria - Wave 1 (version 1.1) v1.1, 15.3 MB</span>
 <a href="https://ggp.colectica.org/Item/int.ggp/dd79">Browse Dataset</a>
 <a href="https://www.ggp-i.org/form/download/95/">Login to Download</a></div>
<p>Other versions: <a href="/dataset/ggs2-at-wave1-v1-0/">View version 1.0</a>
<a href="/dataset/ggs2-at-wave1-v1-1/">View version 1.1</a></p>)"

AT10 <- r"(<h1>Austria – Wave 1 v1.0</h1><p>Version: 1.0</p>
<div><span>Austria - Wave 1 (version 1.0) v1.0, 14.9 MB</span>
 <a href="/form/download/41/">Login to Download</a></div>
<a href="/dataset/ggs2-at-wave1/">View version 1.1</a>)"

NL <- r"(<h1>Netherlands – Wave 1 v2.0</h1><p>Version: 2.0</p>
<div><span>Netherlands - Wave 1, 1.2 GB</span><a href="/form/download/96/">Login to Download</a></div>)"

FR <- "<h1>France – Wave 1</h1><p>Coming soon</p>"

FFS <- r"(<h1>USA – The Family and Fertility Survey</h1><p>Version: 1.0</p>
<div><span>USA FFS, 800 KB</span><a href="/form/download/7/">Login to Download</a></div>)"

row <- function(label, did) {
  sprintf('<li><div>%s</div><div><a href="/form/download/%d/" target="_blank">Download</a></div></li>', label, did)
}

# the logged-in data access page: hidden sections, each list twice (all, then in columns), as on the live site
HH <- sprintf("<h3>Harmonized Histories </h3><ul>%s%s</ul>", row("Part 1", 1), row("Part 2", 84))
EXTRA <- sprintf("<h3>NKPS</h3><ul>%s</ul>", row("NKPS", 61))
FORM <- sprintf(r"(<h1>Data Access</h1><form action="/form/admin/logout/?next=/form/"><button>Logout</button></form>
<section><h2>Generations and Gender Survey - Round 2 (GGS-II)</h2><h3>Austria</h3><ul>%s</ul></section>
<section x-data="{ open: false }"><h2>Harmonized Histories</h2><button>Show 2 datasets</button>
 <div x-show="open"><div>%s</div><div>%s</div></div></section>
<section x-data="{ open: false }"><h2>Additional Datasets</h2><button>Show 1 datasets</button>
 <div x-show="open"><div>%s</div><div><div>%s</div></div></div></section>)",
  row("Wave 1", 95), HH, HH, EXTRA, EXTRA)

# ---- a fake portal, served by webfakes on localhost -------------------------------------------------------------

AT_ZIP <- make_zip(list("Data/AT_w1.dta" = "dta", "Data/AT_w1.sav" = "sav", "Docs/Codebook.pdf" = "pdf"))
FFS_ZIP <- make_zip(list("USA2.DAT" = FFS_DAT, "USA2.SPS" = FFS_SPS,
                         "USA.TBL" = "TABLE 4 BY IND01$01\r\nincome US$ 100\r\n", "Docs/readme.txt" = "r"))
TOKEN_PATH <- "/form/accounts/login/email/s3cr3t/"

portal_routes <- function(at_zip = AT_ZIP) {
  zip_headers <- list("content-type" = "application/zip")
  list(
    "/data-catalog/" = list(body = CATALOG),
    "/dataset/ggs2-at-wave1/" = list(body = AT),
    "/dataset/ggs2-at-wave1-v1-1/" = list(body = AT), # the live site also gives the latest version its own page
    "/dataset/ggs2-at-wave1-v1-0/" = list(body = AT10),
    "/dataset/ggs2-nl-wave1/" = list(body = NL),
    "/dataset/upcoming-ggs2-fr-wave1/" = list(body = FR),
    "/dataset/ffs-us-thefamilyandfertilitysurvey/" = list(body = FFS),
    "/form/download/1/" = list(body = make_zip(list("x.dta" = "d")), headers = zip_headers),
    "/form/download/84/" = list(body = make_zip(list("x.dta" = "d")), headers = zip_headers),
    "/form/download/61/" = list(body = make_zip(list("x.dta" = "d")), headers = zip_headers),
    "/form/download/95/" = list(body = at_zip, headers = list("content-type" = "application/zip",
                                                              "content-disposition" = 'attachment; filename="AT_w1.zip"')),
    "/form/download/7/" = list(body = FFS_ZIP, headers = zip_headers),
    "/form/download/96/" = list(body = "<html>sign the agreement</html>", headers = list("content-type" = "text/html"))
  )
}

# runs in the server process: base R only
portal_handler <- function(req, res) {
  host <- paste0("http://", req$get_header("host"))
  path <- req$path
  if (path == "/form/accounts/login/email/s3cr3t/") {
    res$set_header("Set-Cookie", "sessionid=s1; Path=/")
    return(res$send("ok"))
  }
  if (path == "/form/") {
    ok <- grepl("sessionid=s1", if (is.null(req$get_header("cookie"))) "" else req$get_header("cookie"), fixed = TRUE)
    return(res$send(if (ok) gsub("https://www.ggp-i.org", host, req$app$locals$form, fixed = TRUE)
                    else "we'll send you a secure login link"))
  }
  r <- req$app$locals$routes[[path]]
  if (is.null(r)) {
    return(res$set_status(404L)$send("not found"))
  }
  for (h in names(r$headers)) res$set_header(h, r$headers[[h]])
  res$send(if (is.character(r$body)) gsub("https://www.ggp-i.org", host, r$body, fixed = TRUE) else r$body)
}
environment(portal_handler) <- baseenv()

start_portal <- function(routes, env) {
  app <- webfakes::new_app()
  app$locals$routes <- routes
  app$locals$form <- FORM
  app$use(portal_handler)
  webfakes::local_app_process(app, .local_envir = env)
}

shared_portal <- local({
  server <- NULL
  function() {
    if (is.null(server)) server <<- start_portal(portal_routes(), testthat::teardown_env())
    server
  }
})

# the tests talk to the fake portal (the shared one, or a private one whose AT download is at_zip); no proxy for localhost
local_portal <- function(env = parent.frame(), at_zip = NULL) {
  skip_if_not_installed("webfakes")
  server <- if (is.null(at_zip)) shared_portal() else start_portal(portal_routes(at_zip), env)
  withr::local_options(ggpclient.base = sub("/$", "", server$url()), .local_envir = env)
  withr::local_envvar(NO_PROXY = "127.0.0.1,localhost", no_proxy = "127.0.0.1,localhost",
                      .local_envir = env)
}

token <- function() paste0(ggp_base(), TOKEN_PATH)
