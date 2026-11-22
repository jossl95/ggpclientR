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
