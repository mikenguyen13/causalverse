# Post-process the built pkgdown site in docs/. Run after every pkgdown build:
#
#   Rscript pkgdown/postbuild.R
#
# The site is built locally and committed (see .github/workflows/pkgdown.yaml), so
# this step runs before the commit. It is idempotent: every insertion sits between
# marker comments and is replaced, not duplicated, on the next run. It adds what
# pkgdown 2.1 templates cannot:
#
# - <link rel="canonical"> on every page, with the page's URL under `url:` in
#   _pkgdown.yml (directory URLs for index pages), and the same directory URLs in
#   sitemap.xml. Pages marked noindex (old vignette names) get no canonical link.
# - The block between `<!-- cv-exp:head -->` and `<!-- /cv-exp:head -->` in
#   _pkgdown.yml's in_header (Google Scholar citation_* tags and the box styles),
#   copied into pages that were not rebuilt since it changed.
# - At the end of each vignette listed under `articles:` in _pkgdown.yml: "Related
#   in other books" (from pkgdown/related.yml), "Cite this section" (APA and BibTeX
#   with the vignette's title and URL), and "Use this package with your AI
#   assistant" (the Research Methods Library MCP endpoint).
#
# Base R plus the yaml package (a pkgdown dependency).

docs <- "docs"
cfg <- yaml::read_yaml("_pkgdown.yml")
site <- sub("/?$", "/", cfg$url)
related <- yaml::read_yaml(file.path("pkgdown", "related.yml"))
vignettes <- unlist(lapply(cfg$articles, `[[`, "contents"))

endpoint <- "https://research-methods-mcp.vercel.app/mcp?src=book-causalverse"
cursor_url <- paste0(
  "https://cursor.com/en/install-mcp?name=research-methods&amp;config=",
  "eyJ1cmwiOiJodHRwczovL3Jlc2VhcmNoLW1ldGhvZHMtbWNwLnZlcmNlbC5hcHAvbWNwP3NyYz1ib29rLWNhdXNhbHZlcnNlIn0%3D"
)
vscode_url <- paste0(
  "https://insiders.vscode.dev/redirect/mcp/install?name=research-methods&amp;config=",
  "%7B%22type%22%3A%22http%22%2C%22url%22%3A%22https%3A%2F%2Fresearch-methods-mcp.vercel.app",
  "%2Fmcp%3Fsrc%3Dbook-causalverse%22%7D"
)

# Citation, as in research-methods-mcp/sources.yml (id: causalverse) and inst/CITATION.
doi <- "10.5281/zenodo.8254063"
apa <- paste0(
  "Nguyen, M. (2026). <em>causalverse: The all-in-one causal inference toolkit</em> ",
  "(Version 1.1.0) [Computer software]. Zenodo. ",
  "<a href=\"https://doi.org/", doi, "\">https://doi.org/", doi, "</a>"
)

read_file <- function(path) {
  x <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(x) <- "UTF-8"
  x
}
write_file <- function(x, path) writeBin(charToRaw(enc2utf8(x)), path)

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
html_unescape <- function(x) {
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# Replace the region between two marker comments, or insert `block` before `anchor`.
upsert <- function(html, start, end, block, anchor) {
  pattern <- paste0("(?s)", start, ".*?", end)
  if (grepl(pattern, html, perl = TRUE)) {
    return(sub(pattern, gsub("\\", "\\\\", block, fixed = TRUE), html, perl = TRUE))
  }
  pos <- regexpr(anchor, html, fixed = TRUE)
  if (pos < 0) stop("anchor ", anchor, " not found")
  paste0(substr(html, 1, pos - 1), block, "\n", substr(html, pos, nchar(html)))
}

# The text just after the end of a vignette's content: `</main>` in Bootstrap 5 pages,
# the `</div>` closing `div.contents` in pages still built with the Bootstrap 3 template
# (most vignettes were last built before the site moved to `bootstrap: 5`).
content_end <- function(html) {
  if (grepl("</main>", html, fixed = TRUE)) return("</main>")
  m <- regexpr("(?s)</div>\\s*<div class=\"col-md-3 hidden-xs hidden-sm\" id=\"pkgdown-sidebar\">",
               html, perl = TRUE)
  if (m < 0) stop("cannot find the end of the vignette content")
  regmatches(html, m)
}

head_marks <- c("<!-- cv-exp:head -->", "<!-- /cv-exp:head -->")
in_header <- cfg$template$includes$in_header
head_block <- regmatches(
  in_header,
  regexpr(paste0("(?s)", head_marks[1], ".*?", head_marks[2]), in_header, perl = TRUE)
)
if (length(head_block) != 1) stop("no cv-exp:head block in _pkgdown.yml in_header")

# Reference pages left on gh-pages under a function's old name (the deploy uses
# clean: false). Their canonical link points to the current page, and they are
# dropped from sitemap.xml.
stale <- c(
  "reference/amatheme.html" = "reference/ama_theme.html",
  "reference/plot_PanelEstimate.html" = "reference/plot_panel_estimate.html"
)

# Meta descriptions for pages whose description pkgdown cannot set from a config key
# (vignettes take theirs from the `description:` field of their YAML header).
descriptions <- c(
  "authors.html" = "Authors and citation information for causalverse, the all-in-one causal inference toolkit for R."
)

page_url <- function(rel) {
  if (rel %in% names(stale)) rel <- stale[[rel]]
  rel <- sub("(^|/)index\\.html$", "\\1", rel)
  paste0(site, rel)
}

end_block <- function(name, title, url) {
  title_text <- trimws(sub("^[0-9]+\\.\\s*", "", html_unescape(title)))
  links <- related[[name]]
  rel_html <- if (length(links)) {
    items <- vapply(links, function(l) {
      sprintf("<li><a href=\"%s\">%s</a>, <em>%s</em></li>", l$url, html_escape(l$title), html_escape(l$book))
    }, "")
    paste0(
      "<div class=\"cv-box cv-related\">\n<h2 data-toc-skip>Related in other books</h2>\n<ul>\n",
      paste(items, collapse = "\n"), "\n</ul>\n</div>\n"
    )
  } else ""
  bibtex <- paste0(
    "@manual{Nguyen2026causalverse,\n",
    "  author    = {Nguyen, Mike},\n",
    "  title     = {{causalverse: The All-in-One Causal Inference Toolkit}},\n",
    "  year      = {2026},\n",
    "  note      = {Version 1.1.0. Section: ", gsub("&", "\\&", title_text, fixed = TRUE), ", ", url, "},\n",
    "  publisher = {Zenodo},\n",
    "  doi       = {", doi, "},\n",
    "  url       = {https://doi.org/", doi, "}\n",
    "}"
  )
  paste0(
    "<!-- cv-exp:end -->\n<section class=\"cv-end\">\n",
    rel_html,
    "<div class=\"cv-box cv-cite\">\n<h2 data-toc-skip>Cite this section</h2>\n",
    "<p>", apa, "</p>\n",
    "<p>Section: &ldquo;", html_escape(title_text), "&rdquo;, <a href=\"", url, "\">", url, "</a></p>\n",
    "<details><summary>BibTeX</summary>\n<div class=\"sourceCode\"><pre><code>",
    html_escape(bibtex), "</code></pre></div>\n</details>\n</div>\n",
    "<div class=\"cv-box cv-ai\">\n<h2 data-toc-skip>Use this package with your AI assistant</h2>\n",
    "<p>Ask questions about this package in Claude, Cursor, VS Code, or ChatGPT, with answers that cite the section.</p>\n",
    "<div class=\"sourceCode\"><pre><code>", endpoint, "</code></pre></div>\n",
    "<p class=\"cv-install\"><a href=\"", cursor_url, "\"><img src=\"https://cursor.com/deeplink/mcp-install-dark.svg\" alt=\"Add to Cursor\"></a>",
    "<a href=\"", vscode_url, "\"><img src=\"https://img.shields.io/badge/VS_Code-Install_server-0098FF?logo=visualstudiocode&amp;logoColor=white\" alt=\"Install in VS Code\"></a></p>\n",
    "<p><a href=\"https://research-methods-mcp.vercel.app/\">Setup for Claude, ChatGPT, and other clients</a></p>\n",
    "</div>\n</section>\n<!-- /cv-exp:end -->"
  )
}

files <- list.files(docs, pattern = "\\.html$", recursive = TRUE)
files <- files[!grepl("_files/", files)]
counts <- c(head = 0, canonical = 0, end = 0)

for (rel in files) {
  path <- file.path(docs, rel)
  html <- read_file(path)
  if (!grepl("</head>", html, fixed = TRUE)) next
  orig <- html

  html <- upsert(html, head_marks[1], head_marks[2], head_block, "</head>")
  counts["head"] <- counts["head"] + 1

  if (rel %in% names(descriptions) && !grepl("<meta name=\"description\"", html, fixed = TRUE)) {
    d <- html_escape(descriptions[[rel]])
    tag <- sprintf("<meta name=\"description\" content=\"%s\">\n<meta property=\"og:description\" content=\"%s\">\n", d, d)
    pos <- regexpr("</title>", html, fixed = TRUE)
    html <- paste0(substr(html, 1, pos + 7), "\n", tag, substr(html, pos + 8, nchar(html)))
  }

  html <- gsub("<link rel=\"canonical\"[^>]*>\r?\n?", "", html, perl = TRUE)
  noindex <- grepl("<meta name=\"robots\" content=\"noindex", html, fixed = TRUE)
  if (!noindex && rel != "404.html") {
    tag <- sprintf("<link rel=\"canonical\" href=\"%s\">\n", page_url(rel))
    pos <- regexpr("</head>", html, fixed = TRUE)
    html <- paste0(substr(html, 1, pos - 1), tag, substr(html, pos, nchar(html)))
    counts["canonical"] <- counts["canonical"] + 1
  }

  name <- sub("^articles/(.*)\\.html$", "\\1", rel)
  if (grepl("^articles/", rel) && name %in% vignettes) {
    h1 <- regmatches(html, regexpr("(?s)<h1[^>]*>.*?</h1>", html, perl = TRUE))
    title <- gsub("(?s)</?h1[^>]*>", "", h1, perl = TRUE)
    html <- upsert(html, "<!-- cv-exp:end -->", "<!-- /cv-exp:end -->",
                   end_block(name, title, page_url(rel)), content_end(html))
    counts["end"] <- counts["end"] + 1
  }

  if (!identical(html, orig)) write_file(html, path)
}

sitemap <- file.path(docs, "sitemap.xml")
if (file.exists(sitemap)) {
  sm <- read_file(sitemap)
  sm2 <- gsub("(<loc>[^<]*/)index\\.html</loc>", "\\1</loc>", sm, perl = TRUE)
  for (rel in c("404.html", names(stale))) {
    sm2 <- gsub(paste0("[ \t]*<url><loc>", site, rel, "</loc></url>\r?\n"), "", sm2, fixed = FALSE)
  }
  if (!identical(sm, sm2)) write_file(sm2, sitemap)
}

missing <- setdiff(vignettes, sub("\\.html$", "", basename(files[grepl("^articles/", files)])))
if (length(missing)) warning("vignettes not built: ", paste(missing, collapse = ", "))
message(sprintf("postbuild: head block %d, canonical %d, vignette end blocks %d (of %d pages)",
                counts["head"], counts["canonical"], counts["end"], length(files)))
