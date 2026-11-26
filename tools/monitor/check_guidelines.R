#!/usr/bin/env Rscript
# Journal guideline monitor ----------------------------------------------------
#
# Fetches every author-guideline / style-guide / artwork page listed in
# inst/extdata/journal_sources.csv, reduces it to normalised text, and compares
# it with the snapshot stored in tools/monitor/snapshots/. Changed pages get a
# new snapshot and a diff in the report; the weekly GitHub Action
# (.github/workflows/journal-guidelines.yaml) turns that into a pull request so
# a person can update R/journal_styles_data.R.
#
# Many publisher sites (SAGE, ScienceDirect, Wiley Online Library, APA,
# Chicago, INFORMS PubsOnline, ft.com) answer automated clients with a bot
# challenge. The monitor never tries to get past those. For a blocked page it
# uses the latest Internet Archive (Wayback Machine) capture instead, and pages
# with neither appear in the report's manual-review list.
#
# Usage:
#   Rscript tools/monitor/check_guidelines.R [--sources PATH] [--snapdir DIR]
#                                            [--report FILE] [--only ID,ID]
# Needs: curl, xml2, pdftools (optional, for PDFs).

suppressPackageStartupMessages({
  library(curl)
  library(xml2)
})

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  i <- match(paste0("--", name), args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
sources_path <- opt("sources", "inst/extdata/journal_sources.csv")
snap_dir     <- opt("snapdir", "tools/monitor/snapshots")
report_path  <- opt("report", file.path(tempdir(), "guideline-report.md"))
only         <- opt("only", "")
unreach_path <- opt("unreachable", "")
state_path   <- file.path(dirname(snap_dir), "state.csv")
today        <- format(Sys.Date(), "%Y-%m-%d")
ua <- "Mozilla/5.0 (X11; Linux x86_64) journal-guideline-monitor/1.0"

dir.create(snap_dir, recursive = TRUE, showWarnings = FALSE)
src <- utils::read.csv(sources_path, stringsAsFactors = FALSE)
if (nzchar(only)) src <- src[src$journal_id %in% strsplit(only, ",")[[1]], ]
urls <- unique(src$url)
journals_for <- function(u) paste(unique(src$journal_id[src$url == u]), collapse = ", ")

state <- if (file.exists(state_path)) {
  utils::read.csv(state_path, stringsAsFactors = FALSE, colClasses = "character")
} else {
  data.frame(url = character(), slug = character(), hash = character(),
             last_changed = character(), stringsAsFactors = FALSE)
}

slug_for <- function(u) {
  base <- gsub("^https?://(www\\.)?", "", u)
  base <- gsub("[^A-Za-z0-9]+", "_", base)
  base <- substr(base, 1, 80)
  tf <- tempfile(); writeLines(u, tf, useBytes = TRUE)
  h <- substr(unname(tools::md5sum(tf)), 1, 8); unlink(tf)
  paste0(base, "_", h)
}

hash_text <- function(x) {
  tf <- tempfile(); writeLines(x, tf, useBytes = TRUE)
  h <- unname(tools::md5sum(tf)); unlink(tf); h
}

challenge_re <- paste(
  "Just a moment", "cf-chl", "challenge-platform", "Attention Required",
  "captcha", "Incapsula", "Request unsuccessful", "Pardon Our Interruption",
  "Enable JavaScript and cookies", "Access Denied", "Are you a robot",
  "Additional security check", sep = "|")

fetch <- function(u) {
  h <- curl::new_handle(useragent = ua, followlocation = TRUE, timeout = 60,
                        connecttimeout = 20, ssl_verifypeer = TRUE)
  curl::handle_setheaders(h, "Accept" = "text/html,application/pdf,*/*;q=0.8",
                          "Accept-Language" = "en-US,en;q=0.8")
  r <- tryCatch(curl::curl_fetch_memory(u, handle = h), error = function(e) e)
  if (inherits(r, "error")) return(list(ok = FALSE, status = NA, why = conditionMessage(r)))
  ctype <- tolower(r$type %||% "")
  is_pdf <- grepl("pdf", ctype) || grepl("\\.pdf($|\\?)", u, ignore.case = TRUE) ||
    identical(r$content[1:4], charToRaw("%PDF"))
  if (r$status_code >= 400) return(list(ok = FALSE, status = r$status_code, why = paste("HTTP", r$status_code)))
  if (!is_pdf && grepl("html", ctype)) {
    head_txt <- rawToChar(r$content[seq_len(min(length(r$content), 20000))])
    Encoding(head_txt) <- "UTF-8"
    if (grepl(challenge_re, head_txt, ignore.case = TRUE) && length(r$content) < 60000) {
      return(list(ok = FALSE, status = r$status_code, why = "bot challenge"))
    }
  }
  list(ok = TRUE, status = r$status_code, content = r$content, is_pdf = is_pdf, type = ctype)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

wayback <- function(u) {
  api <- paste0("https://archive.org/wayback/available?url=", curl::curl_escape(u))
  r <- tryCatch(curl::curl_fetch_memory(api, handle = curl::new_handle(useragent = ua, timeout = 60)),
                error = function(e) NULL)
  if (is.null(r) || r$status_code != 200) return(NULL)
  txt <- rawToChar(r$content)
  ts <- regmatches(txt, regexpr('"timestamp": *"[0-9]{14}"', txt))
  if (!length(ts)) return(NULL)
  ts <- gsub("[^0-9]", "", ts)
  snap <- paste0("https://web.archive.org/web/", ts, "id_/", u)
  res <- fetch(snap)
  if (!isTRUE(res$ok)) return(NULL)
  res$via <- paste0("wayback ", substr(ts, 1, 8))
  res
}

html_text_lines <- function(raw) {
  doc <- tryCatch(xml2::read_html(raw, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(doc)) return(character())
  xml2::xml_remove(xml2::xml_find_all(doc,
    "//script|//style|//noscript|//nav|//header|//footer|//svg|//form|//iframe|//aside|//*[@id='wm-ipp-base']"))
  root <- xml2::xml_find_first(doc, "//main")
  if (inherits(root, "xml_missing")) root <- xml2::xml_find_first(doc, "//article")
  if (inherits(root, "xml_missing")) root <- xml2::xml_find_first(doc, "//body")
  nodes <- xml2::xml_find_all(root,
    ".//p|.//li|.//h1|.//h2|.//h3|.//h4|.//h5|.//h6|.//td|.//th|.//dt|.//dd|.//pre|.//blockquote")
  txt <- xml2::xml_text(nodes)
  txt
}

pdf_text_lines <- function(raw) {
  if (!requireNamespace("pdftools", quietly = TRUE)) return(paste("pdf-md5", hash_text(as.character(raw))))
  pages <- tryCatch(pdftools::pdf_text(raw), error = function(e) NULL)
  if (is.null(pages)) return(paste("pdf-md5", hash_text(as.character(raw))))
  unlist(strsplit(pages, "\n"))
}

normalise <- function(lines) {
  x <- gsub("[[:space:] ]+", " ", lines)
  x <- trimws(x)
  x <- x[nzchar(x)]
  # drop lines that change on every visit
  volatile <- "^(©|Copyright|Cookie|We use cookies|Skip to|Accessed|Page last|Last modified)|session|csrf"
  x <- x[!grepl(volatile, x, ignore.case = TRUE)]
  x[c(TRUE, x[-1] != x[-length(x)])]   # collapse consecutive duplicates
}

diff_lines <- function(old, new, max_n = 25) {
  removed <- setdiff(old, new); added <- setdiff(new, old)
  clip <- function(v) {
    v <- substr(v, 1, 300)
    if (length(v) > max_n) c(v[seq_len(max_n)], sprintf("... (%d more)", length(v) - max_n)) else v
  }
  c(if (length(removed)) paste0("- ", clip(removed)),
    if (length(added)) paste0("+ ", clip(added)))
}

results <- vector("list", length(urls))
for (k in seq_along(urls)) {
  u <- urls[[k]]
  slug <- slug_for(u)
  res <- fetch(u)
  via <- "direct"
  if (!isTRUE(res$ok)) {
    why <- res$why
    res <- wayback(u)
    if (is.null(res)) {
      results[[k]] <- data.frame(url = u, journals = journals_for(u), status = "unreachable",
                                 via = why, stringsAsFactors = FALSE)
      cat(sprintf("[%3d/%d] UNREACHABLE (%s) %s\n", k, length(urls), why, u))
      next
    }
    via <- paste(res$via, "-", why)
  }
  lines <- normalise(if (isTRUE(res$is_pdf)) pdf_text_lines(res$content) else html_text_lines(res$content))
  if (length(lines) < 3) {
    results[[k]] <- data.frame(url = u, journals = journals_for(u), status = "unreachable",
                               via = paste(via, "(no text)"), stringsAsFactors = FALSE)
    cat(sprintf("[%3d/%d] NO TEXT %s\n", k, length(urls), u))
    next
  }
  h <- hash_text(lines)
  snap_file <- file.path(snap_dir, paste0(slug, ".txt"))
  prev <- state[state$url == u, , drop = FALSE]
  status <- if (!nrow(prev) || !file.exists(snap_file)) "new" else if (prev$hash[1] == h) "unchanged" else "changed"
  diff <- character()
  if (status == "changed") diff <- diff_lines(readLines(snap_file, encoding = "UTF-8", warn = FALSE), lines)
  if (status %in% c("new", "changed")) {
    writeLines(lines, snap_file, useBytes = TRUE)
    state <- state[state$url != u, , drop = FALSE]
    state <- rbind(state, data.frame(url = u, slug = slug, hash = h, last_changed = today,
                                     stringsAsFactors = FALSE))
  }
  results[[k]] <- data.frame(url = u, journals = journals_for(u), status = status, via = via,
                             stringsAsFactors = FALSE)
  attr(results[[k]], "diff") <- diff
  cat(sprintf("[%3d/%d] %-9s (%s) %s\n", k, length(urls), toupper(status), via, u))
}

state <- state[order(state$url), , drop = FALSE]
utils::write.csv(state, state_path, row.names = FALSE)

# Report -----------------------------------------------------------------------
res_df <- do.call(rbind, results)
n <- table(factor(res_df$status, levels = c("changed", "new", "unchanged", "unreachable")))
rep <- c(
  sprintf("# Journal guideline monitor: %s", today), "",
  sprintf("Checked %d pages: **%d changed**, %d new, %d unchanged, %d unreachable.",
          nrow(res_df), n[["changed"]], n[["new"]], n[["unchanged"]], n[["unreachable"]]), "",
  "If a change affects table or figure rules, update the journal's entry in",
  "`R/journal_styles_data.R`, bump `.jstyle_verified`, and re-run the tests.", ""
)
changed <- which(res_df$status == "changed")
if (length(changed)) {
  rep <- c(rep, "## Changed pages", "")
  for (k in changed) {
    rep <- c(rep, sprintf("### %s", res_df$journals[k]), sprintf("<%s> (via %s)", res_df$url[k], res_df$via[k]),
             "", "```diff", attr(results[[k]], "diff"), "```", "")
  }
}
newp <- which(res_df$status == "new")
if (length(newp)) {
  rep <- c(rep, "## New snapshots", "", sprintf("- %s: <%s> (via %s)", res_df$journals[newp], res_df$url[newp], res_df$via[newp]), "")
}
unr <- which(res_df$status == "unreachable")
if (length(unr)) {
  rep <- c(rep, "## Manual review needed (not reachable directly or via the Wayback Machine)", "",
           sprintf("- [ ] %s: <%s> (%s)", res_df$journals[unr], res_df$url[unr], res_df$via[unr]), "")
}
wb <- which(grepl("^wayback", res_df$via) & res_df$status != "unreachable")
if (length(wb)) {
  rep <- c(rep, "## Checked through the Wayback Machine (site blocks automated clients)", "",
           sprintf("- %s: <%s> (%s)", res_df$journals[wb], res_df$url[wb], res_df$via[wb]), "")
}
txt <- paste(rep, collapse = "\n")
if (nchar(txt) > 60000) txt <- paste0(substr(txt, 1, 60000), "\n\n... report truncated; see the workflow artifact.")
dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)
writeLines(txt, report_path, useBytes = TRUE)
cat(sprintf("\nReport written to %s\n", report_path))

# Standing list of pages nobody can check automatically (kept in one issue)
if (nzchar(unreach_path)) {
  body <- c(
    sprintf("Last run: %s. These guideline pages could not be read directly or through the Wayback Machine,", today),
    "so changes to them will not be detected automatically. Check them by hand before relying on the",
    "matching journal styles, then tick them off.", "",
    if (length(unr)) sprintf("- [ ] %s: <%s> (%s)", res_df$journals[unr], res_df$url[unr], res_df$via[unr])
    else "All pages were reachable on the last run.")
  writeLines(body, unreach_path, useBytes = TRUE)
}

# For GitHub Actions: expose whether anything changed
gh_out <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(gh_out)) {
  cat(sprintf("changed=%d\nnew=%d\nunreachable=%d\n", n[["changed"]], n[["new"]], n[["unreachable"]]),
      file = gh_out, append = TRUE)
}
