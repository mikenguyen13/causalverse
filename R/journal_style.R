#' Journal Formatting Styles for Tables and Figures
#'
#' Look up the table and figure formatting rules of a top business journal
#' (FT50 and UTD24). The rules come from each journal's and publisher's own
#' author guidelines, which causalverse checks weekly for changes (see
#' [journal_sources()]).
#'
#' A style has three layers that are merged in order: generic defaults, a
#' reporting family shared by related journals (for example `"apa7"`, `"aom"`,
#' `"informs"`, `"ama"`, `"aea"`), plus the publisher's artwork rules (for
#' example `"elsevier"`, `"wiley"`, `"sage"`), and finally journal-specific
#' overrides.
#'
#' @param journal Journal id (e.g. `"smj"`), abbreviation (e.g. `"SMJ"`) or
#'   full name (e.g. `"Strategic Management Journal"`), case-insensitive. Use
#'   `"default"` for generic, widely accepted settings.
#' @param ... Overrides merged into the style last, as nested lists, e.g.
#'   `table = list(digits = 2)`.
#'
#' @return An object of class `journal_style`: a list with elements `id`,
#'   `name`, `abbrev`, `publisher`, `lists`, `family`, `artwork`,
#'   `confidence`, `verified`, `caveats`, `table` and `figure`.
#'
#' @section Table rules (`$table`):
#' \describe{
#'   \item{significance}{`"stars"`, `"none"` (journal bans stars) or
#'     `"exact_p"` (report exact p-values).}
#'   \item{stars}{Named thresholds, e.g. `c("***" = .01, "**" = .05, "*" = .10)`.}
#'   \item{uncertainty}{What goes in parentheses: `"se"`, `"t"` or `"ci"`.}
#'   \item{leading_zero}{`"always"` (0.25), `"bounded"` (drop it only for
#'     statistics that cannot exceed 1, such as p-values and R-squared, as in
#'     APA style) or `"never"` (.25).}
#'   \item{numbering}{`"arabic"` (Table 1) or `"roman"` (Table I).}
#'   \item{placement}{`"embedded"`, `"end"` (with `insert_marker` callouts) or
#'     `"either"`.}
#'   \item{max_columns}{Maximum number of columns (AER 9, JF 8).}
#' }
#' @section Figure rules (`$figure`):
#' \describe{
#'   \item{dpi}{Minimum resolution for line art, halftones and combinations.}
#'   \item{widths_mm}{Single, 1.5- and double-column widths in millimetres.}
#'   \item{title_in_image}{Whether the figure title may sit inside the image.}
#'   \item{grayscale}{Whether figures must stay readable in grayscale.}
#'   \item{alt_text}{Whether alt text is required.}
#' }
#'
#' @seealso [journal_list()], [theme_journal()], [export_fig()],
#'   [export_tab()], [journal_check()], [journal_sources()]
#'
#' @examples
#' journal_style("smj")
#' journal_style("Journal of Finance")$table$numbering
#' journal_style("aer")$table$significance
#' @export
journal_style <- function(journal = "default", ...) {
  if (inherits(journal, "journal_style")) {
    style <- journal
  } else {
    entry <- .journal_entry(journal)
    style <- .resolve_journal_style(entry)
  }
  extra <- list(...)
  if (length(extra)) style <- utils::modifyList(style, extra)
  .warn_if_stale()
  style
}

.jstyle_env <- new.env(parent = emptyenv())

# Warn once per session when the registry has not been re-verified for six
# months: the weekly monitor catches page changes, but a person still has to
# review them and update R/journal_styles_data.R.
.warn_if_stale <- function(max_age_days = 183) {
  if (isTRUE(.jstyle_env$warned) || isFALSE(getOption("causalverse.journal_stale_warning", TRUE))) {
    return(invisible())
  }
  age <- as.numeric(Sys.Date() - as.Date(.jstyle_verified))
  if (age > max_age_days) {
    .jstyle_env$warned <- TRUE
    warning(sprintf(paste0(
      "causalverse's journal formatting rules were last verified on %s (%d days ago). ",
      "Update causalverse, or check the journal's current guidelines with journal_sources(). ",
      "Silence with options(causalverse.journal_stale_warning = FALSE)."),
      .jstyle_verified, round(age)), call. = FALSE)
  }
  invisible()
}

#' List the Journals with Known Formatting Styles
#'
#' @param list Which journal list to return: `"all"`, `"ft50"` (the current
#'   FT50, revised April 2026), `"ft50_2016"`, `"ft50_2026"` or `"utd24"`.
#'
#' @return A data frame with one row per journal: `id`, `name`, `abbrev`,
#'   `publisher`, `family`, `artwork`, membership flags `ft50_2016`,
#'   `ft50_2026`, `utd24`, `confidence` and `verified`.
#'
#' @examples
#' head(journal_list("utd24"))
#' table(journal_list()$confidence)
#' @export
journal_list <- function(list = c("all", "ft50", "ft50_2016", "ft50_2026", "utd24")) {
  list <- match.arg(list)
  js <- .jstyle_journals()
  out <- data.frame(
    id         = vapply(js, `[[`, "", "id"),
    name       = vapply(js, `[[`, "", "name"),
    abbrev     = vapply(js, `[[`, "", "abbrev"),
    publisher  = vapply(js, `[[`, "", "publisher"),
    family     = vapply(js, `[[`, "", "family"),
    artwork    = vapply(js, `[[`, "", "artwork"),
    ft50_2016  = vapply(js, function(x) "ft50_2016" %in% x$lists, TRUE),
    ft50_2026  = vapply(js, function(x) "ft50_2026" %in% x$lists, TRUE),
    utd24      = vapply(js, function(x) "utd24" %in% x$lists, TRUE),
    confidence = vapply(js, `[[`, "", "confidence"),
    verified   = .jstyle_verified,
    stringsAsFactors = FALSE
  )
  keep <- switch(list,
    all       = rep(TRUE, nrow(out)),
    ft50      = out$ft50_2026,
    ft50_2016 = out$ft50_2016,
    ft50_2026 = out$ft50_2026,
    utd24     = out$utd24
  )
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Official Guideline Sources Behind Each Journal Style
#'
#' Returns the URLs of the author-guideline, style-guide and artwork pages
#' that each journal style was built from. The same file drives the weekly
#' guideline monitor (`tools/monitor/check_guidelines.R` in the source
#' repository), which opens a pull request when any page changes.
#'
#' @param journal Optional journal id, abbreviation or name. `NULL` returns
#'   all sources.
#'
#' @return A data frame with columns `journal_id`, `scope` and `url`.
#'
#' @examples
#' journal_sources("jm")
#' @export
journal_sources <- function(journal = NULL) {
  path <- system.file("extdata", "journal_sources.csv", package = "causalverse")
  if (!nzchar(path)) stop("journal_sources.csv not found in the installed package.")
  src <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (is.null(journal)) return(src)
  id <- .journal_entry(journal)$id
  fam <- .journal_entry(journal)
  src[src$journal_id %in% c(id, paste0("publisher:", fam$artwork),
                            paste0("family:", fam$family)), , drop = FALSE]
}

#' @export
print.journal_style <- function(x, ...) {
  t <- x$table; f <- x$figure
  fmt_stars <- if (t$significance == "stars") {
    paste(paste0(names(t$stars), " p < ", t$stars), collapse = ", ")
  } else if (t$significance == "none") {
    "none (journal does not allow significance stars)"
  } else {
    "none; report exact p-values"
  }
  cat(sprintf("<journal_style> %s (%s) -- %s\n", x$name, x$abbrev, x$publisher))
  cat(sprintf("  Lists: %s | family: %s | artwork: %s | confidence: %s (verified %s)\n",
              if (length(x$lists)) paste(x$lists, collapse = ", ") else "-",
              x$family, x$artwork, x$confidence, x$verified))
  cat("  Tables\n")
  cat(sprintf("    significance: %s\n", fmt_stars))
  cat(sprintf("    parentheses: %s | leading zero: %s | digits: %d\n",
              c(se = "standard errors", t = "t-statistics", ci = "confidence intervals")[[t$uncertainty]],
              t$leading_zero, as.integer(t$digits)))
  cat(sprintf("    caption: %s | placement: %s%s\n",
              .format_caption("table", 1, "Title", x)$text_oneline, t$placement,
              if (!is.null(t$insert_marker)) paste0(" (\"", sub("{n}", "1", t$insert_marker, fixed = TRUE), "\")") else ""))
  if (is.finite(t$max_columns)) cat(sprintf("    max columns: %d\n", as.integer(t$max_columns)))
  cat("  Figures\n")
  cat(sprintf("    formats (final): %s%s\n", paste(f$formats_final, collapse = ", "),
              if (isTRUE(f$vector_required)) " -- vector required" else ""))
  cat(sprintf("    min dpi: line art %s, halftone %s, combination %s\n",
              f$dpi[["line_art"]], f$dpi[["halftone"]], f$dpi[["combination"]]))
  cat(sprintf("    widths (mm): %s%s\n",
              paste(names(f$widths_mm), f$widths_mm, sep = " ", collapse = ", "),
              if (isTRUE(f$widths_generic)) " (generic; journal does not specify)" else ""))
  cat(sprintf("    font: %s, %s-%s pt | title in image: %s | grayscale: %s | alt text: %s\n",
              f$font_family, f$min_font_pt, f$max_font_pt,
              if (isTRUE(f$title_in_image)) "allowed" else "no (put it in the caption)",
              if (isTRUE(f$grayscale)) "required" else "not required",
              if (isTRUE(f$alt_text)) "required" else "not required"))
  if (length(x$caveats)) {
    cat("  Caveats\n")
    for (cv in x$caveats) cat("    - ", cv, "\n", sep = "")
  }
  invisible(x)
}

# Internal helpers --------------------------------------------------------------

.journal_entry <- function(journal) {
  if (!is.character(journal) || length(journal) != 1L || is.na(journal)) {
    stop("`journal` must be a single journal id, abbreviation or name.", call. = FALSE)
  }
  key <- tolower(trimws(journal))
  if (key %in% c("default", "generic", "")) {
    return(list(id = "default", name = "Generic journal", abbrev = "default",
                publisher = "-", lists = character(), family = "default",
                artwork = "default", confidence = "high", overrides = list(),
                caveats = character()))
  }
  js <- .jstyle_journals()
  norm <- function(s) gsub("[^a-z0-9]", "", tolower(s))
  for (e in js) {
    if (key == e$id || norm(key) == norm(e$name) || norm(key) == norm(e$abbrev)) return(e)
  }
  # partial name match as a fallback
  hits <- Filter(function(e) grepl(norm(key), norm(e$name), fixed = TRUE), js)
  if (length(hits) == 1L) return(hits[[1]])
  ids <- vapply(js, `[[`, "", "id")
  stop(sprintf("Unknown journal '%s'. Known ids: %s. See journal_list().",
               journal, paste(ids, collapse = ", ")), call. = FALSE)
}

.resolve_journal_style <- function(entry) {
  fams <- .jstyle_table_families()
  arts <- .jstyle_artwork_families()
  if (!entry$family %in% names(fams)) stop("Unknown family: ", entry$family)
  if (!entry$artwork %in% names(arts)) stop("Unknown artwork family: ", entry$artwork)
  s <- .jstyle_default()
  s <- utils::modifyList(s, fams[[entry$family]])
  s <- utils::modifyList(s, arts[[entry$artwork]])
  s <- utils::modifyList(s, entry$overrides)
  structure(
    c(list(id = entry$id, name = entry$name, abbrev = entry$abbrev,
           publisher = entry$publisher, lists = entry$lists,
           family = entry$family, artwork = entry$artwork,
           confidence = entry$confidence, verified = .jstyle_verified,
           caveats = entry$caveats),
      s),
    class = c("journal_style", "list")
  )
}

.journal_number <- function(n, numbering) {
  if (identical(numbering, "roman")) as.character(utils::as.roman(n)) else as.character(n)
}

# Build the caption label/title for a table or figure according to a style.
# Returns plain-text pieces plus a one-line preview.
.format_caption <- function(type = c("table", "figure"), n, title, style) {
  type <- match.arg(type)
  if (type == "table") {
    cap <- style$table$caption
    label <- style$table$label
    numbering <- style$table$numbering
  } else {
    cap <- utils::modifyList(style$table$caption, list(position = style$figure$caption_position))
    label <- style$figure$label
    numbering <- style$figure$numbering
  }
  lab <- paste(label, .journal_number(n, numbering))
  if (isTRUE(cap$label_upper)) lab <- toupper(lab)
  ttl <- if (is.null(title)) "" else title
  if (isTRUE(cap$title_upper)) ttl <- toupper(ttl)
  if (isTRUE(cap$end_period) && nzchar(ttl) && !grepl("[.?!]$", ttl)) ttl <- paste0(ttl, ".")
  sep <- cap$sep
  list(
    label = lab, title = ttl, sep = sep,
    label_bold = isTRUE(cap$label_bold), title_italic = isTRUE(cap$title_italic),
    align = cap$align, position = cap$position,
    text = if (nzchar(ttl)) paste0(lab, sep, ttl) else lab,
    text_oneline = if (nzchar(ttl)) paste0(lab, gsub("\n", " / ", sep, fixed = TRUE), ttl) else lab
  )
}
