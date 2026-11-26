#' Check a Figure or Table Against a Journal's Formatting Rules
#'
#' Lints a ggplot figure or a [journal_table()] against a journal's rules and
#' lists what would need fixing before submission.
#'
#' For figures it checks: a title inside the image when the journal wants it
#' in the caption; font sizes below the journal's minimum at the printed
#' width; colours that become indistinguishable in grayscale for journals that
#' print in gray; and more data series than the publisher allows. For tables
#' it checks: significance stars where the journal bans them, leading-zero
#' and column-count rules, and whether the table was built for a different
#' journal.
#'
#' @param x A ggplot object or a `journal_table`.
#' @param journal Journal id, abbreviation, name, or `journal_style`. For a
#'   `journal_table` the default is the journal it was built for.
#' @param width Printed width of a figure in millimetres (for the font-size
#'   check). Default: the journal's single-column width.
#'
#' @return A data frame of class `journal_check` with columns `severity`
#'   (`"error"` = violates a stated rule, `"warning"` = likely problem),
#'   `rule` and `message`. Zero rows means no problems found.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point() +
#'   labs(title = "Fuel economy")
#' journal_check(p, "smj")
#' @export
journal_check <- function(x, journal = NULL, width = NULL) {
  if (inherits(x, "journal_table")) {
    st <- if (is.null(journal)) x$style else journal_style(journal)
    issues <- .check_table(x, st)
  } else if (inherits(x, "ggplot")) {
    st <- journal_style(if (is.null(journal)) "default" else journal)
    issues <- .check_figure(x, st, width)
  } else {
    stop("`x` must be a ggplot object or a journal_table.", call. = FALSE)
  }
  out <- if (length(issues)) do.call(rbind, issues) else
    data.frame(severity = character(), rule = character(), message = character())
  attr(out, "journal") <- st$abbrev
  class(out) <- c("journal_check", "data.frame")
  out
}

#' @export
print.journal_check <- function(x, ...) {
  j <- attr(x, "journal")
  if (!nrow(x)) {
    cat(sprintf("<journal_check> %s: no problems found.\n", j))
    return(invisible(x))
  }
  cat(sprintf("<journal_check> %s: %d problem(s)\n", j, nrow(x)))
  for (i in seq_len(nrow(x))) {
    cat(sprintf("  [%s] %s: %s\n", x$severity[i], x$rule[i], x$message[i]))
  }
  invisible(x)
}

.issue <- function(severity, rule, message) {
  data.frame(severity = severity, rule = rule, message = message, stringsAsFactors = FALSE)
}

.check_table <- function(x, st) {
  t <- st$table
  out <- list()
  if (!identical(x$style$id, st$id)) {
    out[[length(out) + 1]] <- .issue("warning", "built_for_other_journal",
      sprintf("Table was built for %s; rebuild it with journal_table(..., journal = \"%s\").",
              x$style$abbrev, st$id))
  }
  if (isTRUE(x$has_stars) && t$significance != "stars") {
    out[[length(out) + 1]] <- .issue("error", "significance_stars",
      sprintf("%s does not allow significance stars; %s.", st$abbrev,
              if (t$significance == "exact_p") "report exact p-values" else "report standard errors without stars"))
  }
  ncols <- length(x$header)
  if (is.finite(t$max_columns) && ncols > t$max_columns) {
    out[[length(out) + 1]] <- .issue("error", "max_columns",
      sprintf("%d columns; %s allows at most %d. Split the table or move models to an appendix.",
              ncols, st$abbrev, as.integer(t$max_columns)))
  }
  num <- x$body[, -1, drop = FALSE]
  if (t$leading_zero == "never" && any(grepl("(^|[^0-9])-?0\\.[0-9]", num))) {
    out[[length(out) + 1]] <- .issue("error", "leading_zero",
      sprintf("%s writes decimals without a leading zero (.25, not 0.25).", st$abbrev))
  }
  if (t$leading_zero == "always" && any(grepl("(^|[-( =<\\[])\\.[0-9]", num, perl = TRUE))) {
    out[[length(out) + 1]] <- .issue("error", "leading_zero",
      sprintf("%s requires a leading zero (0.25, not .25).", st$abbrev))
  }
  out
}

.check_figure <- function(p, st, width) {
  f <- st$figure
  out <- list()
  if (is.null(width)) width <- f$widths_mm[["single"]]

  if (!isTRUE(f$title_in_image) && !is.null(p$labels$title)) {
    th_title <- tryCatch(ggplot2::calc_element("plot.title", .complete_theme(p)), error = function(e) NULL)
    if (!inherits(th_title, "element_blank")) {
      out[[length(out) + 1]] <- .issue("warning", "title_in_image",
        sprintf("%s wants the figure title in the caption, not the image. export_fig() moves it for you.", st$abbrev))
    }
  }

  th <- .complete_theme(p)
  sizes <- vapply(c("axis.text.x", "axis.text.y", "axis.title.x", "axis.title.y",
                    "legend.text", "legend.title", "strip.text"), function(el) {
    e <- tryCatch(ggplot2::calc_element(el, th), error = function(e) NULL)
    if (is.null(e) || inherits(e, "element_blank") || is.null(e$size)) NA_real_ else as.numeric(e$size)
  }, 0)
  if (any(sizes < f$min_font_pt, na.rm = TRUE)) {
    bad <- names(sizes)[which(sizes < f$min_font_pt)]
    out[[length(out) + 1]] <- .issue("error", "min_font_size",
      sprintf("Text smaller than %s's %g pt minimum at %g mm wide: %s (%s pt). Use theme_journal(\"%s\").",
              st$abbrev, f$min_font_pt, width, paste(bad, collapse = ", "),
              paste(round(sizes[bad], 1), collapse = ", "), st$id))
  }
  if (any(sizes > f$max_font_pt + 2, na.rm = TRUE)) {
    out[[length(out) + 1]] <- .issue("warning", "max_font_size",
      sprintf("Some text is larger than %g pt; it will look oversized at %g mm wide.", f$max_font_pt, width))
  }

  cols <- .plot_colours(p)
  if (isTRUE(f$grayscale) && length(cols) > 1) {
    lum <- sort(.luminance(cols))
    gap <- min(diff(lum))
    need <- f$gray_step * 0.75
    if (gap < need) {
      out[[length(out) + 1]] <- .issue("error", "grayscale",
        sprintf("%s prints in grayscale; %d colours differ by only %.0f%% in lightness (need about %.0f%%). Use scale_colour_journal(\"%s\") or add shapes/line types.",
                st$abbrev, length(cols), gap * 100, f$gray_step * 100, st$id))
    }
  }
  if (is.finite(f$max_series) && length(cols) > f$max_series) {
    out[[length(out) + 1]] <- .issue("warning", "max_series",
      sprintf("%d data series; %s's publisher recommends at most %d per chart.",
              length(cols), st$abbrev, as.integer(f$max_series)))
  }
  out
}

.complete_theme <- function(p) {
  th <- ggplot2::theme_get()
  if (length(p$theme)) th <- th + p$theme
  th
}

# Distinct colours/fills mapped to data in the built plot.
.plot_colours <- function(p) {
  b <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
  if (is.null(b)) return(character())
  mapped <- c(names(p$mapping), unlist(lapply(p$layers, function(l) names(l$mapping))))
  aes_used <- intersect(c("colour", "fill"), sub("^color$", "colour", mapped))
  if (!length(aes_used)) return(character())
  vals <- unlist(lapply(b$data, function(d) unlist(d[intersect(aes_used, names(d))])))
  vals <- unique(vals[!is.na(vals)])
  vals[vals != "transparent"]
}

.luminance <- function(cols) {
  rgb <- grDevices::col2rgb(cols) / 255
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  y <- 0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  # perceptual lightness (CIE L*) scaled to 0-1
  ifelse(y > 0.008856, 1.16 * y^(1 / 3) - 0.16, 9.033 * y)
}
