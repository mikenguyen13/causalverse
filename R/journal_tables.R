#' Build a Regression or Summary Table in a Journal's Style
#'
#' Formats model estimates (or a data frame of statistics) following a
#' journal's reporting rules: whether significance stars are allowed and at
#' which thresholds, what goes in parentheses (standard errors, t-statistics
#' or confidence intervals), whether exact p-values are reported, leading
#' zeros, decimals, table numbering and caption style, and the order of the
#' table notes. Render the result with [export_tab()] or print it.
#'
#' @param x A model, a named list of models (supported by [tidy_causal()]),
#'   a named list of tidy data frames with columns `term`, `estimate`,
#'   `std_error` (or `std.error`) and optionally `p_value`, `ci_lower`,
#'   `ci_upper`, `t_stat`; or a plain data frame of statistics (for example
#'   descriptive statistics), which is formatted cell by cell.
#' @param journal Journal id, abbreviation, name, or a `journal_style`.
#' @param title Table title.
#' @param number Table number. Default `1`.
#' @param coef_map Named character vector mapping coefficient names to
#'   labels; also sets the row order and drops unlisted terms.
#' @param gof Model statistics to add below the estimates: any of `"nobs"`,
#'   `"r.squared"`, `"adj.r.squared"`.
#' @param notes General notes (character vector).
#' @param specific_notes Specific notes, attached to labels with superscript
#'   letters (named character vector: names are the text to mark, values the
#'   notes).
#' @param source Source line.
#' @param digits Decimal places. Default from the journal.
#' @param bounded_cols For a plain data frame: columns whose values cannot
#'   exceed 1 in absolute value (p-values, correlations, proportions). These
#'   lose their leading zero under APA-style rules. Default: detected from the
#'   data.
#' @param significance,uncertainty,stars Override the journal's rules.
#' @param conf_level Confidence level for intervals. Default `0.95`.
#'
#' @return An object of class `journal_table`.
#' @seealso [export_tab()], [journal_style()], [journal_check()]
#' @examples
#' m1 <- lm(mpg ~ am, data = mtcars)
#' m2 <- lm(mpg ~ am + wt, data = mtcars)
#' journal_table(list("(1)" = m1, "(2)" = m2), journal = "aer",
#'               title = "Transmission and Fuel Economy")
#' journal_table(list("(1)" = m1, "(2)" = m2), journal = "jm",
#'               title = "Manual Cars Get More Miles per Gallon")
#' @export
journal_table <- function(x, journal = "default", title = NULL, number = 1,
                          coef_map = NULL, gof = c("nobs", "r.squared"),
                          notes = NULL, specific_notes = NULL, source = NULL,
                          digits = NULL, bounded_cols = NULL,
                          significance = NULL, uncertainty = NULL, stars = NULL,
                          conf_level = 0.95) {
  st <- journal_style(journal)
  t <- st$table
  if (!is.null(significance)) t$significance <- match.arg(significance, c("stars", "none", "exact_p"))
  if (!is.null(uncertainty)) t$uncertainty <- match.arg(uncertainty, c("se", "t", "ci"))
  if (!is.null(stars)) t$stars <- stars
  if (!is.null(digits)) t$digits <- as.integer(digits)
  st$table <- t

  is_model_input <- !is.data.frame(x) || all(c("term", "estimate") %in% names(x))
  if (is_model_input) {
    tab <- .jt_models(x, t, coef_map, gof, conf_level)
  } else {
    tab <- .jt_dataframe(x, t, bounded_cols)
  }

  # notes, in the journal's order
  if (!is.null(specific_notes)) {
    letters_used <- letters[seq_along(specific_notes)]
    for (i in seq_along(specific_notes)) {
      hit <- tab$body == names(specific_notes)[i]
      tab$sup[hit] <- paste0(tab$sup[hit], letters_used[i])
    }
    specific <- paste0(letters_used, " ", unname(specific_notes))
  } else {
    specific <- character()
  }
  parts <- list(
    general      = if (length(c(notes, tab$auto_note))) paste(st$table$note_label, paste(c(notes, tab$auto_note), collapse = " ")) else character(),
    specific     = specific,
    significance = if (tab$has_stars) .star_note(t) else character(),
    source       = if (!is.null(source)) paste("Source:", source) else character()
  )
  ordered_notes <- unlist(parts[t$notes_order], use.names = FALSE)

  structure(list(
    header  = tab$header,
    body    = tab$body,
    sup     = tab$sup,
    rowtype = tab$rowtype,
    caption = .format_caption("table", number, title, st),
    notes   = ordered_notes,
    number  = number,
    style   = st,
    has_stars = tab$has_stars
  ), class = "journal_table")
}

#' Export a Table to a Journal's Specifications
#'
#' Writes a [journal_table()] (or models/data passed straight through) to
#' LaTeX (booktabs + threeparttable), Word (.docx, via \pkg{flextable} and
#' \pkg{officer}), HTML and Excel, using the journal's caption, note and
#' number formatting. For journals that want tables at the end of the
#' manuscript, the in-text callout (e.g. "INSERT TABLE 1 HERE") is reported.
#'
#' @param x A `journal_table`, or anything [journal_table()] accepts.
#' @param filename File name without extension.
#' @param filepath Output directory. Created if missing.
#' @param journal Journal id, abbreviation, name, or `journal_style`. Ignored
#'   when `x` is already a `journal_table`.
#' @param formats Any of `"tex"`, `"docx"`, `"html"`, `"xlsx"`. Default: the
#'   journal's preferred formats.
#' @param latex_caption `"latex"` (default) uses `\\caption{}` so the
#'   journal's LaTeX class numbers and styles the caption; `"journal"` writes
#'   the formatted label (e.g. "TABLE I") with `\\caption*{}` (needs the
#'   caption package).
#' @param font Font family for Word output. Default `"Times New Roman"`.
#' @param font_size Font size in points for Word output. Default `10`.
#' @param archive Also save dated copies in `filepath/archive`. Default `TRUE`.
#' @param check Run [journal_check()] and print problems. Default `TRUE`.
#' @param ... Passed to [journal_table()] when `x` is not a `journal_table`.
#'
#' @return Invisibly, the written file paths.
#' @seealso [journal_table()], [export_fig()]
#' @examples
#' \dontrun{
#' m1 <- lm(mpg ~ am, data = mtcars)
#' m2 <- lm(mpg ~ am + wt, data = mtcars)
#' export_tab(list("(1)" = m1, "(2)" = m2), "tab_mpg", tempdir(),
#'            journal = "smj", title = "Transmission and Fuel Economy")
#' }
#' @export
export_tab <- function(x, filename, filepath = ".", journal = "default",
                       formats = NULL, latex_caption = c("latex", "journal"),
                       font = "Times New Roman", font_size = 10,
                       archive = TRUE, check = TRUE, ...) {
  latex_caption <- match.arg(latex_caption)
  if (!inherits(x, "journal_table")) x <- journal_table(x, journal = journal, ...)
  st <- x$style
  if (is.null(formats)) formats <- st$table$formats
  formats <- tolower(formats)

  if (!dir.exists(filepath)) dir.create(filepath, recursive = TRUE)
  arch_dir <- file.path(filepath, "archive")
  if (archive && !dir.exists(arch_dir)) dir.create(arch_dir, recursive = TRUE)
  stamp <- format(Sys.Date(), "%Y-%m-%d")

  paths <- character()
  for (fmt in formats) {
    out <- file.path(filepath, paste0(filename, ".", fmt))
    ok <- switch(fmt,
      tex  = { writeLines(.jt_latex(x, label = filename, caption_mode = latex_caption), out, useBytes = TRUE); TRUE },
      html = { writeLines(.jt_html(x), out, useBytes = TRUE); TRUE },
      docx = .jt_docx(x, out, font, font_size),
      xlsx = { rio::export(.jt_sheet(x), out, col_names = FALSE); TRUE },
      stop("Unsupported table format: ", fmt, call. = FALSE)
    )
    if (isTRUE(ok)) {
      paths <- c(paths, out)
      if (archive) file.copy(out, file.path(arch_dir, paste0(filename, "_", stamp, ".", fmt)), overwrite = TRUE)
    }
  }

  if (check) {
    chk <- journal_check(x, st)
    if (nrow(chk)) print(chk)
  }
  if (identical(st$table$placement, "end")) {
    marker <- st$table$insert_marker
    msg <- if (!is.null(marker)) {
      sprintf("%s wants tables after the references. Put \"%s\" in the text where Table %s belongs.",
              st$abbrev, sub("{n}", .journal_number(x$number, st$table$numbering), marker, fixed = TRUE),
              .journal_number(x$number, st$table$numbering))
    } else {
      sprintf("%s wants tables after the references, each on its own page.", st$abbrev)
    }
    message(msg)
  }
  invisible(paths)
}

#' @export
print.journal_table <- function(x, ...) {
  cat(x$caption$text, "\n", sep = "")
  m <- rbind(x$header, matrix(paste0(x$body, ifelse(nzchar(x$sup), x$sup, "")), nrow = nrow(x$body)))
  m[is.na(m)] <- ""
  w <- apply(nchar(m, type = "width"), 2, max)
  line <- strrep("-", sum(w) + 2 * (ncol(m) - 1))
  fmt_row <- function(r) paste(mapply(function(s, wi, j) if (j == 1) formatC(s, width = -wi) else formatC(s, width = wi),
                                      r, w, seq_along(r)), collapse = "  ")
  cat(line, "\n", fmt_row(m[1, ]), "\n", line, "\n", sep = "")
  for (i in seq_len(nrow(m))[-1]) {
    if (x$rowtype[i - 1] == "gof" && (i == 2 || x$rowtype[i - 2] != "gof")) cat(line, "\n", sep = "")
    cat(fmt_row(m[i, ]), "\n", sep = "")
  }
  cat(line, "\n", sep = "")
  for (n in x$notes) cat(n, "\n", sep = "")
  invisible(x)
}

# Number formatting -------------------------------------------------------------

# Format numbers with a fixed number of decimals and the journal's leading-zero
# rule. `bounded` marks statistics that cannot exceed 1 (p, R2, r).
.jfmt <- function(x, digits, leading_zero = "always", bounded = FALSE) {
  out <- ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))
  drop <- leading_zero == "never" || (leading_zero == "bounded" && bounded)
  if (drop) out <- sub("^(-?)0\\.", "\\1.", out)
  out
}

.jfmt_p <- function(p, digits, leading_zero) {
  bound <- 10^(-digits)
  ifelse(is.na(p), "",
         ifelse(p < bound,
                paste0("p < ", .jfmt(bound, digits, leading_zero, bounded = TRUE)),
                paste0("p = ", .jfmt(p, digits, leading_zero, bounded = TRUE))))
}

.jstars <- function(p, t) {
  if (t$significance != "stars") return(rep("", length(p)))
  thr <- sort(t$stars)                       # smallest threshold first
  vapply(p, function(pi) {
    if (is.na(pi)) return("")
    hit <- which(pi < thr)
    if (length(hit)) names(thr)[hit[1]] else ""
  }, "")
}

.star_note <- function(t) {
  thr <- sort(t$stars)
  syms <- names(thr)
  pct <- function(v) paste0(formatC(v * 100, format = "fg"), "%")
  pval <- function(v) .jfmt(v, max(2, nchar(sub("^0\\.", "", format(v)))), t$leading_zero, bounded = TRUE)
  txt <- switch(t$star_note,
    sentence = sprintf("%s indicate significance at the %s levels, respectively.",
                       .and_list(rev(syms)), .and_list(pct(rev(thr)))),
    inline   = paste0(paste0(rev(syms), "p < ", vapply(rev(thr), pval, ""), collapse = "; "), "."),
    stacked  = paste0(rev(syms), " p < ", vapply(rev(thr), pval, ""))
  )
  if (!is.null(t$tails)) txt[length(txt)] <- sub("\\.?$", paste0(" (", t$tails, ")."), txt[length(txt)])
  txt
}

.and_list <- function(x) {
  if (length(x) <= 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}

# Table builders ----------------------------------------------------------------

.jt_tidy_one <- function(m, conf_level) {
  if (is.data.frame(m)) {
    df <- m
    nm <- names(df)
    nm[nm == "std.error"] <- "std_error"; nm[nm == "p.value"] <- "p_value"
    nm[nm == "conf.low"] <- "ci_lower"; nm[nm == "conf.high"] <- "ci_upper"
    nm[nm == "statistic"] <- "t_stat"
    names(df) <- nm
  } else {
    df <- tidy_causal(m, conf_level = conf_level, add_model_info = FALSE)
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  if (is.null(df$t_stat) && !is.null(df$std_error)) df$t_stat <- df$estimate / df$std_error
  if (is.null(df$p_value) && !is.null(df$t_stat)) df$p_value <- 2 * stats::pnorm(-abs(df$t_stat))
  if (is.null(df$ci_lower) && !is.null(df$std_error)) {
    df$ci_lower <- df$estimate - z * df$std_error
    df$ci_upper <- df$estimate + z * df$std_error
  }
  df
}

.jt_gof <- function(m, stat) {
  if (is.data.frame(m)) return(NA_real_)
  out <- tryCatch(switch(stat,
    nobs = stats::nobs(m),
    r.squared = {
      if (inherits(m, "fixest")) unname(fixest::r2(m, "r2")) else summary(m)$r.squared
    },
    adj.r.squared = {
      if (inherits(m, "fixest")) unname(fixest::r2(m, "ar2")) else summary(m)$adj.r.squared
    }
  ), error = function(e) NA_real_)
  if (is.null(out) || length(out) != 1) NA_real_ else as.numeric(out)
}

.jt_models <- function(x, t, coef_map, gof, conf_level) {
  # a single model or a single tidy data frame -> one-column table
  if (is.data.frame(x) || !identical(class(x), "list")) {
    x <- list("(1)" = x)
  }
  if (is.null(names(x)) || any(!nzchar(names(x)))) names(x) <- paste0("(", seq_along(x), ")")
  tidies <- lapply(x, .jt_tidy_one, conf_level = conf_level)

  terms <- unique(unlist(lapply(tidies, `[[`, "term")))
  if (!is.null(coef_map)) terms <- intersect(names(coef_map), terms)
  labels <- if (!is.null(coef_map)) unname(coef_map[terms]) else terms

  k <- length(x)
  per_term <- if (t$significance == "exact_p") 3L else 2L
  body <- matrix("", nrow = length(terms) * per_term, ncol = k + 1)
  sup <- matrix("", nrow = nrow(body), ncol = ncol(body))
  rowtype <- rep(c("estimate", "uncertainty", "p")[seq_len(per_term)], length(terms))
  has_stars <- FALSE
  d <- t$digits
  lz <- t$leading_zero

  for (i in seq_along(terms)) {
    r <- (i - 1) * per_term + 1
    body[r, 1] <- labels[i]
    for (j in seq_len(k)) {
      df <- tidies[[j]]
      row <- df[df$term == terms[i], , drop = FALSE]
      if (!nrow(row)) next
      row <- row[1, ]
      body[r, j + 1] <- .jfmt(row$estimate, d, lz)
      s <- .jstars(row$p_value, t)
      if (nzchar(s)) { sup[r, j + 1] <- s; has_stars <- TRUE }
      body[r + 1, j + 1] <- switch(t$uncertainty,
        se = paste0("(", .jfmt(row$std_error, d, lz), ")"),
        t  = paste0("(", .jfmt(row$t_stat, 2, lz), ")"),
        ci = paste0("[", .jfmt(row$ci_lower, d, lz), ", ", .jfmt(row$ci_upper, d, lz), "]")
      )
      if (per_term == 3L) body[r + 2, j + 1] <- .jfmt_p(row$p_value, t$p_digits, lz)
    }
  }

  gof_labels <- c(nobs = "Observations", r.squared = "R-squared", adj.r.squared = "Adjusted R-squared")
  gof <- intersect(gof, names(gof_labels))
  for (g in gof) {
    vals <- vapply(x, .jt_gof, 0, stat = g)
    if (all(is.na(vals))) next
    txt <- if (g == "nobs") ifelse(is.na(vals), "", formatC(vals, format = "d", big.mark = ","))
           else .jfmt(vals, d, lz, bounded = TRUE)
    body <- rbind(body, c(gof_labels[[g]], txt))
    sup <- rbind(sup, rep("", k + 1))
    rowtype <- c(rowtype, "gof")
  }

  auto <- switch(t$uncertainty,
    se = "Standard errors in parentheses.",
    t  = "t-statistics in parentheses.",
    ci = sprintf("%d%% confidence intervals in brackets.", round(conf_level * 100))
  )
  if (t$significance == "exact_p") auto <- paste(auto, "Exact two-sided p-values below.")
  list(header = c("", names(x)), body = body, sup = sup, rowtype = rowtype,
       has_stars = has_stars, auto_note = auto)
}

.jt_dataframe <- function(x, t, bounded_cols) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (is.null(bounded_cols)) {
    bounded_cols <- names(x)[vapply(x, function(v) is.numeric(v) && all(abs(v) <= 1, na.rm = TRUE), TRUE)]
  }
  body <- vapply(names(x), function(nm) {
    v <- x[[nm]]
    if (is.numeric(v)) {
      if (all(v == round(v), na.rm = TRUE)) return(formatC(v, format = "d", big.mark = ","))
      return(.jfmt(v, t$digits, t$leading_zero, bounded = nm %in% bounded_cols))
    }
    as.character(v)
  }, character(nrow(x)))
  body <- matrix(body, nrow = nrow(x))
  body[is.na(body) | body == "NA"] <- ""
  list(header = names(x), body = body, sup = matrix("", nrow(body), ncol(body)),
       rowtype = rep("data", nrow(body)), has_stars = FALSE, auto_note = NULL)
}

# Renderers --------------------------------------------------------------------

.tex_escape <- function(s) {
  s <- gsub("\\", "\001", s, fixed = TRUE)
  s <- gsub("([&%$#_{}])", "\\\\\\1", s)
  s <- gsub("\001", "\\textbackslash{}", s, fixed = TRUE)
  s <- gsub("~", "\\\\textasciitilde{}", s, fixed = TRUE)
  s <- gsub("^", "\\\\textasciicircum{}", s, fixed = TRUE)
  s
}

.tex_sup <- function(s) {
  ifelse(!nzchar(s), "",
         paste0("\\textsuperscript{", gsub("\u2020", "\\dag", .tex_escape(s), fixed = TRUE), "}"))
}

.jt_latex <- function(x, label, caption_mode = "latex") {
  st <- x$style
  cap <- x$caption
  ncol_t <- length(x$header)
  align <- paste0("l", strrep("c", ncol_t - 1))
  cell <- function(i, j) paste0(.tex_escape(x$body[i, j]), .tex_sup(x$sup[i, j]))
  rows <- character()
  for (i in seq_len(nrow(x$body))) {
    if (x$rowtype[i] == "gof" && (i == 1 || x$rowtype[i - 1] != "gof")) rows <- c(rows, "\\midrule")
    rows <- c(rows, paste0(paste(vapply(seq_len(ncol_t), function(j) cell(i, j), ""), collapse = " & "), " \\\\"))
  }
  cap_tex <- if (caption_mode == "journal") {
    lab <- if (cap$label_bold) paste0("\\textbf{", .tex_escape(cap$label), "}") else .tex_escape(cap$label)
    ttl <- if (cap$title_italic) paste0("\\textit{", .tex_escape(cap$title), "}") else .tex_escape(cap$title)
    sep <- if (grepl("\n", cap$sep, fixed = TRUE)) "\\\\" else .tex_escape(cap$sep)
    paste0("\\caption*{", lab, sep, ttl, "}")
  } else {
    paste0("\\caption{", .tex_escape(cap$title), "}")
  }
  notes <- if (length(x$notes)) {
    c("\\begin{tablenotes}[flushleft]", "\\footnotesize",
      paste0("\\item ", .tex_escape(x$notes)), "\\end{tablenotes}")
  }
  c(sprintf("%% Generated by causalverse::export_tab() for %s (%s style, verified %s).",
            st$name, st$family, st$verified),
    "% Requires \\usepackage{booktabs,threeparttable}",
    sprintf("%% Journal caption format: %s", gsub("\n", " / ", cap$text, fixed = TRUE)),
    if (identical(st$table$numbering, "roman")) "% Roman numbering: \\renewcommand{\\thetable}{\\Roman{table}}",
    "\\begin{table}[htbp]",
    "\\centering",
    "\\begin{threeparttable}",
    if (cap$position == "above") c(cap_tex, sprintf("\\label{tab:%s}", label)),
    sprintf("\\begin{tabular}{%s}", align),
    "\\toprule",
    paste0(paste(.tex_escape(x$header), collapse = " & "), " \\\\"),
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    notes,
    "\\end{threeparttable}",
    "\\end{table}")
}

.html_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}

.jt_html <- function(x) {
  cap <- x$caption
  lab <- if (cap$label_bold) paste0("<b>", .html_escape(cap$label), "</b>") else .html_escape(cap$label)
  ttl <- if (cap$title_italic) paste0("<i>", .html_escape(cap$title), "</i>") else .html_escape(cap$title)
  sep <- if (grepl("\n", cap$sep, fixed = TRUE)) "<br>" else .html_escape(cap$sep)
  td <- function(i, j) {
    v <- paste0(.html_escape(x$body[i, j]), if (nzchar(x$sup[i, j])) paste0("<sup>", .html_escape(x$sup[i, j]), "</sup>") else "")
    sprintf("<td style=\"text-align:%s;padding:2px 8px;%s\">%s</td>",
            if (j == 1) "left" else "center",
            if (x$rowtype[i] == "gof" && (i == 1 || x$rowtype[i - 1] != "gof")) "border-top:1px solid black;" else "",
            v)
  }
  body <- vapply(seq_len(nrow(x$body)), function(i)
    paste0("<tr>", paste(vapply(seq_along(x$header), function(j) td(i, j), ""), collapse = ""), "</tr>"), "")
  c("<table style=\"border-collapse:collapse;border-top:2px solid black;border-bottom:2px solid black;font-family:'Times New Roman',serif\">",
    sprintf("<caption style=\"caption-side:top;text-align:%s\">%s%s%s</caption>",
            cap$align, lab, sep, ttl),
    paste0("<tr>", paste(sprintf("<th style=\"border-bottom:1px solid black;padding:2px 8px\">%s</th>",
                                 .html_escape(x$header)), collapse = ""), "</tr>"),
    body,
    "</table>",
    if (length(x$notes)) paste0("<p style=\"font-size:smaller\">", .html_escape(x$notes), "</p>"))
}

.jt_sheet <- function(x) {
  m <- matrix(paste0(x$body, x$sup), nrow = nrow(x$body))
  k <- ncol(m)
  pad <- function(v) c(v, rep("", k - length(v)))
  rows <- rbind(pad(gsub("\n", " ", x$caption$text, fixed = TRUE)), x$header, m,
                do.call(rbind, lapply(x$notes, pad)))
  as.data.frame(rows, stringsAsFactors = FALSE)
}

.jt_docx <- function(x, out, font, font_size) {
  if (!requireNamespace("flextable", quietly = TRUE) || !requireNamespace("officer", quietly = TRUE)) {
    warning("Word output needs the 'flextable' and 'officer' packages; skipping .docx.", call. = FALSE)
    return(FALSE)
  }
  cap <- x$caption
  df <- as.data.frame(x$body, stringsAsFactors = FALSE)
  keys <- paste0("c", seq_len(ncol(df)))
  names(df) <- keys
  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(ft, values = stats::setNames(as.list(x$header), keys))
  for (i in seq_len(nrow(x$sup))) for (j in seq_len(ncol(x$sup))) {
    if (nzchar(x$sup[i, j])) {
      ft <- flextable::compose(ft, i = i, j = j,
        value = flextable::as_paragraph(x$body[i, j], flextable::as_sup(x$sup[i, j])))
    }
  }
  ft <- flextable::border_remove(ft)
  thick <- officer::fp_border(color = "black", width = 1.5)
  thin <- officer::fp_border(color = "black", width = 0.75)
  ft <- flextable::hline_top(ft, border = thick, part = "header")
  ft <- flextable::hline_bottom(ft, border = thin, part = "header")
  ft <- flextable::hline_bottom(ft, border = thick, part = "body")
  first_gof <- which(x$rowtype == "gof")[1]
  if (!is.na(first_gof) && first_gof > 1) ft <- flextable::hline(ft, i = first_gof - 1, border = thin, part = "body")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  if (ncol(df) > 1) ft <- flextable::align(ft, j = 2:ncol(df), align = "center", part = "all")
  ft <- flextable::font(ft, fontname = font, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::autofit(ft)

  tp <- officer::fp_text(font.family = font, font.size = font_size + 1)
  lab_p <- officer::ftext(cap$label, stats::update(tp, bold = cap$label_bold))
  ttl_p <- officer::ftext(cap$title, stats::update(tp, italic = cap$title_italic))
  pa <- officer::fp_par(text.align = cap$align)
  doc <- officer::read_docx()
  if (grepl("\n", cap$sep, fixed = TRUE)) {
    doc <- officer::body_add_fpar(doc, officer::fpar(lab_p, fp_p = pa))
    doc <- officer::body_add_fpar(doc, officer::fpar(ttl_p, fp_p = pa))
  } else {
    doc <- officer::body_add_fpar(doc, officer::fpar(lab_p, officer::ftext(cap$sep, tp), ttl_p, fp_p = pa))
  }
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  np <- officer::fp_text(font.family = font, font.size = font_size - 1)
  for (n in x$notes) doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(n, np)))
  print(doc, target = out)
  TRUE
}
