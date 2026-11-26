# Journal style registry ------------------------------------------------------
#
# Formatting rules for tables and figures at top business journals (FT50,
# UTD24). Rules are organised in three layers that are merged in order:
#
#   1. `.jstyle_default()`        generic defaults for any journal
#   2. a table family             e.g. "apa7", "aom", "informs", "ama"
#      and an artwork family      e.g. "elsevier", "wiley", "sage"
#   3. journal-specific overrides
#
# Every rule was read from the journal's or publisher's own guideline pages on
# the `verified` date. The URLs are in inst/extdata/journal_sources.csv and are
# re-checked weekly by tools/monitor/check_guidelines.R (GitHub Action
# .github/workflows/journal-guidelines.yaml). When the monitor reports a change,
# update the entry here and bump `verified`.
#
# `confidence`: "high"   = journal's own current page/PDF read in full;
#               "medium" = older copy, search excerpt, or partly publisher-wide;
#               "low"    = journal page unreadable, publisher-wide rules only.

.jstyle_verified <- "2026-11-25"

.jstyle_default <- function() {
  list(
    table = list(
      significance   = "stars",            # "stars", "none", "exact_p"
      stars          = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
      star_note      = "sentence",         # "sentence", "inline", "stacked"
      tails          = NULL,               # e.g. "two-tailed tests"
      uncertainty    = "se",               # "se", "t", "ci"
      leading_zero   = "always",           # "always", "bounded", "never"
      digits         = 3L,
      p_digits       = 3L,
      numbering      = "arabic",           # "arabic", "roman"
      label          = "Table",
      caption        = list(label_upper = FALSE, label_bold = TRUE, sep = ". ",
                            title_italic = FALSE, title_upper = FALSE,
                            align = "left", end_period = FALSE,
                            position = "above"),
      notes_order    = c("general", "specific", "significance", "source"),
      note_label     = "Note:",
      specific_marks = "letters",
      panel_label    = "Panel A",
      placement      = "either",           # "embedded", "end", "either"
      insert_marker  = NULL,
      max_columns    = Inf,
      vertical_rules = FALSE,
      formats        = c("tex", "docx", "xlsx")
    ),
    figure = list(
      formats_initial  = c("pdf", "png"),
      formats_final    = c("pdf", "eps", "tiff"),
      vector_preferred = TRUE,
      vector_required  = FALSE,
      dpi              = c(line_art = 600, halftone = 300, combination = 600),
      widths_mm        = c(single = 89, onehalf = 136, double = 183),
      widths_generic   = TRUE,
      font_family      = "sans",
      min_font_pt      = 7,
      max_font_pt      = 12,
      title_in_image   = FALSE,
      label            = "Figure",
      numbering        = "arabic",
      caption_position = "below",
      panel_label      = "(a)",
      grayscale        = FALSE,
      gray_range       = c(0.2, 0.8),
      gray_step        = 0.2,
      max_series       = Inf,
      alt_text         = FALSE,
      alt_text_label   = "Alt text:",
      source_data      = FALSE,
      color_print      = "not specified"
    )
  )
}

# Table / reporting families ---------------------------------------------------
.jstyle_table_families <- function() {
  list(
    default = list(),

    apa7 = list(
      table = list(
        significance = "exact_p", leading_zero = "bounded",
        stars = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
        caption = list(sep = "\n", title_italic = TRUE),
        note_label = "Note.",
        notes_order = c("general", "specific", "significance", "source")
      ),
      figure = list(min_font_pt = 8, max_font_pt = 14, caption_position = "above")
    ),

    aom = list(
      table = list(
        stars = c("**" = 0.01, "*" = 0.05, "\u2020" = 0.10),
        star_note = "stacked", digits = 2L, leading_zero = "bounded",
        caption = list(sep = "\n"),
        placement = "end", insert_marker = "Insert Table {n} about here",
        formats = c("docx", "tex", "xlsx")
      ),
      figure = list(formats_final = c("tiff", "jpeg"), formats_initial = c("tiff", "png"))
    ),

    informs = list(
      table = list(
        stars = c("***" = 0.01, "**" = 0.05, "*" = 0.10), star_note = "inline",
        leading_zero = "always",
        caption = list(sep = ". ", label_bold = TRUE),
        notes_order = c("source", "general", "specific", "significance"),
        note_label = "Note."
      ),
      figure = list(caption_position = "below", panel_label = "(a)")
    ),

    ama = list(
      table = list(
        significance = "exact_p", uncertainty = "se", leading_zero = "never",
        digits = 3L, placement = "embedded", note_label = "Notes:",
        caption = list(sep = ": ")
      ),
      figure = list(font_family = "Arial", grayscale = TRUE, gray_range = c(0.2, 0.8),
                    gray_step = 0.2, source_data = TRUE,
                    formats_final = c("pdf", "eps", "tiff"))
    ),

    aea = list(
      table = list(significance = "none", uncertainty = "se", leading_zero = "always",
                   max_columns = 9, specific_marks = "letters"),
      figure = list(vector_required = TRUE, formats_final = c("pdf", "eps"),
                    formats_initial = c("pdf"))
    ),

    econ = list(
      table = list(stars = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
                   star_note = "sentence", uncertainty = "se", leading_zero = "always",
                   notes_order = c("general", "specific", "source", "significance"))
    ),

    smj = list(
      table = list(
        significance = "none", uncertainty = "se", leading_zero = "bounded",
        caption = list(label_upper = TRUE, label_bold = FALSE, sep = " "),
        placement = "end", insert_marker = "INSERT TABLE {n} HERE"
      ),
      figure = list(label = "FIGURE", caption_position = "below", grayscale = TRUE,
                    formats_final = c("tiff", "png"), formats_initial = c("png", "pdf"))
    )
  )
}

# Artwork families (publisher-wide figure rules) -------------------------------
.jstyle_artwork_families <- function() {
  list(
    default = list(),
    elsevier = list(figure = list(
      dpi = c(line_art = 1000, halftone = 300, combination = 500),
      widths_mm = c(single = 90, onehalf = 140, double = 190), widths_generic = FALSE,
      min_font_pt = 7, formats_final = c("pdf", "eps", "tiff"),
      color_print = "free online; print colour charged after acceptance")),
    wiley = list(figure = list(
      dpi = c(line_art = 600, halftone = 300, combination = 600),
      widths_mm = c(single = 80, onehalf = 130, double = 180), widths_generic = FALSE,
      formats_final = c("pdf", "tiff", "png"))),
    sage = list(figure = list(
      dpi = c(line_art = 800, halftone = 300, combination = 600),
      formats_final = c("eps", "tiff", "pdf"), grayscale = TRUE, max_series = 7,
      color_print = "free online; greyscale in print unless paid; no colour words in captions")),
    springer = list(figure = list(
      dpi = c(line_art = 1200, halftone = 300, combination = 600),
      widths_mm = c(single = 84, onehalf = 129, double = 174), widths_generic = FALSE,
      font_family = "Arial", min_font_pt = 8, max_font_pt = 12,
      formats_final = c("eps", "tiff", "pdf"), panel_label = "(a)",
      color_print = "free online; print colour charged; must survive B/W conversion")),
    oup = list(figure = list(
      dpi = c(line_art = 600, halftone = 300, combination = 600),
      formats_final = c("eps", "tiff", "pdf"), alt_text = TRUE)),
    informs = list(figure = list(
      dpi = c(line_art = 900, halftone = 300, combination = 600),
      widths_mm = c(single = 82.5, onehalf = 124, double = 165),
      formats_final = c("pdf", "eps", "tiff"), formats_initial = c("pdf", "png"))),
    tandf = list(figure = list(
      dpi = c(line_art = 1200, halftone = 300, combination = 600),
      formats_final = c("eps", "tiff", "pdf"))),
    cambridge = list(figure = list(
      dpi = c(line_art = 1000, halftone = 300, combination = 600),
      formats_final = c("eps", "tiff", "pdf"), gray_range = c(0.15, 0.85),
      gray_step = 0.15)),
    chicago = list(figure = list(
      dpi = c(line_art = 1200, halftone = 300, combination = 800),
      formats_final = c("eps", "tiff"), grayscale = TRUE)),
    aom = list(figure = list(
      dpi = c(line_art = 600, halftone = 300, combination = 600),
      formats_final = c("tiff", "jpeg"))),
    apa = list(figure = list(
      dpi = c(line_art = 600, halftone = 300, combination = 600),
      formats_final = c("tiff", "eps", "jpeg"), font_family = "sans",
      color_print = "free online; print colour fee (JAP: $900 first figure)")),
    aaa = list(figure = list(
      dpi = c(line_art = 600, halftone = 300, combination = 600),
      formats_final = c("eps", "pdf", "tiff"), grayscale = TRUE, alt_text = TRUE)),
    ama = list(figure = list(
      dpi = c(line_art = 800, halftone = 300, combination = 800))),
    aea = list(figure = list(
      dpi = c(line_art = 600, halftone = 300, combination = 600)))
  )
}

# Journals --------------------------------------------------------------------
# lists: ft50_2016 = FT50 as published in 2016; ft50_2026 = FT50 after the
# April 2026 revision (Human Relations, J Business Ethics, Organization Studies
# out; AOM Annals, American Sociological Review, Psychological Science in);
# utd24 = UT Dallas 24.
.jstyle_journals <- function() {
  F16 <- "ft50_2016"; F26 <- "ft50_2026"; U <- "utd24"
  j <- function(id, name, abbrev, publisher, lists, family, artwork,
                confidence = "high", overrides = list(), caveats = character()) {
    list(id = id, name = name, abbrev = abbrev, publisher = publisher,
         lists = lists, family = family, artwork = artwork,
         confidence = confidence, overrides = overrides, caveats = caveats)
  }
  list(
    # ---- Accounting --------------------------------------------------------
    j("tar", "The Accounting Review", "TAR", "American Accounting Association",
      c(F16, F26, U), "default", "aaa", "high",
      list(table = list(caption = list(label_upper = TRUE, sep = "\n"), placement = "end",
                        panel_label = "Panel A"),
           figure = list(label = "FIGURE", panel_label = "Panel A",
                         dpi = c(line_art = 600, halftone = 264, combination = 600))),
      "Titles, notes and alt text go in the manuscript, not in the image file."),
    j("jar", "Journal of Accounting Research", "JAR", "Wiley",
      c(F16, F26, U), "default", "wiley", "low",
      caveats = "Wiley author page unreadable; publisher-wide figure rules only."),
    j("jae", "Journal of Accounting and Economics", "JAE", "Elsevier",
      c(F16, F26, U), "default", "elsevier", "low",
      caveats = "ScienceDirect guide unreadable; Elsevier-wide artwork rules only."),
    j("car", "Contemporary Accounting Research", "CAR", "Wiley",
      c(F16, F26), "econ", "wiley", "high",
      list(table = list(
        uncertainty = "t", leading_zero = "always", star_note = "sentence",
        caption = list(label_upper = TRUE, label_bold = FALSE, sep = " ", end_period = TRUE),
        notes_order = c("general", "specific", "significance", "source"),
        note_label = "Note:", placement = "end", panel_label = "Panel A:"),
        figure = list(label = "FIGURE", panel_label = "(A)", grayscale = TRUE)),
      "Stars go in the same cell as the estimate; CAR Author Style Guide (May 2026)."),
    j("rast", "Review of Accounting Studies", "RAST", "Springer Nature",
      c(F16, F26), "default", "springer", "high",
      list(figure = list(label = "Fig.", panel_label = "a"))),
    j("aos", "Accounting, Organizations and Society", "AOS", "Elsevier",
      c(F16, F26), "default", "elsevier", "low",
      caveats = "ScienceDirect guide unreadable; Elsevier-wide artwork rules only."),

    # ---- Marketing ---------------------------------------------------------
    j("jm", "Journal of Marketing", "JM", "SAGE (American Marketing Association)",
      c(F16, F26, U), "ama", "ama", "high",
      caveats = c("No significance stars (AMA editorial policy); exact three-digit p-values.",
                  "Table and figure titles should state the takeaway.",
                  "Supply original Excel/PowerPoint source files at acceptance.")),
    j("jmr", "Journal of Marketing Research", "JMR", "SAGE (American Marketing Association)",
      c(F16, F26, U), "ama", "ama", "high",
      caveats = c("No significance stars (AMA editorial policy); exact three-digit p-values.",
                  "Table and figure titles should state the takeaway.")),
    j("jcr", "Journal of Consumer Research", "JCR", "Oxford University Press",
      c(F16, F26, U), "default", "oup", "high",
      list(table = list(
        significance = "exact_p", uncertainty = "ci", leading_zero = "bounded",
        label = "TABLE", placement = "embedded",
        caption = list(label_upper = TRUE, title_upper = TRUE, sep = "\n",
                       align = "center", label_bold = FALSE)),
        figure = list(label = "FIGURE", font_family = "sans", caption_position = "above")),
      "Exact p-values (p = .035), 95% CIs as [.03, .57]."),
    j("jcp", "Journal of Consumer Psychology", "JCP", "Wiley",
      c(F16, F26), "apa7", "wiley", "medium",
      list(table = list(placement = "end", insert_marker = "INSERT TABLE {n} ABOUT HERE"),
           figure = list(dpi = c(line_art = 1000, halftone = 300, combination = 600),
                         label = "Fig.")),
      "Live page unreadable; 2020 author-guidelines PDF (cites APA 6)."),
    j("jams", "Journal of the Academy of Marketing Science", "JAMS", "Springer Nature",
      c(F16, F26), "default", "springer", "medium",
      list(table = list(placement = "end")),
      "Journal PDF (Aug 2025) has no figure specifications; Springer-wide rules used."),

    # ---- Finance -----------------------------------------------------------
    j("jf", "Journal of Finance", "JF", "Wiley (American Finance Association)",
      c(F16, F26, U), "econ", "wiley", "high",
      list(table = list(numbering = "roman", max_columns = 8),
           figure = list(color_print = "free online; $500 per printed colour page")),
      "JF style guide dated 2017; tables need a self-contained legend (max 300 words)."),
    j("jfe", "Journal of Financial Economics", "JFE", "Elsevier",
      c(F16, F26, U), "econ", "elsevier", "medium",
      caveats = "Elsevier guide unreadable; jfinec.com accepted-paper instructions used."),
    j("rfs", "Review of Financial Studies", "RFS", "Oxford University Press",
      c(F16, F26, U), "econ", "oup", "high",
      list(figure = list(formats_final = c("eps", "tiff"),
                         color_print = "$100 per colour figure file; CMYK"))),
    j("jfqa", "Journal of Financial and Quantitative Analysis", "JFQA", "Cambridge University Press",
      c(F16, F26), "econ", "cambridge", "high",
      list(table = list(caption = list(sep = ": "), formats = c("xlsx", "docx", "tex")),
           figure = list(caption_position = "above", panel_label = "Graph A")),
      "Significance note must list all three levels even if unused; editable (Excel-compatible) source."),
    j("rof", "Review of Finance", "RoF", "Oxford University Press",
      c(F16, F26), "econ", "oup", "high",
      list(figure = list(color_print = "EUR 525 per printed colour figure")),
      "Minus signs in tables must be en dashes."),

    # ---- Economics ---------------------------------------------------------
    j("aer", "American Economic Review", "AER", "American Economic Association",
      c(F16, F26), "aea", "aea", "high",
      caveats = "Do not use asterisks; max 9 columns, portrait, horizontal rules only."),
    j("ecta", "Econometrica", "ECTA", "Wiley (Econometric Society)",
      c(F16, F26), "econ", "wiley", "medium",
      list(table = list(placement = "embedded")),
      "Live pages no longer state table/figure rules; legacy rules (Roman numerals, SEs) not enforced."),
    j("jpe", "Journal of Political Economy", "JPE", "University of Chicago Press",
      c(F16, F26), "econ", "chicago", "medium",
      list(table = list(note_label = "Note."), figure = list(title_in_image = FALSE)),
      "Chicago pages blocked; rules from search excerpts."),
    j("qje", "Quarterly Journal of Economics", "QJE", "Oxford University Press",
      c(F16, F26), "econ", "oup", "high",
      list(table = list(numbering = "roman", placement = "end",
                        caption = list(sep = "\n", align = "center")),
           figure = list(numbering = "roman", grayscale = TRUE,
                         color_print = "print edition is black and white"))),
    j("restud", "Review of Economic Studies", "REStud", "Oxford University Press",
      c(F16, F26), "econ", "oup", "high",
      list(figure = list(color_print = "GBP 350 / USD 600 per printed colour image"))),

    # ---- INFORMS / IS / OM -------------------------------------------------
    j("mnsc", "Management Science", "MS", "INFORMS",
      c(F16, F26, U), "informs", "informs", "high"),
    j("mksc", "Marketing Science", "MKSC", "INFORMS",
      c(F16, F26, U), "informs", "informs", "high"),
    j("isr", "Information Systems Research", "ISR", "INFORMS",
      c(F16, F26, U), "informs", "informs", "high"),
    j("opre", "Operations Research", "OR", "INFORMS",
      c(F16, F26, U), "informs", "informs", "high",
      list(table = list(placement = "end")),
      "Tables grouped after the references; figures embedded."),
    j("orsc", "Organization Science", "OrgSci", "INFORMS",
      c(F16, F26, U), "informs", "informs", "high",
      caveats = "Effect sizes and N required for every model."),
    j("msom", "Manufacturing & Service Operations Management", "MSOM", "INFORMS",
      c(F16, F26, U), "informs", "informs", "high"),
    j("ijoc", "INFORMS Journal on Computing", "IJOC", "INFORMS",
      c(U), "informs", "informs", "high", list(table = list(placement = "embedded"))),
    j("misq", "MIS Quarterly", "MISQ", "MIS Research Center",
      c(F16, F26, U), "default", "default", "medium",
      list(table = list(placement = "embedded"), figure = list(font_family = "Arial")),
      "No figure resolution/format rules found; new style-guide PDF not located."),
    j("jmis", "Journal of Management Information Systems", "JMIS", "Taylor & Francis",
      c(F16, F26), "default", "tandf", "medium",
      list(table = list(placement = "end"))),
    j("pom", "Production and Operations Management", "POM", "SAGE",
      c(F16, F26, U), "default", "sage", "medium",
      list(table = list(placement = "embedded")),
      "Moved from Wiley to SAGE; journal-specific SAGE page unreadable."),
    j("jom", "Journal of Operations Management", "JOM", "Wiley",
      c(F16, F26, U), "default", "wiley", "high",
      list(table = list(placement = "embedded"), figure = list(caption_position = "below"))),

    # ---- Management --------------------------------------------------------
    j("amj", "Academy of Management Journal", "AMJ", "Academy of Management",
      c(F16, F26, U), "aom", "aom", "high"),
    j("amr", "Academy of Management Review", "AMR", "Academy of Management",
      c(F16, F26, U), "aom", "aom", "high"),
    j("annals", "Academy of Management Annals", "AMA", "Academy of Management",
      c(F26), "aom", "aom", "high"),
    j("asq", "Administrative Science Quarterly", "ASQ", "SAGE",
      c(F16, F26, U), "default", "sage", "medium",
      list(table = list(stars = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
                        star_note = "inline", tails = "two-tailed tests",
                        caption = list(sep = ". ", label_bold = TRUE),
                        placement = "end"),
           figure = list(min_font_pt = 8, grayscale = TRUE)),
      c("ASQ advises against marking p < .10; current symbol glyphs not verified.",
        "SAGE pages blocked; formatting-guidelines PDF (c. Dec 2025) via search.")),
    j("smj", "Strategic Management Journal", "SMJ", "Wiley (Strategic Management Society)",
      c(F16, F26, U), "smj", "wiley", "medium",
      list(figure = list(dpi = c(line_art = 600, halftone = 300, combination = 600))),
      c("No cutoff levels of significance: report SEs and/or exact p-values.",
        "Latest guideline PDF is behind a SharePoint sign-in; June 2023 PDF used.")),
    j("sej", "Strategic Entrepreneurship Journal", "SEJ", "Wiley (Strategic Management Society)",
      c(F16, F26), "smj", "wiley", "medium",
      list(table = list(significance = "stars")),
      "Same template as SMJ but no ban on stars."),
    j("jmgt", "Journal of Management", "JOM", "SAGE",
      c(F16, F26), "default", "sage", "low",
      list(table = list(placement = "end")),
      "SAGE pages blocked; JOM Style Guide (2024) via search excerpts only."),
    j("jms", "Journal of Management Studies", "JMS", "Wiley",
      c(F16, F26), "default", "wiley", "high",
      list(table = list(numbering = "roman", placement = "end",
                        insert_marker = "Insert Table {n} about here"))),
    j("orgstud", "Organization Studies", "OS", "SAGE",
      c(F16), "default", "sage", "low",
      caveats = c("Removed from FT50 in April 2026.", "SAGE pages blocked.")),
    j("jap", "Journal of Applied Psychology", "JAP", "American Psychological Association",
      c(F16, F26), "apa7", "apa", "medium",
      caveats = "apa.org blocked by CAPTCHA; JAP checklist PDF + APA 7 guides used."),
    j("obhdp", "Organizational Behavior and Human Decision Processes", "OBHDP", "Elsevier",
      c(F16, F26), "apa7", "elsevier", "medium",
      caveats = "Each figure as a separate file."),
    j("hr", "Human Relations", "HR", "SAGE",
      c(F16), "default", "sage", "medium",
      list(table = list(placement = "end", insert_marker = "INSERT TABLE {n} ABOUT HERE",
                        caption = list(sep = ": "), note_label = "Notes:")),
      c("Removed from FT50 in April 2026.", "LaTeX no longer accepted (Jan 2026).")),
    j("hrm", "Human Resource Management", "HRM", "Wiley",
      c(F16, F26), "default", "wiley", "medium",
      caveats = "Wayback copy (Sep 2024) of Wiley page."),
    j("jibs", "Journal of International Business Studies", "JIBS", "Springer Nature (Palgrave)",
      c(F16, F26, U), "default", "springer", "high",
      list(table = list(significance = "none", uncertainty = "se", leading_zero = "always",
                        placement = "end", caption = list(sep = "\n")),
           figure = list(min_font_pt = 6, max_font_pt = 8)),
      "No asterisks (2017 'What's in a p?' editorial); report SEs, exact p, effect sizes."),
    j("etp", "Entrepreneurship Theory and Practice", "ETP", "SAGE",
      c(F16, F26), "default", "sage", "medium",
      caveats = "Wayback copy (Jun 2025)."),
    j("jbv", "Journal of Business Venturing", "JBV", "Elsevier",
      c(F16, F26), "default", "elsevier", "medium",
      caveats = "Wayback copy (Feb 2024)."),
    j("jbe", "Journal of Business Ethics", "JBE", "Springer Nature",
      c(F16), "default", "springer", "high",
      list(figure = list(label = "Fig.")),
      "Removed from FT50 in April 2026."),
    j("respol", "Research Policy", "RP", "Elsevier",
      c(F16, F26), "default", "elsevier", "medium",
      caveats = "Wayback copy (Feb 2024)."),
    j("asr", "American Sociological Review", "ASR", "SAGE (American Sociological Association)",
      c(F26), "default", "sage", "high",
      list(table = list(stars = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
                        star_note = "inline", tails = "two-tailed tests",
                        leading_zero = "bounded", placement = "end")),
      "Do not mark p < .10 as significant; state one- or two-tailed tests."),
    j("psci", "Psychological Science", "PS", "SAGE (Association for Psychological Science)",
      c(F26), "apa7", "sage", "high",
      list(table = list(uncertainty = "ci", placement = "embedded"),
           figure = list(font_family = "sans", min_font_pt = 9, max_font_pt = 10,
                         caption_position = "below")),
      "Effect sizes with CIs; figures in Helvetica Neue Condensed (falls back to sans)."),
    j("hbr", "Harvard Business Review", "HBR", "Harvard Business Publishing",
      c(F16, F26), "default", "default", "low",
      caveats = "Practitioner outlet; no table/figure specifications published."),
    j("smr", "MIT Sloan Management Review", "SMR", "MIT Sloan School of Management",
      c(F16, F26), "default", "default", "low",
      caveats = c("No table/figure specifications published.",
                  "Print ends after fall 2026; relaunching digital-only."))
  )
}
