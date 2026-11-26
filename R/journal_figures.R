#' ggplot2 Theme for a Journal's Figure Rules
#'
#' A clean publication theme whose font family, font sizes and title handling
#' follow a journal's figure guidelines. Font sizes are in points at the
#' printed size, so pair it with [export_fig()], which saves at the journal's
#' column width.
#'
#' Most journals want the figure title and notes in the manuscript caption,
#' not inside the image. For those journals the theme hides the plot title,
#' subtitle and caption; [export_fig()] moves the title into the caption file.
#'
#' @param journal Journal id, abbreviation, name, or a `journal_style`.
#' @param base_size Base font size in points at the printed size. Defaults to
#'   9 pt, raised to the journal's minimum font size when that is larger.
#' @param base_family Font family. Defaults to the journal's preference
#'   (`"sans"` maps to Arial/Helvetica on most systems).
#' @param keep_title Keep the plot title inside the image even when the
#'   journal wants it in the caption (useful for slides). Default `FALSE`.
#' @param ... Further arguments passed to [ggplot2::theme()].
#'
#' @return A ggplot2 theme.
#' @seealso [journal_style()], [export_fig()], [scale_colour_journal()]
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_journal("smj")
#' @export
theme_journal <- function(journal = "default", base_size = NULL, base_family = NULL,
                          keep_title = FALSE, ...) {
  st <- journal_style(journal)
  f <- st$figure
  if (is.null(base_size)) base_size <- max(9, f$min_font_pt)
  if (is.null(base_family)) base_family <- .device_family(f$font_family)
  small <- max(f$min_font_pt, base_size - 1)

  th <- ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.line        = ggplot2::element_line(colour = "black", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(colour = "black", linewidth = 0.4),
      axis.text        = ggplot2::element_text(colour = "black", size = small),
      axis.title       = ggplot2::element_text(colour = "black", size = base_size),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(size = small),
      legend.text      = ggplot2::element_text(size = small),
      legend.key       = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(size = base_size, face = "bold", hjust = 0),
      plot.title       = ggplot2::element_text(size = base_size + 1, face = "bold", hjust = 0),
      plot.caption     = ggplot2::element_text(size = small, hjust = 0),
      plot.margin      = ggplot2::margin(4, 6, 4, 4)
    )
  if (!isTRUE(f$title_in_image) && !keep_title) {
    th <- th + ggplot2::theme(
      plot.title    = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption  = ggplot2::element_blank()
    )
  }
  th + ggplot2::theme(...)
}

# Journals name fonts (Arial, Helvetica, Times); R's devices know the generic
# families, which map to exactly those fonts (sans = Arial on Windows,
# Helvetica on macOS and in PDF/PostScript; serif = Times New Roman / Times).
.device_family <- function(font) {
  key <- tolower(font)
  if (key %in% c("arial", "helvetica", "helvetica neue", "sans", "sans-serif", "calibri")) return("sans")
  if (key %in% c("times", "times new roman", "serif")) return("serif")
  font
}

#' Grayscale-Safe Colour Palette for a Journal
#'
#' Journals that print in grayscale (for example SMJ, ASQ, TAR, CAR, AMA
#' journals, QJE) require figures to stay readable without colour. For those
#' journals this returns gray levels spaced by the journal's minimum step
#' (AMA: 20% to 80% gray in steps of at least 20%). For other journals it
#' returns viridis colours, whose lightness rises monotonically and therefore
#' also survive grayscale printing.
#'
#' @param journal Journal id, abbreviation, name, or a `journal_style`.
#' @param n Number of colours.
#' @param grayscale Force (`TRUE`) or skip (`FALSE`) the gray palette.
#'   Default follows the journal.
#'
#' @return Character vector of hex colours.
#' @examples
#' journal_palette("jm", 3)
#' journal_palette("mnsc", 4)
#' @export
journal_palette <- function(journal = "default", n, grayscale = NULL) {
  f <- journal_style(journal)$figure
  if (is.null(grayscale)) grayscale <- isTRUE(f$grayscale)
  if (n < 1) return(character())
  if (grayscale) {
    rng <- f$gray_range
    max_n <- floor(round((rng[2] - rng[1]) / f$gray_step, 6)) + 1
    if (n > max_n) {
      warning(sprintf(paste0(
        "%d groups exceed the %d gray levels that %s allows (%g%%-%g%% gray in steps of %g%%). ",
        "Distinguish the remaining groups with shapes or line types."),
        n, max_n, journal_style(journal)$abbrev, rng[1] * 100, rng[2] * 100, f$gray_step * 100),
        call. = FALSE)
    }
    levels <- if (n == 1) mean(rng) else seq(rng[2], rng[1], length.out = n)
    # level = share of black ink; grDevices::gray takes the white share
    return(grDevices::gray(1 - levels))
  }
  scales::viridis_pal(begin = 0, end = 0.85)(n)
}

#' Colour and Fill Scales Following a Journal's Figure Rules
#'
#' Discrete scales built on [journal_palette()].
#'
#' @param journal Journal id, abbreviation, name, or a `journal_style`.
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @return A ggplot2 scale.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point() +
#'   scale_colour_journal("jm") + theme_journal("jm")
#' @export
scale_colour_journal <- function(journal = "default", ...) {
  st <- journal_style(journal)
  ggplot2::discrete_scale(aesthetics = "colour",
                          palette = function(n) journal_palette(st, n), ...)
}

#' @rdname scale_colour_journal
#' @export
scale_color_journal <- scale_colour_journal

#' @rdname scale_colour_journal
#' @export
scale_fill_journal <- function(journal = "default", ...) {
  st <- journal_style(journal)
  ggplot2::discrete_scale(aesthetics = "fill",
                          palette = function(n) journal_palette(st, n), ...)
}

#' Export a Figure to a Journal's Specifications
#'
#' Saves a ggplot2 figure at the journal's column width, in the journal's
#' preferred file formats and at its minimum resolution, and writes the
#' caption, notes and alt text to a text file because most journals want
#' those in the manuscript rather than inside the image.
#'
#' @param plot A ggplot object.
#' @param filename File name without extension.
#' @param filepath Output directory. Created if missing.
#' @param journal Journal id, abbreviation, name, or a `journal_style`.
#' @param width `"single"`, `"onehalf"`, `"double"` (the journal's column
#'   widths) or a number in millimetres.
#' @param height Height in millimetres. Default: 75% of the width.
#' @param stage `"initial"` (review copy: PDF + PNG) or `"final"` (accepted
#'   manuscript: the journal's production formats).
#' @param formats Override the file formats (any of `"pdf"`, `"eps"`,
#'   `"tiff"`, `"png"`, `"jpeg"`, `"svg"`).
#' @param number Figure number, used in the caption label.
#' @param caption Caption title. Defaults to the plot's title.
#' @param notes Figure notes, written under the caption.
#' @param alt_text Alt text. Required by OUP journals (RFS, RoF, QJE, REStud)
#'   and TAR; a warning is given when it is missing.
#' @param content `"line_art"` (charts; default), `"combination"` or
#'   `"halftone"` (photographs). Sets the resolution of raster formats.
#' @param dpi Override the resolution of raster formats.
#' @param data Also write the plot's data to `<filename>_data.csv` (AMA and
#'   SAGE ask for the data or editable source behind each chart). Default
#'   `TRUE` when the journal asks for it.
#' @param archive Also save a dated copy in `filepath/archive`. Default `TRUE`.
#' @param check Run [journal_check()] and report problems. Default `TRUE`.
#'
#' @return Invisibly, a list with the written file paths, the caption text
#'   and the check results.
#' @seealso [theme_journal()], [journal_style()], [journal_check()]
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
#'   labs(title = "Heavier cars use more fuel", x = "Weight", y = "MPG") +
#'   theme_journal("jfe")
#' export_fig(p, "fig_weight", tempdir(), journal = "jfe", stage = "final",
#'            number = 1, alt_text = "Scatter plot: MPG falls as weight rises.")
#' }
#' @export
export_fig <- function(plot, filename, filepath = ".", journal = "default",
                       width = c("single", "onehalf", "double"), height = NULL,
                       stage = c("initial", "final"), formats = NULL,
                       number = 1, caption = NULL, notes = NULL, alt_text = NULL,
                       content = c("line_art", "combination", "halftone"),
                       dpi = NULL, data = NULL, archive = TRUE, check = TRUE) {
  if (!inherits(plot, "ggplot")) stop("`plot` must be a ggplot object.", call. = FALSE)
  st <- journal_style(journal)
  f <- st$figure
  stage <- match.arg(stage)
  content <- match.arg(content)

  width_mm <- if (is.numeric(width)) width else f$widths_mm[[match.arg(width)]]
  height_mm <- if (is.null(height)) round(width_mm * 0.75, 1) else height
  if (is.null(formats)) formats <- if (stage == "final") f$formats_final else f$formats_initial
  formats <- tolower(formats)
  formats[formats == "jpg"] <- "jpeg"
  formats[formats == "tif"] <- "tiff"
  if (is.null(dpi)) dpi <- f$dpi[[content]]
  if (is.null(data)) data <- isTRUE(f$source_data)

  if (isTRUE(f$vector_required) && !any(formats %in% c("pdf", "eps", "svg"))) {
    warning(st$abbrev, " requires vector graphics; add 'pdf' or 'eps' to `formats`.", call. = FALSE)
  }

  # Caption: take the title from the plot when not given, then strip it from
  # the image if the journal wants titles outside the figure.
  if (is.null(caption)) caption <- plot$labels$title
  if (!isTRUE(f$title_in_image)) {
    plot$labels$title <- NULL
    plot$labels$subtitle <- NULL
    if (is.null(notes) && !is.null(plot$labels$caption)) notes <- plot$labels$caption
    plot$labels$caption <- NULL
  }
  if (isTRUE(f$alt_text) && is.null(alt_text)) {
    warning(st$abbrev, " requires alt text for every figure; supply `alt_text`.", call. = FALSE)
  }

  if (!dir.exists(filepath)) dir.create(filepath, recursive = TRUE)
  arch_dir <- file.path(filepath, "archive")
  if (archive && !dir.exists(arch_dir)) dir.create(arch_dir, recursive = TRUE)
  stamp <- format(Sys.Date(), "%Y-%m-%d")

  paths <- character()
  for (fmt in formats) {
    out <- file.path(filepath, paste0(filename, ".", if (fmt == "jpeg") "jpg" else if (fmt == "tiff") "tif" else fmt))
    .save_fig(plot, out, fmt, width_mm, height_mm, dpi)
    paths <- c(paths, out)
    if (archive) {
      file.copy(out, file.path(arch_dir, sub("(\\.[a-z]+)$", paste0("_", stamp, "\\1"), basename(out))),
                overwrite = TRUE)
    }
  }

  cap <- .format_caption("figure", number, caption, st)
  cap_lines <- c(cap$text,
                 if (!is.null(notes)) paste(st$table$note_label, notes),
                 if (!is.null(alt_text)) paste(f$alt_text_label, alt_text))
  cap_file <- file.path(filepath, paste0(filename, "_caption.txt"))
  writeLines(cap_lines, cap_file, useBytes = TRUE)
  paths <- c(paths, cap_file)

  if (isTRUE(data) && is.data.frame(plot$data) && nrow(plot$data) > 0) {
    data_file <- file.path(filepath, paste0(filename, "_data.csv"))
    utils::write.csv(plot$data, data_file, row.names = FALSE)
    paths <- c(paths, data_file)
  }

  chk <- NULL
  if (check) {
    chk <- journal_check(plot, st, width = width_mm)
    if (nrow(chk)) print(chk)
  }
  if (!isTRUE(f$title_in_image) && !is.null(caption)) {
    message(sprintf("%s: title moved to the caption (%s).", st$abbrev, basename(cap_file)))
  }
  invisible(list(files = paths, caption = cap_lines, width_mm = width_mm,
                 height_mm = height_mm, dpi = dpi, check = chk))
}

.save_fig <- function(plot, out, fmt, width_mm, height_mm, dpi) {
  args <- list(filename = out, plot = plot, width = width_mm, height = height_mm,
               units = "mm")
  dev <- switch(fmt,
    pdf  = grDevices::cairo_pdf,      # embeds fonts
    eps  = grDevices::cairo_ps,
    svg  = "svg",
    tiff = "tiff",
    png  = "png",
    jpeg = "jpeg",
    stop("Unsupported figure format: ", fmt, call. = FALSE)
  )
  args$device <- dev
  if (fmt %in% c("tiff", "png", "jpeg")) {
    args$dpi <- dpi
    if (fmt == "tiff") args$compression <- "lzw"
    if (fmt == "jpeg") args$quality <- 100
  }
  if (fmt == "eps") args$fallback_resolution <- dpi
  if (file.exists(out)) unlink(out)
  skip_cairo <- fmt %in% c("pdf", "eps") && isTRUE(getOption("causalverse.no_cairo"))
  ok <- !skip_cairo && tryCatch({
    suppressWarnings(do.call(ggplot2::ggsave, args))
    file.exists(out) && file.size(out) > 0
  }, error = function(e) FALSE)

  # Cairo devices are not always usable (e.g. CRAN R on macOS without
  # XQuartz). Fall back to R's own PDF/PostScript devices, which also embed
  # standard fonts.
  if (!ok && fmt %in% c("pdf", "eps")) {
    args$fallback_resolution <- NULL
    .ensure_generic_fonts()
    args$device <- if (fmt == "pdf") {
      function(filename, width, height, ...) {
        grDevices::pdf(filename, width = width, height = height, useDingbats = FALSE,
                       family = "Helvetica", fonts = c("sans", "serif", "mono"))
      }
    } else {
      function(filename, width, height, ...) {
        grDevices::postscript(filename, width = width, height = height,
                              horizontal = FALSE, onefile = FALSE, paper = "special",
                              family = "Helvetica", fonts = c("sans", "serif", "mono"))
      }
    }
    if (file.exists(out)) unlink(out)
    do.call(ggplot2::ggsave, args)
    ok <- file.exists(out) && file.size(out) > 0
  }
  if (!ok) stop(sprintf("Could not write %s: the %s graphics device failed.", out, fmt), call. = FALSE)
  invisible(out)
}

# Make the generic families ("sans", "serif", "mono") known to the base
# pdf() and postscript() devices, which on some platforms only know named
# fonts. Maps them to Helvetica, Times and Courier.
.ensure_generic_fonts <- function() {
  map <- c(sans = "Helvetica", serif = "Times", mono = "Courier")
  for (db in list(list(get = grDevices::postscriptFonts, set = grDevices::postscriptFonts),
                  list(get = grDevices::pdfFonts, set = grDevices::pdfFonts))) {
    have <- names(db$get())
    for (fam in names(map)) {
      if (!fam %in% have && map[[fam]] %in% have) {
        do.call(db$set, stats::setNames(list(db$get(map[[fam]])[[1]]), fam))
      }
    }
  }
  invisible()
}
