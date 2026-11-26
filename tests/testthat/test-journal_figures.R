library(ggplot2)

base_plot <- function() {
  ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point() +
    labs(title = "Heavier cars use more fuel", x = "Weight", y = "MPG")
}

test_that("theme_journal hides titles for journals that want them in the caption", {
  th <- theme_journal("smj")
  expect_s3_class(ggplot2::calc_element("plot.title", ggplot2::theme_grey() + th), "element_blank")
  th2 <- theme_journal("smj", keep_title = TRUE)
  expect_false(inherits(ggplot2::calc_element("plot.title", ggplot2::theme_grey() + th2), "element_blank"))
})

test_that("theme_journal respects the journal's minimum font size", {
  th <- ggplot2::theme_grey() + theme_journal("asq")
  expect_gte(ggplot2::calc_element("axis.text.x", th)$size, journal_style("asq")$figure$min_font_pt)
})

test_that("journal_palette gives grayscale steps where required", {
  pal <- journal_palette("jm", 4)
  expect_length(pal, 4)
  rgb <- grDevices::col2rgb(pal)
  expect_true(all(rgb[1, ] == rgb[2, ] & rgb[2, ] == rgb[3, ]))  # grays
  expect_warning(journal_palette("jm", 6), "gray levels")
  expect_length(journal_palette("mnsc", 3), 3)
})

test_that("journal_check flags titles, grayscale and small fonts", {
  chk <- journal_check(base_plot(), "jm")
  expect_true(all(c("title_in_image", "grayscale") %in% chk$rule))
  ok <- base_plot() + scale_colour_journal("jm") + theme_journal("jm")
  expect_equal(nrow(journal_check(ok, "jm")), 0)
  tiny <- base_plot() + theme_journal("jm") + theme(axis.text = element_text(size = 4))
  expect_true("min_font_size" %in% journal_check(tiny, "jm")$rule)
})

test_that("export_fig writes journal formats, caption and data files", {
  out <- file.path(tempdir(), "jf_export")
  unlink(out, recursive = TRUE)
  p <- base_plot() + scale_colour_journal("rfs") + theme_journal("rfs")
  res <- suppressMessages(export_fig(p, "f1", out, journal = "rfs", stage = "final",
                                     alt_text = "Scatter plot.", data = TRUE, check = FALSE))
  expect_true(all(file.exists(res$files)))
  expect_true(file.exists(file.path(out, "f1.eps")))
  expect_true(file.exists(file.path(out, "f1.tif")))
  expect_true(file.exists(file.path(out, "f1_data.csv")))
  cap <- readLines(file.path(out, "f1_caption.txt"))
  expect_match(cap[1], "Heavier cars use more fuel")
  expect_true(any(grepl("^Alt text:", cap)))
  expect_equal(res$width_mm, journal_style("rfs")$figure$widths_mm[["single"]])
  expect_equal(res$dpi, journal_style("rfs")$figure$dpi[["line_art"]])
})

test_that("export_fig warns when alt text is required but missing", {
  out <- file.path(tempdir(), "jf_export2")
  p <- base_plot() + theme_journal("qje")
  expect_warning(suppressMessages(export_fig(p, "f2", out, journal = "qje", formats = "png",
                                             archive = FALSE, check = FALSE)),
                 "alt text")
})

test_that("export_fig accepts numeric widths in millimetres", {
  out <- file.path(tempdir(), "jf_export3")
  res <- suppressMessages(export_fig(base_plot() + theme_journal(), "f3", out, width = 120,
                                     formats = "png", archive = FALSE, check = FALSE))
  expect_equal(res$width_mm, 120)
  expect_equal(res$height_mm, 90)
})
