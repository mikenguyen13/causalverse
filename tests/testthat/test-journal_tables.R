models <- list(
  "(1)" = lm(mpg ~ am, data = mtcars),
  "(2)" = lm(mpg ~ am + wt, data = mtcars)
)
cmap <- c(am = "Manual", wt = "Weight")

test_that("number formatting follows leading-zero rules", {
  expect_equal(causalverse:::.jfmt(0.25, 2, "always"), "0.25")
  expect_equal(causalverse:::.jfmt(0.25, 2, "never"), ".25")
  expect_equal(causalverse:::.jfmt(-0.25, 2, "never"), "-.25")
  expect_equal(causalverse:::.jfmt(0.25, 2, "bounded", bounded = FALSE), "0.25")
  expect_equal(causalverse:::.jfmt(0.25, 2, "bounded", bounded = TRUE), ".25")
  expect_equal(causalverse:::.jfmt(1234.5, 1, "always"), "1,234.5")
  expect_equal(causalverse:::.jfmt_p(0.0004, 3, "never"), "p < .001")
  expect_equal(causalverse:::.jfmt_p(0.047, 3, "always"), "p = 0.047")
})

test_that("stars follow the journal's thresholds", {
  t <- journal_style("amj")$table
  expect_equal(causalverse:::.jstars(c(0.2, 0.08, 0.03, 0.005), t), c("", "\u2020", "*", "**"))
  t2 <- journal_style("asr")$table
  expect_equal(causalverse:::.jstars(c(0.08, 0.0005), t2), c("", "***"))
  expect_equal(causalverse:::.jstars(0.0001, journal_style("aer")$table), "")
})

test_that("journals that ban stars get none", {
  for (j in c("aer", "smj", "jm", "jmr", "jibs")) {
    tab <- journal_table(models, j, coef_map = cmap)
    expect_false(tab$has_stars, info = j)
    expect_true(all(!nzchar(tab$sup)), info = j)
  }
  expect_true(journal_table(models, "jf", coef_map = cmap)$has_stars)
})

test_that("exact-p journals report p-values and drop leading zeros", {
  tab <- journal_table(models, "jm", coef_map = cmap)
  expect_true(any(tab$rowtype == "p"))
  expect_true(any(grepl("^p [<=] \\.", tab$body)))
  expect_false(any(grepl("(^|\\()0\\.", tab$body[, -1])))
})

test_that("uncertainty format follows the journal", {
  expect_true(any(grepl("^\\(-?[0-9.]+\\)$", journal_table(models, "aer", coef_map = cmap)$body)))
  expect_true(any(grepl("^\\[", journal_table(models, "jcr", coef_map = cmap)$body)))
  car <- journal_table(models, "car", coef_map = cmap)
  expect_match(paste(car$notes, collapse = " "), "t-statistics")
})

test_that("captions follow the journal's label style", {
  expect_equal(journal_table(models, "jf", title = "X", number = 4)$caption$label, "Table IV")
  expect_equal(journal_table(models, "smj", title = "X")$caption$text, "TABLE 1 X")
  expect_equal(journal_table(models, "car", title = "Results")$caption$text, "TABLE 1 Results.")
  expect_equal(journal_table(models, "jcr", title = "Results")$caption$title, "RESULTS")
  expect_true(journal_table(models, "jap", title = "Results")$caption$title_italic)
})

test_that("notes are ordered per journal", {
  mn <- journal_table(models, "mnsc", coef_map = cmap, notes = "OLS.", source = "mtcars")$notes
  expect_match(mn[1], "^Source:")
  car <- journal_table(models, "car", coef_map = cmap, notes = "OLS.", source = "mtcars")$notes
  expect_match(car[1], "^Note:")
  expect_match(car[length(car)], "^Source:")
})

test_that("data frames are formatted cell by cell", {
  df <- data.frame(Variable = c("A", "B"), Mean = c(2.5, 3.25), r = c(0.5, -0.25))
  tab <- journal_table(df, "jap", title = "Descriptives")
  expect_equal(tab$body[, 3], c(".500", "-.250"))
  expect_equal(tab$body[, 2], c("2.500", "3.250"))
})

test_that("journal_check flags stars and column limits", {
  chk <- journal_check(journal_table(models, "jf", coef_map = cmap), "aer")
  expect_true("significance_stars" %in% chk$rule)
  wide <- stats::setNames(rep(models[1], 10), paste0("(", 1:10, ")"))
  chk2 <- journal_check(journal_table(wide, "aer", coef_map = cmap))
  expect_true("max_columns" %in% chk2$rule)
  expect_equal(nrow(journal_check(journal_table(models, "aer", coef_map = cmap))), 0)
})

test_that("export_tab writes LaTeX, HTML, Excel and Word files", {
  out <- file.path(tempdir(), "jt_export")
  unlink(out, recursive = TRUE)
  fmts <- c("tex", "html", "xlsx")
  if (requireNamespace("flextable", quietly = TRUE) && requireNamespace("officer", quietly = TRUE)) {
    fmts <- c(fmts, "docx")
  }
  expect_message(
    paths <- export_tab(models, "t1", out, journal = "smj", title = "Fuel economy",
                        coef_map = cmap, formats = fmts, check = FALSE),
    "INSERT TABLE 1 HERE"
  )
  expect_true(all(file.exists(paths)))
  expect_length(paths, length(fmts))
  tex <- readLines(file.path(out, "t1.tex"))
  expect_true(any(grepl("\\\\toprule", tex)))
  expect_false(any(grepl("textsuperscript", tex)))
  expect_true(file.exists(file.path(out, "archive", paste0("t1_", format(Sys.Date(), "%Y-%m-%d"), ".tex"))))
})

test_that("LaTeX escaping handles special characters", {
  expect_equal(causalverse:::.tex_escape("a_b & 5%"), "a\\_b \\& 5\\%")
  expect_equal(causalverse:::.tex_escape("x\\y"), "x\\textbackslash{}y")
})
