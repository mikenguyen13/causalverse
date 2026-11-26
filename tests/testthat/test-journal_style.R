test_that("every journal in the registry resolves to a complete style", {
  jl <- journal_list()
  expect_gte(nrow(jl), 50)
  expect_false(any(duplicated(jl$id)))
  for (id in jl$id) {
    st <- journal_style(id)
    expect_s3_class(st, "journal_style")
    expect_true(st$table$significance %in% c("stars", "none", "exact_p"), info = id)
    expect_true(st$table$uncertainty %in% c("se", "t", "ci"), info = id)
    expect_true(st$table$leading_zero %in% c("always", "bounded", "never"), info = id)
    expect_true(st$table$placement %in% c("embedded", "end", "either"), info = id)
    expect_named(st$figure$dpi, c("line_art", "halftone", "combination"), ignore.order = TRUE)
    expect_true(all(c("single", "double") %in% names(st$figure$widths_mm)), info = id)
    expect_true(st$confidence %in% c("high", "medium", "low"), info = id)
  }
})

test_that("journal lists have the expected sizes", {
  expect_equal(nrow(journal_list("ft50_2016")), 50)
  expect_equal(nrow(journal_list("ft50")), 50)
  expect_equal(nrow(journal_list("utd24")), 24)
  ft26 <- journal_list("ft50")$id
  expect_true(all(c("annals", "asr", "psci") %in% ft26))
  expect_false(any(c("hr", "jbe", "orgstud") %in% ft26))
})

test_that("journals can be found by id, abbreviation or name", {
  expect_equal(journal_style("SMJ")$id, "smj")
  expect_equal(journal_style("Strategic Management Journal")$id, "smj")
  expect_equal(journal_style("journal of finance")$id, "jf")
  expect_error(journal_style("Journal of Nonexistent Studies"), "Unknown journal")
  expect_equal(journal_style("default")$id, "default")
})

test_that("key journal rules are encoded", {
  expect_equal(journal_style("aer")$table$significance, "none")
  expect_equal(journal_style("smj")$table$significance, "none")
  expect_equal(journal_style("jibs")$table$significance, "none")
  expect_equal(journal_style("jm")$table$significance, "exact_p")
  expect_equal(journal_style("jm")$table$leading_zero, "never")
  expect_equal(journal_style("mnsc")$table$leading_zero, "always")
  expect_equal(journal_style("jf")$table$numbering, "roman")
  expect_equal(journal_style("qje")$table$numbering, "roman")
  expect_equal(journal_style("aer")$table$max_columns, 9)
  expect_equal(journal_style("car")$table$uncertainty, "t")
  expect_equal(journal_style("amj")$table$digits, 2L)
  expect_true(journal_style("rfs")$figure$alt_text)
  expect_equal(journal_style("jbv")$figure$dpi[["line_art"]], 1000)
  expect_equal(journal_style("jbe")$figure$dpi[["line_art"]], 1200)
})

test_that("overrides merge into the style", {
  st <- journal_style("jf", table = list(digits = 2L))
  expect_equal(st$table$digits, 2L)
  expect_equal(st$table$numbering, "roman")
})

test_that("every journal has at least one monitored source URL", {
  src <- journal_sources()
  expect_true(all(c("journal_id", "scope", "url") %in% names(src)))
  expect_true(all(grepl("^https://", src$url)))
  expect_true(all(journal_list()$id %in% src$journal_id))
  expect_gt(nrow(journal_sources("jm")), 1)
})

test_that("print methods run", {
  expect_output(print(journal_style("smj")), "Strategic Management Journal")
})
