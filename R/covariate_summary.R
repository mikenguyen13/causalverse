#' Comprehensive Covariate Summary Table (Table 1)
#'
#' Generates a publication-ready "Table 1" showing descriptive statistics
#' for all covariates, split by treatment status. Computes means, SDs,
#' medians, percentages (for binary), p-values for group differences, and
#' standardized mean differences.
#'
#' @param data A data frame.
#' @param treatment Character. Binary treatment indicator (0/1).
#' @param vars Character vector. Variables to summarize. If \code{NULL},
#'   all numeric and factor columns except \code{treatment} are used.
#' @param var_labels Named character vector. Display labels for variables.
#'   Names are variable names, values are labels.
#' @param digits Integer. Decimal places for continuous variables. Default
#'   \code{2}.
#' @param pct_digits Integer. Decimal places for percentages. Default
#'   \code{1}.
#' @param test_type Character. Statistical test: \code{"auto"} (default,
#'   t-test for continuous, chi-sq for binary/factor), \code{"ttest"},
#'   \code{"wilcox"}, or \code{"chisq"}.
#' @param caption Character. Table caption. Default \code{"Baseline Characteristics"}.
#' @param col_labels Character vector of length 3. Column headers for
#'   overall, control, treated. Default \code{c("Overall", "Control", "Treated")}.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{variable}{Variable label.}
#'     \item{overall}{Overall summary: mean (SD) for continuous, n (pct) for binary.}
#'     \item{control}{Control group summary.}
#'     \item{treated}{Treated group summary.}
#'     \item{p_value}{p-value for group difference.}
#'     \item{smd}{Standardized mean difference.}
#'   }
#'
#' @examples
#' data(lalonde, package = "MatchIt")
#' covariate_summary(
#'   data      = lalonde,
#'   treatment = "treat",
#'   vars      = c("age", "educ", "re74", "re75", "married", "nodegree")
#' )
#'
#' @importFrom stats t.test wilcox.test chisq.test sd median
#' @export
covariate_summary <- function(data,
                               treatment,
                               vars         = NULL,
                               var_labels   = NULL,
                               digits       = 2,
                               pct_digits   = 1,
                               test_type    = "auto",
                               caption      = "Baseline Characteristics",
                               col_labels   = c("Overall", "Control", "Treated")) {

  if (!treatment %in% names(data)) {
    stop("`treatment` column '", treatment, "' not found.", call. = FALSE)
  }

  D <- as.integer(as.logical(data[[treatment]]))

  if (is.null(vars)) {
    vars <- setdiff(names(data)[vapply(data, function(x) {
      is.numeric(x) || is.logical(x) || is.factor(x)
    }, logical(1))], treatment)
  }

  fmt_mean_sd <- function(x) {
    sprintf("%s (%s)", format(round(mean(x, na.rm = TRUE), digits),
                               nsmall = digits),
            format(round(stats::sd(x, na.rm = TRUE), digits), nsmall = digits))
  }

  fmt_pct <- function(x) {
    n_yes  <- sum(x, na.rm = TRUE)
    n_tot  <- sum(!is.na(x))
    pct    <- round(100 * n_yes / n_tot, pct_digits)
    sprintf("%d (%s%%)", n_yes, format(pct, nsmall = pct_digits))
  }

  rows <- lapply(vars, function(v) {
    x    <- data[[v]]
    x0   <- x[D == 0]
    x1   <- x[D == 1]
    lab  <- if (!is.null(var_labels) && v %in% names(var_labels)) {
      var_labels[v]
    } else {
      v
    }

    is_binary <- length(unique(na.omit(x))) == 2

    # Format summaries
    if (is.numeric(x) && !is_binary) {
      ovr <- fmt_mean_sd(x)
      c0  <- fmt_mean_sd(x0)
      c1  <- fmt_mean_sd(x1)
      # SMD
      pool_sd <- sqrt((stats::sd(x0, na.rm = TRUE)^2 +
                         stats::sd(x1, na.rm = TRUE)^2) / 2)
      smd_val <- if (pool_sd == 0) NA else
        (mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) / pool_sd
      # p-value
      pv <- tryCatch({
        if (test_type == "wilcox") {
          stats::wilcox.test(x1, x0)$p.value
        } else {
          stats::t.test(x1, x0)$p.value
        }
      }, error = function(e) NA)

    } else {
      # Binary / factor
      x   <- as.integer(as.logical(x))
      x0  <- x[D == 0]; x1  <- x[D == 1]
      ovr_n <- sum(x, na.rm = TRUE)
      ovr <- sprintf("%d (%.1f%%)", ovr_n, 100 * ovr_n / length(x))
      c0  <- sprintf("%d (%.1f%%)", sum(x0, na.rm = TRUE),
                     100 * mean(x0, na.rm = TRUE))
      c1  <- sprintf("%d (%.1f%%)", sum(x1, na.rm = TRUE),
                     100 * mean(x1, na.rm = TRUE))
      smd_val <- (mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) /
        sqrt((mean(x0, na.rm = TRUE) * (1 - mean(x0, na.rm = TRUE)) +
                mean(x1, na.rm = TRUE) * (1 - mean(x1, na.rm = TRUE))) / 2)
      pv <- tryCatch({
        tbl <- table(x, D)
        stats::chisq.test(tbl, correct = FALSE)$p.value
      }, error = function(e) NA)
    }

    data.frame(
      variable = lab,
      overall  = ovr,
      control  = c0,
      treated  = c1,
      p_value  = ifelse(is.na(pv), NA, round(pv, 4)),
      smd      = ifelse(is.na(smd_val), NA, round(smd_val, 3)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  out <- do.call(rbind, rows)
  names(out)[2:4] <- col_labels
  rownames(out)   <- NULL

  attr(out, "caption") <- caption
  out
}
