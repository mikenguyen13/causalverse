#' Convert Between Causal Effect Size Metrics
#'
#' Converts between common effect size representations used in causal
#' inference and applied statistics: unstandardized coefficients,
#' Cohen's d, Cohen's f², partial eta², odds ratios, log-odds, Number
#' Needed to Treat (NNT), risk ratios, and percent changes.
#'
#' @param value Numeric. The effect size value to convert from.
#' @param from Character. Input metric. One of: \code{"unstd"} (raw
#'   coefficient), \code{"cohens_d"}, \code{"cohens_f2"}, \code{"eta2"},
#'   \code{"or"} (odds ratio), \code{"log_or"} (log odds ratio),
#'   \code{"rr"} (risk ratio), \code{"rd"} (risk difference),
#'   \code{"nnt"} (number needed to treat), \code{"pct_change"}.
#' @param to Character or character vector. Output metric(s). Same choices
#'   as \code{from}. Default: all available metrics.
#' @param sd_outcome Numeric. Standard deviation of the outcome.
#'   Required for converting to/from unstandardized. Default \code{1}.
#' @param sd_treatment Numeric. SD of the treatment. Required for some
#'   conversions. Default \code{1}.
#' @param baseline_risk Numeric. Baseline risk P(Y=1 | D=0) for
#'   binary outcome conversions. Default \code{0.1}.
#' @param verbose Logical. Print interpretation. Default \code{TRUE}.
#'
#' @return A named numeric vector of converted effect sizes, or a data
#'   frame with interpretation guidelines.
#'
#' @examples
#' # Convert Cohen's d = 0.3 to NNT, risk difference, etc.
#' effect_size_convert(
#'   value   = 0.3,
#'   from    = "cohens_d",
#'   to      = c("nnt", "rd", "pct_change")
#' )
#'
#' # Convert regression coefficient to standardized metrics
#' effect_size_convert(
#'   value       = 5.2,
#'   from        = "unstd",
#'   sd_outcome  = 15.3
#' )
#'
#' @export
effect_size_convert <- function(value,
                                 from           = "cohens_d",
                                 to             = NULL,
                                 sd_outcome     = 1,
                                 sd_treatment   = 1,
                                 baseline_risk  = 0.1,
                                 verbose        = TRUE) {

  all_metrics <- c("unstd", "cohens_d", "cohens_f2", "eta2", "or",
                   "log_or", "rr", "rd", "nnt", "pct_change")

  if (!from %in% all_metrics) {
    stop("`from` must be one of: ", paste(all_metrics, collapse = ", "),
         call. = FALSE)
  }

  if (is.null(to)) to <- setdiff(all_metrics, from)

  # --- Convert input to Cohen's d (intermediate) ---
  d_val <- switch(from,
    "cohens_d"   = value,
    "unstd"      = value / sd_outcome,
    "cohens_f2"  = sqrt(value / (1 - value / (1 + value))),   # approx
    "eta2"       = sqrt(2 * value / (1 - value)),             # approx for balanced
    "or"         = log(value) / (pi / sqrt(3)),
    "log_or"     = value / (pi / sqrt(3)),
    "rr"         = {
      # P(Y=1|D=1) from RR
      p1 <- baseline_risk * value
      p1 <- pmin(pmax(p1, 0.001), 0.999)
      p0 <- baseline_risk
      (p1 - p0) / sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
    },
    "rd"         = {
      # Risk difference → Cohen's d
      p0 <- baseline_risk
      p1 <- pmin(pmax(baseline_risk + value, 0.001), 0.999)
      (p1 - p0) / sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
    },
    "nnt"        = {
      # NNT = 1/RD
      rd_val <- 1 / value
      p0 <- baseline_risk
      p1 <- pmin(pmax(p0 + rd_val, 0.001), 0.999)
      (p1 - p0) / sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
    },
    "pct_change" = value / 100  # Treat pct change as approx Cohen's d
  )

  # --- Convert Cohen's d to output metrics ---
  convert_to <- function(metric, d) {
    p0 <- baseline_risk
    # Approximate P(Y=1|D=1) from d assuming normal distributions
    p1 <- pmin(pmax(p0 + d * sqrt((p0 * (1 - p0))), 0.001), 0.999)

    switch(metric,
      "cohens_d"   = d,
      "unstd"      = d * sd_outcome,
      "cohens_f2"  = d^2 / (1 + d^2),          # approx
      "eta2"       = d^2 / (4 + d^2),           # approx balanced design
      "or"         = exp(d * pi / sqrt(3)),
      "log_or"     = d * pi / sqrt(3),
      "rr"         = p1 / p0,
      "rd"         = p1 - p0,
      "nnt"        = 1 / (p1 - p0),
      "pct_change" = d * 100,
      NA
    )
  }

  result <- vapply(to, function(m) convert_to(m, d_val), numeric(1))

  if (verbose) {
    cat("Effect size conversion: from", from, "=", value, "\n\n")
    for (m in to) {
      v <- result[m]
      interp <- switch(m,
        "cohens_d" = sprintf("Cohen's d = %.4f (%s effect)",
          v, ifelse(abs(v) < 0.2, "negligible",
             ifelse(abs(v) < 0.5, "small",
             ifelse(abs(v) < 0.8, "medium", "large")))),
        "nnt"      = sprintf("NNT = %.1f (treat %.1f people to prevent 1 event)", v, v),
        "or"       = sprintf("OR = %.4f", v),
        "rr"       = sprintf("RR = %.4f", v),
        "rd"       = sprintf("Risk difference = %.4f (%.2f%%)", v, v*100),
        "pct_change" = sprintf("Percent change = %.2f%%", v),
        sprintf("%s = %.6f", m, v)
      )
      cat(" ", interp, "\n")
    }
  }

  invisible(result)
}
