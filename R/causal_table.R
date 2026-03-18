#' Publication-Ready Causal Inference Results Table
#'
#' Produces a formatted regression table comparing multiple causal inference
#' estimates, suitable for journal submission. Supports HTML, LaTeX, and
#' plain text output via \pkg{modelsummary} or a built-in fallback.
#'
#' @param models Named list of model objects or tidy data frames (from
#'   \code{tidy_causal()}).
#' @param coef_map Named character vector. Mapping from coefficient names
#'   to display names. E.g., \code{c(treat = "Treatment", age = "Age")}.
#' @param gof_rows Character vector. Goodness-of-fit statistics to include.
#'   Default \code{c("nobs", "r.squared", "adj.r.squared")}.
#' @param title Character. Table title.
#' @param notes Character or \code{NULL}. Table footnote.
#' @param stars Logical or named numeric vector. Significance thresholds.
#'   Default \code{c("*" = 0.1, "**" = 0.05, "***" = 0.01)}.
#' @param output_format Character. \code{"html"} (default), \code{"latex"},
#'   or \code{"dataframe"}.
#' @param digits Integer. Decimal places. Default \code{3}.
#'
#' @return A \pkg{modelsummary} table, \pkg{gt} table, or data frame.
#'
#' @examples
#' \dontrun{
#' m1 <- lm(mpg ~ am, data = mtcars)
#' m2 <- lm(mpg ~ am + wt, data = mtcars)
#' m3 <- lm(mpg ~ am + wt + hp, data = mtcars)
#'
#' causal_table(
#'   models   = list("(1) Bivariate" = m1, "(2) + Weight" = m2,
#'                   "(3) Full" = m3),
#'   coef_map = c(am = "Manual Transmission", wt = "Weight (1000 lbs)",
#'                hp = "Horsepower"),
#'   title    = "Effect of Transmission Type on MPG"
#' )
#' }
#'
#' @export
causal_table <- function(models,
                         coef_map      = NULL,
                         gof_rows      = c("nobs", "r.squared", "adj.r.squared"),
                         title         = "Causal Inference Results",
                         notes         = NULL,
                         stars         = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
                         output_format = c("html", "latex", "dataframe"),
                         digits        = 3) {

  output_format <- match.arg(output_format)

  # Use modelsummary if available
  if (requireNamespace("modelsummary", quietly = TRUE)) {
    gof_map_use <- if (!is.null(gof_rows)) {
      # Filter to available GOF stats
      ms_gof <- modelsummary::gof_map
      ms_gof[ms_gof$clean %in% gof_rows | ms_gof$raw %in% gof_rows, ]
    } else {
      "none"
    }

    out <- modelsummary::modelsummary(
      models         = models,
      coef_map       = coef_map,
      gof_map        = gof_map_use,
      title          = title,
      notes          = notes,
      stars          = stars,
      fmt            = digits,
      output         = switch(output_format,
        "html"       = "html",
        "latex"      = "latex",
        "dataframe"  = "dataframe"
      )
    )
    return(out)
  }

  # Fallback: build comparison data frame manually
  message("Package 'modelsummary' not available. Returning a tidy data frame.")

  rows <- lapply(names(models), function(nm) {
    obj <- models[[nm]]
    if (is.data.frame(obj)) {
      df <- obj
      if (!"model" %in% names(df)) df$model <- nm
    } else {
      df <- tryCatch(tidy_causal(obj), error = function(e) NULL)
      if (!is.null(df)) df$model <- nm
    }
    df
  })

  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (!is.null(coef_map)) {
    out$term[out$term %in% names(coef_map)] <-
      coef_map[out$term[out$term %in% names(coef_map)]]
  }
  out
}
