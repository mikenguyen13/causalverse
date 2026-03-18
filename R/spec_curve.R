#' Specification Curve Analysis
#'
#' Runs multiple model specifications and produces a specification curve plot
#' following Simonsohn, Simmons, and Nelson (2020). This is useful for
#' demonstrating that results are robust across reasonable analytical choices.
#'
#' @param data A data frame containing all variables.
#' @param y Character string. The dependent variable name.
#' @param x Character string. The treatment/main independent variable name.
#' @param controls A list of character vectors, where each vector is a set of
#'   control variables for one specification. For example,
#'   \code{list(c("age"), c("age", "income"), c("age", "income", "education"))}.
#' @param fixed_effects A character vector of fixed effect variable names to
#'   cycle through (each used individually). Default is \code{NULL}.
#' @param cluster_var Optional character string. Variable name for clustered
#'   standard errors. Default is \code{NULL}.
#' @param family Character string. Either \code{"gaussian"} (default) or
#'   \code{"binomial"} for logistic regression.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{results}}{A data frame with columns: \code{spec_id},
#'     \code{estimate}, \code{std_error}, \code{ci_lower}, \code{ci_upper},
#'     \code{p_value}, \code{n_obs}, \code{controls}, \code{fe}.}
#'   \item{\code{plot}}{A ggplot2 object showing the specification curve.}
#'   \item{\code{median_estimate}}{The median coefficient across all specifications.}
#'   \item{\code{pct_significant}}{Percentage of specifications with p < 0.05.}
#'   \item{\code{pct_positive}}{Percentage of specifications with positive estimates.}
#' }
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' result <- spec_curve(
#'   data = mtcars,
#'   y = "mpg",
#'   x = "am",
#'   controls = list(
#'     c("wt"),
#'     c("wt", "hp"),
#'     c("wt", "hp", "disp"),
#'     c("wt", "hp", "disp", "drat")
#'   )
#' )
#' result$plot
#' }
#'
#' @references
#' Simonsohn, U., Simmons, J. P., and Nelson, L. D. (2020). "Specification
#' Curve Analysis." *Nature Human Behaviour*, 4(11), 1208-1214.
#'
#' @importFrom stats lm coef confint pnorm nobs
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline
#'   theme_minimal labs coord_flip scale_color_manual
#' @export
spec_curve <- function(data,
                       y,
                       x,
                       controls,
                       fixed_effects = NULL,
                       cluster_var = NULL,
                       family = "gaussian") {

  if (!is.list(controls)) {
    stop("`controls` must be a list of character vectors.")
  }

  # Build all specifications
  specs <- list()
  spec_id <- 0

  fe_options <- if (is.null(fixed_effects)) list(NULL) else c(list(NULL), as.list(fixed_effects))

  for (ctrl in controls) {
    for (fe in fe_options) {
      spec_id <- spec_id + 1

      # Build formula
      rhs <- x
      if (length(ctrl) > 0 && !all(ctrl == "")) {
        rhs <- paste(c(rhs, ctrl), collapse = " + ")
      }

      if (!is.null(fe) && requireNamespace("fixest", quietly = TRUE)) {
        fml <- stats::as.formula(paste(y, "~", rhs, "|", fe))
        if (!is.null(cluster_var)) {
          fit <- fixest::feols(fml, data = data, cluster = cluster_var)
        } else {
          fit <- fixest::feols(fml, data = data)
        }
        coefs <- fixest::coeftable(fit)
        idx <- which(rownames(coefs) == x)
        if (length(idx) == 0) next
        est <- coefs[idx, 1]
        se <- coefs[idx, 2]
        pval <- coefs[idx, 4]
        ci <- est + c(-1, 1) * 1.96 * se
        n <- stats::nobs(fit)
      } else {
        fml <- stats::as.formula(paste(y, "~", rhs))
        if (family == "binomial") {
          fit <- stats::glm(fml, data = data, family = "binomial")
        } else {
          fit <- stats::lm(fml, data = data)
        }
        coefs <- summary(fit)$coefficients
        idx <- which(rownames(coefs) == x)
        if (length(idx) == 0) next
        est <- coefs[idx, 1]
        se <- coefs[idx, 2]
        pval <- coefs[idx, 4]
        ci <- est + c(-1, 1) * 1.96 * se
        n <- stats::nobs(fit)
      }

      specs[[spec_id]] <- data.frame(
        spec_id = spec_id,
        estimate = est,
        std_error = se,
        ci_lower = ci[1],
        ci_upper = ci[2],
        p_value = pval,
        n_obs = n,
        controls = paste(ctrl, collapse = ", "),
        fe = if (is.null(fe)) "none" else fe,
        stringsAsFactors = FALSE
      )
    }
  }

  results <- do.call(rbind, specs)
  results <- results[order(results$estimate), ]
  results$rank <- seq_len(nrow(results))
  results$significant <- results$p_value < 0.05

  # Summary statistics
  median_est <- stats::median(results$estimate)
  pct_sig <- mean(results$significant) * 100
  pct_pos <- mean(results$estimate > 0) * 100

  # Create plot
  p <- ggplot2::ggplot(results, ggplot2::aes(x = rank, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_hline(yintercept = median_est, linetype = "dotted", color = "blue") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper, color = significant),
      width = 0, alpha = 0.4
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = significant),
      size = 1.5
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "black", "FALSE" = "gray70"),
      labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
      name = "Significance"
    ) +
    ggplot2::labs(
      x = "Specification (ranked by estimate)",
      y = paste("Coefficient on", x),
      caption = paste0(
        "Median = ", round(median_est, 3),
        " | ", round(pct_sig, 1), "% significant",
        " | ", round(pct_pos, 1), "% positive",
        " | N specifications = ", nrow(results)
      )
    ) +
    ggplot2::theme_minimal()

  list(
    results = results,
    plot = p,
    median_estimate = median_est,
    pct_significant = pct_sig,
    pct_positive = pct_pos
  )
}
