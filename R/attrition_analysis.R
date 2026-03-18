#' Attrition Analysis for Experimental & Panel Data
#'
#' Analyses sample attrition (dropout / non-response) in experiments or
#' panel studies. Tests whether attrition is differential (correlated with
#' treatment), estimates attrition bias using Lee (2009) trimming bounds,
#' and produces diagnostic plots.
#'
#' @param data A data frame.
#' @param outcome Character. Name of the outcome variable (can have \code{NA}
#'   for attriters).
#' @param treatment Character. Name of the binary treatment indicator (0/1).
#' @param covariates Character vector. Pre-treatment covariates to check for
#'   differential attrition. Default: all numeric columns except \code{outcome}
#'   and \code{treatment}.
#' @param alpha Numeric. Significance level. Default \code{0.05}.
#' @param plot Logical. Produce diagnostic plots. Default \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{rates}}{Data frame: overall, treatment-group, control-group
#'       attrition rates and a chi-square test of differential attrition.}
#'     \item{\code{differential_test}}{Results of a logistic regression of
#'       attrition on treatment (and covariates). Tests \eqn{H_0}: treatment
#'       coefficient = 0.}
#'     \item{\code{covariate_balance}}{SMD table comparing attriters vs
#'       completers within each treatment arm.}
#'     \item{\code{lee_bounds}}{Lee (2009) trimming bounds on the treatment
#'       effect (lower bound, upper bound, 95 percent CIs).}
#'     \item{\code{plot}}{A ggplot2 object (if \code{plot = TRUE}).}
#'   }
#'
#' @references
#' Lee, D. S. (2009). Training, wages, and sample selection: Estimating
#' sharp bounds on treatment effects. \emph{Review of Economic Studies},
#' 76(3), 1071-1102.
#'
#' @examples
#' set.seed(7)
#' n <- 400
#' df <- data.frame(
#'   treat  = rbinom(n, 1, 0.5),
#'   age    = runif(n, 18, 65),
#'   female = rbinom(n, 1, 0.5)
#' )
#' # Differential attrition: control units with low age more likely to drop
#' retain <- rbinom(n, 1, ifelse(df$treat == 1, 0.85, 0.75 + 0.002 * df$age))
#' df$y   <- ifelse(retain == 1, df$treat * 2 + rnorm(n), NA_real_)
#'
#' res <- attrition_analysis(df, outcome = "y", treatment = "treat",
#'                            covariates = c("age", "female"))
#' res$rates
#' res$lee_bounds
#'
#' @export
attrition_analysis <- function(data, outcome, treatment,
                                covariates = NULL,
                                alpha      = 0.05,
                                plot       = TRUE) {

  # Identify attrition: outcome is NA
  data$.observed <- !is.na(data[[outcome]])
  data$.treat    <- as.integer(data[[treatment]])

  n_total   <- nrow(data)
  n_treated <- sum(data$.treat == 1, na.rm = TRUE)
  n_control <- sum(data$.treat == 0, na.rm = TRUE)

  # Attrition rates
  attr_overall <- mean(!data$.observed)
  attr_treat   <- mean(!data$.observed[data$.treat == 1])
  attr_ctrl    <- mean(!data$.observed[data$.treat == 0])

  chi_test <- chisq.test(table(data$.treat, data$.observed))

  rates <- data.frame(
    group          = c("Overall", "Treated", "Control"),
    n              = c(n_total, n_treated, n_control),
    attrition_rate = c(attr_overall, attr_treat, attr_ctrl),
    pct_retained   = c(1 - attr_overall, 1 - attr_treat, 1 - attr_ctrl) * 100
  )

  # Differential attrition test (logistic regression)
  diff_data <- data
  if (is.null(covariates)) {
    covariates <- setdiff(names(data)[sapply(data, is.numeric)],
                          c(outcome, treatment, ".observed", ".treat"))
  }
  covariates <- intersect(covariates, names(data))

  rhs <- paste(c(".treat", covariates), collapse = " + ")
  fml_diff <- as.formula(paste(".observed ~", rhs))
  diff_fit  <- tryCatch(
    glm(fml_diff, data = diff_data, family = binomial),
    error = function(e) NULL
  )

  diff_test <- NULL
  if (!is.null(diff_fit)) {
    sm        <- summary(diff_fit)$coefficients
    treat_row <- sm[".treat", ]
    diff_test <- data.frame(
      term      = ".treat",
      estimate  = treat_row[1],
      std_error = treat_row[2],
      z_stat    = treat_row[3],
      p_value   = treat_row[4],
      significant = treat_row[4] < alpha,
      interpretation = ifelse(
        treat_row[4] < alpha,
        "Differential attrition detected (p < alpha): attrition is correlated with treatment.",
        "No evidence of differential attrition (p >= alpha)."
      )
    )
  }

  # Covariate balance: attriters vs completers by treatment arm
  cov_balance <- NULL
  if (length(covariates) > 0) {
    bal_rows <- lapply(covariates, function(v) {
      if (!is.numeric(data[[v]])) return(NULL)

      # Within treated
      x_t_obs  <- data[[v]][data$.treat == 1 & data$.observed]
      x_t_miss <- data[[v]][data$.treat == 1 & !data$.observed]
      smd_t    <- if (length(x_t_obs) > 1 && length(x_t_miss) > 1) {
        (mean(x_t_obs, na.rm = TRUE) - mean(x_t_miss, na.rm = TRUE)) /
          sqrt((var(x_t_obs, na.rm = TRUE) + var(x_t_miss, na.rm = TRUE)) / 2)
      } else NA_real_

      # Within control
      x_c_obs  <- data[[v]][data$.treat == 0 & data$.observed]
      x_c_miss <- data[[v]][data$.treat == 0 & !data$.observed]
      smd_c    <- if (length(x_c_obs) > 1 && length(x_c_miss) > 1) {
        (mean(x_c_obs, na.rm = TRUE) - mean(x_c_miss, na.rm = TRUE)) /
          sqrt((var(x_c_obs, na.rm = TRUE) + var(x_c_miss, na.rm = TRUE)) / 2)
      } else NA_real_

      data.frame(
        covariate       = v,
        smd_treated_arm = round(smd_t, 3),
        smd_control_arm = round(smd_c, 3),
        flagged         = (!is.na(smd_t) && abs(smd_t) > 0.1) ||
                          (!is.na(smd_c) && abs(smd_c) > 0.1),
        stringsAsFactors = FALSE
      )
    })
    cov_balance <- do.call(rbind, Filter(Negate(is.null), bal_rows))
  }

  # Lee (2009) bounds — use causalverse::lee_bounds if available
  lee_result <- tryCatch({
    causalverse::lee_bounds(
      data      = data[, c(outcome, treatment)],
      outcome   = outcome,
      selection = ".observed",
      treatment = treatment
    )
  }, error = function(e) {
    # Minimal manual Lee bounds
    # Trim the distribution of Y for the group with lower attrition
    Y1   <- data[[outcome]][data$.treat == 1 & data$.observed]
    Y0   <- data[[outcome]][data$.treat == 0 & data$.observed]
    p1   <- 1 - attr_treat
    p0   <- 1 - attr_ctrl

    if (is.null(Y1) || length(Y1) == 0 || is.null(Y0) || length(Y0) == 0) {
      return(data.frame(lower = NA, upper = NA, lower_ci = NA, upper_ci = NA))
    }

    if (p1 >= p0) {
      # Trim treated from below for upper bound, from above for lower bound
      q_lo  <- quantile(Y1, 1 - p0 / p1, na.rm = TRUE)
      q_hi  <- quantile(Y1, p0 / p1, na.rm = TRUE)
      ub <- mean(Y1[Y1 >= q_lo], na.rm = TRUE) - mean(Y0, na.rm = TRUE)
      lb <- mean(Y1[Y1 <= q_hi], na.rm = TRUE) - mean(Y0, na.rm = TRUE)
    } else {
      q_lo  <- quantile(Y0, 1 - p1 / p0, na.rm = TRUE)
      q_hi  <- quantile(Y0, p1 / p0, na.rm = TRUE)
      ub <- mean(Y1, na.rm = TRUE) - mean(Y0[Y0 <= q_lo], na.rm = TRUE)
      lb <- mean(Y1, na.rm = TRUE) - mean(Y0[Y0 >= q_hi], na.rm = TRUE)
    }
    se_lb <- sd(Y1, na.rm = TRUE) / sqrt(length(Y1))
    se_ub <- se_lb

    data.frame(
      lower    = lb,
      upper    = ub,
      lower_ci = lb - qnorm(1 - alpha / 2) * se_lb,
      upper_ci = ub + qnorm(1 - alpha / 2) * se_ub
    )
  })

  out <- list(
    rates               = rates,
    differential_test   = diff_test,
    covariate_balance   = cov_balance,
    lee_bounds          = lee_result,
    plot                = NULL
  )

  if (plot) {
    # Plot 1: Attrition rates by group
    p1 <- ggplot2::ggplot(rates,
        ggplot2::aes(x = group, y = attrition_rate * 100,
                     fill = group)) +
      ggplot2::geom_col(width = 0.5, alpha = 0.85) +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.1f%%", attrition_rate * 100)),
        vjust = -0.3, size = 3.5
      ) +
      ggplot2::labs(
        title = "Attrition Rates by Group",
        x     = NULL,
        y     = "Attrition Rate (%)"
      ) +
      ggplot2::scale_fill_manual(
        values = c("Overall" = "grey60", "Treated" = "#2166ac", "Control" = "#d73027"),
        guide  = "none"
      ) +
      causalverse::ama_theme()

    # Plot 2: Lee bounds
    if (!is.null(lee_result) && !all(is.na(lee_result))) {
      lee_df <- data.frame(
        bound  = c("Lower", "Upper"),
        est    = c(lee_result$lower[1], lee_result$upper[1]),
        ci_lo  = c(lee_result$lower_ci[1], NA),
        ci_hi  = c(NA, lee_result$upper_ci[1])
      )
      p2 <- ggplot2::ggplot(lee_df,
          ggplot2::aes(x = est, y = bound)) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
        ggplot2::geom_point(size = 4, color = "#2166ac") +
        ggplot2::geom_errorbar(
          ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
          width = 0.2, color = "#2166ac", na.rm = TRUE, orientation = "y"
        ) +
        ggplot2::labs(
          title = "Lee (2009) Trimming Bounds",
          x     = "Treatment Effect",
          y     = NULL
        ) +
        causalverse::ama_theme()

      if (requireNamespace("patchwork", quietly = TRUE)) {
        out$plot <- patchwork::wrap_plots(p1, p2, ncol = 2)
      } else {
        out$plot <- p1
      }
    } else {
      out$plot <- p1
    }
  }

  invisible(out)
}
