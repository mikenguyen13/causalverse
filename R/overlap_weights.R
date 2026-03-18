#' Overlap Weights and Trimming for Propensity Score Analysis
#'
#' Computes overlap weights (also called average overlap weights, ATO weights)
#' or trimmed inverse probability weights. Overlap weights target the
#' population with the most covariate overlap and have the desirable property
#' of minimizing the asymptotic variance among all balancing weights.
#'
#' @param data A data frame.
#' @param treatment Character. Name of the binary treatment indicator (0/1).
#' @param covariates Character vector. Covariates for the propensity score
#'   model.
#' @param ps_formula Formula or \code{NULL}. Custom propensity score formula.
#'   If \code{NULL}, uses a main-effects logistic regression on
#'   \code{covariates}.
#' @param weight_type Character. Type of weight:
#'   \code{"overlap"} (default, ATO), \code{"ipw"} (ATE weights),
#'   \code{"att"} (ATT weights), \code{"trim"} (trimmed IPW).
#' @param trim_threshold Numeric. Propensity scores below this or above
#'   \code{1 - trim_threshold} are trimmed (excluded). Only used when
#'   \code{weight_type = "trim"}. Default \code{0.1}.
#' @param normalize Logical. Whether to normalize weights to sum to 1 within
#'   each treatment arm. Default \code{TRUE}.
#' @param plot Logical. If \code{TRUE}, produce a propensity score overlap
#'   plot. Default \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{weights}{Numeric vector of length \code{nrow(data)}.}
#'     \item{ps}{Numeric vector of propensity scores.}
#'     \item{ess_treated}{Effective sample size in the treated arm.}
#'     \item{ess_control}{Effective sample size in the control arm.}
#'     \item{n_trimmed}{Number of trimmed observations (only for
#'       \code{"trim"}).}
#'     \item{plot}{ggplot2 overlap plot.}
#'   }
#'
#' @details
#' Overlap weights for unit \eqn{i} are:
#' \deqn{w_i = D_i (1 - e_i) + (1 - D_i) e_i}
#' where \eqn{e_i = P(D=1|X_i)}. These automatically down-weight units near
#' the boundaries of the propensity score distribution.
#'
#' @references
#' Li, F., Morgan, K. L., & Zaslavsky, A. M. (2018). Balancing covariates via
#' propensity score weighting. \emph{Journal of the American Statistical
#' Association}, 113(521), 390–400.
#'
#' Crump, R. K., Hotz, V. J., Imbens, G. W., & Mitnik, O. A. (2009). Dealing
#' with limited overlap in estimation of average treatment effects.
#' \emph{Biometrika}, 96(1), 187–199.
#'
#' @examples
#' data(lalonde, package = "MatchIt")
#' ow <- overlap_weights(
#'   data       = lalonde,
#'   treatment  = "treat",
#'   covariates = c("age", "educ", "re74", "re75")
#' )
#' ow$ess_treated
#' ow$plot
#'
#' @importFrom stats glm binomial predict weighted.mean
#' @importFrom ggplot2 ggplot aes geom_density geom_vline scale_fill_manual
#'   labs theme_minimal
#' @export
overlap_weights <- function(data,
                            treatment,
                            covariates,
                            ps_formula      = NULL,
                            weight_type     = c("overlap", "ipw", "att", "trim"),
                            trim_threshold  = 0.1,
                            normalize       = TRUE,
                            plot            = TRUE) {

  weight_type <- match.arg(weight_type)

  if (!treatment %in% names(data)) {
    stop("`treatment` column not found.", call. = FALSE)
  }
  D <- as.integer(as.logical(data[[treatment]]))

  # --- Estimate propensity score ---
  if (is.null(ps_formula)) {
    ps_formula <- stats::as.formula(
      paste(treatment, "~", paste(covariates, collapse = " + "))
    )
  }
  ps_model <- stats::glm(ps_formula, data = data, family = stats::binomial())
  ps <- stats::predict(ps_model, type = "response")
  ps <- pmax(pmin(ps, 1 - 1e-6), 1e-6)

  n_trimmed <- 0L

  # --- Compute weights ---
  wts <- switch(weight_type,
    "overlap" = D * (1 - ps) + (1 - D) * ps,
    "ipw"     = D / ps + (1 - D) / (1 - ps),
    "att"     = D + (1 - D) * ps / (1 - ps),
    "trim"    = {
      in_support <- ps >= trim_threshold & ps <= (1 - trim_threshold)
      n_trimmed  <- sum(!in_support)
      w <- D / ps + (1 - D) / (1 - ps)
      w[!in_support] <- 0
      w
    }
  )

  # --- Normalize within arm ---
  if (normalize) {
    wts[D == 1] <- wts[D == 1] / sum(wts[D == 1], na.rm = TRUE)
    wts[D == 0] <- wts[D == 0] / sum(wts[D == 0], na.rm = TRUE)
  }

  # --- Effective sample sizes ---
  ess <- function(w) sum(w)^2 / sum(w^2)
  ess_t <- ess(wts[D == 1 & wts > 0])
  ess_c <- ess(wts[D == 0 & wts > 0])

  out <- list(
    weights     = wts,
    ps          = ps,
    ess_treated = round(ess_t, 1),
    ess_control = round(ess_c, 1),
    n_trimmed   = n_trimmed,
    plot        = NULL
  )

  if (plot) {
    df_p <- data.frame(
      ps    = ps,
      group = factor(D, levels = c(0, 1), labels = c("Control", "Treated"))
    )
    p <- ggplot2::ggplot(df_p, ggplot2::aes(
        x = ps, fill = group, color = group
      )) +
      ggplot2::geom_density(alpha = 0.4) +
      ggplot2::geom_vline(xintercept = trim_threshold, linetype = "dashed",
                          color = "gray40") +
      ggplot2::geom_vline(xintercept = 1 - trim_threshold, linetype = "dashed",
                          color = "gray40") +
      ggplot2::scale_fill_manual(
        values = c("Control" = "#4393C3", "Treated" = "#D73027")
      ) +
      ggplot2::scale_color_manual(
        values = c("Control" = "#4393C3", "Treated" = "#D73027")
      ) +
      ggplot2::labs(
        x     = "Propensity Score",
        y     = "Density",
        title = paste0("Propensity Score Distribution (", weight_type, " weights)"),
        subtitle = sprintf(
          "ESS treated = %.0f | ESS control = %.0f",
          ess_t, ess_c
        ),
        fill  = "Group",
        color = "Group"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    out$plot <- p
  }

  invisible(out)
}
