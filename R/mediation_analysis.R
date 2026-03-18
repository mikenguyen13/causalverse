#' Causal Mediation Analysis with ACME and ADE Estimation
#'
#' Performs causal mediation analysis to decompose the total treatment effect
#' into the Average Causal Mediation Effect (ACME / indirect effect) and the
#' Average Direct Effect (ADE). Wraps \pkg{mediation} package functionality
#' with enhanced output and visualization.
#'
#' @param data A data frame.
#' @param outcome Character. Name of the outcome variable.
#' @param treatment Character. Name of the binary treatment variable.
#' @param mediator Character. Name of the mediator variable.
#' @param covariates Character vector. Additional covariates in both models.
#'   Default \code{NULL}.
#' @param treat_mediator_interaction Logical. Whether to include a
#'   treatment × mediator interaction in the outcome model. Default
#'   \code{FALSE}.
#' @param family_mediator Character. GLM family for the mediator model.
#'   Default \code{"gaussian"}. Use \code{"binomial"} for binary mediators.
#' @param family_outcome Character. GLM family for the outcome model.
#'   Default \code{"gaussian"}.
#' @param sims Integer. Number of quasi-Bayesian Monte Carlo simulations.
#'   Default \code{1000}.
#' @param conf_level Numeric. Confidence level for CIs. Default \code{0.95}.
#' @param seed Integer. Random seed. Default \code{42}.
#' @param plot Logical. If \code{TRUE} (default), produce a decomposition plot.
#'
#' @return A list with:
#'   \describe{
#'     \item{acme}{Numeric. Average Causal Mediation Effect (indirect effect).}
#'     \item{ade}{Numeric. Average Direct Effect.}
#'     \item{total}{Numeric. Total treatment effect.}
#'     \item{prop_mediated}{Numeric. Proportion of effect mediated.}
#'     \item{summary_df}{Data frame with ACME, ADE, Total, and their CIs.}
#'     \item{mediation_obj}{The \code{mediate} object (if \pkg{mediation} is available).}
#'     \item{plot}{ggplot2 decomposition bar chart.}
#'   }
#'
#' @details
#' This function implements the approach from Imai et al. (2010) via the
#' \pkg{mediation} package. When \pkg{mediation} is not available, a simple
#' product-of-coefficients (Baron-Kenny) approach is used.
#'
#' The sequential ignorability assumption (no unmeasured confounding of
#' mediator-outcome relationship) is required for causal interpretation.
#' Use \code{sensitivity_plot()} or the \code{sens.analysis} output from
#' the \pkg{mediation} package to assess robustness.
#'
#' @references
#' Imai, K., Keele, L., & Tingley, D. (2010). A general approach to causal
#' mediation analysis. \emph{Psychological Methods}, 15(4), 309–334.
#'
#' Baron, R. M., & Kenny, D. A. (1986). The moderator-mediator variable
#' distinction in social psychological research. \emph{Journal of Personality
#' and Social Psychology}, 51(6), 1173–1182.
#'
#' @examples
#' set.seed(123)
#' n <- 200
#' df <- data.frame(
#'   treat = rbinom(n, 1, 0.5),
#'   X     = rnorm(n)
#' )
#' df$mediator <- 0.5 * df$treat + 0.3 * df$X + rnorm(n, 0, 0.5)
#' df$outcome  <- 0.3 * df$mediator + 0.2 * df$treat + rnorm(n)
#'
#' result <- mediation_analysis(
#'   data      = df,
#'   outcome   = "outcome",
#'   treatment = "treat",
#'   mediator  = "mediator"
#' )
#' result$summary_df
#' result$plot
#'
#' @importFrom stats glm lm gaussian binomial coef vcov as.formula
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar geom_hline
#'   scale_fill_manual labs theme_minimal
#' @export
mediation_analysis <- function(data,
                               outcome,
                               treatment,
                               mediator,
                               covariates                 = NULL,
                               treat_mediator_interaction = FALSE,
                               family_mediator            = "gaussian",
                               family_outcome             = "gaussian",
                               sims                       = 1000,
                               conf_level                 = 0.95,
                               seed                       = 42,
                               plot                       = TRUE) {

  req_cols <- c(outcome, treatment, mediator)
  if (!is.null(covariates)) req_cols <- c(req_cols, covariates)
  missing  <- setdiff(req_cols, names(data))
  if (length(missing) > 0) {
    stop("Columns not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # Build formulas
  rhs_med  <- paste(c(treatment, covariates), collapse = " + ")
  rhs_out  <- if (treat_mediator_interaction) {
    paste(c(paste0(treatment, "*", mediator), covariates), collapse = " + ")
  } else {
    paste(c(treatment, mediator, covariates), collapse = " + ")
  }

  form_med <- stats::as.formula(paste(mediator, "~", rhs_med))
  form_out <- stats::as.formula(paste(outcome,  "~", rhs_out))

  med_fam  <- match.arg(family_mediator, c("gaussian", "binomial", "poisson"))
  out_fam  <- match.arg(family_outcome,  c("gaussian", "binomial", "poisson"))

  fam_fn <- function(f) {
    switch(f,
      "gaussian" = stats::gaussian(),
      "binomial" = stats::binomial(),
      "poisson"  = stats::poisson()
    )
  }

  med_model <- stats::glm(form_med, data = data, family = fam_fn(med_fam))
  out_model <- stats::glm(form_out, data = data, family = fam_fn(out_fam))

  # --- Use mediation package if available ---
  med_obj <- NULL
  if (requireNamespace("mediation", quietly = TRUE)) {
    set.seed(seed)
    med_obj <- tryCatch({
      mediation::mediate(
        model.m  = med_model,
        model.y  = out_model,
        treat    = treatment,
        mediator = mediator,
        sims     = sims,
        conf.level = conf_level
      )
    }, error = function(e) {
      message("mediation::mediate() failed: ", e$message,
              "\nFalling back to Baron-Kenny approach.")
      NULL
    })
  }

  # --- Extract or compute ACME / ADE ---
  if (!is.null(med_obj)) {
    s    <- summary(med_obj)
    acme <- s$d.avg
    ade  <- s$z.avg
    tot  <- s$tau.coef
    prop <- s$n.avg
    ci_acme <- c(s$d.avg.ci[1], s$d.avg.ci[2])
    ci_ade  <- c(s$z.avg.ci[1], s$z.avg.ci[2])
    ci_tot  <- c(s$tau.ci[1], s$tau.ci[2])
  } else {
    # Baron-Kenny: ACME ≈ a * b; ADE = c' (direct path)
    a    <- stats::coef(med_model)[treatment]
    b    <- stats::coef(out_model)[mediator]
    cp   <- stats::coef(out_model)[treatment]
    acme <- a * b
    ade  <- cp
    tot  <- acme + ade
    prop <- if (tot != 0) acme / tot else NA
    # Delta-method SE for ACME
    var_a <- stats::vcov(med_model)[treatment, treatment]
    var_b <- stats::vcov(out_model)[mediator, mediator]
    se_acme <- sqrt(b^2 * var_a + a^2 * var_b)
    z   <- stats::qnorm(1 - (1 - conf_level) / 2)
    ci_acme <- c(acme - z * se_acme, acme + z * se_acme)
    se_ade  <- sqrt(stats::vcov(out_model)[treatment, treatment])
    ci_ade  <- c(ade - z * se_ade, ade + z * se_ade)
    ci_tot  <- c(ci_ade[1] + ci_acme[1], ci_ade[2] + ci_acme[2])
  }

  summary_df <- data.frame(
    effect     = c("ACME (Indirect)", "ADE (Direct)", "Total Effect"),
    estimate   = c(acme, ade, tot),
    ci_lower   = c(ci_acme[1], ci_ade[1], ci_tot[1]),
    ci_upper   = c(ci_acme[2], ci_ade[2], ci_tot[2]),
    stringsAsFactors = FALSE
  )

  out <- list(
    acme           = acme,
    ade            = ade,
    total          = tot,
    prop_mediated  = prop,
    summary_df     = summary_df,
    mediation_obj  = med_obj,
    plot           = NULL
  )

  if (plot) {
    df_p <- summary_df
    df_p$effect <- factor(df_p$effect,
                          levels = c("Total Effect", "ADE (Direct)", "ACME (Indirect)"))
    p <- ggplot2::ggplot(df_p, ggplot2::aes(
        x    = effect,
        y    = estimate,
        fill = effect
      )) +
      ggplot2::geom_col(alpha = 0.8, width = 0.5) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
        width = 0.2
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::scale_fill_manual(
        values = c(
          "ACME (Indirect)" = "#4393C3",
          "ADE (Direct)"    = "#D73027",
          "Total Effect"    = "#2CA02C"
        )
      ) +
      ggplot2::labs(
        x     = NULL,
        y     = "Effect Estimate",
        title = "Causal Mediation Analysis",
        subtitle = sprintf(
          "Proportion mediated: %.1f%%",
          if (is.na(prop)) NaN else prop * 100
        )
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none")

    out$plot <- p
  }

  invisible(out)
}
