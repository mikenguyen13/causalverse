#' Tidy Output from Causal Inference Model Objects
#'
#' Extracts and standardizes estimates, standard errors, confidence intervals,
#' and p-values from a wide range of causal inference model objects into a
#' consistent tidy data frame. Supports \pkg{fixest}, \pkg{ivreg},
#' \pkg{rdrobust}, \pkg{MatchIt}, \pkg{synthdid}, \pkg{grf}, and more.
#'
#' @param model A model object. Supported classes:
#'   \itemize{
#'     \item \code{fixest} (from \pkg{fixest})
#'     \item \code{ivreg} (from \pkg{ivreg})
#'     \item \code{rdrobust} (from \pkg{rdrobust})
#'     \item \code{lm}, \code{glm} (from base R)
#'     \item \code{causal_forest} (from \pkg{grf})
#'     \item \code{synthdid_estimate} (from \pkg{synthdid})
#'     \item Named numeric list with \code{estimate} and \code{se} elements
#'   }
#' @param term Character or \code{NULL}. Specific term/coefficient to extract.
#'   If \code{NULL}, extracts all available terms.
#' @param conf_level Numeric. Confidence level. Default \code{0.95}.
#' @param add_model_info Logical. Add model class, n observations, R-squared
#'   if available. Default \code{TRUE}.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{term}{Term name.}
#'     \item{estimate}{Point estimate.}
#'     \item{std_error}{Standard error.}
#'     \item{t_stat}{t-statistic.}
#'     \item{p_value}{Two-sided p-value.}
#'     \item{ci_lower}{Lower confidence bound.}
#'     \item{ci_upper}{Upper confidence bound.}
#'     \item{n_obs}{Sample size (if available).}
#'     \item{model_class}{Class of the input model.}
#'   }
#'
#' @examples
#' # OLS
#' mod <- lm(mpg ~ am + wt, data = mtcars)
#' tidy_causal(mod, term = "am")
#'
#' # fixest
#' \dontrun{
#' mod2 <- feols(mpg ~ am + wt | cyl, data = mtcars)
#' tidy_causal(mod2)
#' }
#'
#' @importFrom stats coef vcov confint qnorm pnorm
#' @export
tidy_causal <- function(model,
                        term           = NULL,
                        conf_level     = 0.95,
                        add_model_info = TRUE) {

  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  model_class <- class(model)[1]

  # --- Dispatch by class ---

  # Generic list with estimate/se
  if (is.list(model) && all(c("estimate", "se") %in% names(model))) {
    est <- model$estimate
    se  <- model$se
    t_s <- est / se
    pv  <- 2 * stats::pnorm(-abs(t_s))
    return(data.frame(
      term        = if (!is.null(term)) term else "estimate",
      estimate    = est,
      std_error   = se,
      t_stat      = t_s,
      p_value     = pv,
      ci_lower    = est - z_crit * se,
      ci_upper    = est + z_crit * se,
      n_obs       = NA_integer_,
      model_class = "list",
      stringsAsFactors = FALSE
    ))
  }

  # fixest
  if (inherits(model, "fixest")) {
    ct <- fixest::coeftable(model)
    coef_names <- rownames(ct)
    if (!is.null(term)) {
      idx <- match(term, coef_names)
      if (anyNA(idx)) {
        # Try partial matching
        idx <- grep(paste(term, collapse = "|"), coef_names)
      }
      ct <- ct[idx, , drop = FALSE]
      coef_names <- rownames(ct)
    }
    n_obs <- nobs(model)
    df_out <- data.frame(
      term        = coef_names,
      estimate    = ct[, 1],
      std_error   = ct[, 2],
      t_stat      = ct[, 3],
      p_value     = ct[, 4],
      ci_lower    = ct[, 1] - z_crit * ct[, 2],
      ci_upper    = ct[, 1] + z_crit * ct[, 2],
      n_obs       = n_obs,
      model_class = "fixest",
      row.names   = NULL,
      stringsAsFactors = FALSE
    )
    return(df_out)
  }

  # lm / glm
  if (inherits(model, c("lm", "glm"))) {
    ct  <- summary(model)$coefficients
    cfs <- rownames(ct)
    if (!is.null(term)) {
      idx <- match(term, cfs)
      ct  <- ct[idx, , drop = FALSE]
      cfs <- rownames(ct)
    }
    n_obs <- length(stats::residuals(model))
    df_out <- data.frame(
      term        = cfs,
      estimate    = ct[, 1],
      std_error   = ct[, 2],
      t_stat      = ct[, 3],
      p_value     = ct[, 4],
      ci_lower    = ct[, 1] - z_crit * ct[, 2],
      ci_upper    = ct[, 1] + z_crit * ct[, 2],
      n_obs       = n_obs,
      model_class = model_class,
      row.names   = NULL,
      stringsAsFactors = FALSE
    )
    return(df_out)
  }

  # ivreg
  if (inherits(model, "ivreg")) {
    ct  <- summary(model)$coefficients
    cfs <- rownames(ct)
    if (!is.null(term)) {
      idx <- match(term, cfs)
      ct  <- ct[idx, , drop = FALSE]
      cfs <- rownames(ct)
    }
    n_obs <- nrow(model$model)
    df_out <- data.frame(
      term        = cfs,
      estimate    = ct[, 1],
      std_error   = ct[, 2],
      t_stat      = ct[, 3],
      p_value     = ct[, 4],
      ci_lower    = ct[, 1] - z_crit * ct[, 2],
      ci_upper    = ct[, 1] + z_crit * ct[, 2],
      n_obs       = n_obs,
      model_class = "ivreg",
      row.names   = NULL,
      stringsAsFactors = FALSE
    )
    return(df_out)
  }

  # rdrobust
  if (inherits(model, "rdrobust")) {
    est <- model$coef[1]
    se  <- model$se[1]
    pv  <- model$pv[1]
    df_out <- data.frame(
      term        = "RD Effect",
      estimate    = est,
      std_error   = se,
      t_stat      = est / se,
      p_value     = pv,
      ci_lower    = model$ci[1, 1],
      ci_upper    = model$ci[1, 2],
      n_obs       = sum(model$N),
      model_class = "rdrobust",
      stringsAsFactors = FALSE
    )
    return(df_out)
  }

  # grf causal_forest
  if (inherits(model, "causal_forest")) {
    preds <- predict(model, estimate.variance = TRUE)
    ate   <- mean(preds$predictions)
    se    <- sqrt(mean(preds$variance.estimates) / length(preds$predictions))
    pv    <- 2 * stats::pnorm(-abs(ate / se))
    return(data.frame(
      term        = "ATE",
      estimate    = ate,
      std_error   = se,
      t_stat      = ate / se,
      p_value     = pv,
      ci_lower    = ate - z_crit * se,
      ci_upper    = ate + z_crit * se,
      n_obs       = length(model$Y.orig),
      model_class = "causal_forest",
      stringsAsFactors = FALSE
    ))
  }

  # synthdid_estimate
  if (inherits(model, "synthdid_estimate")) {
    est <- as.numeric(model)
    se  <- tryCatch(
      sqrt(vcov(model, method = "placebo")[1, 1]),
      error = function(e) NA_real_
    )
    pv <- if (!is.na(se) && se > 0) 2 * stats::pnorm(-abs(est / se)) else NA_real_
    return(data.frame(
      term        = "SDID Effect",
      estimate    = est,
      std_error   = se,
      t_stat      = if (!is.na(se)) est / se else NA_real_,
      p_value     = pv,
      ci_lower    = est - z_crit * se,
      ci_upper    = est + z_crit * se,
      n_obs       = NA_integer_,
      model_class = "synthdid_estimate",
      stringsAsFactors = FALSE
    ))
  }

  # Fallback: try generic coef/vcov
  tryCatch({
    cf <- stats::coef(model)
    vc <- stats::vcov(model)
    if (!is.null(term)) {
      cf <- cf[term]
      vc <- vc[term, term, drop = FALSE]
    }
    se  <- sqrt(diag(as.matrix(vc)))
    t_s <- cf / se
    pv  <- 2 * stats::pnorm(-abs(t_s))
    data.frame(
      term        = names(cf),
      estimate    = unname(cf),
      std_error   = se,
      t_stat      = t_s,
      p_value     = pv,
      ci_lower    = unname(cf) - z_crit * se,
      ci_upper    = unname(cf) + z_crit * se,
      n_obs       = NA_integer_,
      model_class = model_class,
      row.names   = NULL,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    stop("Cannot extract tidy output from model of class '",
         model_class, "': ", e$message, call. = FALSE)
  })
}
