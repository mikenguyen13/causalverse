#' Treatment Effect Heterogeneity Visualization
#'
#' Creates publication-ready visualizations for treatment effect heterogeneity
#' (HTE) across subgroups or continuous moderators. Supports forest plots,
#' scatter plots of CATE vs. covariates, and marginal effects plots.
#'
#' @param data A data frame.
#' @param cate_var Character. Name of the column containing estimated CATEs.
#'   Required for CATE-based plots.
#' @param subgroup_var Character. Name of a discrete subgroup variable.
#'   If provided, creates a forest-style subgroup HTE plot.
#' @param moderator_var Character. Name of a continuous moderator variable.
#'   If provided, creates a scatter/LOESS plot of CATE vs. moderator.
#' @param outcome Character. Outcome variable name (for computing raw
#'   subgroup effects when \code{cate_var} is not available).
#' @param treatment Character. Treatment variable name.
#' @param weights Numeric vector. Optional analytical weights.
#' @param overall_effect Numeric or \code{NULL}. Overall ATE to show as
#'   reference line. If \code{NULL}, estimated from data.
#' @param conf_level Numeric. Confidence level. Default \code{0.95}.
#' @param plot_type Character. \code{"forest"} (default for subgroups),
#'   \code{"scatter"} (for continuous moderator), or \code{"violin"}.
#' @param title Character. Plot title.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' library(grf)
#' n <- 500; p <- 5
#' X <- data.frame(matrix(rnorm(n * p), n, p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- pmax(X[, 1], 0) * W + rnorm(n)
#' cf <- causal_forest(as.matrix(X), Y, W)
#' df <- cbind(X, cate = predict(cf)$predictions)
#'
#' # Scatter: CATE vs covariate
#' heterogeneity_plot(df, cate_var = "cate", moderator_var = "X1",
#'                    plot_type = "scatter")
#'
#' # Subgroup forest plot
#' df$subgroup <- ifelse(X[, 1] > 0, "High X1", "Low X1")
#' heterogeneity_plot(df, cate_var = "cate", subgroup_var = "subgroup")
#' }
#'
#' @importFrom stats lm coef vcov qnorm weighted.mean loess
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline
#'   geom_smooth labs theme_minimal geom_violin geom_boxplot
#' @export
heterogeneity_plot <- function(data,
                               cate_var       = NULL,
                               subgroup_var   = NULL,
                               moderator_var  = NULL,
                               outcome        = NULL,
                               treatment      = NULL,
                               weights        = NULL,
                               overall_effect = NULL,
                               conf_level     = 0.95,
                               plot_type      = c("forest", "scatter", "violin"),
                               title          = "Treatment Effect Heterogeneity") {

  plot_type <- match.arg(plot_type)

  if (is.null(weights)) weights <- rep(1, nrow(data))

  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)

  # Overall reference effect
  if (is.null(overall_effect)) {
    if (!is.null(cate_var) && cate_var %in% names(data)) {
      overall_effect <- stats::weighted.mean(data[[cate_var]], weights, na.rm = TRUE)
    } else if (!is.null(outcome) && !is.null(treatment)) {
      D <- as.integer(as.logical(data[[treatment]]))
      Y <- data[[outcome]]
      overall_effect <- mean(Y[D == 1], na.rm = TRUE) - mean(Y[D == 0], na.rm = TRUE)
    } else {
      overall_effect <- 0
    }
  }

  # --- SCATTER: CATE vs continuous moderator ---
  if (plot_type == "scatter") {
    if (is.null(cate_var) || is.null(moderator_var)) {
      stop("Both `cate_var` and `moderator_var` are required for scatter plot.",
           call. = FALSE)
    }
    df_p <- data.frame(
      cate = data[[cate_var]],
      mod  = data[[moderator_var]]
    )
    p <- ggplot2::ggplot(df_p, ggplot2::aes(x = mod, y = cate)) +
      ggplot2::geom_point(alpha = 0.3, size = 1.5, color = "#2166AC") +
      ggplot2::geom_smooth(method = "loess", se = TRUE,
                           color = "#D73027", fill = "#D73027", alpha = 0.2) +
      ggplot2::geom_hline(yintercept = overall_effect, linetype = "dashed",
                          color = "gray40") +
      ggplot2::labs(
        x     = moderator_var,
        y     = "Estimated CATE",
        title = title,
        subtitle = sprintf("Overall ATE = %.4f (dashed)", overall_effect)
      ) +
      ggplot2::theme_minimal(base_size = 12)
    return(p)
  }

  # --- VIOLIN: distribution of CATEs by subgroup ---
  if (plot_type == "violin") {
    if (is.null(cate_var)) {
      stop("`cate_var` required for violin plot.", call. = FALSE)
    }
    df_p <- data.frame(cate = data[[cate_var]])
    if (!is.null(subgroup_var)) {
      df_p$group <- as.character(data[[subgroup_var]])
    } else {
      df_p$group <- "All"
    }
    p <- ggplot2::ggplot(df_p, ggplot2::aes(x = group, y = cate, fill = group)) +
      ggplot2::geom_violin(trim = FALSE, alpha = 0.7) +
      ggplot2::geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.5) +
      ggplot2::geom_hline(yintercept = overall_effect, linetype = "dashed") +
      ggplot2::labs(
        x     = if (!is.null(subgroup_var)) subgroup_var else "",
        y     = "Estimated CATE",
        title = title
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none")
    return(p)
  }

  # --- FOREST: subgroup mean CATE or raw subgroup effects ---
  if (is.null(subgroup_var)) {
    stop("`subgroup_var` required for forest plot.", call. = FALSE)
  }

  groups <- unique(as.character(data[[subgroup_var]]))
  rows <- lapply(groups, function(g) {
    idx <- which(as.character(data[[subgroup_var]]) == g)
    n_g <- length(idx)
    if (!is.null(cate_var) && cate_var %in% names(data)) {
      taus <- data[[cate_var]][idx]
      wts  <- weights[idx]
      est  <- stats::weighted.mean(taus, wts, na.rm = TRUE)
      se   <- stats::sd(taus) / sqrt(n_g)
    } else if (!is.null(outcome) && !is.null(treatment)) {
      D <- as.integer(as.logical(data[[treatment]][idx]))
      Y <- data[[outcome]][idx]
      est  <- mean(Y[D == 1], na.rm = TRUE) - mean(Y[D == 0], na.rm = TRUE)
      nt   <- sum(D == 1); nc <- sum(D == 0)
      se   <- sqrt(stats::var(Y[D == 1], na.rm = TRUE) / nt +
                     stats::var(Y[D == 0], na.rm = TRUE) / nc)
    } else {
      stop("Need `cate_var` or both `outcome` and `treatment`.", call. = FALSE)
    }
    data.frame(
      group    = g,
      n        = n_g,
      estimate = est,
      se       = se,
      ci_lower = est - z_crit * se,
      ci_upper = est + z_crit * se,
      stringsAsFactors = FALSE
    )
  })
  df_forest <- do.call(rbind, rows)
  df_forest$group <- factor(df_forest$group,
                             levels = df_forest$group[order(df_forest$estimate)])

  p <- ggplot2::ggplot(df_forest,
    ggplot2::aes(x = estimate, y = group)) +
    ggplot2::geom_vline(xintercept = overall_effect, linetype = "dashed",
                        color = "gray40") +
    ggplot2::geom_vline(xintercept = 0, color = "gray70") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.3, color = "#2166AC"
    ) +
    ggplot2::geom_point(size = 3.5, color = "#2166AC") +
    ggplot2::labs(
      x     = "Treatment Effect Estimate",
      y     = subgroup_var,
      title = title,
      subtitle = sprintf("Dashed line: overall ATE = %.4f", overall_effect)
    ) +
    ggplot2::theme_minimal(base_size = 12)

  p
}
