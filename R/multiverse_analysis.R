#' Multiverse / Specification Curve Analysis
#'
#' Runs a model across all combinations of analytical choices (model
#' specification, sample restrictions, functional forms, etc.) and
#' visualizes the resulting distribution of estimates. This reveals how
#' sensitive conclusions are to reasonable but arbitrary researcher decisions.
#'
#' @param data A data frame.
#' @param outcome Character. Name of the outcome variable.
#' @param treatment Character. Name of the treatment variable.
#' @param choices A named list. Each element is a character vector of
#'   alternative choices for that analytical dimension. Supported keys:
#'   \describe{
#'     \item{\code{controls}}{List of covariate sets to add. Each element
#'       is a character vector of variable names, e.g.
#'       \code{list(c(), c("age"), c("age","female"))}.}
#'     \item{\code{sample_filters}}{Named character vector of filter expressions
#'       (as strings), e.g. \code{c(full="TRUE", adults="age>=18")}.}
#'     \item{\code{outcome_transforms}}{Named character vector of transformations
#'       applied to the outcome, e.g. \code{c(levels="y", log="log(y+1)")}.}
#'     \item{\code{se_types}}{Character vector of SE types:
#'       \code{"OLS"}, \code{"HC1"}, \code{"HC3"}, \code{"cluster"}.}
#'     \item{\code{cluster_var}}{Character. Variable to cluster on (used when
#'       \code{se_types} includes \code{"cluster"}).}
#'   }
#' @param family Character. \code{"gaussian"} (default), \code{"binomial"},
#'   or \code{"poisson"}. Model family.
#' @param alpha Numeric. Significance level for highlighting. Default \code{0.05}.
#' @param sort_by Character. Sort specifications by \code{"estimate"} (default)
#'   or \code{"p_value"}.
#' @param plot Logical. Produce the multiverse plot. Default \code{TRUE}.
#' @param parallel Logical. Run specifications in parallel via
#'   \code{parallel::mclapply}. Default \code{FALSE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{results}}{Data frame of all specification results:
#'       \code{spec_id}, \code{estimate}, \code{std_error}, \code{t_stat},
#'       \code{p_value}, \code{ci_lo}, \code{ci_hi}, \code{significant},
#'       plus one column per analytical dimension.}
#'     \item{\code{summary}}{Summary statistics across all specifications:
#'       median estimate, % significant, % positive, IQR.}
#'     \item{\code{plot}}{A ggplot2 object (if \code{plot = TRUE}).}
#'   }
#'
#' @references
#' Simonsohn, U., Simmons, J. P., & Nelson, L. D. (2020). Specification curve
#' analysis. \emph{Nature Human Behaviour}, 4, 1208-1214.
#'
#' Steegen, S., Tuerlinckx, F., Gelman, A., & Vanpaemel, W. (2016).
#' Increasing transparency through a multiverse analysis.
#' \emph{Perspectives on Psychological Science}, 11(5), 702-712.
#'
#' @examples
#' set.seed(42)
#' n <- 300
#' df <- data.frame(
#'   y      = rnorm(n),
#'   treat  = rbinom(n, 1, 0.5),
#'   age    = runif(n, 18, 65),
#'   female = rbinom(n, 1, 0.5),
#'   income = rnorm(n)
#' )
#'
#' mv <- multiverse_analysis(
#'   data      = df,
#'   outcome   = "y",
#'   treatment = "treat",
#'   choices   = list(
#'     controls        = list(c(), c("age"), c("age", "female"), c("age", "female", "income")),
#'     sample_filters  = c(full = "TRUE", age30plus = "age >= 30"),
#'     outcome_transforms = c(levels = "y")
#'   )
#' )
#' mv$summary
#'
#' @export
multiverse_analysis <- function(data,
                                 outcome,
                                 treatment,
                                 choices        = list(),
                                 family         = "gaussian",
                                 alpha          = 0.05,
                                 sort_by        = c("estimate", "p_value"),
                                 plot           = TRUE,
                                 parallel       = FALSE) {

  sort_by <- match.arg(sort_by)

  # --- Parse choices ---
  ctrl_sets    <- if (!is.null(choices$controls)) choices$controls else list(c())
  filters      <- if (!is.null(choices$sample_filters)) choices$sample_filters else c(full = "TRUE")
  transforms   <- if (!is.null(choices$outcome_transforms)) choices$outcome_transforms else c(levels = outcome)
  se_types     <- if (!is.null(choices$se_types)) choices$se_types else "OLS"
  cluster_var  <- choices$cluster_var

  # Build all combinations
  combos <- expand.grid(
    ctrl_idx   = seq_along(ctrl_sets),
    filter_idx = seq_along(filters),
    trans_idx  = seq_along(transforms),
    se_idx     = seq_along(se_types),
    stringsAsFactors = FALSE
  )

  run_one <- function(i) {
    ctrl_idx  <- combos$ctrl_idx[i]
    filt_idx  <- combos$filter_idx[i]
    trans_idx <- combos$trans_idx[i]
    se_idx    <- combos$se_idx[i]

    ctrl_vars  <- ctrl_sets[[ctrl_idx]]
    filt_expr  <- filters[filt_idx]
    trans_expr <- transforms[trans_idx]
    se_type    <- se_types[se_idx]

    # Apply sample filter
    filtered <- tryCatch(
      data[eval(parse(text = filt_expr), envir = data), , drop = FALSE],
      error = function(e) data
    )
    if (nrow(filtered) < 10) return(NULL)

    # Apply outcome transform
    filtered$.outcome_mv <- tryCatch(
      eval(parse(text = trans_expr), envir = filtered),
      error = function(e) filtered[[outcome]]
    )

    # Build formula
    rhs <- paste(c(treatment, ctrl_vars), collapse = " + ")
    fml <- as.formula(paste(".outcome_mv ~", rhs))

    # Fit model
    fit <- tryCatch(
      glm(fml, data = filtered, family = family),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)

    # Compute SE
    coef_mat <- tryCatch({
      if (se_type == "OLS") {
        sm <- summary(fit)$coefficients
      } else if (se_type %in% c("HC1", "HC3")) {
        if (requireNamespace("sandwich", quietly = TRUE) &&
            requireNamespace("lmtest", quietly = TRUE)) {
          vcv <- sandwich::vcovHC(fit, type = se_type)
          sm  <- lmtest::coeftest(fit, vcov = vcv)
        } else {
          sm <- summary(fit)$coefficients
        }
      } else if (se_type == "cluster" && !is.null(cluster_var) &&
                 requireNamespace("sandwich", quietly = TRUE) &&
                 requireNamespace("lmtest", quietly = TRUE)) {
        cl  <- filtered[[cluster_var]]
        vcv <- sandwich::vcovCL(fit, cluster = cl)
        sm  <- lmtest::coeftest(fit, vcov = vcv)
      } else {
        sm <- summary(fit)$coefficients
      }
      sm
    }, error = function(e) summary(fit)$coefficients)

    # Extract treatment row
    treat_row <- tryCatch(coef_mat[treatment, ], error = function(e) NULL)
    if (is.null(treat_row) || length(treat_row) < 4) return(NULL)

    est <- treat_row[1]
    se  <- treat_row[2]
    tst <- treat_row[3]
    pv  <- treat_row[4]

    data.frame(
      spec_id    = i,
      estimate   = est,
      std_error  = se,
      t_stat     = tst,
      p_value    = pv,
      ci_lo      = est - qnorm(1 - alpha / 2) * se,
      ci_hi      = est + qnorm(1 - alpha / 2) * se,
      significant= pv < alpha,
      controls   = paste(ctrl_vars, collapse = ", "),
      filter     = names(filters)[filt_idx],
      transform  = names(transforms)[trans_idx],
      se_type    = se_type,
      n_obs      = nrow(filtered),
      stringsAsFactors = FALSE
    )
  }

  # Run all specifications
  idx_list <- seq_len(nrow(combos))
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    res_list <- parallel::mclapply(idx_list, run_one, mc.cores = parallel::detectCores() - 1)
  } else {
    res_list <- lapply(idx_list, run_one)
  }

  results <- do.call(rbind, Filter(Negate(is.null), res_list))
  if (is.null(results) || nrow(results) == 0) {
    stop("No valid specifications could be estimated.")
  }

  # Sort
  results <- results[order(if (sort_by == "estimate") results$estimate else results$p_value), ]
  results$rank <- seq_len(nrow(results))

  # Summary
  summ <- data.frame(
    n_specs        = nrow(results),
    median_est     = median(results$estimate, na.rm = TRUE),
    iqr_est        = stats::IQR(results$estimate, na.rm = TRUE),
    pct_positive   = mean(results$estimate > 0, na.rm = TRUE) * 100,
    pct_significant= mean(results$significant, na.rm = TRUE) * 100,
    min_est        = min(results$estimate, na.rm = TRUE),
    max_est        = max(results$estimate, na.rm = TRUE)
  )

  out <- list(results = results, summary = summ, plot = NULL)

  if (plot) {
    # Top panel: estimates with CIs, colored by significance
    p_top <- ggplot2::ggplot(results,
      ggplot2::aes(x = rank, y = estimate,
                   ymin = ci_lo, ymax = ci_hi,
                   color = significant)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_linerange(alpha = 0.6) +
      ggplot2::geom_point(size = 1.2) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = "#2166ac", "FALSE" = "#d1d1d1"),
        labels = c("TRUE" = "Significant", "FALSE" = "Not significant"),
        name   = NULL
      ) +
      ggplot2::labs(
        title = "Multiverse / Specification Curve Analysis",
        x     = NULL,
        y     = paste("Treatment Effect on", outcome)
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank()) +
      causalverse::ama_theme()

    # Bottom panel: specification tiles
    long_specs <- tidyr::pivot_longer(
      results[, c("rank", "controls", "filter", "transform", "se_type")],
      cols = -rank, names_to = "dimension", values_to = "choice"
    )
    p_bot <- ggplot2::ggplot(long_specs,
      ggplot2::aes(x = rank, y = choice, color = dimension)) +
      ggplot2::geom_point(shape = 15, size = 0.8, alpha = 0.7) +
      ggplot2::facet_grid(dimension ~ ., scales = "free_y", space = "free") +
      ggplot2::labs(x = "Specification (sorted)", y = NULL) +
      ggplot2::theme(legend.position = "none",
                     strip.text.y   = ggplot2::element_text(angle = 0)) +
      causalverse::ama_theme()

    # Combine with patchwork if available
    if (requireNamespace("patchwork", quietly = TRUE)) {
      out$plot <- patchwork::wrap_plots(p_top, p_bot, ncol = 1, heights = c(2, 1))
    } else {
      out$plot <- p_top
      message("Install 'patchwork' for a combined two-panel multiverse plot.")
    }
  }

  invisible(out)
}
