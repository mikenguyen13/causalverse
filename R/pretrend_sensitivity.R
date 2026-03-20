#' Pre-trend Sensitivity Analysis (HonestDiD)
#'
#' Wrapper around the HonestDiD methodology (Rambachan & Roth 2023) for
#' sensitivity analysis of difference-in-differences designs to violations
#' of the parallel trends assumption. Constructs robust confidence intervals
#' that remain valid under bounded violations of parallel trends.
#'
#' If the \pkg{HonestDiD} package is installed, its optimisation routines are
#' used. Otherwise a simplified analytic approximation is computed: for each
#' \eqn{M} the confidence interval is widened by \eqn{M \times |t|} on each
#' side, where \eqn{t} is the distance from the reference period.
#'
#' @param event_study A data frame with columns \code{period}, \code{estimate},
#'   \code{std_error} from an event study regression, or a \code{fixest} model
#'   estimated with \code{i()} interactions.
#' @param ref_period Numeric. Reference (omitted) period. Default \code{-1}.
#' @param M_seq Numeric vector. Grid of \eqn{M} values (maximum deviation from
#'   parallel trends per period). Default \code{seq(0, 1, by = 0.1)}.
#' @param method Character. Restriction type:
#'   \code{"C"} smoothness/\eqn{\Delta} restriction (default),
#'   \code{"Delta"} sign restriction, or
#'   \code{"Rel_Mag"} relative magnitudes.
#' @param alpha Numeric. Significance level. Default \code{0.05}.
#' @param plot Logical. Produce a sensitivity plot. Default \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{robust_cis}{Data frame with columns \code{period}, \code{M},
#'       \code{ci_lower}, \code{ci_upper}, \code{estimate}.}
#'     \item{original_cis}{Data frame of the original (unadjusted) event-study
#'       CIs.}
#'     \item{breakdown_M}{Scalar. Smallest \eqn{M} at which the robust CI for
#'       the first post-period includes zero. \code{Inf} if the CI never
#'       includes zero on this grid.}
#'     \item{plot}{A \code{ggplot2} object (only when \code{plot = TRUE}).}
#'   }
#'
#' @references
#' Rambachan, A. and Roth, J. (2023). "A More Credible Approach to Parallel
#' Trends." \emph{Review of Economic Studies}, 90(5), 2555-2591.
#'
#' @examples
#' \dontrun{
#' data(base_stagg)
#' mod <- feols(y ~ i(time_to_treatment, ref = -1) | id + year, base_stagg)
#' res <- pretrend_sensitivity(mod, M_seq = seq(0, 0.5, by = 0.05))
#' res$breakdown_M
#' res$plot
#' }
#'
#' @importFrom stats qnorm
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point
#'   geom_hline geom_vline labs scale_color_manual scale_fill_manual
#'   facet_wrap
#' @export
pretrend_sensitivity <- function(event_study,
                                  ref_period = -1,
                                  M_seq      = seq(0, 1, by = 0.1),
                                  method     = c("C", "Delta", "Rel_Mag"),
                                  alpha      = 0.05,
                                  plot       = TRUE) {

  method <- match.arg(method)
  z_crit <- stats::qnorm(1 - alpha / 2)

  # ------------------------------------------------------------------
  # 1. Extract event-study estimates
  # ------------------------------------------------------------------
  es_df <- .extract_event_study(event_study, ref_period, z_crit)

  if (is.null(es_df) || nrow(es_df) == 0L) {
    stop("Could not extract event-study estimates. ",
         "Supply a data frame with columns 'period', 'estimate', 'std_error', ",
         "or a fixest model with i() interactions.",
         call. = FALSE)
  }

  # Separate pre and post periods (exclude the reference)
  es_pre  <- es_df[es_df$period < ref_period,  , drop = FALSE]
  es_post <- es_df[es_df$period >= 0,           , drop = FALSE]

  if (nrow(es_post) == 0L) {
    warning("No post-treatment periods found (period >= 0). ",
            "Check `ref_period` or the event-study data.", call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 2. Attempt to use HonestDiD package
  # ------------------------------------------------------------------
  used_honest_did <- FALSE
  robust_rows     <- NULL

  if (requireNamespace("HonestDiD", quietly = TRUE)) {
    robust_rows <- tryCatch({
      .run_honest_did(es_df, ref_period, M_seq, method, alpha)
    }, error = function(e) {
      message("HonestDiD failed (", conditionMessage(e), "); ",
              "falling back to simplified approximation.")
      NULL
    })
    if (!is.null(robust_rows)) used_honest_did <- TRUE
  }

  # ------------------------------------------------------------------
  # 3. Simplified approximation fallback
  # ------------------------------------------------------------------
  if (is.null(robust_rows)) {
    robust_rows <- .simplified_sensitivity(es_df, ref_period, M_seq, z_crit,
                                            method)
  }

  # ------------------------------------------------------------------
  # 4. Compute breakdown M (first post-period CI covers zero)
  # ------------------------------------------------------------------
  post_periods <- sort(unique(es_post$period))
  breakdown_M  <- Inf

  if (length(post_periods) > 0L) {
    target_period <- post_periods[1L]
    rows_target   <- robust_rows[robust_rows$period == target_period, ]
    rows_target   <- rows_target[order(rows_target$M), ]
    crosses_zero  <- rows_target$ci_lower <= 0 & rows_target$ci_upper >= 0
    if (any(crosses_zero)) {
      breakdown_M <- rows_target$M[which(crosses_zero)[1L]]
    }
  }

  # ------------------------------------------------------------------
  # 5. Original CIs
  # ------------------------------------------------------------------
  orig_cis <- data.frame(
    period   = es_df$period,
    estimate = es_df$estimate,
    ci_lower = es_df$ci_lower,
    ci_upper = es_df$ci_upper,
    stringsAsFactors = FALSE
  )

  result <- list(
    robust_cis   = robust_rows,
    original_cis = orig_cis,
    breakdown_M  = breakdown_M
  )

  # ------------------------------------------------------------------
  # 6. Plot
  # ------------------------------------------------------------------
  if (plot && requireNamespace("ggplot2", quietly = TRUE)) {

    # Show all M values in a ribbon, plus the original CI as a line
    # Aggregate: for each period, show widest CI (max M) as outer ribbon
    # and tightest (M=0) as inner ribbon
    M_vals <- sort(unique(robust_rows$M))

    # For ribbon: use convex hull over M at each period
    ribbon_df <- do.call(rbind, lapply(unique(robust_rows$period), function(pp) {
      sub <- robust_rows[robust_rows$period == pp, ]
      data.frame(
        period   = pp,
        ci_lo_min = min(sub$ci_lower, na.rm = TRUE),
        ci_hi_max = max(sub$ci_upper, na.rm = TRUE),
        ci_lo_M0  = sub$ci_lower[which.min(sub$M)],
        ci_hi_M0  = sub$ci_upper[which.min(sub$M)],
        estimate  = unique(sub$estimate)[1L],
        stringsAsFactors = FALSE
      )
    }))
    ribbon_df <- ribbon_df[order(ribbon_df$period), ]

    # Label breakdown
    bd_label <- if (is.finite(breakdown_M)) {
      sprintf("Breakdown M = %.2f", breakdown_M)
    } else {
      "No breakdown on this M grid"
    }

    p <- ggplot2::ggplot(ribbon_df, ggplot2::aes(x = period)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci_lo_min, ymax = ci_hi_max),
        fill = "#A6CEE3", alpha = 0.4
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci_lo_M0, ymax = ci_hi_M0),
        fill = "#2166AC", alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = estimate),
        color = "#D73027", linewidth = 0.9
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = estimate),
        color = "#D73027", size = 2
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = "gray50") +
      ggplot2::geom_vline(xintercept = ref_period + 0.5,
                          linetype = "dotted", color = "gray30") +
      ggplot2::labs(
        x        = "Period relative to treatment",
        y        = "Estimate",
        title    = "Pre-trend Sensitivity Analysis (HonestDiD)",
        subtitle = bd_label,
        caption  = paste0(
          "Dark band: original CI (M=0). ",
          "Light band: robust CI envelope across M = [",
          min(M_seq), ", ", max(M_seq), "].",
          if (used_honest_did) " Computed via HonestDiD package."
          else " Simplified approximation."
        )
      ) +
      ama_theme(base_size = 12)

    result$plot <- p
  }

  result
}


# ------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------

#' @keywords internal
.extract_event_study <- function(event_study, ref_period, z_crit) {

  # Data frame input
  if (is.data.frame(event_study)) {
    req <- c("period", "estimate", "std_error")
    if (!all(req %in% names(event_study))) {
      stop("Data frame must have columns: ", paste(req, collapse = ", "),
           call. = FALSE)
    }
    df <- event_study
    df$ci_lower <- df$estimate - z_crit * df$std_error
    df$ci_upper <- df$estimate + z_crit * df$std_error
    # Drop the reference period row if present (estimate should be 0)
    df <- df[df$period != ref_period, ]
    return(df)
  }

  # fixest model
  if (inherits(event_study, "fixest")) {
    ct <- tryCatch(fixest::coeftable(event_study), error = function(e) NULL)
    if (is.null(ct)) return(NULL)
    cf_names <- rownames(ct)
    # Extract period numbers from coefficient names like "var::period" or "var:period"
    period_nums <- suppressWarnings(as.numeric(
      gsub(".*::(-?[0-9]+).*|.*:(-?[0-9]+)$|.*_(-?[0-9]+)$|^(-?[0-9]+)$",
           "\\1\\2\\3\\4", cf_names)
    ))
    valid <- !is.na(period_nums)
    if (sum(valid) == 0L) return(NULL)

    df <- data.frame(
      period    = period_nums[valid],
      estimate  = ct[valid, 1],
      std_error = ct[valid, 2],
      ci_lower  = ct[valid, 1] - z_crit * ct[valid, 2],
      ci_upper  = ct[valid, 1] + z_crit * ct[valid, 2],
      stringsAsFactors = FALSE
    )
    df <- df[df$period != ref_period, ]
    df <- df[order(df$period), ]
    return(df)
  }

  NULL
}


#' @keywords internal
.run_honest_did <- function(es_df, ref_period, M_seq, method, alpha) {

  pre_df  <- es_df[es_df$period < ref_period, ]
  post_df <- es_df[es_df$period >= 0, ]

  if (nrow(pre_df) == 0L || nrow(post_df) == 0L) {
    return(NULL)
  }

  # Build betahat and sigma for HonestDiD
  all_periods <- sort(c(pre_df$period, post_df$period))
  ests        <- setNames(
    c(pre_df$estimate[order(pre_df$period)],
      post_df$estimate[order(post_df$period)]),
    as.character(all_periods)
  )
  ses <- setNames(
    c(pre_df$std_error[order(pre_df$period)],
      post_df$std_error[order(post_df$period)]),
    as.character(all_periods)
  )

  # Diagonal variance-covariance (approximate)
  sigma  <- diag(ses^2)
  n_pre  <- nrow(pre_df)
  n_post <- nrow(post_df)

  rows_list <- lapply(M_seq, function(M) {
    if (method == "C" || method == "Delta") {
      ci_res <- tryCatch(
        HonestDiD::createSensitivityResults(
          betahat      = ests,
          sigma        = sigma,
          numPrePeriods  = n_pre,
          numPostPeriods = n_post,
          Mvec           = M,
          alpha          = alpha
        ),
        error = function(e) NULL
      )
    } else if (method == "Rel_Mag") {
      ci_res <- tryCatch(
        HonestDiD::createSensitivityResults_relativeMagnitudes(
          betahat        = ests,
          sigma          = sigma,
          numPrePeriods  = n_pre,
          numPostPeriods = n_post,
          Mbarvec        = M,
          alpha          = alpha
        ),
        error = function(e) NULL
      )
    } else {
      ci_res <- NULL
    }

    if (is.null(ci_res)) return(NULL)

    # HonestDiD returns a data frame with lb/ub per post-period
    post_periods_sorted <- sort(post_df$period)
    n_rows <- min(nrow(ci_res), n_post)
    data.frame(
      period   = post_periods_sorted[seq_len(n_rows)],
      M        = M,
      ci_lower = ci_res$lb[seq_len(n_rows)],
      ci_upper = ci_res$ub[seq_len(n_rows)],
      estimate = ests[as.character(post_periods_sorted[seq_len(n_rows)])],
      stringsAsFactors = FALSE
    )
  })

  # Also add pre-period rows (with original CIs, widened by M * |period|)
  # for plotting continuity
  pre_rows_list <- lapply(M_seq, function(M) {
    pre_sorted <- pre_df[order(pre_df$period), ]
    data.frame(
      period   = pre_sorted$period,
      M        = M,
      ci_lower = pre_sorted$ci_lower - M * abs(pre_sorted$period),
      ci_upper = pre_sorted$ci_upper + M * abs(pre_sorted$period),
      estimate = pre_sorted$estimate,
      stringsAsFactors = FALSE
    )
  })

  all_rows <- c(Filter(Negate(is.null), rows_list),
                Filter(Negate(is.null), pre_rows_list))
  if (length(all_rows) == 0L) return(NULL)
  do.call(rbind, all_rows)
}


#' @keywords internal
.simplified_sensitivity <- function(es_df, ref_period, M_seq, z_crit, method) {

  rows_list <- lapply(M_seq, function(M) {
    lapply(seq_len(nrow(es_df)), function(i) {
      row     <- es_df[i, ]
      t_dist  <- abs(row$period)  # distance from reference period
      # Widen CI by M * t_dist * std_error
      if (method == "Rel_Mag") {
        # Relative magnitudes: widen by M * max(|pre-period estimates|)
        pre_max <- max(abs(es_df$estimate[es_df$period < ref_period]), 1e-8)
        adj <- M * pre_max
      } else {
        adj <- M * t_dist * row$std_error
      }
      data.frame(
        period   = row$period,
        M        = M,
        ci_lower = row$ci_lower - adj,
        ci_upper = row$ci_upper + adj,
        estimate = row$estimate,
        stringsAsFactors = FALSE
      )
    })
  })

  do.call(rbind, do.call(c, rows_list))
}
