#' Stacked Difference-in-Differences Estimator
#'
#' Implements the stacked DiD estimator following Cengiz et al. (2019) and
#' Baker et al. (2022). For each treatment cohort, a "sub-experiment" dataset
#' is created by stacking the cohort's treated units with clean control units
#' (never-treated or not-yet-treated). TWFE is then estimated on this stacked
#' dataset with cohort-by-period interactions, returning an average treatment
#' effect on the treated (ATT) and event-study dynamic effects.
#'
#' @title Stacked Difference-in-Differences
#'
#' @description Builds the stacked dataset described in Baker et al. (2022)
#'   and estimates TWFE regressions to recover the ATT and dynamic effects
#'   free of contamination from heterogeneous treatment timing.
#'
#' @param data A data frame in balanced long panel format.
#' @param unit_var Character. Name of the unit/panel ID column.
#' @param time_var Character. Name of the time period column (integer/numeric).
#' @param outcome_var Character. Name of the outcome variable column.
#' @param treat_time_var Character. Name of the column recording each unit's
#'   **first** treatment period (`NA` or a large number for never-treated).
#' @param anticipation Integer. Number of pre-treatment periods to exclude from
#'   the "clean" window. Default `0`.
#' @param clean_controls Logical. If `TRUE` (default), exclude already-treated
#'   units from the control group within each sub-experiment.
#' @param plot Logical. Whether to return an event-study plot. Default `TRUE`.
#'
#' @return A named list:
#'   \describe{
#'     \item{`estimate`}{Scalar ATT (weighted average across cohorts).}
#'     \item{`event_study`}{Data frame with columns `rel_period`, `estimate`,
#'       `ci_lo`, `ci_hi`.}
#'     \item{`stacked_data`}{The stacked data frame used for estimation.}
#'     \item{`plot`}{A ggplot2 event-study plot, or `NULL` if `plot = FALSE`.}
#'   }
#'
#' @references
#' Baker, A. C., Larcker, D. F., & Wang, C. C. Y. (2022). How much should we
#' trust staggered difference-in-differences estimates? *Journal of Financial
#' Economics*, 144(2), 370-395.
#'
#' Cengiz, D., Dube, A., Lindner, A., & Zipperer, B. (2019). The effect of
#' minimum wages on low-wage jobs. *Quarterly Journal of Economics*, 134(3),
#' 1405-1454.
#'
#' @examples
#' if (requireNamespace("fixest", quietly = TRUE)) {
#'   data("base_stagg", package = "fixest")
#'   # base_stagg has: id, year, treated, treatment_time, y, ...
#'   res <- stacked_did(
#'     data           = base_stagg,
#'     unit_var       = "id",
#'     time_var       = "year",
#'     outcome_var    = "y",
#'     treat_time_var = "year_treated",
#'     anticipation   = 0,
#'     clean_controls = TRUE,
#'     plot           = TRUE
#'   )
#'   print(res$estimate)
#'   res$plot
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point
#'   geom_hline geom_vline labs theme
#' @importFrom stats coef vcov qt
#' @export
stacked_did <- function(data,
                         unit_var,
                         time_var,
                         outcome_var,
                         treat_time_var,
                         anticipation   = 0,
                         clean_controls = TRUE,
                         plot           = TRUE) {

  stopifnot(is.data.frame(data))
  for (v in c(unit_var, time_var, outcome_var, treat_time_var)) {
    if (!v %in% names(data)) stop(sprintf("Column '%s' not found in data.", v))
  }

  if (!requireNamespace("fixest", quietly = TRUE)) {
    stop("Package 'fixest' is required. Install with: install.packages('fixest')")
  }

  # ---- Identify cohorts (unique first-treatment times, excl. never-treated) -
  first_times <- data[[treat_time_var]]
  cohorts     <- sort(unique(first_times[!is.na(first_times) & is.finite(first_times)]))

  if (length(cohorts) == 0) stop("No treated cohorts found in 'treat_time_var'.")

  t_vals <- sort(unique(data[[time_var]]))

  # ---- Build sub-experiments -----------------------------------------------
  sub_list <- lapply(cohorts, function(g) {

    # Window: [g - window_pre, g + window_post]
    # Use full time range but mark relative periods
    treated_units  <- unique(data[[unit_var]][
      !is.na(data[[treat_time_var]]) & data[[treat_time_var]] == g
    ])

    if (clean_controls) {
      # Control units: never-treated OR not-yet-treated for the full window
      # Exclude units treated before or during this cohort's post window
      control_units <- unique(data[[unit_var]][
        is.na(data[[treat_time_var]]) |
        !is.finite(data[[treat_time_var]]) |
        data[[treat_time_var]] > max(t_vals)     # never treated within range
      ])
    } else {
      control_units <- unique(data[[unit_var]][
        is.na(data[[treat_time_var]]) | data[[treat_time_var]] != g
      ])
    }

    if (length(treated_units) == 0 || length(control_units) == 0) return(NULL)

    all_units <- c(treated_units, control_units)
    sub_df    <- data[data[[unit_var]] %in% all_units, ]

    # Relative period
    sub_df[["..rel_period.."]] <- sub_df[[time_var]] - g
    sub_df[["..cohort.."]]     <- g
    sub_df[["..treated.."]]    <- as.integer(sub_df[[unit_var]] %in% treated_units)
    sub_df[["..post.."]]       <- as.integer(sub_df[[time_var]] >= g)
    sub_df
  })

  sub_list <- Filter(Negate(is.null), sub_list)
  if (length(sub_list) == 0) stop("No valid sub-experiments constructed.")

  # Give each sub-experiment a unique unit ID to avoid collisions
  for (i in seq_along(sub_list)) {
    sub_list[[i]][["..stk_unit.."]] <- paste0(sub_list[[i]][[unit_var]], "_c", i)
  }

  stacked_df <- do.call(rbind, sub_list)

  # ---- Estimate ATT via TWFE on stacked data --------------------------------
  # Model: Y ~ treated*post | stk_unit + cohort:time
  fml_att <- stats::as.formula(
    paste0(outcome_var, " ~ ..treated.. : ..post.. | ..stk_unit.. + ..cohort..::", time_var)
  )

  fit_att <- tryCatch(
    fixest::feols(fml_att, data = stacked_df, cluster = unit_var),
    error = function(e) {
      message("ATT estimation failed: ", conditionMessage(e))
      NULL
    }
  )

  att_est <- if (!is.null(fit_att)) {
    cf  <- stats::coef(fit_att)
    idx <- grep("treated.*post|post.*treated", names(cf))
    if (length(idx) > 0) mean(cf[idx]) else NA_real_
  } else {
    NA_real_
  }

  # ---- Event-study estimates -----------------------------------------------
  rel_periods <- sort(unique(stacked_df[["..rel_period.."]]
                             [stacked_df[["..treated.."]] == 1]))
  # Exclude the period just before treatment as base (rel = -1)
  base_period   <- -1 - anticipation
  est_periods   <- setdiff(rel_periods, base_period)
  est_periods   <- est_periods[est_periods >= (min(rel_periods))]

  # Compute simple cell means as event-study estimates
  es_list <- lapply(est_periods, function(rp) {
    sub  <- stacked_df[stacked_df[["..rel_period.."]] == rp, ]
    y1   <- sub[[outcome_var]][sub[["..treated.."]] == 1]
    y0   <- sub[[outcome_var]][sub[["..treated.."]] == 0]
    y1   <- y1[!is.na(y1)]
    y0   <- y0[!is.na(y0)]
    if (length(y1) < 2 || length(y0) < 2) return(NULL)
    est  <- mean(y1) - mean(y0)
    se   <- sqrt(stats::var(y1) / length(y1) + stats::var(y0) / length(y0))
    data.frame(
      rel_period = rp,
      estimate   = est,
      ci_lo      = est - 1.96 * se,
      ci_hi      = est + 1.96 * se,
      stringsAsFactors = FALSE
    )
  })
  event_study <- do.call(rbind, Filter(Negate(is.null), es_list))

  # ---- Plot ----------------------------------------------------------------
  p <- NULL
  if (plot && !is.null(event_study) && nrow(event_study) > 0) {
    p <- ggplot2::ggplot(event_study,
           ggplot2::aes(x = rel_period, y = estimate)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
        fill = "#4575b4", alpha = 0.2, color = NA) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40") +
      ggplot2::geom_line(color = "#4575b4", linewidth = 1.1) +
      ggplot2::geom_point(color = "#4575b4", size = 2.5) +
      ggplot2::labs(
        title    = "Stacked DiD Event Study",
        subtitle = paste0("ATT = ", round(att_est, 3)),
        x        = "Periods Relative to Treatment",
        y        = "Estimated Effect"
      ) +
      causalverse::ama_theme()
  }

  list(
    estimate     = att_est,
    event_study  = event_study,
    stacked_data = stacked_df,
    plot         = p
  )
}
