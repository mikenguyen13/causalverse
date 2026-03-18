#' Compute Abnormal Returns and CARs for Finance Event Studies
#'
#' A self-contained function that estimates the market model, computes
#' abnormal returns (AR), cumulative abnormal returns (CAR), and
#' performs standard statistical tests (t-test, Patell, BMP) on the
#' results. Designed as a one-stop function for finance event studies.
#'
#' @param returns Data frame in long format with columns for firm ID,
#'   event time (relative to event), firm return, and market return.
#' @param firm_var Character. Column name for firm identifier. Default
#'   \code{"firm_id"}.
#' @param time_var Character. Column name for event time (integer, 0 =
#'   event day). Default \code{"event_time"}.
#' @param ret_var Character. Column name for firm return. Default
#'   \code{"ret"}.
#' @param mkt_var Character. Column name for market return. Default
#'   \code{"market_ret"}.
#' @param est_window Integer vector of length 2. Estimation window in
#'   event time. Default \code{c(-200, -11)}.
#' @param event_window Integer vector of length 2. Event window in
#'   event time. Default \code{c(-5, 5)}.
#' @param car_window Integer vector of length 2. Window for CAR
#'   aggregation in statistical tests. Default \code{c(-1, 1)}.
#' @param model Character. Benchmark model: \code{"market"} (default),
#'   \code{"mean"} (constant mean), or \code{"market_adj"} (market-
#'   adjusted, alpha=0 and beta=1).
#' @param test Character. Statistical test: \code{"ttest"} (default),
#'   \code{"patell"}, or \code{"bmp"}.
#' @param plot Logical. Produce AAR and CAAR plots. Default \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{parameters}{Data frame of firm-level model parameters.}
#'     \item{ar_data}{Data frame with firm-level abnormal returns.}
#'     \item{aar_series}{Data frame: event-time AAR and CAAR.}
#'     \item{car_results}{Data frame: firm-level CARs for the CAR window.}
#'     \item{test_results}{Named list with test statistic, p-value, n.}
#'     \item{plot_aar}{ggplot2: AAR/CAAR time series.}
#'     \item{plot_car_dist}{ggplot2: CAR histogram.}
#'   }
#'
#' @references
#' MacKinlay, A. C. (1997). Event studies in economics and finance.
#' \emph{Journal of Economic Literature}, 35(1), 13-39.
#'
#' Brown, S. J., & Warner, J. B. (1985). Using daily stock returns:
#' The case of event studies. \emph{Journal of Financial Economics},
#' 14(1), 3-31.
#'
#' @examples
#' \dontrun{
#' # Using simulated data (see h_event_study vignette for full example)
#' result <- event_study_finance(
#'   returns      = sim_data,
#'   car_window   = c(-1, 1),
#'   model        = "market",
#'   test         = "bmp"
#' )
#' result$test_results
#' result$plot_aar
#' }
#'
#' @importFrom stats lm coef residuals sd qt pt
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_hline geom_vline
#'   geom_ribbon labs theme_minimal
#' @export
event_study_finance <- function(returns,
                                firm_var     = "firm_id",
                                time_var     = "event_time",
                                ret_var      = "ret",
                                mkt_var      = "market_ret",
                                est_window   = c(-200, -11),
                                event_window = c(-5, 5),
                                car_window   = c(-1, 1),
                                model        = c("market", "mean", "market_adj"),
                                test         = c("ttest", "patell", "bmp"),
                                plot         = TRUE) {

  model <- match.arg(model)
  test  <- match.arg(test)

  req_cols <- c(firm_var, time_var, ret_var)
  if (model %in% c("market", "market_adj")) req_cols <- c(req_cols, mkt_var)
  missing <- setdiff(req_cols, names(returns))
  if (length(missing) > 0) {
    stop("Columns not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  df <- data.frame(
    firm  = returns[[firm_var]],
    t     = returns[[time_var]],
    ret   = returns[[ret_var]],
    mkt   = if (mkt_var %in% names(returns)) returns[[mkt_var]] else NA_real_,
    stringsAsFactors = FALSE
  )

  firms <- unique(df$firm)

  # --- Step 1: Estimate model parameters ---
  params <- do.call(rbind, lapply(firms, function(f) {
    est_df <- df[df$firm == f & df$t >= est_window[1] & df$t <= est_window[2], ]
    if (nrow(est_df) < 20) return(NULL)

    if (model == "market") {
      m  <- stats::lm(ret ~ mkt, data = est_df)
      alpha <- stats::coef(m)[1]
      beta  <- stats::coef(m)[2]
      sigma <- summary(m)$sigma
    } else if (model == "mean") {
      alpha <- mean(est_df$ret, na.rm = TRUE)
      beta  <- 0
      sigma <- stats::sd(est_df$ret, na.rm = TRUE)
    } else {
      alpha <- 0
      beta  <- 1
      sigma <- stats::sd(est_df$ret - est_df$mkt, na.rm = TRUE)
    }

    data.frame(firm = f, alpha = alpha, beta = beta, sigma = sigma,
               n_est = nrow(est_df), stringsAsFactors = FALSE)
  }))
  params <- params[!is.null(params), ]

  # --- Step 2: Compute abnormal returns ---
  evt_df <- df[df$t >= event_window[1] & df$t <= event_window[2], ]
  ar_data <- merge(evt_df, params, by = "firm")
  ar_data$ar <- ar_data$ret - ar_data$alpha - ar_data$beta * ar_data$mkt

  # Standardized AR (for Patell/BMP)
  ar_data$sar <- ar_data$ar / ar_data$sigma

  # --- Step 3: Aggregate ---
  aar_series <- do.call(rbind, lapply(sort(unique(ar_data$t)), function(d) {
    sub <- ar_data[ar_data$t == d, ]
    data.frame(
      event_time = d,
      aar        = mean(sub$ar, na.rm = TRUE),
      se_aar     = stats::sd(sub$ar, na.rm = TRUE) / sqrt(nrow(sub)),
      n          = nrow(sub)
    )
  }))
  aar_series$caar <- cumsum(aar_series$aar)
  aar_series$ci_lo <- aar_series$aar - 1.96 * aar_series$se_aar
  aar_series$ci_hi <- aar_series$aar + 1.96 * aar_series$se_aar

  # --- Step 4: CAR for test window ---
  car_results <- do.call(rbind, lapply(firms, function(f) {
    sub <- ar_data[ar_data$firm == f & ar_data$t >= car_window[1] &
                     ar_data$t <= car_window[2], ]
    if (nrow(sub) == 0) return(NULL)
    s  <- params$sigma[params$firm == f]
    data.frame(
      firm   = f,
      car    = sum(sub$ar, na.rm = TRUE),
      scar   = sum(sub$sar, na.rm = TRUE) / sqrt(nrow(sub)),
      sigma  = s,
      n_days = nrow(sub),
      stringsAsFactors = FALSE
    )
  }))
  car_results <- car_results[!is.null(car_results), ]

  # --- Step 5: Statistical test ---
  n_firms_t <- nrow(car_results)
  mean_car  <- mean(car_results$car, na.rm = TRUE)
  sd_car    <- stats::sd(car_results$car, na.rm = TRUE)
  t_stat    <- mean_car / (sd_car / sqrt(n_firms_t))
  df_t      <- n_firms_t - 1
  p_val     <- 2 * stats::pt(-abs(t_stat), df = df_t)

  if (test == "patell") {
    # Patell: standardized abnormal returns
    z_patell <- mean(car_results$scar, na.rm = TRUE) * sqrt(n_firms_t)
    p_val    <- 2 * stats::pnorm(-abs(z_patell))
    t_stat   <- z_patell
  } else if (test == "bmp") {
    # BMP: cross-sectional standardization of SCAR
    bmp_mean <- mean(car_results$scar, na.rm = TRUE)
    bmp_se   <- stats::sd(car_results$scar, na.rm = TRUE) / sqrt(n_firms_t)
    t_stat   <- bmp_mean / bmp_se
    p_val    <- 2 * stats::pt(-abs(t_stat), df = df_t)
  }

  test_results <- list(
    test_type  = test,
    n          = n_firms_t,
    mean_car   = mean_car,
    t_stat     = t_stat,
    p_value    = p_val,
    window     = car_window,
    stars      = ifelse(p_val < 0.01, "***",
                 ifelse(p_val < 0.05, "**",
                 ifelse(p_val < 0.10, "*", "")))
  )

  out <- list(
    parameters   = params,
    ar_data      = ar_data,
    aar_series   = aar_series,
    car_results  = car_results,
    test_results = test_results,
    plot_aar     = NULL,
    plot_car_dist = NULL
  )

  if (plot) {
    # AAR/CAAR plot
    out$plot_aar <- ggplot2::ggplot(aar_series, ggplot2::aes(x = event_time)) +
      ggplot2::geom_col(ggplot2::aes(y = aar), fill = "#4393C3", alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = caar), color = "#D73027",
                         linewidth = 1.2) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = cumsum(ci_lo), ymax = cumsum(ci_hi)),
        fill = "#D73027", alpha = 0.15
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") +
      ggplot2::labs(
        x     = "Event Time (Trading Days)",
        y     = "Return",
        title = "Event Study: AAR (Bars) and CAAR (Line)",
        subtitle = sprintf(
          "CAR%s = %.4f%s (p = %.4f, %s test, N = %d)",
          paste0("[", car_window[1], ",", car_window[2], "]"),
          mean_car, test_results$stars, p_val, toupper(test), n_firms_t
        )
      ) +
      ggplot2::theme_minimal(base_size = 12)

    # CAR distribution
    out$plot_car_dist <- ggplot2::ggplot(car_results, ggplot2::aes(x = car * 100)) +
      ggplot2::geom_histogram(bins = 25, fill = "#4393C3", alpha = 0.7,
                              color = "white") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::geom_vline(xintercept = mean_car * 100, color = "#D73027",
                          linewidth = 1) +
      ggplot2::labs(
        x     = "CAR (%)",
        y     = "Count",
        title = "Distribution of Firm-Level CARs",
        subtitle = sprintf("Mean = %.3f%%, SD = %.3f%%",
                           mean_car * 100, sd_car * 100)
      ) +
      ggplot2::theme_minimal(base_size = 12)
  }

  invisible(out)
}
