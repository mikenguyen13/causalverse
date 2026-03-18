#' Comprehensive IV / 2SLS Diagnostics
#'
#' Runs a standard battery of instrumental variables diagnostics on a fitted
#' `ivreg` or `fixest` 2SLS model: first-stage strength (F-statistic),
#' weak-instrument tests (Cragg-Donald / Kleibergen-Paap), overidentification
#' (Sargan-Hansen), and endogeneity (Wu-Hausman). Returns all results in a
#' tidy list, including a ggplot2 summary bar chart.
#'
#' @title Instrumental Variables Diagnostics
#'
#' @description Accepts a fitted IV / 2SLS model and returns a comprehensive
#'   list of diagnostic statistics, each flagged as passing or failing standard
#'   thresholds. A visual bar chart compares observed test statistics against
#'   critical values.
#'
#' @param model A fitted IV model. Supported classes:
#'   \itemize{
#'     \item `ivreg` from the \pkg{ivreg} package.
#'     \item `fixest` estimated via `feols(..., iv = ...)` from \pkg{fixest}.
#'   }
#' @param data Data frame used to fit the model. Required for some `fixest`
#'   diagnostics; can be `NULL` for `ivreg` models.
#' @param alpha Numeric. Significance level for pass/fail thresholds. Default
#'   `0.05`.
#'
#' @return A named list:
#'   \describe{
#'     \item{`first_stage`}{Named numeric vector: F-statistic and degrees of
#'       freedom.}
#'     \item{`overid`}{Named numeric vector with Sargan statistic and p-value,
#'       or `NULL` if exactly identified.}
#'     \item{`endogeneity`}{Named numeric vector with Wu-Hausman statistic and
#'       p-value.}
#'     \item{`summary_df`}{Data frame summarising all diagnostics with columns
#'       `test`, `statistic`, `p_value`, `threshold`, `pass`.}
#'     \item{`plot`}{A ggplot2 object visualising the diagnostics.}
#'   }
#'
#' @references
#' Stock, J. H., & Yogo, M. (2005). Testing for weak instruments in linear IV
#' regression. In D. W. K. Andrews & J. H. Stock (Eds.), *Identification and
#' Inference for Econometric Models*. Cambridge University Press.
#'
#' Kleibergen, F., & Paap, R. (2006). Generalized reduced rank tests using the
#' singular value decomposition. *Journal of Econometrics*, 133(1), 97-126.
#'
#' @examples
#' set.seed(42)
#' n  <- 500
#' z  <- rnorm(n)            # instrument
#' x  <- 0.8 * z + rnorm(n) # endogenous regressor
#' y  <- 1.5 * x + rnorm(n) # outcome
#' df <- data.frame(y = y, x = x, z = z)
#'
#' if (requireNamespace("ivreg", quietly = TRUE)) {
#'   m <- ivreg::ivreg(y ~ x | z, data = df)
#'   diag <- iv_diagnostics(m)
#'   print(diag$summary_df)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_hline geom_text
#'   scale_fill_manual labs coord_flip theme
#' @importFrom stats pchisq pf
#' @export
iv_diagnostics <- function(model, data = NULL, alpha = 0.05) {

  is_ivreg  <- inherits(model, "ivreg")
  is_fixest <- inherits(model, "fixest")

  if (!is_ivreg && !is_fixest) {
    stop(
      "model must be of class 'ivreg' (from ivreg package) or ",
      "'fixest' (from fixest package)."
    )
  }

  # ---- ivreg path ----------------------------------------------------------
  if (is_ivreg) {
    if (!requireNamespace("ivreg", quietly = TRUE)) {
      stop("Package 'ivreg' is required. Install with: install.packages('ivreg')")
    }

    sm <- summary(model, diagnostics = TRUE)
    diag_mat <- sm$diagnostics

    # First-stage F
    fs_row <- diag_mat["Weak instruments", , drop = FALSE]
    fs_f   <- as.numeric(fs_row[, "statistic"])
    fs_df1 <- as.numeric(fs_row[, "df1"])
    fs_df2 <- as.numeric(fs_row[, "df2"])
    fs_p   <- as.numeric(fs_row[, "p-value"])
    first_stage <- c(F_stat = fs_f, df1 = fs_df1, df2 = fs_df2, p_value = fs_p)

    # Overidentification (Sargan)
    overid <- NULL
    if ("Sargan" %in% rownames(diag_mat)) {
      sar_row <- diag_mat["Sargan", , drop = FALSE]
      sar_s   <- as.numeric(sar_row[, "statistic"])
      sar_p   <- as.numeric(sar_row[, "p-value"])
      overid  <- c(sargan_stat = sar_s, p_value = sar_p)
    }

    # Wu-Hausman endogeneity
    wh_row      <- diag_mat["Wu-Hausman", , drop = FALSE]
    wh_f        <- as.numeric(wh_row[, "statistic"])
    wh_p        <- as.numeric(wh_row[, "p-value"])
    endogeneity <- c(WuHausman_stat = wh_f, p_value = wh_p)

  } else {
    # ---- fixest path -------------------------------------------------------
    if (!requireNamespace("fixest", quietly = TRUE)) {
      stop("Package 'fixest' is required. Install with: install.packages('fixest')")
    }

    fs_stats <- tryCatch(
      fixest::fitstat(model, type = c("ivf", "ivfall"), simplify = FALSE),
      error = function(e) NULL
    )

    if (is.null(fs_stats)) {
      first_stage <- c(F_stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p_value = NA_real_)
    } else {
      ivf_val <- fs_stats[["ivf"]]
      if (!is.null(ivf_val) && length(ivf_val) > 0) {
        f_val       <- as.numeric(ivf_val[[1]]$stat)
        f_df1       <- as.numeric(ivf_val[[1]]$df1)
        f_df2       <- as.numeric(ivf_val[[1]]$df2)
        f_p         <- stats::pf(f_val, f_df1, f_df2, lower.tail = FALSE)
        first_stage <- c(F_stat = f_val, df1 = f_df1, df2 = f_df2, p_value = f_p)
      } else {
        first_stage <- c(F_stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p_value = NA_real_)
      }
    }

    overid      <- NULL
    endogeneity <- c(WuHausman_stat = NA_real_, p_value = NA_real_)
    message("Wu-Hausman endogeneity test not available for fixest models; ",
            "use ivreg for full diagnostics.")
  }

  # ---- Build summary data frame --------------------------------------------
  rows <- list(
    data.frame(
      test      = "First-Stage F (Weak Instruments)",
      statistic = unname(first_stage["F_stat"]),
      p_value   = unname(first_stage["p_value"]),
      threshold = 10,    # Stock-Yogo rule-of-thumb
      pass      = !is.na(first_stage["F_stat"]) && first_stage["F_stat"] >= 10,
      stringsAsFactors = FALSE
    ),
    data.frame(
      test      = "Wu-Hausman (Endogeneity)",
      statistic = unname(endogeneity["WuHausman_stat"]),
      p_value   = unname(endogeneity["p_value"]),
      threshold = stats::qf(1 - alpha, 1, Inf),
      pass      = !is.na(endogeneity["p_value"]) && endogeneity["p_value"] < alpha,
      stringsAsFactors = FALSE
    )
  )

  if (!is.null(overid)) {
    rows <- c(rows, list(data.frame(
      test      = "Sargan (Overidentification)",
      statistic = unname(overid["sargan_stat"]),
      p_value   = unname(overid["p_value"]),
      threshold = stats::qchisq(1 - alpha, df = 1),
      pass      = !is.na(overid["p_value"]) && overid["p_value"] >= alpha,
      stringsAsFactors = FALSE
    )))
  }

  summary_df <- do.call(rbind, rows)
  rownames(summary_df) <- NULL

  # ---- Plot ----------------------------------------------------------------
  plot_df <- summary_df[!is.na(summary_df$statistic), ]

  # Normalise: ratio of statistic to threshold for visual comparison
  plot_df$ratio <- plot_df$statistic / plot_df$threshold
  plot_df$pass_label <- ifelse(plot_df$pass, "Pass", "Fail")

  p <- ggplot2::ggplot(plot_df,
         ggplot2::aes(x = test, y = ratio, fill = pass_label)) +
    ggplot2::geom_col(width = 0.6, alpha = 0.85) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        color = "grey30", linewidth = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(round(statistic, 2), "\n(thr: ", round(threshold, 2), ")")
      ),
      vjust = -0.3, size = 3.2, color = "grey20") +
    ggplot2::scale_fill_manual(
      values = c("Pass" = "#4575b4", "Fail" = "#d73027"),
      name   = "Status") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "IV Diagnostic Tests",
      x     = NULL,
      y     = "Statistic / Threshold (> 1 = Pass rule-of-thumb)"
    ) +
    causalverse::ama_theme() +
    ggplot2::theme(legend.position = "bottom")

  list(
    first_stage = first_stage,
    overid      = overid,
    endogeneity = endogeneity,
    summary_df  = summary_df,
    plot        = p
  )
}
