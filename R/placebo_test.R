#' Randomization Inference and Placebo Tests for Treatment Effects
#'
#' Conducts Fisher's randomization inference (permutation test) for causal
#' effects, computing sharp null p-values and visualizing the randomization
#' distribution. Also supports in-time and in-space placebo tests.
#'
#' @param data A data frame.
#' @param outcome Character. Name of the outcome variable.
#' @param treatment Character. Name of the binary treatment indicator.
#' @param estimator Function or character. The estimator to use:
#'   \code{"ate"} (default, difference in means), \code{"did"}
#'   (difference-in-differences requires \code{time_var}), or a custom
#'   function \code{f(data, outcome, treatment, ...)} returning a scalar.
#' @param unit_var Character. Unit identifier (for panel/DiD). Default
#'   \code{NULL}.
#' @param time_var Character. Time period variable (for DiD). Default
#'   \code{NULL}.
#' @param post_var Character. Binary indicator for post-treatment period
#'   (for DiD). Default \code{NULL}.
#' @param n_perms Integer. Number of permutations. Default \code{1000}.
#' @param test_stat Character. Test statistic: \code{"mean"} (difference in
#'   means) or \code{"t"} (t-statistic). Default \code{"mean"}.
#' @param two_sided Logical. Two-sided p-value. Default \code{TRUE}.
#' @param seed Integer. Random seed. Default \code{42}.
#' @param plot Logical. Whether to plot the permutation distribution. Default
#'   \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{observed_stat}{Numeric. Observed test statistic.}
#'     \item{p_value}{Numeric. Randomization inference p-value.}
#'     \item{perm_dist}{Numeric vector of permuted test statistics.}
#'     \item{plot}{ggplot2 histogram of the permutation distribution.}
#'   }
#'
#' @references
#' Fisher, R. A. (1935). \emph{The Design of Experiments}. Oliver & Boyd.
#'
#' Imbens, G. W., & Rubin, D. B. (2015). \emph{Causal Inference for Statistics,
#' Social, and Biomedical Sciences}. Cambridge University Press.
#'
#' @examples
#' set.seed(42)
#' df <- data.frame(
#'   Y = c(rnorm(50, mean = 0.5), rnorm(50, mean = 0)),
#'   D = c(rep(1, 50), rep(0, 50))
#' )
#' result <- placebo_test(data = df, outcome = "Y", treatment = "D")
#' result$p_value
#' result$plot
#'
#' @importFrom stats sd
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline labs
#'   theme_minimal
#' @export
placebo_test <- function(data,
                         outcome,
                         treatment,
                         estimator  = "ate",
                         unit_var   = NULL,
                         time_var   = NULL,
                         post_var   = NULL,
                         n_perms    = 1000,
                         test_stat  = c("mean", "t"),
                         two_sided  = TRUE,
                         seed       = 42,
                         plot       = TRUE) {

  test_stat <- match.arg(test_stat)

  req_cols <- c(outcome, treatment)
  missing  <- setdiff(req_cols, names(data))
  if (length(missing) > 0) {
    stop("Columns not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # --- Define test statistic function ---
  compute_stat <- function(df, tvar) {
    D <- as.integer(as.logical(df[[tvar]]))
    Y <- df[[outcome]]

    if (is.character(estimator) && estimator == "ate") {
      if (test_stat == "mean") {
        return(mean(Y[D == 1], na.rm = TRUE) - mean(Y[D == 0], na.rm = TRUE))
      } else {
        # t-statistic
        yt <- Y[D == 1]; yc <- Y[D == 0]
        nt <- sum(!is.na(yt)); nc <- sum(!is.na(yc))
        se <- sqrt(stats::var(yt, na.rm = TRUE) / nt +
                     stats::var(yc, na.rm = TRUE) / nc)
        if (se == 0) return(0)
        return((mean(yt, na.rm = TRUE) - mean(yc, na.rm = TRUE)) / se)
      }
    } else if (is.character(estimator) && estimator == "did") {
      if (is.null(post_var)) {
        stop("`post_var` required for DiD estimator.", call. = FALSE)
      }
      post  <- as.integer(as.logical(df[[post_var]]))
      ate11 <- mean(Y[D == 1 & post == 1], na.rm = TRUE)
      ate10 <- mean(Y[D == 1 & post == 0], na.rm = TRUE)
      ate01 <- mean(Y[D == 0 & post == 1], na.rm = TRUE)
      ate00 <- mean(Y[D == 0 & post == 0], na.rm = TRUE)
      return((ate11 - ate10) - (ate01 - ate00))
    } else if (is.function(estimator)) {
      return(estimator(df, outcome, tvar))
    } else {
      stop("Invalid `estimator`. Use 'ate', 'did', or a function.", call. = FALSE)
    }
  }

  # Observed statistic
  obs_stat <- compute_stat(data, treatment)

  # --- Permutation loop ---
  set.seed(seed)
  treat_vals  <- data[[treatment]]
  perm_stats  <- numeric(n_perms)

  for (i in seq_len(n_perms)) {
    perm_data             <- data
    perm_data[[treatment]] <- sample(treat_vals)
    perm_stats[i]          <- compute_stat(perm_data, treatment)
  }

  # --- P-value ---
  p_val <- if (two_sided) {
    mean(abs(perm_stats) >= abs(obs_stat))
  } else {
    mean(perm_stats >= obs_stat)
  }

  out <- list(
    observed_stat = obs_stat,
    p_value       = p_val,
    perm_dist     = perm_stats,
    plot          = NULL
  )

  if (plot) {
    df_plot <- data.frame(stat = perm_stats)
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = stat)) +
      ggplot2::geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7,
                              color = "white") +
      ggplot2::geom_vline(xintercept = obs_stat, color = "red",
                          linetype = "solid", linewidth = 1.2) +
      ggplot2::geom_vline(xintercept = -obs_stat,
                          color = "red", linetype = "dashed",
                          linewidth = 0.8) +
      ggplot2::labs(
        x     = "Permuted Test Statistic",
        y     = "Count",
        title = "Randomization Inference: Permutation Distribution",
        subtitle = sprintf(
          "Observed stat = %.4f | Randomization p-value = %.4f",
          obs_stat, p_val
        ),
        caption = sprintf("%d permutations; red line = observed statistic", n_perms)
      ) +
      ggplot2::theme_minimal(base_size = 12)

    out$plot <- p
  }

  invisible(out)
}
