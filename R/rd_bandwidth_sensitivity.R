#' RD Bandwidth Sensitivity Analysis
#'
#' Runs \code{\link[rdrobust]{rdrobust}} over a grid of bandwidths and plots
#' point estimates with confidence intervals against bandwidth values. This
#' helps assess how sensitive the RD treatment effect estimate is to the choice
#' of bandwidth.
#'
#' @param data A data frame containing the outcome and running variable.
#' @param y Character string. Name of the outcome variable.
#' @param x Character string. Name of the running variable.
#' @param c Numeric. The cutoff value for the RD design. Default is \code{0}.
#' @param bw_multiples Numeric vector. Multipliers applied to the MSE-optimal
#'   bandwidth from \code{rdrobust}. Default is \code{seq(0.5, 2, 0.1)}.
#' @param kernel Character string. Kernel function for \code{rdrobust}.
#'   Default is \code{"tri"} (triangular).
#' @param p Integer. Order of the local polynomial. Default is \code{1}.
#' @param conf_level Numeric. Confidence level for intervals. Default is
#'   \code{0.95}.
#' @param theme_use A ggplot2 theme to apply to the plot. Default is
#'   \code{causalverse::ama_theme()}.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{results}}{A data frame with columns: \code{bandwidth},
#'     \code{bw_multiple}, \code{estimate}, \code{std_error}, \code{ci_lower},
#'     \code{ci_upper}, \code{p_value}, \code{n_left}, \code{n_right}.}
#'   \item{\code{plot}}{A ggplot2 object showing estimates and CIs across
#'     bandwidths.}
#'   \item{\code{optimal_bw}}{The MSE-optimal bandwidth from \code{rdrobust}.}
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' n <- 1000
#' x <- runif(n, -1, 1)
#' y <- 3 + 2 * (x >= 0) + 0.5 * x + rnorm(n)
#' df <- data.frame(y = y, x = x)
#'
#' result <- rd_bandwidth_sensitivity(
#'   data = df,
#'   y = "y",
#'   x = "x",
#'   c = 0,
#'   bw_multiples = seq(0.5, 2, 0.25)
#' )
#'
#' result$plot
#' result$results
#' result$optimal_bw
#' }
#'
#' @references
#' Calonico, S., Cattaneo, M. D., and Titiunik, R. (2014). "Robust
#' Nonparametric Confidence Intervals for Regression-Discontinuity Designs."
#' \emph{Econometrica}, 82(6), 2295-2326.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_vline
#'   geom_hline labs
#' @importFrom stats complete.cases
#' @export
rd_bandwidth_sensitivity <- function(data,
                                     y,
                                     x,
                                     c = 0,
                                     bw_multiples = seq(0.5, 2, 0.1),
                                     kernel = "tri",
                                     p = 1,
                                     conf_level = 0.95,
                                     theme_use = causalverse::ama_theme()) {

    if (!requireNamespace("rdrobust", quietly = TRUE)) {
        stop(
            "Package 'rdrobust' is required but not installed. ",
            "Install it with: install.packages('rdrobust')",
            call. = FALSE
        )
    }

    # Extract variables
    y_vec <- data[[y]]
    x_vec <- data[[x]]

    if (is.null(y_vec) || is.null(x_vec)) {
        stop("Variables '", y, "' and/or '", x, "' not found in data.",
             call. = FALSE)
    }

    # Remove missing values
    complete <- stats::complete.cases(y_vec, x_vec)
    y_vec <- y_vec[complete]
    x_vec <- x_vec[complete]

    # Run rdrobust once to get the optimal bandwidth
    rd_opt <- rdrobust::rdrobust(y = y_vec, x = x_vec, c = c,
                                 kernel = kernel, p = p)
    optimal_bw <- rd_opt$bws[1, 1]

    # Run rdrobust across the bandwidth grid
    results_list <- lapply(bw_multiples, function(mult) {
        bw <- optimal_bw * mult
        tryCatch({
            rd_fit <- rdrobust::rdrobust(
                y = y_vec,
                x = x_vec,
                c = c,
                h = bw,
                kernel = kernel,
                p = p
            )

            data.frame(
                bandwidth    = bw,
                bw_multiple  = mult,
                estimate     = rd_fit$coef[1],
                std_error    = rd_fit$se[3],
                ci_lower     = rd_fit$ci[3, 1],
                ci_upper     = rd_fit$ci[3, 2],
                p_value      = rd_fit$pv[3],
                n_left       = rd_fit$N_h[1],
                n_right      = rd_fit$N_h[2],
                stringsAsFactors = FALSE
            )
        }, error = function(e) {
            NULL
        })
    })

    results <- do.call(rbind, results_list[!vapply(results_list, is.null,
                                                    logical(1))])
    rownames(results) <- NULL

    if (nrow(results) == 0) {
        stop("All rdrobust estimations failed. Check your data and parameters.",
             call. = FALSE)
    }

    # Build the plot
    p <- ggplot2::ggplot(results, ggplot2::aes(x = bandwidth, y = estimate)) +
        ggplot2::geom_hline(
            yintercept = 0,
            linetype   = "dashed",
            color      = "gray50"
        ) +
        ggplot2::geom_vline(
            xintercept = optimal_bw,
            linetype   = "dotted",
            color      = "blue",
            linewidth  = 0.8
        ) +
        ggplot2::geom_errorbar(
            ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
            width = optimal_bw * 0.02,
            color = "gray40"
        ) +
        ggplot2::geom_point(size = 2, color = "black") +
        ggplot2::labs(
            x       = "Bandwidth",
            y       = "RD Estimate",
            title   = "RD Sensitivity to Bandwidth Choice",
            caption = paste0("Optimal bandwidth = ", round(optimal_bw, 3),
                             " | Kernel: ", kernel, " | Poly order: ", p)
        ) +
        theme_use

    list(
        results    = results,
        plot       = p,
        optimal_bw = optimal_bw
    )
}
