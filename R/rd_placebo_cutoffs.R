#' RD Placebo Cutoff Test
#'
#' Tests the validity of the RD design by estimating treatment effects at
#' placebo cutoff values where no discontinuity should exist. A significant
#' effect at a placebo cutoff may indicate a violation of the RD assumptions.
#'
#' @param data A data frame containing the outcome and running variable.
#' @param y Character string. Name of the outcome variable.
#' @param x Character string. Name of the running variable.
#' @param true_cutoff Numeric. The true cutoff value of the RD design.
#' @param placebo_cutoffs Numeric vector. Placebo cutoff values to test. If
#'   \code{NULL} (the default), equally spaced cutoffs are generated
#'   automatically from the support of \code{x}, excluding a neighbourhood
#'   around \code{true_cutoff}.
#' @param n_placebo Integer. Number of placebo cutoffs to generate when
#'   \code{placebo_cutoffs} is \code{NULL}. Default is \code{10}.
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
#'   \item{\code{results}}{A data frame with columns: \code{cutoff},
#'     \code{is_true_cutoff}, \code{estimate}, \code{std_error},
#'     \code{ci_lower}, \code{ci_upper}, \code{p_value}, \code{n_left},
#'     \code{n_right}.}
#'   \item{\code{plot}}{A ggplot2 object showing estimates and CIs at each
#'     cutoff, with the true cutoff highlighted.}
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' n <- 2000
#' x <- runif(n, -1, 1)
#' y <- 3 + 2 * (x >= 0) + 0.5 * x + rnorm(n)
#' df <- data.frame(y = y, x = x)
#'
#' result <- rd_placebo_cutoffs(
#'   data = df,
#'   y = "y",
#'   x = "x",
#'   true_cutoff = 0,
#'   placebo_cutoffs = c(-0.5, -0.25, 0.25, 0.5)
#' )
#'
#' result$plot
#' result$results
#' }
#'
#' @references
#' Imbens, G. W. and Lemieux, T. (2008). "Regression Discontinuity Designs:
#' A Guide to Practice." \emph{Journal of Econometrics}, 142(2), 615-635.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline
#'   geom_vline labs scale_color_manual scale_shape_manual
#' @importFrom stats complete.cases quantile
#' @export
rd_placebo_cutoffs <- function(data,
                               y,
                               x,
                               true_cutoff,
                               placebo_cutoffs = NULL,
                               n_placebo = 10,
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

    # Auto-generate placebo cutoffs if not provided
    if (is.null(placebo_cutoffs)) {
        x_range <- range(x_vec)
        # Use the 10th to 90th percentile to avoid boundary issues
        x_lo <- stats::quantile(x_vec, 0.10)
        x_hi <- stats::quantile(x_vec, 0.90)
        candidates <- seq(x_lo, x_hi, length.out = n_placebo + 2)
        # Exclude values too close to the true cutoff
        buffer <- (x_hi - x_lo) * 0.05
        placebo_cutoffs <- candidates[
            abs(candidates - true_cutoff) > buffer
        ]
    }

    # Combine true and placebo cutoffs
    all_cutoffs <- c(true_cutoff, placebo_cutoffs)

    # Run rdrobust at each cutoff
    results_list <- lapply(all_cutoffs, function(cc) {
        # Only use observations on both sides of the cutoff
        tryCatch({
            rd_fit <- rdrobust::rdrobust(
                y      = y_vec,
                x      = x_vec,
                c      = cc,
                kernel = kernel,
                p      = p
            )

            data.frame(
                cutoff         = cc,
                is_true_cutoff = (cc == true_cutoff),
                estimate       = rd_fit$coef[1],
                std_error      = rd_fit$se[3],
                ci_lower       = rd_fit$ci[3, 1],
                ci_upper       = rd_fit$ci[3, 2],
                p_value        = rd_fit$pv[3],
                n_left         = rd_fit$N_h[1],
                n_right        = rd_fit$N_h[2],
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

    results$cutoff_type <- ifelse(results$is_true_cutoff, "True Cutoff",
                                  "Placebo")

    # Build the plot
    p <- ggplot2::ggplot(
        results,
        ggplot2::aes(x = cutoff, y = estimate, color = cutoff_type,
                     shape = cutoff_type)
    ) +
        ggplot2::geom_hline(
            yintercept = 0,
            linetype   = "dashed",
            color      = "gray50"
        ) +
        ggplot2::geom_vline(
            xintercept = true_cutoff,
            linetype   = "dotted",
            color      = "blue",
            linewidth  = 0.8
        ) +
        ggplot2::geom_errorbar(
            ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
            width = diff(range(results$cutoff)) * 0.02
        ) +
        ggplot2::geom_point(size = 3) +
        ggplot2::scale_color_manual(
            values = c("True Cutoff" = "red", "Placebo" = "black"),
            name   = ""
        ) +
        ggplot2::scale_shape_manual(
            values = c("True Cutoff" = 17, "Placebo" = 16),
            name   = ""
        ) +
        ggplot2::labs(
            x       = "Cutoff Value",
            y       = "RD Estimate",
            title   = "RD Placebo Cutoff Test",
            caption = paste0("True cutoff = ", true_cutoff,
                             " | Kernel: ", kernel,
                             " | Poly order: ", p)
        ) +
        theme_use

    list(
        results = results,
        plot    = p
    )
}
