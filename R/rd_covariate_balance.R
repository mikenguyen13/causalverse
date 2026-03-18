#' RD Covariate Balance Test
#'
#' Tests covariate balance at the RD cutoff by running
#' \code{\link[rdrobust]{rdrobust}} with each covariate as the outcome
#' variable. If the RD design is valid, predetermined covariates should not
#' exhibit a discontinuity at the cutoff.
#'
#' @param data A data frame containing the covariates and running variable.
#' @param covariates Character vector. Names of covariate columns to test.
#' @param x Character string. Name of the running variable.
#' @param c Numeric. The cutoff value for the RD design. Default is \code{0}.
#' @param bw Numeric or \code{NULL}. Bandwidth to use for all tests. If
#'   \code{NULL} (the default), \code{rdrobust} selects the MSE-optimal
#'   bandwidth separately for each covariate.
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
#'   \item{\code{table}}{A data frame with columns: \code{covariate},
#'     \code{estimate}, \code{std_error}, \code{ci_lower}, \code{ci_upper},
#'     \code{p_value}, \code{bandwidth}, \code{n_left}, \code{n_right},
#'     \code{significant}.}
#'   \item{\code{plot}}{A ggplot2 coefficient plot showing the estimate and
#'     CI for each covariate.}
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' n <- 1000
#' x <- runif(n, -1, 1)
#' df <- data.frame(
#'   x   = x,
#'   age = 30 + 5 * x + rnorm(n),
#'   inc = 50000 + 10000 * x + rnorm(n, sd = 5000),
#'   edu = 12 + 2 * x + rnorm(n, sd = 1.5)
#' )
#'
#' result <- rd_covariate_balance(
#'   data       = df,
#'   covariates = c("age", "inc", "edu"),
#'   x          = "x",
#'   c          = 0
#' )
#'
#' result$plot
#' result$table
#' }
#'
#' @references
#' Lee, D. S. and Lemieux, T. (2010). "Regression Discontinuity Designs in
#' Economics." \emph{Journal of Economic Literature}, 48(2), 281-355.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_vline
#'   labs scale_color_manual
#' @importFrom stats complete.cases
#' @export
rd_covariate_balance <- function(data,
                                 covariates,
                                 x,
                                 c = 0,
                                 bw = NULL,
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

    if (length(covariates) == 0) {
        stop("At least one covariate name must be provided.", call. = FALSE)
    }

    x_vec <- data[[x]]
    if (is.null(x_vec)) {
        stop("Running variable '", x, "' not found in data.", call. = FALSE)
    }

    # Run rdrobust for each covariate
    results_list <- lapply(covariates, function(cov) {
        y_vec <- data[[cov]]
        if (is.null(y_vec)) {
            warning("Covariate '", cov, "' not found in data. Skipping.",
                    call. = FALSE)
            return(NULL)
        }

        # Remove missing values for this pair
        complete <- stats::complete.cases(y_vec, x_vec)
        y_sub <- y_vec[complete]
        x_sub <- x_vec[complete]

        tryCatch({
            rd_args <- list(
                y      = y_sub,
                x      = x_sub,
                c      = c,
                kernel = kernel,
                p      = p
            )
            if (!is.null(bw)) {
                rd_args$h <- bw
            }

            rd_fit <- do.call(rdrobust::rdrobust, rd_args)

            data.frame(
                covariate   = cov,
                estimate    = rd_fit$coef[1],
                std_error   = rd_fit$se[3],
                ci_lower    = rd_fit$ci[3, 1],
                ci_upper    = rd_fit$ci[3, 2],
                p_value     = rd_fit$pv[3],
                bandwidth   = rd_fit$bws[1, 1],
                n_left      = rd_fit$N_h[1],
                n_right     = rd_fit$N_h[2],
                stringsAsFactors = FALSE
            )
        }, error = function(e) {
            warning("rdrobust failed for covariate '", cov, "': ",
                    conditionMessage(e), call. = FALSE)
            NULL
        })
    })

    tbl <- do.call(rbind, results_list[!vapply(results_list, is.null,
                                                logical(1))])
    rownames(tbl) <- NULL

    if (is.null(tbl) || nrow(tbl) == 0) {
        stop("All rdrobust estimations failed. Check your data and parameters.",
             call. = FALSE)
    }

    tbl$significant <- tbl$p_value < (1 - conf_level)

    # Order covariates by estimate for a clean plot
    tbl$covariate <- factor(tbl$covariate,
                            levels = tbl$covariate[order(tbl$estimate)])

    # Build coefficient plot
    p <- ggplot2::ggplot(
        tbl,
        ggplot2::aes(y = covariate, x = estimate,
                     color = significant)
    ) +
        ggplot2::geom_vline(
            xintercept = 0,
            linetype   = "dashed",
            color      = "gray50"
        ) +
        ggplot2::geom_errorbar(
            ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
            width = 0.2, orientation = "y"
        ) +
        ggplot2::geom_point(size = 3) +
        ggplot2::scale_color_manual(
            values = c("TRUE" = "red", "FALSE" = "black"),
            labels = c("TRUE" = paste0("p < ", 1 - conf_level),
                        "FALSE" = paste0("p >= ", 1 - conf_level)),
            name   = "Significance"
        ) +
        ggplot2::labs(
            x       = "RD Estimate",
            y       = "Covariate",
            title   = "Covariate Balance at RD Cutoff",
            caption = paste0("Cutoff = ", c,
                             " | Kernel: ", kernel,
                             " | Poly order: ", p)
        ) +
        theme_use

    list(
        table = tbl,
        plot  = p
    )
}
