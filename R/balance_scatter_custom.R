#' Custom function to visualize the balance between treatment and control groups
#'
#' @param pm_result_list List of PanelMatch result objects (output of PanelMatch::PanelMatch()).
#' @param panel.data A PanelData object created by PanelMatch::PanelData().
#' @param set.names Vector of names for matched sets. Defaults to NULL.
#' @param show.legend Boolean to determine if legend should be shown. Defaults to TRUE.
#' @param legend.title Legend title. Defaults to "Type".
#' @param legend.position Position of legend. Defaults to "right".
#' @param xlim Vector defining x-axis limits. Defaults to c(0, 0.8).
#' @param ylim Vector defining y-axis limits. Defaults to c(0, 0.8).
#' @param main Main title for the plot. Defaults to "Standardized Mean Difference of Covariates".
#' @param pchs Plot characters. Defaults to NULL.
#' @param dot.size Size of dots in the scatter plot. Defaults to NULL.
#' @param covariates Covariates for calculating balance.
#' @param x.axis.label x-axis label. Defaults to "Before Refinement".
#' @param y.axis.label y-axis label. Defaults to "After Refinement".
#' @param theme_use Custom theme that follows ggplots2. Defaults to causalverse::ama_theme().
#' @param ... Additional arguments passed to the labs() function
#'
#' @return ggplot object
#'
#' @examples
#' \dontrun{
#' library(PanelMatch)
#' pd <- PanelData(panel.data = dem, unit.id = "wbcode2",
#'                 time.id = "year", treatment = "dem", outcome = "y")
#'
#' # Maha 4-year lag, up to 5 matches
#' PM.results.5m <- PanelMatch(
#'    panel.data = pd,
#'    lag = 4,
#'    refinement.method = "mahalanobis",
#'    match.missing = TRUE,
#'    covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
#'    size.match = 5,
#'    qoi = "att",
#'    lead = 0:4,
#'    forbid.treatment.reversal = FALSE,
#'    use.diagonal.variance.matrix = TRUE
#' )
#'
#' # Maha 4-year lag, up to 10 matches
#' PM.results.10m <- PanelMatch(
#'    panel.data = pd,
#'    lag = 4,
#'    refinement.method = "mahalanobis",
#'    match.missing = TRUE,
#'    covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
#'    size.match = 10,
#'    qoi = "att",
#'    lead = 0:4,
#'    forbid.treatment.reversal = FALSE,
#'    use.diagonal.variance.matrix = TRUE
#' )
#'
#' # Using the function
#' balance_scatter_custom(
#'    pm_result_list = list(PM.results.5m, PM.results.10m),
#'    panel.data = pd,
#'    set.names = c("Maha 4 Lag 5 Matches", "Maha 4 Lag 10 Matches"),
#'    covariates = c("y", "tradewb")
#' )
#' }
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual scale_x_continuous scale_y_continuous labs geom_hline geom_abline theme
#' @importFrom PanelMatch get_covariate_balance PanelMatch
#' @export
balance_scatter_custom <- function(pm_result_list,
                                   panel.data,
                                   set.names = NULL,
                                   show.legend = TRUE,
                                   legend.title = "Type",
                                   legend.position = "right",
                                   xlim = c(0, 0.8),
                                   ylim = c(0, 0.8),
                                   main = "Standardized Mean Difference of Covariates",
                                   pchs = NULL,
                                   dot.size = NULL,
                                   covariates,
                                   x.axis.label = "Before Refinement",
                                   y.axis.label = "After Refinement",
                                   theme_use = causalverse::ama_theme(),
                                   ...) {

  if (length(pm_result_list) < 1)
    stop("Please provide at least one PanelMatch result object")

  if (!is.null(set.names) && length(set.names) != length(pm_result_list)) {
    stop("The length of set.names should match the number of PanelMatch results")
  }

  # Setting pchs dynamically
  if (is.null(pchs)) {
    pchs <- 1:length(pm_result_list)
  } else if (length(pchs) != length(pm_result_list)) {
    stop("The length of pchs should match the number of PanelMatch results")
  }

  # Get unrefined (benchmark) balance from the first result
  unrefined_bal <- PanelMatch::get_covariate_balance(
    pm_result_list[[1]],
    panel.data = panel.data,
    covariates = covariates,
    include.unrefined = TRUE
  )
  # Extract the unrefined balance matrix
  unrefined_attr <- attr(unrefined_bal, "unrefined.balance.results")
  unrefined_matrix <- unrefined_attr[[1]][[1]]
  benchmark <- as.vector(unrefined_matrix)

  # Get refined balance for each result
  refined_balance <- list()
  for (i in seq_along(pm_result_list)) {
    bal <- PanelMatch::get_covariate_balance(
      pm_result_list[[i]],
      panel.data = panel.data,
      covariates = covariates
    )
    # Extract the balance matrix from the nested structure
    refined_balance[[i]] <- bal[[1]][[1]]
  }

  compared <- sapply(refined_balance, function(x) x[1:(nrow(x)),])

  # Prepare data for ggplot
  set.names <- if (is.null(set.names)) seq_along(refined_balance) else set.names
  refined_balance_df <- data.frame(
    compared = as.vector(compared),
    set.name = rep(set.names, each = length(benchmark)),
    benchmark = rep(benchmark, length(refined_balance))
  )
  refined_balance_df$set.name <- as.factor(refined_balance_df$set.name)

  gg <- ggplot2::ggplot(refined_balance_df, ggplot2::aes(x = abs(.data$benchmark), y = abs(.data$compared), shape = .data$set.name)) +
    ggplot2::geom_point(size = dot.size) +
    ggplot2::scale_shape_manual(values = pchs, name = legend.title) +
    ggplot2::scale_x_continuous(limits = xlim) +
    ggplot2::scale_y_continuous(limits = ylim) +
    ggplot2::labs(x = x.axis.label, y = y.axis.label, title = main, ...) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    theme_use

  gg <- gg + ggplot2::theme(legend.position = legend.position)

  if (!show.legend) {
    gg <- gg + ggplot2::theme(legend.position = "none")
  }
  return(gg)
}
