#' Custom function to visualize the balance between treatment and control groups
#' 
#' @param matched_set_list List of matched sets
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
#' @param data Dataset for balance calculation.
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
#' # Maha 4-year lag, up to 5 matches
#' PM.results.maha.4lag.5m <- PanelMatch::PanelMatch(
#'    lag = 4,
#'    time.id = "year",
#'    unit.id = "wbcode2",
#'    treatment = "dem",
#'    refinement.method = "mahalanobis",
#'    data = PanelMatch::dem,
#'    match.missing = TRUE,
#'    covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
#'    size.match = 5,
#'    qoi = "att",
#'    outcome.var = "y",
#'    lead = 0:4,
#'    forbid.treatment.reversal = FALSE,
#'    use.diagonal.variance.matrix = TRUE
#' )
#'
#' # Maha 4-year lag, up to 10 matches
#' PM.results.maha.4lag.10m <- PanelMatch::PanelMatch(
#'    lag = 4,
#'    time.id = "year",
#'    unit.id = "wbcode2",
#'    treatment = "dem",
#'    refinement.method = "mahalanobis",
#'    data = PanelMatch::dem,
#'    match.missing = TRUE,
#'    covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
#'    size.match = 10,
#'    qoi = "att",
#'    outcome.var = "y",
#'    lead = 0:4,
#'    forbid.treatment.reversal = FALSE,
#'    use.diagonal.variance.matrix = TRUE
#' )
#'
#' # Using the function
#' balance_scatter_custom(
#'    matched_set_list = list(PM.results.maha.4lag.5m$att, PM.results.maha.4lag.10m$att),
#'    set.names = c("Maha 4 Lag 5 Matches", "Maha 4 Lag 10 Matches"),
#'    data = dem,
#'    covariates = c("y", "tradewb")
#' )
#' }
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual scale_x_continuous scale_y_continuous labs geom_hline geom_abline
#' @importFrom PanelMatch get_covariate_balance PanelMatch 
#' @export
balance_scatter_custom <- function(matched_set_list,
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
                                   data,
                                   x.axis.label = "Before Refinement",
                                   y.axis.label = "After Refinement",
                                   theme_use = causalverse::ama_theme(),
                                   ...) {
  if (length(matched_set_list) < 1)
    stop("Please provide at least one matched.set object")
  
  if (!is.null(set.names) && length(set.names) != length(matched_set_list)) {
    stop("The length of set.names should match the number of matched sets")
  }
  
  # Setting pchs dynamically
  if (is.null(pchs)) {
    pchs <- 1:length(matched_set_list)
  } else if (length(pchs) != length(matched_set_list)) {
    stop("The length of pchs should match the number of matched sets")
  }
  
  non_refined_balance <- PanelMatch::get_covariate_balance(
    matched.sets = matched_set_list[[1]],
    data = data,
    covariates = covariates,
    use.equal.weights = TRUE
  )
  
  refined_balance <- list()
  for (i in 1:length(matched_set_list)) {
    refined_balance[[i]] <- PanelMatch::get_covariate_balance(
      matched.sets = matched_set_list[[i]],
      data = data,
      covariates = covariates
    )
  }
  
  benchmark <- as.vector(non_refined_balance)
  compared <- sapply(refined_balance, function(x) x[1:(nrow(x)),])
  
  # Prepare data for ggplot
  set.names <- if (is.null(set.names)) 1:length(refined_balance) else set.names
  refined_balance_df <- data.frame(
    compared = as.vector(compared),
    set.name = rep(set.names, each = length(benchmark)),
    benchmark = rep(benchmark, length(refined_balance))
  ) 
  refined_balance_df$set.name <- as.factor(refined_balance_df$set.name)
  
  gg <- ggplot(refined_balance_df, aes(x = abs(benchmark), y = abs(compared), shape = set.name)) +
    geom_point(size = dot.size) +
    scale_shape_manual(values = pchs, name = legend.title) +
    scale_x_continuous(limits = xlim) +
    scale_y_continuous(limits = ylim) +
    labs(x = x.axis.label, y = y.axis.label, title = main, ...) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + 
    theme_use
  
  gg <- gg + theme(legend.position = legend.position)
  
  if (!show.legend) {
    gg <- gg + theme(legend.position = "none")
  }
  return(gg)
  # print(gg)
}
