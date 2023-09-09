#' @title Plot Coefficients of Parallel Trends
#' @description This function generates coefplots or iplots based on fixest outputs, 
#' allowing the user to visualize interaction coefficients with ease.
#'
#' @param data Data frame containing the data to be used in the model.
#' @param dependent_vars Named list of dependent variables to model and their respective labels.
#' @param time_var Name of the time variable in the data.
#' @param unit_treatment_status Name of the treatment status variable.
#' @param unit_id_var Name of the unit identification variable.
#' @param plot_type Type of plot to generate. Either "coefplot" or "iplot".
#' @param combined_plot Logical indicating whether to combine plots for all dependent variables.
#' @param legend_position Position of the legend on the plot.
#' @param legend_title Title for the legend.
#' @param legend_args List of additional arguments to customize the legend.
#' @param plot_args List of additional arguments to customize the plot.
#'
#' @return A plot visualizing interaction coefficients.
#' @export
#' @importFrom utils modifyList
#' @importFrom graphics legend
#' @importFrom stats as.formula
#' @importFrom fixest feols coefplot iplot
#' @examples
#' \dontrun{
#' library(fixest)
#' data("base_did")
#'
#' # Sample call to the function:
#' plot_coef_par_trends(
#'   data = base_did,
#'   dependent_vars = c(y = "Outcome 1", x1 = "Outcome 2"),
#'   time_var = "period",
#'   unit_treatment_status = "treat",
#'   unit_id_var = "id",
#'   plot_type = "coefplot",
#'   combined_plot = TRUE,
#'   plot_args = list(main = "Interaction coefficients Plot"),
#'   legend_title = "Metrics",
#'   legend_position = "bottomleft"
#' )
#'
#' plot_coef_par_trends(
#'   data = base_did,
#'   dependent_vars = c(y = "Outcome 1", x1 = "Outcome 2"),
#'   time_var = "period",
#'   unit_treatment_status = "treat",
#'   unit_id_var = "id",
#'   plot_type = "coefplot",
#'   combined_plot = FALSE
#' )
#' }
plot_coef_par_trends <- function(data, 
                                 dependent_vars, 
                                 time_var, 
                                 unit_treatment_status, 
                                 unit_id_var, 
                                 plot_type = "coefplot", 
                                 combined_plot = TRUE,
                                 legend_position = "bottomleft",
                                 legend_title = "Legend Title", 
                                 legend_args = list(), 
                                 plot_args = list()) {
  
  # Validate plot type
  if (!plot_type %in% c("coefplot", "iplot")) {
    stop("plot_type should be either 'coefplot' or 'iplot'")
  }
  
  # Derive variable names and labels
  dependent_var_names  <- names(dependent_vars)
  dependent_var_labels <- unlist(dependent_vars)
  
  # Set default colors for plots
  if (is.null(plot_args$col)) plot_args$col <- 1:length(dependent_var_names)
  
  # Set default legend arguments
  legend_args <- utils::modifyList(list(
    x      = legend_position,
    col    = 1:length(dependent_var_names),
    lwd    = length(dependent_var_names),
    legend = dependent_var_labels,
    title  = legend_title
  ), legend_args)
  
  # Initialize a list to store individual plots
  plot_list <- list()
  
  # Generate individual plots if combined_plot is FALSE
  if (!combined_plot) {
    for (metric_name in dependent_var_names) {
      formula_str <- paste(metric_name, paste0("~ i(", time_var, ",", unit_treatment_status, ") |", unit_id_var, "+", time_var))
      model <- fixest::feols(stats::as.formula(formula_str), vcov = "hetero", data = data)
      
      # Store the generated plot
      plot_obj <- if (plot_type == "coefplot") {
        do.call(fixest::coefplot, c(list(model, main = dependent_var_labels[metric_name]), plot_args))
      } else {
        do.call(fixest::iplot, c(list(model, main = dependent_var_labels[metric_name]), plot_args))
      }
      
      # Store the plot in the list
      plot_list[[metric_name]] <- plot_obj
    }
    return(plot_list) # Return list of plots
  } else {
    # Combined formula_str
    combined_vars_str <- paste("c(", paste0(dependent_var_names, collapse = ","), ")")
    # print(combined_vars_str)
    formula_str <- paste(combined_vars_str, paste0("~ i(", time_var, ",", unit_treatment_status, ") |", unit_id_var, "+", time_var))
    # print(formula_str)
    model <- fixest::feols(stats::as.formula(formula_str), vcov = "hetero", data = data)
    
    # Placeholder for combined plot
    plot_obj <- if (plot_type == "coefplot") {
      do.call(fixest::coefplot, c(list(model), plot_args))
    } else {
      do.call(fixest::iplot, c(list(model), plot_args))
    }
    
    # Add legend to the combined plot
    do.call(graphics::legend, legend_args)
    
    return(plot_obj) # Return the combined plot
  }
}