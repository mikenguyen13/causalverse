#' Plot Density by Treatment
#'
#' This function creates a list of ggplot density plots for specified variables by treatment groups.
#'
#' @param data A data frame containing the variables to plot and a treatment variable.
#' @param var_map A named list mapping the column names in the data to display names for plotting.
#' @param treatment_var A named vector where the name is the treatment column in the data and the value is the legend title.
#' @param theme_use ggplot2 theme. Defaults to `ggplot2::theme_minimal()`.
#' @param ... Additional arguments to be passed to `geom_density`.
#' 
#' @importFrom ggplot2 ggplot aes geom_density labs theme_minimal theme element_text
#' @importFrom rlang sym
#' @importFrom rlang .data
#'
#' @return A list of ggplot objects for each variable in `var_map`.
#' @export
#' @examples 
#' \dontrun{
#' data(mtcars)
#' data <- mtcars %>% 
#'   dplyr::select(mpg, cyl) %>% 
#'   dplyr::rowwise() %>% 
#'   dplyr::mutate(treatment = sample(c(0,1), 1, replace = TRUE)) %>% 
#'   dplyr::ungroup()
#'
#' plots <- plot_density_by_treatment(
#'   data = data,
#'   var_map = list("mpg" = "Var 1",
#'                  "cyl" = "Var 2"),
#'   treatment_var = c("treatment" = "Treatment Name\nin Legend")
#' )
#' }
plot_density_by_treatment <- function(data, var_map, treatment_var, theme_use = ggplot2::theme_minimal(), ...) {
  plot_list <- list()
  
  # Extract variable name and legend title from the treatment_var list
  treatment_variable <- names(treatment_var)[1]
  legend_title <- treatment_var[[1]]
  
  # Loop through each variable in the mapping
  for (var in names(var_map)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(!!rlang::sym(var), fill = factor(!!rlang::sym(treatment_variable)))) +
      ggplot2::geom_density(alpha = 0.5, ...) +  # Passing the ... to geom_density
      ggplot2::labs(
        fill = legend_title,
        title = paste("Density Distribution of", var_map[[var]], "by Treatment Group")
      ) +
      ggplot2::ylab("Density") + 
      ggplot2::xlab(var_map[[var]]) +
      theme_use +
      # center the title
      ggplot2::theme(plot.title.position = "plot", plot.title = ggplot2::element_text(hjust = 0.5))
    
    plot_list[[var_map[[var]]]] <- p
  }
  
  return(plot_list)
}
