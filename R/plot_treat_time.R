#' Plot number of treated units over time or return a dataframe.
#'
#' @param data Dataframe containing data.
#' @param time_var Time variable for aggregating the number of treated units.
#' @param unit_treat Variable indicating if the unit was treated in a specific time period.
#' @param outlier_method Method for outlier detection ("iqr" or "z-score").
#' @param show_legend Logical indicating whether to show legend.
#' @param theme_use ggplot2 theme to use.
#' @param legend_title Title for legend.
#' @param legend_labels Labels for regular and outlier points.
#' @param regular_size Size of regular points.
#' @param outlier_size Size of outlier points.
#' @param regular_color Color of regular points.
#' @param outlier_color Color of outlier points.
#' @param regular_shape Shape of regular points.
#' @param outlier_shape Shape of outlier points.
#' @param title Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param output Type of output ("plot" or "dataframe").
#' @param ... Additional arguments to pass to ggplot2::labs.
#' @return ggplot2 object or dataframe.
#' @importFrom dplyr group_by summarise ungroup arrange select
#' @importFrom stats quantile setNames
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Example usage:
#' \dontrun{
#' data <- data.frame(time = c(1,1,2,2,3,3), treat = c(0,1,1,1,0,0))
#' plot_treat_time(data, time_var = time, unit_treat = treat)
#' plot_treat_time(data, time_var = time, unit_treat = treat, output = "dataframe")
#' }
plot_treat_time <- function(data,
                            time_var,      
                            unit_treat,    
                            outlier_method = "iqr",
                            show_legend = FALSE,
                            theme_use = causalverse::ama_theme(),
                            legend_title = "Point Type",
                            legend_labels = c("Regular", "Outlier"),
                            regular_size = 3,
                            outlier_size = 5,
                            regular_color = "black",
                            outlier_color = "red",
                            regular_shape = 16,
                            outlier_shape = 17,
                            title = "Random Time Assignment",
                            xlab = "Time",
                            ylab = "Number of Treated Units",
                            output = "plot",
                            ...) {
  
  # Aggregate data based on time and calculate the sum of treated units
  agg_data <- data %>%
    dplyr::group_by({{time_var}}) %>%
    dplyr::summarise(units_sum = sum({{unit_treat}}, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # Function to detect outliers based on a specified method
  detect_outliers <- function(data, method = "iqr") {
    outliers <- NULL
    if (method == "iqr") {
      q1 <- stats::quantile(data, 0.25)
      q3 <- stats::quantile(data, 0.75)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      outliers <- data[data < lower_bound | data > upper_bound]
    } else if (method == "z-score") {
      z_scores <- scale(data)
      threshold <- 1.96
      outliers <- data[abs(z_scores) > threshold]
    } else {
      stop("Unknown method. Please use either 'iqr' or 'z-score'.")
    }
    return(outliers)
  }
  
  # Detect outliers based on the chosen method
  outliers_detected <- detect_outliers(agg_data$units_sum, outlier_method)
  
  # Categorize points as regular or outlier
  agg_data$point_type <- ifelse(agg_data$units_sum %in% outliers_detected, legend_labels[2], legend_labels[1])
  agg_data$point_size <- ifelse(agg_data$units_sum %in% outliers_detected, outlier_size, regular_size)
  agg_data$outliers <- ifelse(agg_data$units_sum %in% outliers_detected, 1, 0) # New column
  
  # If the output is a dataframe, sort, select the necessary columns and return it
  if(output == "dataframe") {
    return(dplyr::arrange(agg_data, {{time_var}}) %>%
             dplyr::select(-point_type, -point_size)) # exclude point_type and point_size columns
  }
  
  # Define color and shape scales
  color_scale <- stats::setNames(c(regular_color, outlier_color), legend_labels)
  shape_scale <- stats::setNames(c(regular_shape, outlier_shape), legend_labels)
  
  # Create the plot
  p <- ggplot2::ggplot(agg_data, ggplot2::aes(x = {{time_var}}, y = .data$units_sum)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$point_type, shape = .data$point_type, size = .data$point_size)) +
    ggplot2::scale_color_manual(values = color_scale) +
    ggplot2::scale_shape_manual(values = shape_scale) +
    ggplot2::labs(title = title, x = xlab, y = ylab, ...) +
    theme_use +
    ggplot2::scale_size_identity()
  
  # Add or remove the legend as specified
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::labs(color = legend_title, shape = legend_title, size = legend_title)
  }
  
  return(p)
}
