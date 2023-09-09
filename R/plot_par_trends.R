#' Plot Parallel Trends
#'
#' Plots parallel trends for given metrics.
#'
#' @param data A data frame containing the data to plot.
#' @param metrics_and_names A named list of metrics to plot.
#' @param treatment_status_var The variable indicating treatment status.
#' @param time_var The variable indicating time.
#' @param conf_level Confidence level for confidence intervals (default is 0.95).
#' @param non_negative Logical; if TRUE, sets negative lower confidence bounds to 0.
#' @param display_CI Logical; if TRUE, displays confidence intervals.
#' @param output_format Format of the output; "plot" returns a list of ggplots, "data.frame" returns a data frame.
#' @param smoothing_method Method to use for smoothing; NULL means no smoothing.
#' @param title_prefix A character string specifying the prefix for the plot title (default is "Parallel Trends for").
#' @param theme_use Custom theme that follows ggplots2
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes labs scale_color_manual scale_fill_manual scale_linetype_manual guides geom_smooth geom_line geom_ribbon
#'
#' @return A list of ggplot objects or a data frame.
#'
#' @examples 
#' \dontrun{
#' library(tidyverse)
#' data <- expand.grid(entity = 1:100, time = 1:10) %>%
#'   dplyr::arrange(entity, time) %>%
#'   dplyr::mutate(
#'     treatment = ifelse(entity <= 50, "Treated", "Control"),
#'     outcome1 = 0.5 * time + rnorm(n(), 0, 2) + ifelse(treatment == "Treated", 0, 0),
#'     outcome2 = 3 + 0.3 * time + rnorm(n(), 0, 1) + ifelse(treatment == "Treated", 0, 2)
#'   )
#' results <- plot_par_trends(
#'   data = data,
#'   metrics_and_names = list(outcome1 = "Outcome 1", outcome2 = "Outcome 2"),
#'   treatment_status_var = "treatment",
#'   time_var = list(time = "Time"),
#'   smoothing_method = "loess"
#' )
#' library(gridExtra)
#' gridExtra::grid.arrange(grobs = results, ncol = 1)
#' }
#' 
#' @export
plot_par_trends <- function(data,
                            metrics_and_names,
                            treatment_status_var,
                            time_var,
                            conf_level       = 0.95,
                            non_negative     = FALSE,
                            display_CI       = TRUE,
                            output_format    = "plot",
                            smoothing_method = NULL,
                            title_prefix     = "Parallel Trends for",
                            # scale_fill_use   = causalverse::ama_scale_fill(),
                            # scale_color_use  = causalverse::ama_scale_color(),
                            theme_use        = causalverse::ama_theme()) {
  
  
  # Extract time variable and custom name
  time_variable <- names(time_var)[1]
  time_name <- unlist(time_var)
  
  # Extract dependent variables and custom names
  dependent_vars <- names(metrics_and_names)
  custom_names <- unlist(metrics_and_names)
  
  # Pivoting and summarizing the data for analysis
  analysis_pt <- data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(dependent_vars), names_to = "metric") %>%
    dplyr::group_by(!!dplyr::sym(treatment_status_var), !!dplyr::sym(time_variable), .data$metric) %>%
    dplyr::summarise(
      mean_val = mean(.data$value, na.rm = TRUE),
      ci_lower = if (display_CI) stats::confint(stats::lm(.data$value ~ 1), level = conf_level)[1] else NA,
      ci_upper = if (display_CI) stats::confint(stats::lm(.data$value ~ 1), level = conf_level)[2] else NA,
      .groups = "drop"
    ) %>%
    
    # Adjust bounds if needed
    dplyr::mutate(
      ci_lower = dplyr::if_else(non_negative &
                                  .data$ci_lower < 0, 0, .data$ci_lower),
      ci_upper = dplyr::if_else(non_negative &
                                  .data$ci_upper < 0, 0, .data$ci_upper),
      mean_val = dplyr::if_else(non_negative &
                                  .data$mean_val < 0, 0, .data$mean_val)
    )
  
  # Return as data frame if specified
  if (output_format == "data.frame") {
    return(analysis_pt)
  }
  
  # Initialize list to store plots
  plot_list <- list()
  
  # Generate plots for each metric
  for (metric_name in dependent_vars) {
    plot_data <- analysis_pt %>%
      dplyr::filter(.data$metric == metric_name)
    
    # Extract custom metric name or use the original
    plot_name <- ifelse(!is.null(custom_names[metric_name]), custom_names[metric_name], metric_name)
    
    # Define colors and linetypes for the plot
    colors <- c("red", "blue")
    linetypes <- c("solid", "dashed")
    
    # Construct the plot using ggplot
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = !!dplyr::sym(time_variable),
        y = .data$mean_val,
        color = as.factor(!!dplyr::sym(treatment_status_var)),
        fill = as.factor(!!dplyr::sym(treatment_status_var)),
        linetype = as.factor(!!dplyr::sym(treatment_status_var))
      )
    ) +
      ggplot2::labs(title = paste(title_prefix, plot_name),
                    x = time_name,
                    y = plot_name) +
      ggplot2::scale_color_manual(name = "Treatment\nStatus", values = colors) +
      ggplot2::scale_fill_manual(name = "Treatment\nStatus", values = scales::alpha(colors, 0.2)) +
      ggplot2::scale_linetype_manual(name = "Treatment\nStatus", values = linetypes) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 0.2))) +
      
      
      # use custom theme
      theme_use
    
    # Add smoothing or lines and ribbons based on the input
    if (!is.null(smoothing_method)) {
      p <- suppressMessages(suppressWarnings(
        p + ggplot2::geom_smooth(
          formula = y ~ x,
          method = smoothing_method,
          se = display_CI,
          level = conf_level,
          ggplot2::aes(group = as.factor(!!dplyr::sym(
            treatment_status_var
          ))) 
        ) +
          
          theme_use
      ))
    } else {
      p <- p + ggplot2::geom_line() +
        theme_use
      
      if (display_CI) {
        p <-
          p + ggplot2::geom_ribbon(ggplot2::aes(
            ymin = ifelse(non_negative &
                            .data$ci_lower < 0, 0, .data$ci_lower),
            ymax = .data$ci_upper
          ), alpha = 0.2) +
          
          theme_use
      }
    }
    
    # Append the plot to the list
    plot_list[[length(plot_list) + 1]] <- p
  }
  
  return(plot_list)
}
