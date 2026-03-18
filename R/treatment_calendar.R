#' Treatment Calendar Heatmap for Staggered Adoption Designs
#'
#' Creates a heatmap / calendar visualization showing when each unit received
#' treatment in a staggered adoption panel design. Useful for communicating
#' treatment timing variation to readers and reviewers.
#'
#' @param data A data frame in long (panel) format.
#' @param unit_var Character. Name of the unit identifier column.
#' @param time_var Character. Name of the time period column.
#' @param treat_var Character. Name of the treatment indicator column
#'   (binary 0/1).
#' @param cohort_var Character or \code{NULL}. Name of a cohort/group variable
#'   (e.g., year first treated). If \code{NULL} (default), cohorts are
#'   inferred automatically.
#' @param max_units Integer. Maximum number of units to display. Units are
#'   sorted by first treatment date. Default \code{50}.
#' @param colors Character vector of length 2. Colors for untreated and
#'   treated cells. Default \code{c("#F7F7F7", "#2166AC")}.
#' @param title Character. Plot title. Default \code{"Treatment Calendar"}.
#'
#' @return A ggplot2 heatmap object.
#'
#' @examples
#' library(fixest)
#' treatment_calendar(
#'   data      = base_stagg,
#'   unit_var  = "id",
#'   time_var  = "year",
#'   treat_var = "treated"
#' )
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual labs
#'   theme_minimal theme element_text element_blank
#' @export
treatment_calendar <- function(data,
                               unit_var   = "id",
                               time_var   = "year",
                               treat_var  = "treatment",
                               cohort_var = NULL,
                               max_units  = 50,
                               colors     = c("#F7F7F7", "#2166AC"),
                               title      = "Treatment Calendar") {

  req_cols <- c(unit_var, time_var, treat_var)
  missing  <- setdiff(req_cols, names(data))
  if (length(missing) > 0) {
    stop("Columns not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  df <- data.frame(
    unit  = data[[unit_var]],
    time  = data[[time_var]],
    treat = as.integer(as.logical(data[[treat_var]])),
    stringsAsFactors = FALSE
  )

  # Determine first treatment period per unit
  first_treat <- tapply(
    ifelse(df$treat == 1, df$time, Inf),
    df$unit,
    min,
    na.rm = TRUE
  )
  first_treat[is.infinite(first_treat)] <- NA

  # Assign cohort labels
  if (!is.null(cohort_var) && cohort_var %in% names(data)) {
    cohort_lookup <- stats::setNames(
      data[[cohort_var]][match(unique(df$unit), data[[unit_var]])],
      unique(df$unit)
    )
    df$cohort <- cohort_lookup[as.character(df$unit)]
  } else {
    df$cohort <- first_treat[as.character(df$unit)]
  }

  # Sort units: never-treated last, then by first treat time
  unit_order <- sort(unique(df$unit),
    method = "radix",
    decreasing = FALSE
  )

  ft_vals <- first_treat[as.character(unit_order)]
  sort_key <- ifelse(is.na(ft_vals), Inf, ft_vals)
  unit_order <- unit_order[order(sort_key)]

  # Limit to max_units
  if (length(unit_order) > max_units) {
    unit_order <- unit_order[seq_len(max_units)]
  }
  df <- df[df$unit %in% unit_order, ]
  df$unit_f <- factor(df$unit, levels = rev(unit_order))

  # Color by cohort or treatment status
  df$fill_var <- factor(df$treat, levels = c(0, 1),
                        labels = c("Untreated", "Treated"))

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = time,
    y    = unit_f,
    fill = fill_var
  )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(colors, c("Untreated", "Treated")),
      name   = "Status"
    ) +
    ggplot2::labs(
      x     = time_var,
      y     = unit_var,
      title = title,
      subtitle = sprintf(
        "Showing %d of %d units. Dark = treated.",
        length(unit_order),
        length(unique(data[[unit_var]]))
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.y   = if (length(unit_order) > 30) {
        ggplot2::element_blank()
      } else {
        ggplot2::element_text(size = 8)
      },
      panel.grid    = ggplot2::element_blank(),
      legend.position = "right"
    )

  p
}
