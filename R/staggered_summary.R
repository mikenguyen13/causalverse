#' Summarize Staggered Adoption Panel Designs
#'
#' Provides a comprehensive summary of a staggered adoption design,
#' including cohort sizes, treatment timing distribution, share never
#' treated, and a cohort-by-period treatment matrix.
#'
#' @param data A data frame in long (panel) format.
#' @param unit_var Character. Unit identifier column.
#' @param time_var Character. Time period column.
#' @param treat_var Character. Binary treatment indicator (0/1 or TRUE/FALSE).
#' @param first_treat_var Character or \code{NULL}. Column with the first
#'   period of treatment for each unit. If \code{NULL} (default), it is
#'   computed automatically.
#' @param plot Logical. Whether to produce visualizations. Default
#'   \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{cohort_sizes}{Data frame: cohort (first treatment period),
#'       number of units, and share.}
#'     \item{never_treated_n}{Integer. Number of never-treated units.}
#'     \item{n_units}{Integer. Total number of units.}
#'     \item{n_periods}{Integer. Total number of periods.}
#'     \item{treat_matrix}{Matrix: units × periods, 1 = treated.}
#'     \item{cohort_plot}{ggplot2 bar chart of cohort sizes.}
#'     \item{calendar_plot}{ggplot2 treatment calendar heatmap.}
#'   }
#'
#' @examples
#' library(fixest)
#' staggered_summary(
#'   data      = base_stagg,
#'   unit_var  = "id",
#'   time_var  = "year",
#'   treat_var = "treated"
#' )
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_text labs theme_minimal
#' @export
staggered_summary <- function(data,
                               unit_var,
                               time_var,
                               treat_var,
                               first_treat_var = NULL,
                               plot            = TRUE) {

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

  # Compute first treatment period
  if (!is.null(first_treat_var) && first_treat_var %in% names(data)) {
    ft_df <- unique(data.frame(
      unit  = data[[unit_var]],
      first = data[[first_treat_var]],
      stringsAsFactors = FALSE
    ))
  } else {
    ft_df <- do.call(rbind, lapply(split(df, df$unit), function(u) {
      ft <- min(u$time[u$treat == 1], na.rm = TRUE)
      data.frame(unit = u$unit[1], first = ifelse(is.infinite(ft), NA, ft),
                 stringsAsFactors = FALSE)
    }))
  }

  # Summary statistics
  units      <- unique(df$unit)
  periods    <- unique(df$time)
  n_units    <- length(units)
  n_periods  <- length(periods)
  n_never    <- sum(is.na(ft_df$first))

  # Cohort sizes
  cohort_sizes <- as.data.frame(table(first = ft_df$first[!is.na(ft_df$first)]),
                                stringsAsFactors = FALSE)
  names(cohort_sizes) <- c("first_treat_period", "n_units")
  cohort_sizes$share  <- round(cohort_sizes$n_units / n_units, 4)

  # Treatment matrix (wide format): rows = units, cols = periods
  treat_wide <- tapply(df$treat, list(df$unit, df$time), mean, na.rm = TRUE)

  out <- list(
    cohort_sizes     = cohort_sizes,
    never_treated_n  = n_never,
    n_units          = n_units,
    n_periods        = n_periods,
    time_range       = range(periods),
    treat_matrix     = treat_wide,
    cohort_plot      = NULL,
    calendar_plot    = NULL
  )

  if (plot) {
    # Cohort bar chart
    cs_plot <- cohort_sizes
    cs_plot$first_treat_period <- factor(
      cs_plot$first_treat_period,
      levels = sort(unique(cs_plot$first_treat_period))
    )

    out$cohort_plot <- ggplot2::ggplot(cs_plot,
      ggplot2::aes(x = first_treat_period, y = n_units)) +
      ggplot2::geom_col(fill = "#4393C3", alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = n_units), vjust = -0.3, size = 3) +
      ggplot2::labs(
        x       = "First Treatment Period (Cohort)",
        y       = "Number of Units",
        title   = "Treatment Cohort Sizes",
        caption = sprintf("Never treated: %d (%.1f%%)",
                          n_never, 100 * n_never / n_units)
      ) +
      ggplot2::theme_minimal(base_size = 12)

    # Calendar heatmap
    out$calendar_plot <- treatment_calendar(
      data     = data,
      unit_var = unit_var,
      time_var = time_var,
      treat_var = treat_var,
      title    = "Treatment Timing Calendar"
    )
  }

  out$message <- sprintf(
    "Staggered design: %d units, %d periods (%d-%d), %d cohorts, %d never-treated.",
    n_units, n_periods,
    out$time_range[1], out$time_range[2],
    nrow(cohort_sizes), n_never
  )
  message(out$message)

  invisible(out)
}
