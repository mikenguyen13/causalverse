#' Quick Causal DAG Visualization
#'
#' Creates a styled causal directed acyclic graph (DAG) for visualizing
#' assumed causal structures. Wraps \pkg{dagitty} and \pkg{ggdag} (if
#' available) to produce publication-ready DAG figures with highlighted
#' backdoor paths, adjustment sets, and treatment/outcome nodes.
#'
#' @param dag_string Character. A \code{dagitty} DAG specification string,
#'   e.g. \code{"dag{ X -> Y; Z -> X; Z -> Y }"}. Can also be a
#'   \code{dagitty} object.
#' @param exposure Character. Name of the treatment/exposure node.
#' @param outcome Character. Name of the outcome node.
#' @param layout Character. Layout algorithm. One of \code{"circle"},
#'   \code{"star"}, \code{"tree"}, \code{"auto"}. Default \code{"auto"}.
#' @param show_adjustment_set Logical. Highlight a minimal adjustment set.
#'   Default \code{TRUE}.
#' @param show_paths Logical. Annotate causal paths. Default \code{FALSE}.
#' @param title Character. Plot title. Default \code{NULL}.
#' @param node_size Numeric. Node size for ggdag. Default \code{14}.
#'
#' @return A ggplot2 object. If neither \pkg{dagitty} nor \pkg{ggdag} is
#'   installed, returns the raw \code{dag_string} invisibly with a message.
#'
#' @references
#' Textor, J., van der Zander, B., Gilthorpe, M. S., Liskiewicz, M., &
#' Ellison, G. T. (2016). Robust causal inference using directed acyclic
#' graphs: the R package 'dagitty'. \emph{International Journal of
#' Epidemiology}, 45(6), 1887-1894.
#'
#' @examples
#' dag_plot(
#'   dag_string = "dag{ X -> Y; Z -> X; Z -> Y }",
#'   exposure   = "X",
#'   outcome    = "Y",
#'   title      = "Simple Confounded DAG"
#' )
#'
#' @export
dag_plot <- function(dag_string,
                      exposure              = NULL,
                      outcome               = NULL,
                      layout                = c("auto", "circle", "star", "tree"),
                      show_adjustment_set   = TRUE,
                      show_paths            = FALSE,
                      title                 = NULL,
                      node_size             = 14) {

  layout <- match.arg(layout)

  # Check dependencies
  has_dagitty <- requireNamespace("dagitty", quietly = TRUE)
  has_ggdag   <- requireNamespace("ggdag",   quietly = TRUE)

  if (!has_dagitty) {
    message("Package 'dagitty' is required. Install with: install.packages('dagitty')")
    return(invisible(dag_string))
  }

  # Parse DAG
  dag_obj <- if (inherits(dag_string, "dagitty")) {
    dag_string
  } else {
    dagitty::dagitty(dag_string)
  }

  # Set exposure/outcome
  if (!is.null(exposure)) dagitty::exposures(dag_obj)  <- exposure
  if (!is.null(outcome))  dagitty::outcomes(dag_obj)   <- outcome

  # Compute adjustment set
  adj_set <- NULL
  if (show_adjustment_set && !is.null(exposure) && !is.null(outcome)) {
    adj_set <- tryCatch(
      dagitty::adjustmentSets(dag_obj, type = "minimal"),
      error = function(e) NULL
    )
  }

  # ggdag approach (preferred)
  if (has_ggdag) {
    dag_tdy <- ggdag::tidy_dagitty(dag_obj)

    # Status coloring
    status_df <- dag_tdy$data
    status_df$status <- "other"
    if (!is.null(exposure)) status_df$status[status_df$name == exposure] <- "exposure"
    if (!is.null(outcome))  status_df$status[status_df$name == outcome]  <- "outcome"
    dag_tdy$data <- status_df

    p <- ggdag::ggdag(dag_tdy, node_size = node_size) +
      ggplot2::scale_fill_manual(
        values = c("exposure" = "#4575b4", "outcome" = "#d73027", "other" = "grey80"),
        na.value = "grey80"
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

    if (!is.null(title)) {
      p <- p + ggplot2::labs(title = title)
    }

    # Add adjustment set annotation
    if (show_adjustment_set && !is.null(adj_set) && length(adj_set) > 0) {
      adj_nodes <- unlist(adj_set[[1]])
      note <- if (length(adj_nodes) == 0) {
        "Adjustment set: \u2205 (no adjustment needed)"
      } else {
        paste0("Minimal adjustment set: {", paste(adj_nodes, collapse = ", "), "}")
      }
      p <- p + ggplot2::labs(caption = note)
    }

    return(p)
  }

  # Fallback: base graphics via dagitty::plot
  message("Package 'ggdag' not available. Using base dagitty plot.")
  plot(dag_obj, main = title %||% "Causal DAG")

  # Return adjustment set info
  if (show_adjustment_set && !is.null(adj_set)) {
    cat("\nMinimal adjustment sets:\n")
    for (a in adj_set) {
      if (length(a) == 0) cat("  \u2205 (empty set)\n") else cat(" ", paste(a, collapse = ", "), "\n")
    }
  }

  invisible(dag_obj)
}

# Null-coalescing operator (local)
`%||%` <- function(x, y) if (!is.null(x)) x else y


#' List Adjustment Sets for a DAG
#'
#' Identifies minimal sufficient adjustment sets for estimating the causal
#' effect of \code{exposure} on \code{outcome} in a given DAG.
#'
#' @param dag_string Character or dagitty object. The causal DAG.
#' @param exposure Character. Treatment node name.
#' @param outcome Character. Outcome node name.
#' @param type Character. \code{"minimal"} (default), \code{"canonical"}, or
#'   \code{"all"}.
#'
#' @return A list of adjustment sets (each set is a character vector).
#'
#' @export
#' @examples
#' dag_adjustment_sets(
#'   "dag{ X -> Y; Z -> X; Z -> Y; U -> Z; U -> Y }",
#'   exposure = "X", outcome = "Y"
#' )
dag_adjustment_sets <- function(dag_string, exposure, outcome,
                                  type = c("minimal","canonical","all")) {
  type <- match.arg(type)
  if (!requireNamespace("dagitty", quietly = TRUE)) {
    stop("Package 'dagitty' is required. Install with: install.packages('dagitty')")
  }
  dag_obj <- if (inherits(dag_string, "dagitty")) dag_string else dagitty::dagitty(dag_string)
  dagitty::exposures(dag_obj) <- exposure
  dagitty::outcomes(dag_obj)  <- outcome
  sets <- dagitty::adjustmentSets(dag_obj, type = type)
  lapply(sets, function(s) sort(as.character(s)))
}


#' Test Conditional Independence Implied by a DAG
#'
#' Given a causal DAG and observed data, tests all conditional independence
#' relationships implied by the DAG's d-separation structure. Useful for
#' evaluating whether the assumed DAG is compatible with the data.
#'
#' @param dag_string Character or dagitty object. The causal DAG.
#' @param data A data frame. Variable names must match node names in the DAG.
#' @param alpha Numeric. Significance level. Default \code{0.05}.
#'
#' @return A data frame with columns: \code{x}, \code{y},
#'   \code{conditioning_set}, \code{p_value}, \code{rejected} (whether
#'   the independence is rejected at \code{alpha}).
#'
#' @export
#' @examples
#' \dontrun{
#' dag <- "dag{ X -> Y; Z -> X; Z -> Y }"
#' set.seed(1)
#' n   <- 300
#' Z   <- rnorm(n)
#' X   <- 0.5 * Z + rnorm(n)
#' Y   <- 0.3 * X + 0.4 * Z + rnorm(n)
#' df  <- data.frame(X = X, Y = Y, Z = Z)
#' dag_test_implications(dag, df)
#' }
dag_test_implications <- function(dag_string, data, alpha = 0.05) {
  if (!requireNamespace("dagitty", quietly = TRUE)) {
    stop("Package 'dagitty' is required.")
  }
  dag_obj  <- if (inherits(dag_string, "dagitty")) dag_string else dagitty::dagitty(dag_string)
  imp_list <- dagitty::impliedConditionalIndependencies(dag_obj)

  results <- lapply(imp_list, function(impl) {
    x    <- impl$X
    y    <- impl$Y
    cond <- impl$Z

    if (!all(c(x, y, cond) %in% names(data))) return(NULL)

    # Partial correlation test
    pv <- tryCatch({
      if (length(cond) == 0) {
        ct <- stats::cor.test(data[[x]], data[[y]])
        ct$p.value
      } else {
        # Partial correlation via regression residuals
        rx <- residuals(lm(as.formula(paste(x, "~", paste(cond, collapse = "+"))), data = data))
        ry <- residuals(lm(as.formula(paste(y, "~", paste(cond, collapse = "+"))), data = data))
        stats::cor.test(rx, ry)$p.value
      }
    }, error = function(e) NA_real_)

    data.frame(
      x                = x,
      y                = y,
      conditioning_set = paste(cond, collapse = ", "),
      p_value          = round(pv, 4),
      rejected         = !is.na(pv) && pv < alpha,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, Filter(Negate(is.null), results))
  if (is.null(out) || nrow(out) == 0) {
    message("No testable conditional independencies found in this DAG.")
    return(invisible(NULL))
  }
  out
}
