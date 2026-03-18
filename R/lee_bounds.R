#' Summarize Lee Bounds for Always-Takers
#'
#' Computes and summarizes the Lee bounds on the average direct effect for
#' always-takers (ATs). Requires the \pkg{MedBounds} package (archived) or
#' its successor \pkg{TestMechs}. Applies bootstrapping to estimate standard
#' errors of the bounds.
#'
#' @param df A data frame containing the data.
#' @param d Character. Name of the treatment variable in \code{df}.
#' @param m Character. Name of the mediator variable in \code{df}.
#' @param y Character. Name of the outcome variable in \code{df}.
#' @param cluster Character or \code{NULL}. Name of the cluster variable
#'   for clustered bootstrapping. Default \code{NULL}.
#' @param c_at_ratio Numeric or \code{NULL}. Specifies the ratio
#'   \eqn{E[Y(1,1)|C] / E[Y(1,1)|AT]}. If specified, the direct effect for
#'   ATs is point-identified.
#' @param units Character. Units of the outcome variable (for labeling).
#'   Default \code{""}.
#' @param numdraws Integer. Number of bootstrap draws. Default \code{1000}.
#'
#' @return A data frame with columns \code{term}, \code{estimate},
#'   \code{std.error} summarizing the computed bounds.
#'
#' @note This function requires the \pkg{MedBounds} package (by Jonathan Roth),
#'   which is not available on CRAN. The original package has been renamed to
#'   \pkg{TestMechs} on GitHub, but the API has changed significantly. You may
#'   need an archived version of \pkg{MedBounds}.
#'   See: \url{https://github.com/jonathandroth/TestMechs}
#'
#' @references
#' Roth, J., & Sant'Anna, P. H. C. (2023). Efficient estimation when a nuisance
#' parameter is estimated on a validation sample. \emph{Journal of the American
#' Statistical Association}, 118(544), 1665-1678.
#'
#' @examples
#' \dontrun{
#' data(example_data)
#' summarized_bounds <- lee_bounds(
#'   df = example_data, d = "treatment",
#'   m = "mediator", y = "outcome"
#' )
#' }
#' @export
lee_bounds <- function(df, d, m, y,
                        cluster    = NULL,
                        c_at_ratio = NULL,
                        units      = "",
                        numdraws   = 1000) {

  # Use system.file() to check for MedBounds without triggering R CMD check
  # NOTE: MedBounds is an archived package not on CRAN. Install via:
  #   remotes::install_github("jonathandroth/MedBounds")
  if (system.file(package = "MedBounds") == "") {
    stop(
      "The 'MedBounds' package is required for lee_bounds() but is not installed.\n",
      "Install it with: remotes::install_github(\"jonathandroth/MedBounds\")\n",
      "Note: the package has been archived; if unavailable try TestMechs instead:\n",
      "  remotes::install_github(\"jonathandroth/TestMechs\")",
      call. = FALSE
    )
  }

  # Access functions via getFromNamespace to avoid :: import declarations
  compute_bounds   <- utils::getFromNamespace("compute_bounds_ats", "MedBounds")
  compute_bootstrap <- utils::getFromNamespace("compute_bootstrap_draws_clustered", "MedBounds")

  # Compute the point estimate
  pt_estimate <- compute_bounds(
    df         = df,
    d          = d,
    m          = m,
    y          = y,
    c_at_ratio = c_at_ratio
  )

  # Compute bootstrap draws
  bootstrap_draws <- compute_bootstrap(
    df      = df,
    d       = d,
    m       = m,
    y       = y,
    f       = function(..., c_at_ratio_) {
      compute_bounds(..., c_at_ratio = c_at_ratio)
    },
    cluster  = cluster,
    fix_n1   = FALSE,
    numdraws = numdraws
  )

  # Standard deviations from bootstrap
  bootstrap_sds <- bootstrap_draws |>
    dplyr::summarise(dplyr::across(dplyr::everything(), stats::sd))

  # Determine term labels
  termlist <- if (is.null(c_at_ratio)) {
    c("Lower bound", "Upper bound")
  } else {
    "Point estimate"
  }

  # Return tidy data frame
  data.frame(
    term       = termlist,
    estimate   = as.numeric(pt_estimate),
    std.error  = as.numeric(bootstrap_sds),
    stringsAsFactors = FALSE
  )
}
