#' Publication-Ready Covariate Balance Table
#'
#' Produces a publication-ready covariate balance table comparing means,
#' standard deviations, standardized mean differences (SMD), and variance
#' ratios between treated and control groups. Optionally returns a \pkg{gt}
#' or \pkg{knitr} formatted table.
#'
#' @param data A data frame containing the covariates and treatment indicator.
#' @param treatment Character. Name of the binary treatment indicator column
#'   (0/1 or TRUE/FALSE).
#' @param covariates Character vector. Names of covariate columns to include.
#'   If \code{NULL} (default), all numeric and logical columns except
#'   \code{treatment} are used.
#' @param weights Numeric vector of length \code{nrow(data)}. Optional
#'   analytical weights (e.g., inverse probability weights).
#' @param smd_threshold Numeric. Threshold for flagging imbalanced covariates
#'   (|SMD| > threshold). Default \code{0.1}.
#' @param digits Integer. Number of decimal places. Default \code{3}.
#' @param output_format Character. Table format: \code{"df"} (data frame,
#'   default), \code{"kable"} (knitr table), or \code{"gt"} (gt table).
#' @param caption Character. Table caption for formatted outputs.
#'
#' @return A data frame (when \code{output_format = "df"}) or formatted table
#'   with columns:
#'   \describe{
#'     \item{covariate}{Covariate name.}
#'     \item{mean_control}{Weighted mean in control group.}
#'     \item{mean_treated}{Weighted mean in treated group.}
#'     \item{sd_control}{Standard deviation in control group.}
#'     \item{sd_treated}{Standard deviation in treated group.}
#'     \item{smd}{Standardized mean difference.}
#'     \item{var_ratio}{Variance ratio (treated/control).}
#'     \item{flag}{Logical. Whether |SMD| exceeds \code{smd_threshold}.}
#'   }
#'
#' @details
#' The standardized mean difference (SMD) is computed as:
#' \deqn{SMD = \frac{\bar{X}_t - \bar{X}_c}{\sqrt{(s_t^2 + s_c^2)/2}}}
#' following @rubin2001using. Values below 0.1 are conventionally considered
#' well-balanced.
#'
#' @references
#' Rubin, D. B. (2001). Using propensity scores to help design observational
#' studies: application to the tobacco litigation. \emph{Health Services and
#' Outcomes Research Methodology}, 2(3–4), 169–188.
#'
#' @examples
#' data(lalonde, package = "MatchIt")
#' balance_table(
#'   data = lalonde,
#'   treatment = "treat",
#'   covariates = c("age", "educ", "re74", "re75")
#' )
#'
#' @importFrom stats weighted.mean sd var
#' @export
balance_table <- function(data,
                          treatment,
                          covariates     = NULL,
                          weights        = NULL,
                          smd_threshold  = 0.1,
                          digits         = 3,
                          output_format  = c("df", "kable", "gt"),
                          caption        = "Covariate Balance Table") {

  output_format <- match.arg(output_format)

  if (!treatment %in% names(data)) {
    stop("`treatment` column '", treatment, "' not found in data.", call. = FALSE)
  }

  treat_vec <- as.integer(as.logical(data[[treatment]]))

  if (is.null(covariates)) {
    covariates <- setdiff(names(data)[vapply(data, function(x) {
      is.numeric(x) || is.logical(x)
    }, logical(1))], treatment)
  }

  # Check all covariates exist
  missing_vars <- setdiff(covariates, names(data))
  if (length(missing_vars) > 0) {
    stop("Covariates not found: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  }

  if (is.null(weights)) {
    weights <- rep(1, nrow(data))
  }

  w0 <- weights[treat_vec == 0]
  w1 <- weights[treat_vec == 1]

  rows <- lapply(covariates, function(v) {
    x <- data[[v]]
    x0 <- x[treat_vec == 0]
    x1 <- x[treat_vec == 1]

    # Weighted means
    m0 <- stats::weighted.mean(x0, w0, na.rm = TRUE)
    m1 <- stats::weighted.mean(x1, w1, na.rm = TRUE)

    # Weighted SDs
    wsd <- function(xi, wi) {
      wi  <- wi / sum(wi, na.rm = TRUE)
      mui <- sum(wi * xi, na.rm = TRUE)
      sqrt(sum(wi * (xi - mui)^2, na.rm = TRUE))
    }

    s0 <- wsd(x0, w0)
    s1 <- wsd(x1, w1)

    # SMD (pooled SD denominator)
    pooled_sd <- sqrt((s0^2 + s1^2) / 2)
    smd <- if (pooled_sd == 0) NA_real_ else (m1 - m0) / pooled_sd

    # Variance ratio
    vr <- if (s0 == 0) NA_real_ else s1^2 / s0^2

    data.frame(
      covariate   = v,
      mean_control = round(m0, digits),
      mean_treated = round(m1, digits),
      sd_control   = round(s0, digits),
      sd_treated   = round(s1, digits),
      smd          = round(smd, digits),
      var_ratio    = round(vr, digits),
      flag         = !is.na(smd) && abs(smd) > smd_threshold,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  if (output_format == "df") {
    return(out)
  }

  if (output_format == "kable") {
    if (!requireNamespace("knitr", quietly = TRUE)) {
      warning("Package 'knitr' not available. Returning data frame.",
              call. = FALSE)
      return(out)
    }
    knitr::kable(out[, setdiff(names(out), "flag")],
                 caption = caption,
                 digits  = digits)
  } else if (output_format == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      warning("Package 'gt' not available. Returning data frame.",
              call. = FALSE)
      return(out)
    }
    gt::gt(out[, setdiff(names(out), "flag")]) |>
      gt::tab_header(title = caption) |>
      gt::fmt_number(columns = where(is.numeric), decimals = digits)
  }
}
