#' Instrumental Variable Diagnostic Summary
#'
#' Extracts key diagnostic statistics from an instrumental variables regression,
#' including the first-stage F-statistic, Wu-Hausman endogeneity test, and
#' effective F-statistic (when available). Supports \code{fixest} IV models
#' and \code{ivreg} models.
#'
#' @param model An IV model object. Supported classes: \code{fixest} (from
#'   \code{\link[fixest]{feols}} with IV syntax) or \code{ivreg} (from
#'   \code{\link[ivreg]{ivreg}}).
#' @param conf_level Numeric. Confidence level for decision thresholds.
#'   Default is \code{0.95}.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{test}{Character. Name of the diagnostic test.}
#'     \item{statistic}{Numeric. Test statistic value.}
#'     \item{p_value}{Numeric. P-value (where applicable; \code{NA} for
#'       effective F).}
#'     \item{decision}{Character. Plain-language interpretation at the
#'       given confidence level.}
#'   }
#'
#' @examples
#' \dontrun{
#' # IV regression with fixest
#' mod <- feols(y ~ x1 | z1 + z2 ~ endo1, data = mydata)
#' iv_diagnostic_summary(mod)
#'
#' # IV regression with ivreg
#' library(ivreg)
#' mod2 <- ivreg(y ~ endo1 + x1 | z1 + z2 + x1, data = mydata)
#' iv_diagnostic_summary(mod2)
#' }
#'
#' @importFrom stats pf
#' @export
iv_diagnostic_summary <- function(model, conf_level = 0.95) {

  alpha <- 1 - conf_level
  results <- list()

  if (inherits(model, "fixest")) {
    # --- fixest IV model ---
    if (!isTRUE(model$iv)) {
      stop("`model` does not appear to be a fixest IV regression.", call. = FALSE)
    }

    # First-stage F-statistic
    fs <- tryCatch({
      fitstat <- fixest::fitstat(model, type = "ivf")
      # fitstat returns a list; extract first-stage F
      fs_val <- fitstat$ivf$stat
      fs_p   <- fitstat$ivf$p
      data.frame(
        test      = "First-Stage F",
        statistic = fs_val,
        p_value   = fs_p,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        test      = "First-Stage F",
        statistic = NA_real_,
        p_value   = NA_real_,
        stringsAsFactors = FALSE
      )
    })
    results[["fs"]] <- fs

    # Wu-Hausman endogeneity test
    wh <- tryCatch({
      wh_stat <- fixest::fitstat(model, type = "wh")
      data.frame(
        test      = "Wu-Hausman",
        statistic = wh_stat$wh$stat,
        p_value   = wh_stat$wh$p,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        test      = "Wu-Hausman",
        statistic = NA_real_,
        p_value   = NA_real_,
        stringsAsFactors = FALSE
      )
    })
    results[["wh"]] <- wh

    # Effective F (Olea & Pflueger)
    ef <- tryCatch({
      ef_stat <- fixest::fitstat(model, type = "ivf.eff")
      data.frame(
        test      = "Effective F (Olea-Pflueger)",
        statistic = ef_stat$ivf.eff$stat,
        p_value   = NA_real_,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        test      = "Effective F (Olea-Pflueger)",
        statistic = NA_real_,
        p_value   = NA_real_,
        stringsAsFactors = FALSE
      )
    })
    results[["ef"]] <- ef

  } else if (inherits(model, "ivreg")) {
    # --- ivreg model ---
    if (!requireNamespace("ivreg", quietly = TRUE)) {
      stop("Package 'ivreg' is required but not installed.", call. = FALSE)
    }

    summ <- summary(model, diagnostics = TRUE)

    # First-stage (Weak instruments) test
    wi <- tryCatch({
      diag_weak <- summ$diagnostics["Weak instruments", ]
      data.frame(
        test      = "First-Stage F (Weak Instruments)",
        statistic = diag_weak["statistic"],
        p_value   = diag_weak["p-value"],
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        test      = "First-Stage F (Weak Instruments)",
        statistic = NA_real_,
        p_value   = NA_real_,
        stringsAsFactors = FALSE
      )
    })
    results[["fs"]] <- wi

    # Wu-Hausman
    wh <- tryCatch({
      diag_wh <- summ$diagnostics["Wu-Hausman", ]
      data.frame(
        test      = "Wu-Hausman",
        statistic = diag_wh["statistic"],
        p_value   = diag_wh["p-value"],
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        test      = "Wu-Hausman",
        statistic = NA_real_,
        p_value   = NA_real_,
        stringsAsFactors = FALSE
      )
    })
    results[["wh"]] <- wh

    # Sargan (overidentification) if available
    sargan <- tryCatch({
      diag_s <- summ$diagnostics["Sargan", ]
      data.frame(
        test      = "Sargan (Overidentification)",
        statistic = diag_s["statistic"],
        p_value   = diag_s["p-value"],
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      NULL
    })
    if (!is.null(sargan)) {
      results[["sargan"]] <- sargan
    }

  } else {
    stop(
      "Unsupported model class: ", paste(class(model), collapse = ", "), ". ",
      "Supported: fixest (IV), ivreg.",
      call. = FALSE
    )
  }

  # Combine results
  out <- do.call(rbind, results)
  rownames(out) <- NULL

  # Add decision column
  out$decision <- vapply(seq_len(nrow(out)), function(i) {
    test_name <- out$test[i]
    stat_val  <- out$statistic[i]
    p_val     <- out$p_value[i]

    if (is.na(stat_val)) {
      return("Not available")
    }

    if (grepl("First-Stage|Weak Instruments", test_name)) {
      if (stat_val >= 10) {
        return("Strong instruments (F >= 10)")
      } else {
        return("Weak instruments (F < 10)")
      }
    }

    if (grepl("Effective F", test_name)) {
      if (stat_val >= 10) {
        return("Strong instruments (Eff. F >= 10)")
      } else {
        return("Potentially weak instruments (Eff. F < 10)")
      }
    }

    if (grepl("Wu-Hausman", test_name)) {
      if (!is.na(p_val) && p_val < alpha) {
        return(paste0("Endogeneity detected (p < ", alpha, ")"))
      } else {
        return(paste0("No endogeneity detected (p >= ", alpha, ")"))
      }
    }

    if (grepl("Sargan", test_name)) {
      if (!is.na(p_val) && p_val < alpha) {
        return(paste0("Overidentification rejected (p < ", alpha, ")"))
      } else {
        return(paste0("Overidentification not rejected (p >= ", alpha, ")"))
      }
    }

    "No interpretation available"
  }, character(1))

  out
}
