#' Install Backend Packages for Causal Inference Methods
#'
#' Installs suggested packages that provide backends for various causal
#' inference methods. You can install all backends at once or select a
#' specific method category.
#'
#' @param method Character string specifying which method's packages to install.
#'   One of \code{"all"}, \code{"did"}, \code{"rd"}, \code{"sc"}, \code{"iv"},
#'   \code{"matching"}, \code{"sensitivity"}, \code{"rct"}, \code{"event_study"}.
#'   Default is \code{"all"}.
#' @param quiet Logical. If \code{TRUE}, suppress installation messages.
#'   Default is \code{FALSE}.
#'
#' @details
#' The \code{causalverse} package uses a lightweight architecture where backend
#' packages are listed in \code{Suggests} rather than \code{Imports}. This means
#' they are not installed automatically with \code{causalverse}, keeping the
#' initial install fast and lightweight.
#'
#' Use this function to install the backends you need for your analysis. Each
#' method category installs a curated set of state-of-the-art packages:
#'
#' \describe{
#'   \item{\code{"did"}}{did, did2s, DRDID, HonestDiD, bacondecomp,
#'     fect, staggered, TwoWayFEWeights, didimputation}
#'   \item{\code{"rd"}}{rdrobust, rddensity, rdpower, rdlocrand, rdmulti, rdbounds}
#'   \item{\code{"sc"}}{Synth, augsynth, gsynth, MCPanel}
#'   \item{\code{"iv"}}{ivreg, ivmodel, hdm, SteinIV}
#'   \item{\code{"matching"}}{MatchIt, cobalt, WeightIt, optmatch, CBPS, ebal}
#'   \item{\code{"sensitivity"}}{sensemakr, EValue, HonestDiD, rbounds, konfound}
#'   \item{\code{"rct"}}{estimatr, DeclareDesign, randomizr, ri2, grf}
#'   \item{\code{"event_study"}}{EventStudy, estudy2}
#' }
#'
#' @return Invisibly returns a character vector of the packages that were
#'   installed or already available.
#'
#' @examples
#' \dontrun{
#' # Install all backends
#' install_backends()
#'
#' # Install only DID-related packages
#' install_backends("did")
#'
#' # Install IV and sensitivity packages
#' install_backends("iv")
#' install_backends("sensitivity")
#' }
#'
#' @export
install_backends <- function(method = "all", quiet = FALSE) {
  backend_lists <- list(
    did = c("did", "did2s", "DRDID", "HonestDiD", "bacondecomp",
            "fect", "staggered", "TwoWayFEWeights", "didimputation"),
    rd = c("rdrobust", "rddensity", "rdpower", "rdlocrand", "rdmulti"),
    sc = c("Synth", "gsynth"),
    iv = c("ivreg", "ivmodel"),
    matching = c("MatchIt", "cobalt", "WeightIt", "optmatch", "CBPS"),
    sensitivity = c("sensemakr", "EValue", "HonestDiD", "rbounds", "konfound"),
    rct = c("estimatr", "DeclareDesign", "randomizr", "ri2"),
    event_study = c("EventStudy", "estudy2")
  )

  # GitHub-only packages that need special repos
  github_pkgs <- list(
    rdbounds = "francoisgerard/rdbounds/R",
    augsynth = "ebenmichael/augsynth",
    MCPanel  = "susanathey/MCPanel",
    synthdid = "synth-inference/synthdid"
  )

  method <- match.arg(method, c("all", names(backend_lists)))

  if (method == "all") {
    pkgs <- unique(unlist(backend_lists))
  } else {
    pkgs <- backend_lists[[method]]
  }

  # Separate CRAN and GitHub packages
  cran_pkgs <- setdiff(pkgs, names(github_pkgs))
  gh_pkgs <- intersect(pkgs, names(github_pkgs))

  # Check what's already installed
  already <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  to_install_cran <- setdiff(cran_pkgs, pkgs[already])
  to_install_gh <- setdiff(gh_pkgs, pkgs[already])

  if (length(to_install_cran) == 0 && length(to_install_gh) == 0) {
    if (!quiet) message("All ", method, " backend packages are already installed.")
    return(invisible(pkgs))
  }

  if (!quiet) {
    n_total <- length(to_install_cran) + length(to_install_gh)
    message("Installing ", n_total, " package(s) for '", method, "' methods...")
  }

  # Install CRAN packages

  if (length(to_install_cran) > 0) {
    if (!quiet) message("  CRAN: ", paste(to_install_cran, collapse = ", "))
    utils::install.packages(to_install_cran, quiet = quiet)
  }

  # Install GitHub packages
  if (length(to_install_gh) > 0) {
    if (!quiet) message("  GitHub: ", paste(to_install_gh, collapse = ", "))
    if (requireNamespace("pak", quietly = TRUE)) {
      for (pkg in to_install_gh) {
        pak::pkg_install(github_pkgs[[pkg]], ask = FALSE)
      }
    } else if (requireNamespace("remotes", quietly = TRUE)) {
      for (pkg in to_install_gh) {
        remotes::install_github(github_pkgs[[pkg]], quiet = quiet)
      }
    } else {
      warning(
        "GitHub packages (", paste(to_install_gh, collapse = ", "),
        ") require 'pak' or 'remotes' to install. ",
        "Run install.packages('pak') first."
      )
    }
  }

  invisible(pkgs)
}


#' List Available Backend Packages and Their Status
#'
#' Shows which backend packages are installed and which are missing,
#' organized by method category.
#'
#' @return A data frame with columns: \code{method}, \code{package},
#'   \code{installed}, \code{version}.
#'
#' @examples
#' \dontrun{
#' check_backends()
#' }
#'
#' @export
check_backends <- function() {
  backend_lists <- list(
    did = c("did", "did2s", "DRDID", "HonestDiD", "bacondecomp",
            "fect", "staggered", "TwoWayFEWeights", "didimputation"),
    rd = c("rdrobust", "rddensity", "rdpower", "rdlocrand", "rdmulti",
           "rdbounds"),
    sc = c("Synth", "augsynth", "gsynth", "MCPanel"),
    iv = c("ivreg", "ivmodel"),
    matching = c("MatchIt", "cobalt", "WeightIt", "optmatch", "CBPS"),
    sensitivity = c("sensemakr", "EValue", "HonestDiD", "rbounds", "konfound"),
    rct = c("estimatr", "DeclareDesign", "randomizr", "ri2"),
    event_study = c("EventStudy", "estudy2")
  )

  rows <- list()
  for (m in names(backend_lists)) {
    for (pkg in backend_lists[[m]]) {
      installed <- requireNamespace(pkg, quietly = TRUE)
      version <- if (installed) {
        as.character(utils::packageVersion(pkg))
      } else {
        NA_character_
      }
      rows <- c(rows, list(data.frame(
        method = m, package = pkg,
        installed = installed, version = version,
        stringsAsFactors = FALSE
      )))
    }
  }

  result <- do.call(rbind, rows)

  # Print summary
  n_installed <- sum(result$installed)
  n_total <- nrow(result)
  message(
    "causalverse backends: ", n_installed, "/", n_total, " installed\n"
  )

  for (m in names(backend_lists)) {
    sub <- result[result$method == m, ]
    n_ok <- sum(sub$installed)
    n_all <- nrow(sub)
    status <- if (n_ok == n_all) "\u2705" else "\u26a0\ufe0f"
    message("  ", status, " ", m, ": ", n_ok, "/", n_all)
    missing <- sub$package[!sub$installed]
    if (length(missing) > 0) {
      message("     missing: ", paste(missing, collapse = ", "))
    }
  }

  message("\nRun causalverse::install_backends() to install all, or")
  message("causalverse::install_backends(\"method\") for a specific method.")

  invisible(result)
}
