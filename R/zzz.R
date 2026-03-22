# Core packages that are attached when causalverse is loaded.
# This mirrors the tidyverse approach: users get these packages
# available without needing separate library() calls.
.core_packages <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "purrr",
  "magrittr",
  "data.table",
  "fixest",
  "rio"
)

# Attach a package from the same library location as causalverse
# (mirrors tidyverse:::same_library)
.same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) {
    dirname(getNamespaceInfo(pkg, "path"))
  }
  library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
}

.onAttach <- function(libname, pkgname) {
  # Attach each core package, collecting any that fail
  failed <- character()
  attached <- character()

  for (pkg in .core_packages) {
    success <- tryCatch(
      {
        suppressPackageStartupMessages(.same_library(pkg))
        TRUE
      },
      error = function(e) FALSE
    )

    if (success) {
      attached <- c(attached, pkg)
    } else {
      failed <- c(failed, pkg)
    }
  }

  # Build startup message
  versions <- vapply(attached, function(pkg) {
    as.character(utils::packageVersion(pkg))
  }, character(1))

  # Format like tidyverse: two columns of package:version
  pkg_labels <- paste0(attached, " ", versions)
  n <- length(pkg_labels)
  if (n > 0) {
    half <- ceiling(n / 2)
    col1 <- pkg_labels[seq_len(half)]
    col2 <- if (half < n) pkg_labels[(half + 1):n] else character()
    # Pad col1 for alignment
    max_width <- max(nchar(col1))
    col1 <- formatC(col1, width = -max_width, flag = "-")
    lines <- character(half)
    for (i in seq_along(col1)) {
      if (i <= length(col2)) {
        lines[i] <- paste0(col1[i], "   ", col2[i])
      } else {
        lines[i] <- col1[i]
      }
    }
    header <- "-- Attaching causalverse core packages --"
    msg <- paste(c(header, lines), collapse = "\n")
    packageStartupMessage(msg)
  }

  if (length(failed) > 0) {
    packageStartupMessage(
      "! Failed to attach: ", paste(failed, collapse = ", ")
    )
  }
}
