#!/usr/bin/env Rscript

# Reverse dependency source tarball downloader for mlr3pipelines.
# Usage:
#   Rscript download_revdeps.R [--package=mlr3pipelines] [--dest=revdeps-src]
#                              [--repo=https://cran.r-project.org] [--overwrite]
#
# The script pulls the latest CRAN metadata, discovers all reverse dependencies
# of the target package, then downloads their source tarballs into the target
# directory. Existing files are skipped unless --overwrite is set.

suppressPackageStartupMessages({
  # no additional packages required
})

`%||%` <- function(x, y) if (is.null(x) || is.na(x) || identical(x, "")) y else x

usage <- function() {
  cat(
    "download_revdeps.R - download source tarballs for CRAN reverse dependencies\n\n",
    "Options:\n",
    "  --package=<pkg>    Package whose reverse dependencies are targeted.\n",
    "                     Defaults to mlr3pipelines.\n",
    "  --dest=<path>      Directory to store downloaded tarballs.\n",
    "                     Defaults to 'revdeps-src'.\n",
    "  --repo=<url>       CRAN-like repository base URL. Defaults to the\n",
    "                     current getOption('repos')[['CRAN']] (falls back to\n",
    "                     https://cran.r-project.org if unset).\n",
    "  --overwrite        Redownload tarballs even if the destination file\n",
    "                     already exists.\n",
    "  --help             Print this help message and exit.\n",
    sep = ""
  )
}

parse_args <- function(args) {
  defaults <- list(
    package = "mlr3pipelines",
    dest = "revdeps-src",
    repo = NULL,
    overwrite = FALSE
  )
  if (!length(args)) {
    return(defaults)
  }
  for (arg in args) {
    if (arg %in% c("--help", "-h")) {
      usage()
      quit(status = 0)
    }
    if (arg == "--overwrite") {
      defaults$overwrite <- TRUE
      next
    }
    if (!grepl("^--[^=]+=.+", arg)) {
      stop("Unrecognised argument: ", arg)
    }
    parts <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    key <- parts[1]
    value <- parts[2]
    if (!key %in% names(defaults)) {
      stop("Unknown flag: --", key)
    }
    defaults[[key]] <- value
  }
  defaults
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

repo <- args$repo %||% getOption("repos")[["CRAN"]] %||% "https://cran.r-project.org"
if (identical(repo, "@CRAN@")) {
  repo <- "https://cran.r-project.org"
}

dest_dir <- args$dest
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
}

message("Using CRAN repo: ", repo)
message("Target package: ", args$package)
message("Destination dir: ", normalizePath(dest_dir, winslash = "/", mustWork = FALSE))

cran_contrib <- utils::contrib.url(repo, type = "source")
available <- utils::available.packages(contriburl = cran_contrib)

revdeps_raw <- tools::package_dependencies(
  packages = args$package,
  db = available,
  reverse = TRUE,
  which = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
)

revdep_pkgs <- sort(unique(unlist(revdeps_raw, use.names = FALSE)))
if (!length(revdep_pkgs)) {
  message("No reverse dependencies found on CRAN.")
  quit(status = 0)
}

missing_meta <- setdiff(revdep_pkgs, rownames(available))
if (length(missing_meta)) {
  warning(
    "Skipping packages absent from available.packages(): ",
    paste(missing_meta, collapse = ", ")
  )
  revdep_pkgs <- intersect(revdep_pkgs, rownames(available))
}

download_one <- function(pkg, overwrite) {
  version <- available[pkg, "Version"]
  tarball <- sprintf("%s_%s.tar.gz", pkg, version)
  destfile <- file.path(dest_dir, tarball)
  if (!overwrite && file.exists(destfile)) {
    message("[skip] ", tarball, " already exists.")
    return(invisible(TRUE))
  }
  url <- paste0(cran_contrib, "/", tarball)
  message("[download] ", url)
  tryCatch(
    {
      utils::download.file(url, destfile = destfile, mode = "wb", quiet = FALSE)
      TRUE
    },
    error = function(err) {
      warning("Failed to download ", tarball, ": ", conditionMessage(err))
      FALSE
    }
  )
}

results <- vapply(revdep_pkgs, download_one, logical(1), overwrite = args$overwrite)
summary <- table(factor(results, levels = c(TRUE, FALSE)))
message("Download complete. Success: ", summary[["TRUE"]], " / ", length(revdep_pkgs),
  "; Failures: ", summary[["FALSE"]])
