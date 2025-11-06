#!/usr/bin/env Rscript

# Install Suggests dependencies of all CRAN reverse dependencies for
# mlr3pipelines (or a specified package).
# Usage:
#   Rscript install_revdep_suggests.R [--package=mlr3pipelines]
#                                     [--repo=https://cran.r-project.org]
#                                     [--lib=<path>] [--type=source]
#
# The script resolves reverse dependencies on CRAN, gathers their Suggests
# fields, and installs the union of those packages. Reverse dependencies
# themselves are not installed.

`%||%` <- function(x, y) if (is.null(x) || is.na(x) || identical(x, "")) y else x

usage <- function() {
  cat(
    "install_revdep_suggests.R - install Suggests of all reverse dependencies\n\n",
    "Options:\n",
    "  --package=<pkg>  Package whose reverse dependencies are analysed.\n",
    "                   Defaults to mlr3pipelines.\n",
    "  --repo=<url>     CRAN-like repository base URL. Defaults to the current\n",
    "                   getOption('repos')[['CRAN']] (falls back to\n",
    "                   https://cran.r-project.org if unset).\n",
    "  --lib=<path>     Target library directory for installations. Defaults to\n",
    "                   R_LIBS_USER.\n",
    "  --type=<type>    Package type passed to install.packages(). Defaults to\n",
    "                   getOption('pkgType').\n",
    "  --help           Print this help message and exit.\n",
    sep = ""
  )
}

parse_args <- function(args) {
  defaults <- list(
    package = "mlr3pipelines",
    repo = NULL,
    lib = NULL,
    type = getOption("pkgType")
  )
  if (!length(args)) {
    return(defaults)
  }
  for (arg in args) {
    if (arg %in% c("--help", "-h")) {
      usage()
      quit(status = 0)
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

lib_path <- args$lib %||% Sys.getenv("R_LIBS_USER")
if (!dir.exists(lib_path)) {
  dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
}

install_type <- args$type %||% getOption("pkgType") %||% "source"

message("Using CRAN repo: ", repo)
message("Target package: ", args$package)
message("Library path: ", normalizePath(lib_path, winslash = "/", mustWork = FALSE))
message("Install type: ", install_type)

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

suggest_map <- tools::package_dependencies(
  packages = revdep_pkgs,
  db = available,
  which = "Suggests",
  recursive = FALSE
)

suggested_pkgs <- sort(unique(unlist(suggest_map, use.names = FALSE)))
suggested_pkgs <- suggested_pkgs[nzchar(suggested_pkgs)]

if (!length(suggested_pkgs)) {
  message("Reverse dependencies do not list any Suggests packages on CRAN.")
  quit(status = 0)
}

suggested_pkgs <- setdiff(suggested_pkgs, revdep_pkgs)

installed <- character(0)
try({
  installed <- utils::installed.packages(lib.loc = lib_path)[, "Package"]
}, silent = TRUE)

to_install <- setdiff(suggested_pkgs, installed)
if (!length(to_install)) {
  message("All suggested packages already installed in ", lib_path)
  quit(status = 0)
}

message("Installing ", length(to_install), " packages...")
utils::install.packages(
  pkgs = to_install,
  repos = repo,
  lib = lib_path,
  type = install_type,
  dependencies = FALSE
)

message("Installation attempt finished. Review output for any failures.")
