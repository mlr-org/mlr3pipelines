#!/usr/bin/env Rscript
## Robust load-test of installed packages in isolated R processes.

suppressWarnings(suppressMessages({
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required. Install it with: install.packages('callr')", call. = FALSE)
  }
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package 'parallel' is required (part of base R on Linux).", call. = FALSE)
  }
}))

## ----------------------------
## CLI argument parsing (no extra deps)
## ----------------------------
args <- commandArgs(trailingOnly = TRUE)

arg_val <- function(flag, default = NULL, is_flag = FALSE) {
  i <- which(grepl(paste0("^", flag, "(=|$)"), args))
  if (length(i) == 0) return(default)
  if (is_flag) return(TRUE)
  a <- args[i[1]]
  if (grepl("=", a)) sub("^[^=]+=\\s*", "", a) else default
}

help <- any(args %in% c("-h", "--help"))
if (help) {
  cat("
Usage: Rscript check_packages_callr.R [options]

Options:
  --cores=N                 Number of parallel workers (default: all available)
  --timeout=SEC             Per-package timeout in seconds (default: 60)
  --include-recommended     Also test 'Recommended' packages (default: FALSE)
  --pattern=REGEX           Only test packages whose names match REGEX
  --exclude=REGEX           Exclude packages whose names match REGEX
  --list                    Only list selected package set and exit
  --seed=INT                RNG seed for reproducibility of package behavior
  -h, --help                Show this help

Examples:
  Rscript check_packages_callr.R --cores=8 --timeout=45
  Rscript check_packages_callr.R --pattern='^(mlr|keras)$'
  Rscript check_packages_callr.R --exclude='^(mlr|rJava)$'
\n")
  quit(save = "no", status = 0)
}

cores   <- as.integer(arg_val("--cores", parallel::detectCores()))
timeout <- as.numeric(arg_val("--timeout", 60))
inc_rec <- isTRUE(arg_val("--include-recommended", is_flag = TRUE))
pat     <- arg_val("--pattern", NULL)
exc     <- arg_val("--exclude", NULL)
list_only <- isTRUE(arg_val("--list", is_flag = TRUE))
seed    <- arg_val("--seed", NULL); if (!is.null(seed)) set.seed(as.integer(seed))

## ----------------------------
## Select packages to test
## ----------------------------
ip <- utils::installed.packages()
pkgs <- rownames(ip)

## Drop base and recommended by default
if (!inc_rec) {
  prio <- ip[, "Priority"]
  pkgs <- pkgs[is.na(prio) | prio == ""]
}

## Apply include/exclude filters
if (!is.null(pat)) pkgs <- pkgs[grepl(pat, pkgs)]
if (!is.null(exc)) pkgs <- pkgs[!grepl(exc, pkgs)]

pkgs <- sort(unique(pkgs))

if (list_only) {
  cat(sprintf("Selected %d package(s):\n", length(pkgs)))
  cat(paste(pkgs, collapse = "\n"), "\n")
  quit(save = "no", status = 0)
}

if (length(pkgs) == 0) {
  cat("No packages selected. Nothing to do.\n")
  quit(save = "no", status = 0)
}

## ----------------------------
## Worker: load a single package in a clean R
## ----------------------------
test_one <- function(pkg, timeout_sec) {
  started <- Sys.time()
  status  <- "ok"
  msg     <- NA_character_
  ver     <- as.character(utils::packageVersion(pkg))
  ## Use callr::r_safe to spawn a minimal clean process.
  res <- tryCatch(
    {
      callr::r_safe(
        function(p) {
          suppressPackageStartupMessages({
            library(p, character.only = TRUE)
          })
          TRUE
        },
        args = list(p = pkg),
        timeout = timeout_sec
      )
    },
    callr_timeout_error = function(e) { status <<- "timeout"; msg <<- conditionMessage(e); NA },
    callr_status_error  = function(e) { status <<- "crash";   msg <<- conditionMessage(e); NA },
    error               = function(e) { status <<- "error";   msg <<- conditionMessage(e); NA }
  )
  finished <- Sys.time()
  data.frame(
    package  = pkg,
    version  = ver,
    status   = status,
    message  = ifelse(is.na(msg), "", substr(msg, 1L, 500L)),
    duration = as.numeric(difftime(finished, started, units = "secs")),
    stringsAsFactors = FALSE
  )
}

## ----------------------------
## Parallel execution
## ----------------------------
cat(sprintf("Starting load test for %d package(s) with %d core(s), timeout=%ss …\n",
            length(pkgs), cores, timeout))

## mclapply (Linux/macOS). On Linux this is fine and efficient.
## If mc.cores==1, it runs sequentially.
results_list <- parallel::mclapply(
  pkgs,
  function(p) test_one(p, timeout),
  mc.cores = max(1L, cores),
  mc.preschedule = FALSE
)

res <- do.call(rbind, results_list)

## ----------------------------
## Report
## ----------------------------
ok_n      <- sum(res$status == "ok")
timeout_n <- sum(res$status == "timeout")
crash_n   <- sum(res$status == "crash")
error_n   <- sum(res$status == "error")
fail_n    <- nrow(res) - ok_n

cat("\n==================== SUMMARY ====================\n")
cat(sprintf("Total packages tested : %d\n", nrow(res)))
cat(sprintf("OK                    : %d\n", ok_n))
cat(sprintf("ERROR                 : %d\n", error_n))
cat(sprintf("TIMEOUT               : %d\n", timeout_n))
cat(sprintf("CRASH (non-zero exit) : %d\n", crash_n))
cat("=================================================\n\n")

if (fail_n > 0) {
  cat("Packages that failed to load cleanly:\n")
  print(utils::head(res[res$status != "ok", c("package", "version", "status", "duration", "message")], 50), row.names = FALSE)
  if (fail_n > 50) {
    cat(sprintf("… and %d more. Use --pattern/--exclude to focus, or write to file (see below).\n\n", fail_n - 50))
  } else {
    cat("\n")
  }

  ## Aggregate by status/message to spot common causes (e.g., missing libssl, GLIBCXX)
  agg <- aggregate(list(n = res$package[res$status != "ok"]),
                   by = list(status = res$status[res$status != "ok"],
                             message = res$message[res$status != "ok"]),
                   FUN = length)
  ord <- order(agg$n, decreasing = TRUE)
  cat("Most common failure messages:\n")
  print(utils::head(agg[ord, ], 10), row.names = FALSE)
  cat("\n")

  ## Convenience: suggest reinstall-from-source line for purely 'error' cases
  reinstall <- res$package[res$status %in% c("error")]
  if (length(reinstall)) {
    cmd <- sprintf("install.packages(c(%s), type = 'source')",
                   paste(sprintf("'%s'", sort(unique(reinstall))), collapse = ", "))
    cat("Suggested reinstall from source for 'error' cases:\n")
    cat(cmd, "\n\n")
  }
} else {
  cat("All selected packages loaded successfully in isolated sessions.\n\n")
}

## Optional: write CSVs if env vars are set (no extra CLI complexity)
out_csv <- Sys.getenv("PKG_CHECK_OUT_CSV", "")
if (nzchar(out_csv)) {
  utils::write.csv(res, file = out_csv, row.names = FALSE)
  cat(sprintf("Wrote full results to: %s\n", out_csv))
}

invisible(NULL)

