# Base-R exact-output resource probe, with a guarded family that has no units.
source("attic/cnf_verify3/unit_queue/variant.R")
n = as.integer(Sys.getenv("CNF_CHAIN_N", "32"))
kind = Sys.getenv("CNF_QUEUE_VARIANT", "queued")
family = Sys.getenv("CNF_CHAIN_FAMILY", "unit")
reverse = Sys.getenv("CNF_CHAIN_ORDER", "reverse") == "reverse"
measure = Sys.getenv("CNF_CHAIN_MEASURE", "0") == "1"
stopifnot(n >= 2L, family %in% c("unit", "guarded"))
max_frames = 0L
register_frames = integer()
eliminate_frames = integer()
max_stack = 0
max_eval = 0
max_pending = 0L
observe = if (measure) function(event, frame) {
  depth = sys.nframe()
  max_frames <<- max(max_frames, depth)
  stack = Cstack_info()
  max_stack <<- max(max_stack, stack[["current"]])
  max_eval <<- max(max_eval, stack[["eval_depth"]])
  if (event == "register_unit") register_frames[[length(register_frames) + 1L]] <<- depth
  if (event == "eliminate_symbol_from_clause") eliminate_frames[[length(eliminate_frames) + 1L]] <<- depth
  if (event == "unit_enqueued") {
    max_pending <<- max(max_pending, length(get("unit_work", frame, inherits = TRUE)) -
      get("unit_work_head", frame, inherits = TRUE) + 1L)
  }
} else NULL
simplify = make_unit_variant(kind, observe)
symbols = paste0("X", seq_len(n))
universe = setNames(rep(list(c("0", "1")), n), symbols)
seed = setNames(list("1"), symbols[[1L]])
links = lapply(seq_len(n - 1L), function(i) setNames(list("0", "1"), symbols[c(i, i + 1L)]))
if (family == "guarded") {
  universe = c(list(G = c("0", "1")), universe)
  seed = c(list(G = "1"), seed)
  links = lapply(links, function(clause) c(list(G = "1"), clause))
}
if (reverse) links = rev(links)
input = c(list(seed), links)
start = proc.time()[["elapsed"]]
result = tryCatch(simplify(input, universe), error = identity)
elapsed = proc.time()[["elapsed"]] - start
cat(R.version.string, "\n")
cat("kind:", kind, "family:", family, "n:", n, "reverse:", reverse, "seconds:", elapsed, "\n")
if (inherits(result, "error")) {
  cat("ERROR:", conditionMessage(result), "\n")
} else {
  expected_length = if (family == "unit") 1L else 2L
  stopifnot(length(result) == n, all(lengths(result) == expected_length))
  seen = vapply(result, function(clause) setdiff(names(clause), "G"), character(1))
  stopifnot(setequal(seen, symbols))
  stopifnot(all(vapply(result, function(clause) all(unlist(clause) == "1"), logical(1))))
  cat("PASS: exactly", n, "entailed", if (family == "unit") "units" else "guarded clauses", "\n")
}
if (measure) {
  cat("max_frames:", max_frames, "max_Cstack:", max_stack, "max_eval_depth:", max_eval,
    "max_pending:", max_pending, "\n")
  cat("register_frames:", paste(register_frames, collapse = ","), "\n")
  cat("eliminate_frames:", paste(eliminate_frames, collapse = ","), "\n")
}
