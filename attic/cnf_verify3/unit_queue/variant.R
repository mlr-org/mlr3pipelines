# Diagnostic variants evaluated only in memory; production source is untouched.
# A synchronous no-skip control separates removing the optimization from queuing.
make_unit_variant = function(kind = c("production", "no_skip", "queued"), observe = NULL) {
  kind = match.arg(kind)
  src = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
  replace_once = function(old, new) {
    positions = gregexpr(old, src, fixed = TRUE)[[1L]]
    stopifnot(length(positions) == 1L, positions[[1L]] > 0L)
    src <<- sub(old, new, src, fixed = TRUE)
  }
  if (kind == "no_skip") {
    replace_once("    if (!is.null(is_not_subset_of) && length(unit_domains[[nu]]) == length(unit[[1L]])) {",
      "    if (FALSE) {")
  }
  if (kind == "queued") {
    replace_once("  is_not_subset_of = NULL  # see further down", paste(c(
      "  is_not_subset_of = NULL  # see further down",
      "  unit_work = character()",
      "  unit_work_head = 1L",
      "  unit_work_active = FALSE"
    ), collapse = "\n"))
    start = regexpr("    # The symbol registry is empty at the start;", src, fixed = TRUE)[[1L]]
    finish_anchor = "    FALSE  # no contradiction\n  }"
    finish = regexpr(finish_anchor, src, fixed = TRUE)[[1L]]
    stopifnot(start > 0L, finish > start)
    old = substr(src, start, finish + nchar(finish_anchor) - 1L)
    new = paste(c(
      "    # The logical unit has already been registered or merged above.",
      "    # Every birth queues its symbol; repeated symbols revisit earlier targets",
      "    # when a nested merge strengthens a domain during a drain.",
      "    unit_work[[length(unit_work) + 1L]] <<- nu",
      "    if (unit_work_active) FALSE else drain_unit_work()",
      "  }",
      "",
      "  drain_unit_work = function() {",
      "    unit_work_active <<- TRUE",
      "    on.exit(unit_work_active <<- FALSE)",
      "    while (unit_work_head <= length(unit_work)) {",
      "      nu = unit_work[[unit_work_head]]",
      "      unit_work_head <<- unit_work_head + 1L",
      "      for (s_clause_idx in symbol_registry[[nu]]) {",
      "        if (eliminated[[s_clause_idx]]) next",
      "        # Read the latest effective range separately for every target.",
      "        adr = apply_domain_restriction(s_clause_idx, nu, unit_domains[[nu]], TRUE)",
      "        if (identical(adr, TRUE)) return(TRUE)",
      "      }",
      "    }",
      "    unit_work <<- character()",
      "    unit_work_head <<- 1L",
      "    FALSE",
      "  }"
    ), collapse = "\n")
    replace_once(old, new)
  }
  if (!is.null(observe)) {
    for (helper in c("register_unit", "apply_domain_restriction", "eliminate_symbol_from_clause",
      "on_updated_subset_relations", "try_sse_2nd_order")) {
      anchor = paste0("  ", helper, " = function(")
      at = regexpr(anchor, src, fixed = TRUE)[[1L]]
      stopifnot(at > 0L)
      end = at + regexpr(" {\n", substr(src, at, nchar(src)), fixed = TRUE)[[1L]] + 2L
      src = paste0(substr(src, 1L, end),
        "    queue_observe('", helper, "', environment())\n", substr(src, end + 1L, nchar(src)))
    }
    replace_once("  remaining_entries = which(!eliminated)[order(lengths(entries[!eliminated]), decreasing = TRUE)]",
      paste0("  queue_observe('before_hla', environment())\n",
        "  remaining_entries = which(!eliminated)[order(lengths(entries[!eliminated]), decreasing = TRUE)]"))
    if (kind == "queued") {
      replace_once("    unit_work[[length(unit_work) + 1L]] <<- nu",
        paste0("    unit_work[[length(unit_work) + 1L]] <<- nu\n",
          "    queue_observe('unit_enqueued', environment())"))
      replace_once("      unit_work_head <<- unit_work_head + 1L",
        paste0("      unit_work_head <<- unit_work_head + 1L\n",
          "      queue_observe('unit_dequeued', environment())"))
      replace_once("    unit_work_head <<- 1L\n    FALSE",
        "    unit_work_head <<- 1L\n    queue_observe('queue_drained', environment())\n    FALSE")
    }
  }
  evaluation = new.env(parent = environment())
  evaluation$queue_observe = observe
  eval(parse(text = src), envir = evaluation)
  evaluation$simplify_cnf
}
