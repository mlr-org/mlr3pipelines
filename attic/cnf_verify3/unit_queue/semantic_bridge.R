suppressMessages(library(jsonlite))
source("attic/cnf_verify3/unit_queue/variant.R")
statistics = new.env(parent = emptyenv())
increment = function(name, count = 1L) {
  old = statistics[[name]]
  statistics[[name]] = if (is.null(old)) count else old + count
}

observe_variant = function(kind) function(event, frame) {
  increment(event)
  read = function(name) get(name, envir = frame, inherits = TRUE)
  if (event %in% c("on_updated_subset_relations", "try_sse_2nd_order")) {
    entries = read("entries")
    available = read("available")
    eliminated = read("eliminated")
    is_unit = read("is_unit")
    unit_domains = read("unit_domains")
    matrices = read("is_not_subset_of")
    if (event == "on_updated_subset_relations") {
      sources = read("meta_idx")
      target = read("meta_idx_other")
    } else {
      sources = c(read("meta_idx_oneend"), read("meta_idx_twoends"))
      target = read("meta_idx_target")
    }
    ids = available[c(sources, target)]
    stopifnot(!any(eliminated[ids] | is_unit[ids]))
    for (source in sources) {
      matrix = matrices[[source]]
      for (s in colnames(matrix)[!matrix[target, ]]) {
        domain = unit_domains[[s]]
        if (is.null(domain)) domain = read("universe")[[s]]
        a = intersect(entries[[available[[source]]]][[s]], domain)
        b = intersect(entries[[available[[target]]]][[s]], domain)
        stopifnot(all(a %in% b))
        increment("contextual_false_comparisons")
      }
    }
  }
  if (event %in% c("before_hla", "queue_drained")) {
    entries = read("entries")
    eliminated = read("eliminated")
    is_unit = read("is_unit")
    registry = read("symbol_registry")
    unit_domains = read("unit_domains")
    for (s in names(unit_domains)) {
      for (i in registry[[s]]) {
        stopifnot(!eliminated[[i]], !is_unit[[i]])
        stopifnot(all(entries[[i]][[s]] %in% unit_domains[[s]]))
        if (kind != "production") {
          stopifnot(length(entries[[i]][[s]]) < length(unit_domains[[s]]))
        }
        increment("unit_containments")
      }
    }
    if (kind == "queued") stopifnot(!length(read("unit_work")))
    if (event != "before_hla") return(invisible(NULL))
    active = which(!eliminated & !is_unit)
    inverse = read("available_inverse")
    matrices = read("is_not_subset_of")
    counts = read("not_subset_count")
    for (s in names(read("universe"))) {
      expected = active[vapply(entries[active], function(clause) s %in% names(clause), logical(1))]
      stopifnot(setequal(registry[[s]], expected))
    }
    for (i in active) for (j in setdiff(active, i)) {
      m = matrices[[inverse[[i]]]]
      exact = vapply(colnames(m), function(s) !all(entries[[i]][[s]] %in% entries[[j]][[s]]), logical(1))
      stopifnot(identical(unname(exact), unname(m[inverse[[j]], ])))
      stopifnot(counts[inverse[[i]], inverse[[j]]] == sum(exact), sum(exact) > 0L)
      increment("quiescent_pair_checks")
    }
  }
  invisible(NULL)
}

kinds = c("production", "no_skip", "queued")
plain = setNames(lapply(kinds, make_unit_variant), kinds)
audited = setNames(lapply(kinds, function(kind) make_unit_variant(kind, observe_variant(kind))), kinds)
run_request = function(request) {
  universe = lapply(request$domains, function(x) as.character(unlist(x, use.names = FALSE)))
  entries = lapply(request$clauses, function(clause) {
    lapply(clause, function(x) as.character(unlist(x, use.names = FALSE)))
  })
  outputs = list()
  for (kind in kinds) {
    rm(list = ls(statistics, all.names = TRUE), envir = statistics)
    result = plain[[kind]](entries, universe)
    checked = audited[[kind]](entries, universe)
    stopifnot(identical(result, checked))
    outputs[[kind]] = list(result = c(result), statistics = as.list(statistics))
  }
  list(ok = TRUE, outputs = outputs)
}
input = file("stdin", "r")
repeat {
  line = readLines(input, n = 1L, warn = FALSE)
  if (!length(line)) break
  result = tryCatch(run_request(fromJSON(line, simplifyVector = FALSE)),
    error = function(error) list(ok = FALSE, message = conditionMessage(error)))
  cat(toJSON(result, auto_unbox = TRUE, null = "null", digits = NA), "\n", sep = "")
  flush.console()
}
