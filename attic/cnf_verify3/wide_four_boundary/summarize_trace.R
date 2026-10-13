suppressMessages(library(jsonlite))
audit_dir = "attic/cnf_verify3/wide_four_boundary"
saved = readRDS(file.path(audit_dir, "checks_r36.rds"))
records = list()
for (pass in 1:2) {
  previous = NULL
  trace = saved$traces[["1"]][[pass]]
  events = list()
  for (i in seq_along(trace)) {
    event = trace[[i]]
    if (event$kind == "hla") {
      events[[length(events) + 1L]] = c(list(event_index = i), event)
      next
    }
    current = list(entries = event$entries, eliminated = event$eliminated)
    if (!identical(previous, current)) {
      events[[length(events) + 1L]] = c(list(event_index = i, kind = "state", owner = event$owner), current)
      previous = current
    }
  }
  records[[pass]] = events
}
write_json(records, file.path(audit_dir, "one_padding_state_changes.json"), pretty = TRUE, auto_unbox = TRUE)
