# Cross-version artifact checks. Run after the three scripts on both versions.
out_dir = "attic/cnf_verify3/boolean_occurrence_totality"
read_pair = function(stem) lapply(c("r36", "r46"), function(tag) {
  readRDS(file.path(out_dir, paste0(stem, "_", tag, ".rds")))
})
observations = read_pair("observations")
stopifnot(identical(observations[[1L]]$counts, observations[[2L]]$counts),
  identical(observations[[1L]]$controls, observations[[2L]]$controls),
  identical(observations[[1L]]$source_md5, observations[[2L]]$source_md5),
  observations[[1L]]$counts$cases == 5259L,
  length(observations[[1L]]$controls) == 5L,
  all(observations[[1L]]$source_md5 == tools::md5sum(names(observations[[1L]]$source_md5))))
reproduction = read_pair("reproduction")
for (i in 1:2) reproduction[[i]]$R = NULL
stopifnot(identical(reproduction[[1L]], reproduction[[2L]]),
  reproduction[[1L]]$reduced_shape_checks == 20L,
  !any(reproduction[[1L]]$input_truth),
  identical(reproduction[[1L]]$input_truth, reproduction[[1L]]$output_truth),
  identical(reproduction[[1L]]$hypothetical_pivot, character(0L)))
costs = read_pair("cost_controls")
stopifnot(identical(costs[[1L]]$cases, costs[[2L]]$cases), length(costs[[1L]]$cases) == 9L,
  costs[[1L]]$cases$reverse_duplicate_unit_chain_48$max_helper_depth == 144L,
  costs[[1L]]$cases$fixed_two_clauses_128$helper_roots == 131L,
  costs[[1L]]$cases$fixed_three_clauses_128$max_children$register_unit == 129L)
sites = lapply(c("r36", "r46"), function(tag) read.csv(file.path(out_dir, paste0("sites_", tag, ".csv"))))
stopifnot(identical(sites[[1L]], sites[[2L]]))
cat("Cross-version counts, controls, source identity, public reproduction, reduced shapes, cost families and recursion controls agree.\n")
