rep_suffix = function(x, n) {
  sprintf("%s_%04i", x, seq_len(n))
}

# FIXME --- vvv do this using paradox when paradox is able to do that
# paramsets [named list of ParamSet]
union_param_sets = function(paramsets) {
  # loop over all nodes, and add their paramsets (with prefix) to result object
  allparams = unlist(map(paramsets, function(x) x$clone(deep = TRUE)$params), recursive = FALSE)
  imap(allparams, function(param, id) param$id = id)
  ParamSet$new(allparams)
}


# parvalname [character(1)] name of parameter values inside parvalhavers
# parvalhavers [named(!) list of things x so that x[[parvalname]] are the parameter values ]
# newval [named list] new parameter values
union_param_vals = function(param_set, parvalhavers, parvalname, newval) {

  if (!missing(newval)) {
    # collect all parameter ID mappings
    parids = unlist(imap(parvalhavers, function(pop, popid) {
      imap(pop[[parvalname]], function(pv, pvid) list(popid, pvid))
    }), recursive = FALSE)
    assert_list(parids, names = "unique")

    assert_list(newval, names = "unique")  # length may not change
    assert(all(names(newval) %in% names(parids)))
    if (!param_set$test(newval)) {
      stop("Parameters out of bounds")
    }

    for (pidx in names(newval)) {
      poid = parids[[pidx]][[1]]  # PipeOp ID of the PipeOp this pertains to
      parid = parids[[pidx]][[2]]  # original parameter id, as the PipeOp knows it
      parvalhavers[[poid]][[parvalname]][[parid]] = newval[[pidx]]
    }
  }
  parvals = unlist(map(parvalhavers, "param_vals"), recursive = FALSE)
  assert_list(parvals, names = "unique")
  parvals
}
