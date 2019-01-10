rep_suffix = function(x, n) {
  sprintf("%s_%04i", x, seq_len(n))
}

# FIXME --- vvv do this using paradox when paradox is able to do that
# paramsets [possibly named list of ParamSet]. if the list is named, param-IDs are renamed to
#   [name in list].[paramid], otherwise the param IDs are just concatenated.
union_param_sets = function(paramsets) {
  # loop over all nodes, and add their paramsets (with prefix) to result object
  allparams = unlist(map(paramsets, function(x) x$clone(deep = TRUE)$params), recursive = FALSE)
  imap(allparams, function(param, id) param$id = id)
  ParamSet$new(allparams)
}


# parvalname [character] name of parameter values inside parvalhavers. if length 1 it us used for all,
#   otherwise it must be indexable by parvalhavers' index.
# parvalhavers [possibly named list of things x so that x[[parvalname]] are the parameter values ]
# newval [named list] new parameter values
union_param_vals = function(param_sets, parvalhavers, parvalname, newval) {

  getpvn = function(popid) {
    if (length(parvalname) == 1) {
      parvalname
    } else {
      parvalname[[popid]]
    }
  }
  if (!missing(newval)) {
    psunion = union_param_sets(param_sets)
    # collect all parameter ID mappings
    parids = unlist(imap(parvalhavers, function(pop, popid) {
      imap(param_sets[[popid]]$params, function(pv, pvid) list(popid, pvid))
    }), recursive = FALSE)
    assert_list(parids, names = "unique")

    assert_list(newval, names = "unique")  # length may not change
    assert(all(names(newval) %in% names(psunion$params)))
    if (!psunion$test(newval)) {
      stop("Parameters out of bounds")
    }

    for (pidx in names(newval)) {
      poid = parids[[pidx]][[1]]  # PipeOp ID of the PipeOp this pertains to
      parid = parids[[pidx]][[2]]  # original parameter id, as the PipeOp knows it
      parvalhavers[[poid]][[getpvn(poid)]][[parid]] = newval[[pidx]]
    }
  }
  parvals = unlist(imap(parvalhavers, function(pop, popid) pop[[getpvn(popid)]]), recursive = FALSE)
  assert_list(parvals, names = "unique")
  parvals
}
