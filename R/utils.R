rep_suffix = function(x, n) {
  sprintf("%s_%04i", x, seq_len(n))
}

# FIXME --- vvv do stuff below here using paradox when paradox is able to do that

# paramsets [possibly named list of ParamSet]. if the list is named, param-IDs are renamed to
#   [name in list].[paramid], otherwise the param IDs are just concatenated.
union_param_sets = function(paramsets) {
  # loop over all nodes, and add their paramsets (with prefix) to result object
  allparams = unlist(map(paramsets, function(x) x$clone(deep = TRUE)$params), recursive = FALSE)
  imap(allparams, function(param, id) param$id = id)
  ParamSet$new(allparams)
}

# param_sets [possibly named list of ParamSet]. same as in union_param_sets
# parvalhavers [possibly named list of things such that x[[parvalname]] are the parameter values]
#   e.g. a bunch of Learners or PipeOps
# parvalname [character] name of parameter values inside parvalhavers. if length 1 it us recycled for all
#   entries of paravalhavers, otherwise it must be the same length and with the same names as parvalhavers.
#   E.g. if paravalhavers is a bunch of Learners, this would be "param_vals"
# newval [named list] new parameter values, if param_vals are to be updated.
#   Same as the parameter in active binding functions.
union_param_vals = function(param_sets, parvalhavers, parvalname, newval) {

  # do the recycling of paravalname. poid is the name or numeric index of the paravalhavers entry under consideration.
  getpvn = function(popid) {
    if (length(parvalname) == 1) {
      parvalname
    } else {
      parvalname[[popid]]
    }
  }

  if (!missing(newval)) {  # updating values

    assert_list(newval, names = "unique")
    psunion = union_param_sets(param_sets)
    assert(all(names(newval) %in% names(psunion$params)))
    if (!psunion$test(newval)) {
      stop("Parameters out of bounds")
    }

    # collect all parameter ID mappings
    # These are lists with names [PipeOpID].[ParamID] and entries list([PipeOpID], [ParamID]).
    # This avoid ambiguities when dispatching Parameter values if PipeOpID or ParamID contain dots.
    parids = unlist(imap(parvalhavers, function(pop, popid) {
      imap(param_sets[[popid]]$params, function(pv, pvid) list(popid, pvid))
    }), recursive = FALSE)
    assert_list(parids, names = "unique")

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
