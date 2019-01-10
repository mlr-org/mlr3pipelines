rep_suffix = function(x, n) {
  sprintf("%s_%04i", x, seq_len(n))
}

# replace columns with new data. A bit hacky because mlr3 doesn't supply this out of the box
# and makes it a bit harder than it needs to be. if no primary key column is present, it is
# added. In that case the number of rows must be identical.
#
# task will be cloned.
task_update_data = function(task, newdata) {
  rowids = task$row_ids
  keyname = names(rowids)
  assert(length(keyname) == 1)
  if (keyname %nin% rownames(newdata)) {
    assert(nrow(newdata) == task$nrow)
    newdata = cbind(newdata, rowids)
  }
  overwriting = setdiff(intersect(colnames(newdata), task$col_info$id), keyname)  # must take all col_info$id columns
  adding = setdiff(colnames(newdata), c(overwriting, keyname))

  task = task$clone(deep = TRUE)$
    select(overwriting)$
    replace_columns(newdata[, c(overwriting, keyname), with = FALSE])$
    cbind(newdata[, c(adding, keyname), with = FALSE])

  task$set_col_role(setdiff(colnames(newdata), keyname), "feature")
}


# FIXME --- vvv do this using paradox when paradox is able to do that
# paramsets [possibly named list of ParamSet]
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
