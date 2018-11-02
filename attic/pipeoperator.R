#' @export
`%>>%` = function(cpo1, cpo2) {
  UseMethod("%>>%")
}

#FIXME: test that this works
#' @export
`%>>%.data.frame` = function(cpo1, cpo2) {
  `%>>%.Task`(cpo1, cpo2)
}

#' @export
`%>>%.Task` = function(cpo1, cpo2) {
  #FIXME: handle NULLCPO again
  # add asserts for cpo2
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if (any(c("CPOTrained", "CPO") %in% class(cpo2))) {
    trainPipe(cpo1, cpo2)
  }
}


#' @export
`%>>%.PipeNode` = function(cpo1, cpo2) {
  BBmisc::messagef("join %s >> %s", cpo1$id, cpo2$id)
  #FIXME: add assert for cpo2
  if (inherits(cpo2, "PipeNode")) {
    compound2Ops(cpo1, cpo2)
  } else if ("Learner" %in% class(cpo2)) {
    #FIXME: allow attach to learner
    # attachCPO(cpo1, cpo2)
  }
}

#' @export
# FIXME: check what this is
`%>>%.CPOTrained` = function(cpo1, cpo2) {
  if (is.nullcpo(cpo2)) {
    cpo1
  } else if ("CPOTrained" %in% class(cpo2)) {
    composeCPO(cpo1, cpo2)
  } else if ("WrappedModel" %in% class(cpo2)) {
    stop("Attaching CPO Retrafo to a model is not implemented.")
  } else if ("CPO" %in% class(cpo2) || "CPOConstructor" %in% class(cpo2)) {
    stop("Cannot compose CPO Retrafo with CPO.")
  } else {
    stopf("Cannot compose CPO Retrafo with object of class c(%s)", paste0('"', class(cpo2), '"', collapse = ", "))
  }
}

