formatInlist = function(inlist, dformat) {
  if (dformat == "task") {
    return(inlist) 
  } else if (dformat == "data-target") {
    task = inlist$task
    d = task$data()
    d[[task$target]] = NULL
    newlist = list(itask = task, data = d, target = task$truth())
  } else {
    stopf("Format not implemented: %s", dformat)
  }
  return(newlist)
}

formatOutlist = function(outlist, dformat, task) {
  if (dformat == "task") {
    return(outlist) 
  } else if (dformat == "data") {
    #FIXME: this is bad we need to clone tasks
    newdata = outlist$data
    newdata[[task$target]] = task$truth()[[1L]]
    outlist$task = TaskClassif$new(data = newdata, target = task$target)
    outlist$data = NULL
    return(outlist)
  } else {
    stopf("Format not implemented: %s", dformat)
  }
}




