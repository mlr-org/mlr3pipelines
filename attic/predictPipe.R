#FIXME: maybe have an option to store all generated stuff (eg data) during training and prediction?

#FIXME: can we unify trainPipe and predictPipe?
predictPipe = function(task, model) {
  curnode = model
  input = list(task = task)
  result = NULL
  repeat {
    curcpo = curnode$cpo
    curctrl = curnode$control
    BBmisc::messagef("predict pipe el: id=%s; control=[%s]", curcpo$id, collapse(names(curctrl), ","))
    input2 = formatInlist(input, dformat = curcpo$in.format)
    output = curcpo$predict(input2, curctrl)
    output2 = formatOutlist(output, dformat = curcpo$out.format, task = task)
    #FIXME: we dont need to store all of this here? maybe only for debugging?
    rn = ResultNode$new(cpo = curnode, control = output2$control)
    if (is.null(result)) {
      result = rn
    } else {
      #FIXME: here we need to handle multiple results
      result.last$children[[1L]] = rn
    }
    ch = curnode$children
    BBmisc::messagef("predict done. #children=%i", length(ch))
    if (length(ch) == 0L)
      break
    curnode = ch[[1]]
    result.last = rn
    input = output2
  }
  #FIXME: what do we return here? at least the final output object?
  #and then the result graph separately?
  return(task)
}

