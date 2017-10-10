trainPipe = function(task, pipe) {
  current.node = pipe
  input = list(task = task)
  result = NULL
  repeat {
    messagef("train pipe el: id=%s, par.vals=[%s]", current.node$id, listToShortString(current.node$par.vals))
    input2 = formatInlist(input, dformat = current.node$in.format)
    input2$par.vals = current.node$par.vals
    output = current.node$train(input2)
    output2 = formatOutlist(output, dformat = current.node$out.format, task = task)
    rn = ResultNode$new(cpo = current.node, control = output2$control)
    if (is.null(result)) {
      result = rn
    } else {
      #FIXME: here we need to handle multiple results
      result.last$children[[1L]] = rn
    }
    ch = current.node$children
    messagef("train done. #children=%i", length(ch))
    if (length(ch) == 0L)
      break
    current.node = ch[[1]]
    result.last = rn
    input = output2

  }
  return(result)
}
