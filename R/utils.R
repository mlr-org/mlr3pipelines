collapse = function(x) {
  paste(x, collapse = ",")
}


readonly = function(name) {
  body = substitute({
    var = private[[paste0(".", name)]]
    if (!missing(x) && !identical(x, var)) stopf("Variable %s is read-only", name)
    var
  })
  eval(call("function", as.pairlist(alist(x = )), body))
}


