#FIXME: have node as abstract superclass for pipe and result?
ResultNode = R6Class("ResultNode", 
  public = list(
    cpo = NULL,
    control = NULL,
    children  = NULL,

    initialize = function(cpo, control) {
      self$cpo = cpo
      self$control = control
      self$children = vector("list", length(cpo$children))
    }
  )
)
