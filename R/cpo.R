
PipeNode = R6Class("PipeNode", 
  public = list(
    id = NULL,
    children  = list(),
    in.format = NULL,
    out.format = NULL,
    train = NULL,
    predict = NULL,
    par.set = NULL,
    par.vals = NULL,

    initialize = function(id, in.format, out.format, train, predict, par.set, par.vals) {
      self$id = id
      self$in.format = in.format
      self$out.format = out.format
      self$train = train
      self$predict = predict
      self$par.set = par.set
      #FIXME: we really need a funtion in ph2 now to get defaults
      self$par.vals = extractSubList(par.set$params, "default", simplify = FALSE)
    }

  )
)
