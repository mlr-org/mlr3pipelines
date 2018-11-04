PipeOpMultiplexer = R6Class("PipeOpMultiplexer", 
 
  inherit = PipeOp,
  
  public = list(
    
    ops = NULL,

    initialize = function(ops) {
      op_ids = BBmisc::extractSubList(ops, "id")
      names(ops) = op_ids
      self$ops = ops
      ps = ParamSet$new(params = list(
        ParamCategorical$new(id = "selected", values = op_ids)  
      ))
      super$initialize("multiplex", ps)
      private$.par_vals$selected = op_ids[[1L]]
    },

    train2 = function(input) {
      op = self$ops[[self$par_vals$selected]]
      op$train(input)
    },

    predict2 = function(input) {
      op = self$ops[[self$par_vals$selected]]
      op$predict(input)
    }
  )

  #FIXME: wir brauchen so ein hierarchiches parset?
  # active = list(
  #   par_set = function() self$learner$par_set,
    
  #   par_vals = function(value) {
  #     if (missing(value)) return(self$learner$par.vals)
  #     else self$learner$par_set = value
  #   }
  # )
)



