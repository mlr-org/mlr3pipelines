PipeOpFanOut = R6Class("PipeOpFanOut", 
 
  inherit = PipeOp,
  
  public = list(
    initialize = function(nout) {
      ps = ParamSet$new(params = list(
        ParamInt$new(id = "nout", lower = 1L)  
      ))
      super$initialize("fanout", ps)
      private$.par_vals$nout = nout
    },
      

    train2 = function() {
      private$copy_input_ntimes()
    },

    predict2 = function() {
      private$copy_input_ntimes()
    }
  ),

  private = list(
    # FIXME deep copy or not? make this an option of the OP?
    copy_input_ntimes = function() {
      lapply(1:self$par_vals$nout, function(i) self$inputs[[1L]]$clone())
    }
  )
)




