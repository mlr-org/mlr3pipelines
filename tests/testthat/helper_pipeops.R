PipeOpTest1 = R6::R6Class("PipeOpTest1", inherit = PipeOp,
  public = list(
    initialize = function() {
      ps = paradox::ParamSet$new(list(paradox::ParamDbl$new("dbl", lower = 1, upper = 10)))
      super$initialize("th_po_1", param_set = ps, param_vals = list(dbl = 1))
      self$packages = "package1"
    },
    train = function(inputs) {return(1)},
    predict = function() {return(2)}
  )
)

th_po_1 = PipeOpTest1$new()

