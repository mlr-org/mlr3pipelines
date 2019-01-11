PipeOpTest1 = R6Class("PipeOpTest1", inherit = PipeOp,
  public = list(
    initialize = function() {
      ps = paradox::ParamSet$new(list(paradox::ParamDbl$new("dbl", lower = 1, upper = 10)))
      super$initialize("th_po", param_set = ps, param_vals = list(dbl = 1))
      self$packages = "package1"
    },
    train = function(inputs) {
      self$state = 1
      return(1)
    },
    predict = function() {return(2)}
  )
)


PipeOpDebugBasic = R6Class("BasicPO",
  inherit = PipeOp,
  public = list(
      train = function(inputs) {
        catf("training %s", self$id)
        self$state = inputs
      },
      predict = function(inputs) {
        catf("predicting %s", self$id)
        self$state = c(self$state, inputs)
      },
      initialize = function(id = "debug.basic", param_set = ParamSet$new(), param_vals = list()) {
        super$initialize(id = id, param_set = param_set, param_vals = param_vals,
          input = data.table(name = "input", train = "*", predict = "*"),
          output = data.table(name = "output", train = "*", predict = "*")
        )
      }
  )
)

# initialize with inputs / outputs: input / output channel names, or number of channels
PipeOpDebugMulti = R6Class("BasicPOAny",
  inherit = PipeOp,
  public = list(
      nin = NULL,
      nout = NULL,
      train = function(inputs) {
        catf("Training %s with input %s", self$id, deparse(inputs))
        self$state = inputs
        iin = inputs[[1]]
        as.list(iin + seq_len(self$nout))
      },
      predict = function(inputs) {
        catf("Predicting %s with input %s and state %s",
          self$id, deparse(inputs), deparse(self$state))
        iin = inputs[[1]]
        as.list(iin + seq_len(self$nout))
      },
      initialize = function(inputs, outputs, id = "debug.multi") {
        if (is.numeric(inputs)) {
          inputs = paste0("input_", seq_len(inputs))
        }
        if (is.numeric(outputs)) {
          inputs = paste0("output_", seq_len(outputs))
        }
        p = ParamInt$new(id = "par", lower = 0, upper = 10, default = 0)
        self$nin = length(inputs)
        self$nout = length(outputs)
        super$initialize(id, ParamSet$new(list(p)),
          input = data.table(name = inputs, train = "*", predict = "*"),
          output = data.table(name = outputs, train = "*", predict = "*"))
      }
  )
)

