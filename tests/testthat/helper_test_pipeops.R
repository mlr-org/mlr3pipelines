PipeOpDebugBasic = R6Class("PipeOpDebugBasic",
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
      initialize = function(id = "debug.basic", param_set = ParamSet$new()) {
        super$initialize(id = id, param_set = param_set,
          input = data.table(name = "input", train = "*", predict = "*"),
          output = data.table(name = "output", train = "*", predict = "*")
        )
      }
  )
)

# initialize with inputs / outputs: input / output channel names, or number of channels
PipeOpDebugMulti = R6Class("PipeOpDebugMulti",
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

