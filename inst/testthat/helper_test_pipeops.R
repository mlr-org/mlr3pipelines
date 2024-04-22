PipeOpDebugBasic = R6Class("PipeOpDebugBasic",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "debug.basic", param_set = ps()) {
      super$initialize(id = id, param_set = param_set,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = "output", train = "*", predict = "*")
      )
    }),
  private = list(
    .train = function(inputs) {
      catf("Training %s", self$id)
      self$state = inputs
    },
    .predict = function(inputs) {
      catf("Predicting %s", self$id)
      self$state = c(self$state, inputs)
    },
    .additional_phash_input = function() NULL
  )
)

deparse_list_safe = function(l) {
  # deparse list, but work the same on all systems
  if (!is.null(names(l))) {
    sprintf("list(%s)", paste(names(l), sapply(l, deparse), sep = " = ", collapse = ", "))
  } else {
    sprintf("list(%s)", paste(as.character(l), sep = ", "))
  }
}

# initialize with inputs / outputs: input / output channel names, or number of channels
PipeOpDebugMulti = R6Class("PipeOpDebugMulti",
  inherit = PipeOp,
  public = list(
    nin = NULL,
    nout = NULL,
    initialize = function(inputs, outputs, id = "debug.multi") {
      if (is.numeric(inputs)) {
        inputs = paste0("input_", seq_len(inputs))
      }
      if (is.numeric(outputs)) {
        outputs = paste0("output_", seq_len(outputs))
      }
      p = ps(par = p_int(lower = 0, upper = 10, default = 0, tags = c("train", "predict")))
      self$nin = length(inputs)
      self$nout = length(outputs)
      super$initialize(id, param_set = p,
        input = data.table(name = inputs, train = "*", predict = "*"),
        output = data.table(name = outputs, train = "*", predict = "*"))
    }),
  private = list(
    .train = function(inputs) {
      catf("Training %s with input %s", self$id, deparse_list_safe(inputs))
      self$state = inputs
      iin = inputs[[1]]
      as.list(iin + seq_len(self$nout))
    },
    .predict = function(inputs) {
      catf("Predicting %s with input %s and state %s",
        self$id, deparse_list_safe(inputs), deparse_list_safe(self$state))
      iin = inputs[[1]]
      as.list(iin + seq_len(self$nout))
    },
    .additional_phash_input = function() c(self$nin, self$nout)
  )
)

VarargPipeop = R6Class("VarargPipeop",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "vararg", innum = 0, param_vals = list()) {
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = c("...", rep_suffix("input", innum)), train = "*", predict = "*"),
        output = data.table(name = "output", train = "*", predict = "*")
      )
    }),
    private = list(
    .train = function(inputs) {
      self$state = inputs
      list(inputs)
    },
    .predict = function(inputs) {
      self$state = inputs
      list(inputs)
    },
    .additional_phash_input = function() self$input$name
  )
)
