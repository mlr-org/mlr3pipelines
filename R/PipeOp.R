
PipeOp = R6Class("PipeOp",
  public = list(
    packages = character(0),

    initialize = function(id, param_set = ParamSet$new()) {
      private$.id = id
      private$.param_set = param_set
      #FIXME: we really need a function in paradox now to get defaults
      private$.param_vals = param_set$data$default
    },

    reset = function() {
      self$params = NULL
      self$result.train = NULL
      self$result.predict = NULL
      invisible(self)
    },

    print = function(...) {
      catf("PipeOp: <%s>", self$id)
      catf("parvals: <%s>", as_short_string(self$param_vals))
      catf("is_learnt=%s", self$is_learnt)
      catf("Input: %s", as_short_string(self$inputs))
      catf("Result: %s", as_short_string(self$result))
      catf("Prev ops: %s", self$prev_ops$print_str)
      catf("Next ops: %s", self$next_ops$print_str)
    },

    #FIXME: AB machen
    set_param_vals = function(vals) {
      private$.param_vals = insert(private$.param_vals, vals)
      invisible(self)
    },



    set_prev = function(ops) {
      self$prev_ops = OpList$new(ops)
      for (op in ops) {
        op$next_ops = OpList$new(list(self))
      }
    }


  ),

  active = list(
    id = function() private$.id,
    param_set = function() private$.param_set,
    param_vals = function() private$.param_vals,
    params = function() private$.params,
    result = function() private$.result,
    is_learnt = function() !is.null(self$params),
    has_result = function() !is.null(self$result)
  ),

  private = list(
    .id = NULL,
    .param_set = NULL,
    .param_vals = NULL,
    .params = NULL,
    .result = NULL
  )
)
