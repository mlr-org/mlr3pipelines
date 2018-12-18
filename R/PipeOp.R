
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
      BBmisc::catf("PipeOp: <%s>", self$id)
      BBmisc::catf("parvals: <%s>", BBmisc::listToShortString(self$param_vals))
      BBmisc::catf("is_learnt=%s", self$is_learnt)
      BBmisc::catf("Input: %s", BBmisc::listToShortString(self$inputs))
      BBmisc::catf("Result: %s", BBmisc::listToShortString(self$result))
      BBmisc::catf("Prev ops: %s", self$prev_ops$print_str)
      BBmisc::catf("Next ops: %s", self$next_ops$print_str)
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
