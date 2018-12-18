
PipeOp = R6::R6Class("PipeOp",
  public = list(
    packages = character(0),

    initialize = function(id, param_set = ParamSet$new(), param_vals = NULL, ...) {
      private$.id = id
      private$.param_set = param_set
      #FIXME: we really need a function in paradox now to get defaults
      private$.param_vals = BBmisc::extractSubList(param_set$params, "default", simplify = FALSE)
      addnl_params = c(list(...), param_vals)
      checkmate::assert_list(addnl_params, names = "unique")
      for (n in names(addnl_params)) {
        private$.param_vals[[n]] = addnl_params[[n]]
      }
    },
    print = function(...) {
      BBmisc::catf("PipeOp: <%s>", self$id)
      BBmisc::catf("parvals: <%s>", BBmisc::listToShortString(self$param_vals))
      BBmisc::catf("is_learnt=%s", self$is_learnt)
      BBmisc::catf("Input: %s", BBmisc::listToShortString(self$inputs))
      BBmisc::catf("Result: %s", BBmisc::listToShortString(self$result))
    }

    train = function(...) stop("no train function given"),  # TODO: some better way to make smth abstract?
    predict = function(...) stop("no predict function given")
  ),

  active = list(
    id = function() private$.id,
    param_set = function() private$.param_set,
    param_vals = function(vals) {
      if (missing(vals)) {
        private$.param_vals
      } else {
        # TODO: param check
        private$.param_vals = vals
      }
    }
    state = function() private$.state,
    result = function() private$.result,
    is_learnt = function() !is.null(self$state),
    intype = function() private$.intype,
    outtype = function() private$.outtype
  ),

  private = list(
    .id = NULL,  # id, name within a graph, must be unique within that graph
    .param_set = NULL,
    .param_vals = NULL,
    .state = NULL,
    .intype = NULL,  # list of character vectors, identifying the input classes
    .outtype = NULL  # list of character vectors, identifying output classes
  )
)
