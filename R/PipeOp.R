#' @include utils.R

PipeOp = R6::R6Class("PipeOp",
  public = list(
    packages = character(0),
    state = NULL,
    result = NULL,
    initialize = function(id, param_set = ParamSet$new(), param_vals = NULL) {
      private$.id = id
      private$.param_set = param_set
      #FIXME: we really need a function in paradox now to get defaults
      private$.param_vals = param_set$data$default
      names(private$.param_vals) = param_set$ids
      private$.param_vals = insert_named(private$.param_vals, param_vals)
      if (!param_set$test(private$.param_vals)) {
        stop("Parameters out of bounds")
      }
    },

    print = function(...) {
      catf("PipeOp: <%s>", self$id)
      catf("parvals: <%s>", as_short_string(self$param_vals))
      catf("is_learnt=%s", self$is_learnt)
      catf("Input: %s", as_short_string(self$inputs))
      catf("Result: %s", as_short_string(self$result))
    },

    train = function(...) stop("no train function given"),  # TODO: some better way to make smth abstract?
    predict = function(...) stop("no predict function given")
  ),

  active = list(
    id = function(id) {
      if (missing(id)) {
        private$.id
      } else {
        private$.id = id

        # TODO: maybe notify the graph about changed ID?
      }
    },
    param_set = function() private$.param_set,
    param_vals = function(vals) {
      if (!missing(vals)) {
        # TODO: param check
        if (!self$param_set$test(vals)) {
          stop("Parameters out of bounds")
        }
        private$.param_vals = vals
      }
      private$.param_vals
    },
    intype = function() private$.intype,
    outtype = function() private$.outtype,
    takeslist = function() {
      tl = private$.takeslist
      assert(tl || length(self$intype) == 1)
      tl
    },
    returnslist = function() {
      rl = private$.returnslist
      assert(rl || length(self$outtype) == 1)
      rl
    },

    # ------------ BELOW HERE SHOULD BE DROPPED AT SOME POINT
    is_trained = function() !is.null(self$state)
  ),

  private = list(
    .id = NULL,  # id, name within a graph, must be unique within that graph
    .param_set = NULL,
    .param_vals = NULL,
    .intype = NULL,  # list of character vectors, identifying the input classes
    .outtype = NULL,  # list of character vectors, identifying output classes
    .takeslist = TRUE,  # may be FALSE, but only if length(intype) is 1
    .returnslist = TRUE,  # may be FALSE, but only if length(outtype) is 1

    # ------------ BELOW HERE SHOULD BE DROPPED AT SOME POINT
    .params = NULL
  )
)
