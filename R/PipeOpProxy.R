#' @include PipeOp.R PipeOpNOP.R assert_graph.R
PipeOpProxy = R6Class("PipeOpProxy",
  inherit = PipeOp,
  public = list(
    initialize = function(innum = 1, outnum = 1, id = "proxy", param_vals = list()) {
      assert(
        check_int(innum, lower = 0),
        check_character(innum, min.len = 1, any.missing = FALSE)
      )

      inname = if (innum) rep_suffix("input", innum) else "..."
      input = data.table(name = inname, train = "*", predict = "*")
      output = data.table(name = rep_suffix("output", outnum), train = "*", predict = "*")

      ps = ParamSet$new(params = list(
          ParamUty$new("content", custom_check = check_proper_content(innum, outnum)) # TODO
        )
      )

      super$initialize(id, param_set = ps, input = input, output = output, param_vals = param_vals)
    },
    train_internal = function(input) {
      private$check_content_set()
      self$state = as_graph(self$param_set$values$content, deep_copy = TRUE)
      # if we have only one input, we automatically distribute it to
      # all graph inputs.
      if (length(input) == 1) {
        self$state$train(input[[1]], single_input = TRUE)
      } else {
        self$state$train(input, single_input = FALSE)
      }
    },
    predict_internal = function(input) {
      # if we have only one input, we automatically distribute it to
      # all graph inputs.
      if (length(input) == 1) {
        self$state$predict(input[[1]], single_input = TRUE)
      } else {
        self$state$predict(input, single_input = FALSE)
      }
    }
  ),

  private = list(
    check_content_set = function() {
      if(check_null(self$param_set$values$content)) {
        stop("content not set")
      }
    }
  )
)

check_proper_content = function(innum, outnum) {
  return(
    function(x) {
      x = as_graph(x)
      if (innum > 1 && innum != x$innum) {
        # if innum is 0 it means PipeOpProxy is "vararg" --> we don't know innum compatibility yet
        # if innum is 1 it means PipeOpProxy we implicitly copy the input to all inputs of content
        sprintf("Graph %s must have %s input but has %s input",
          x$id, innum, x$innum)
      }
      if (outnum != x$outnum) {
        sprintf("Graph %s must have %s output but has %s output",
          x$id, outnum, x$outnum)
      } else {
        TRUE
      }
    }
  )
}
