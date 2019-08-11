PipeOpProxy = R6Class("PipeOpProxy",
  inherit = PipeOp,
  public = list(
    initialize = function(innum = 1, outnum = 1, id = "proxy") {
      assert(
        check_int(innum, lower = 0),
        check_character(innum, min.len = 1, any.missing = FALSE)
      )

      inname = if (innum) rep_suffix("input", innum) else "..."
      input = data.table(name = inname, train = "Task", predict = "Task")
      output = data.table(name = rep_suffix("output", outnum), train = "Task", predict = "Task")

      ps = ParamSet$new(params = list(
          ParamUty$new("content", custom_check = check_proper_content(innum, outnum), default = NULL) # TODO
        )
      )

      super$initialize(id, param_set = ps, input = input, output = output)
    },
    train_internal = function(input) {
      private$check_content_set()
      self$state = self$content$clone(deep = TRUE)
      output = self$state$train(input)
      return(output)
    },
    predict_internal = function(input) {
      private$check_content_set()
      return(self$state$predict(input))
    }
  ),

  private = list(
    check_content_set = function() {
      if(check_null(self$content)) {
        stop("content not set")
      }
    }
  ),

  active = list(
    content = function(value) {
      if (!missing(value)) {
        self$param_set$values$content = value
      }
      return(self$param_set$values$content)
    }
  ),

)

check_proper_content = function(innum, outnum) {
  return(
    function(x) {
      return(
        check_multi_class(x, c('Graph', 'PipeOp')) &
          check_true(innum == x$innum) &
          check_true(outnum == x$outnum)
        # TODO for Graph: innum == nrow(x$input)?
        # TODO maybe add this to Graph?
      )
    }
  )
}
