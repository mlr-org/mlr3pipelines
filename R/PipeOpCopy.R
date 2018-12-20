


PipeOpCopy = R6::R6Class("PipeOpCopy",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "scatter") {
      super$initialize(id)
      private$.intype = list("any")
      private$.outtype = rep(list("any"), outnum)
      private$.outnum = outnum
    },
    train = function(input) {
      self$state = list()
      rep(input, self$outnum)
    },
    predict = function(input) {
      rep(input, self$outnum)
    }
  ),
  private = list(
    .outnum = NULL
  ),
  active = list(
    outnum = function() private$.outnum
  )
)
