PipeOpTest1 = R6::R6Class("PipeOpTest1", inherit = PipeOp,
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


BasicPO = R6::R6Class("BasicPO",
  inherit = PipeOp,
  public = list(
      train = function(...) print("hi"),
      predict = function(...) print("yo"),
      initialize = function(...) {
        super$initialize(...)
        private$.intype = list("data.frame")
        private$.outtype = list("data.frame")
      }
  )
)

BasicPOAny = R6::R6Class("BasicPOAny",
  inherit = PipeOp,
  public = list(
      nin = NULL,
      nout = NULL,
      train = function(input) {
        catf("Training %s with input %s", self$id, deparse(input))
        self$state = input
        iin = input[[1]]
        as.list(iin + seq_len(self$nout))
      },
      predict = function(input) {
        catf("Predicting %s with input %s and state %s", self$id, deparse(input), deparse(self$state))
        iin = input[[1]]
        as.list(iin + seq_len(self$nout))
      },
      initialize = function(nin, nout, id, ...) {
        p = ParamInt$new(id = "par", lower = 0, upper = 10, default = 0)
        self$nin = nin
        self$nout = nout
        super$initialize(id, ParamSet$new(list(p)), list(...))
        private$.intype = rep(list("data.frame"), nin)
        private$.outtype = rep(list("data.table"), nout)
      }
  )
)

BasicPOAnyNamed = R6::R6Class("BasicPOAnyNamed",
  inherit = PipeOp,
  public = list(
      train = function(...) print("hi"),
      predict = function(...) print("yo"),
      initialize = function(nin, nout, ...) {
        super$initialize(...)
        private$.intype = rep(list("data.frame"), nin)
        names(private$.intype) = letters[seq_along(private$.intype)]
        private$.outtype = rep(list("data.table"), nout)
        names(private$.outtype) = letters[seq_along(private$.outtype)]
      }
  )
)
