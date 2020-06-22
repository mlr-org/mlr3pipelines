#' @title Implicate Multiplicity
#' #export
PipeOpImplicateMultiplicity = R6Class("PipeOpImplicateMultiplicity",
  inherit = PipeOp,
  public = list(
    initialize = function(innum = 0L, id = "implicatemultiplicity", param_vals = list()) {
      assert(
        check_int(innum, lower = 0L),
        check_character(innum, min.len = 1L, any.missing = FALSE)
      )
      if (is.numeric(innum)) {
        private$.named = FALSE
      } else {
        innum = length(innum)
        private$.named = TRUE
      }
      inname = if (innum) rep_suffix("input", innum) else "..."
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = inname, train = "*", predict = "*"),
        output = data.table(name = "output", train = "[*]", predict = "[*]"),
        tags = "multiplicity"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      if (!private$.named) inputs = unname(inputs)
      list(as.Multiplicity(inputs))
    },
    .predict = function(inputs) {
      if (!private$.named) inputs = unname(inputs)
      list(as.Multiplicity(inputs))
    },
    .named = NULL
  )
)

mlr_pipeops$add("implicatemultiplicity", PipeOpImplicateMultiplicity)

#' @title Explicate Multiplicity
#' #export
PipeOpExplicateMultiplicity = R6Class("PipeOpExplicateMultiplicity",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "explicatemultiplicity", param_vals = list()) {
      assert_int(outnum, lower = 1)
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "[*]", predict = "[*]"),
        output = data.table(name = rep_suffix("output", outnum), train = "*", predict = "*"),
        tags = "multiplicity"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      unclass(inputs[[1]])
    },
    .predict = function(inputs) {
      rep_len(inputs, self$outnum)
      unclass(inputs[[1]])
    }
  )
)

mlr_pipeops$add("explicatemultiplicity", PipeOpExplicateMultiplicity, list("N"))


#' @title Replicate through Multiplicity
#' #export
PipeOpReplicate = R6Class("PipeOpReplicate",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "replicate", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamInt$new("reps", lower = 1, tags = c("train", "predict", "required"))
      ))
      ps$values = list(reps = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = "output", train = "[*]", predict = "[*]"),
        tags = "multiplicity"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      list(as.Multiplicity(rep(unname(inputs), self$param_set$values$reps)))
    },
    .predict = function(inputs) {
      list(as.Multiplicity(rep(unname(inputs), self$param_set$values$reps)))
    }
  )
)

mlr_pipeops$add("replicate", PipeOpReplicate)
