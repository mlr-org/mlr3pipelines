#' @title Implicate a Multiplicity
#'
#' @usage NULL
#' @name mlr_pipeops_multiplicityimply
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Implicate a [`Multiplicity`] by returning the input(s) converted to a [`Multiplicity`].
#'
#' This [`PipeOp`] has multiple input channels; all inputs are collected into a [`Multiplicity`]
#' and then are forwarded along a single edge, causing the following [`PipeOp`]s to be called
#' multiple times, once for each [`Multiplicity`] member.
#'
#' Note that [`Multiplicity`] is currently an experimental features and the implementation or UI
#' may change.
#'
#' @section Construction:
#' ```
#' PipeOpMultiplicityImply$new(innum = 0, id = "multiplicityimply", param_vals = list())
#' ```
#'
#' * `innum` :: `numeric(1)` | `character`\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number
#'   of inputs. If `innum` is a `character` vector, the number of input channels is the length of
#'   `innum`.
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"multiplicityimply"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpMultiplicityImply`] has multiple input channels depending on the `innum` construction
#' argument, named `"input1"`, `"input2"`, ... if `innum` is nonzero; if `innum` is 0, there is
#' only one *vararg* input channel named `"..."`. All input channels take any input (`"*"`) both
#' during training and prediction.
#'
#' [`PipeOpMultiplicityImply`] has one output channel named `"output"`, emitting a [`Multiplicity`]
#' of type any (`"[*]"`), i.e., returning the input(s) converted to a [`Multiplicity`] both during
#' training and prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpMultiplicityImply`] has no Parameters.
#'
#' @section Internals:
#' If `innum` is not `numeric`, e.g., a `character`, the output [`Multiplicity`] will be named based
#' on the input channel names
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @family Experimental Features
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#' task1 = tsk("iris")
#' task2 = tsk("mtcars")
#' po = po("multiplicityimply")
#' po$train(list(task1, task2))
#' po$predict(list(task1, task2))
PipeOpMultiplicityImply = R6Class("PipeOpMultiplicityImply",
  inherit = PipeOp,
  public = list(
    initialize = function(innum = 0L, id = "multiplicityimply", param_vals = list()) {
      assert(
        check_int(innum, lower = 0L),
        check_character(innum, min.len = 1L, any.missing = FALSE)
      )
      if (is.numeric(innum)) {
        inname = if (innum) rep_suffix("input", innum) else "..."
        private$.named = FALSE
      } else {
        inname = innum
        innum = length(innum)
        private$.named = TRUE
      }
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
    .named = NULL,
    .additional_phash_input = function() self$input$name
  )
)

mlr_pipeops$add("multiplicityimply", PipeOpMultiplicityImply)

#' @title Explicate a Multiplicity
#'
#' @usage NULL
#' @name mlr_pipeops_multiplicityexply
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Explicate a [`Multiplicity`] by turning the input [`Multiplicity`] into multiple outputs.
#'
#' This [`PipeOp`] has multiple output channels; the members of the input [`Multiplicity`]
#' are forwarded each along a single edge. Therefore, only multiplicities with exactly as many
#' members as `outnum` are accepted.
#'
#' Note that [`Multiplicity`] is currently an experimental features and the implementation or UI
#' may change.
#'
#' @section Construction:
#' ```
#' PipeOpMultiplicityExply$new(outnum , id = "multiplicityexply", param_vals = list())
#' ```
#'
#' * `outnum` :: `numeric(1)` | `character`\cr
#'   Determines the number of output channels.
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"multiplicityexply"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpMultiplicityExply`] has a single input channel named `"input"`, collecting a
#' [`Multiplicity`] of type any (`"[*]"`) both during training and prediction.
#'
#' [`PipeOpMultiplicityExply`] has multiple output channels depending on the `outnum` construction
#' argument, named `"output1"`, `"output2"` returning the elements of the unclassed input
#' [`Multiplicity`].
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpMultiplicityExply`] has no Parameters.
#'
#' @section Internals:
#' `outnum` should match the number of elements of the unclassed input [`Multiplicity`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @family Experimental Features
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#' task1 = tsk("iris")
#' task2 = tsk("mtcars")
#' po = po("multiplicityexply", outnum = 2)
#' po$train(list(Multiplicity(task1, task2)))
#' po$predict(list(Multiplicity(task1, task2)))
PipeOpMultiplicityExply = R6Class("PipeOpMultiplicityExply",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "multiplicityexply", param_vals = list()) {
      assert_int(outnum, lower = 1L)
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
      unclass(inputs[[1L]])
    },
    .predict = function(inputs) {
      rep_len(inputs, self$outnum)
      unclass(inputs[[1L]])
    },
    .additional_phash_input = function() self$output$name
  )
)

mlr_pipeops$add("multiplicityexply", PipeOpMultiplicityExply, list("N"))

#' @title Replicate the Input as a Multiplicity
#'
#' @usage NULL
#' @name mlr_pipeops_replicate
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Replicate the input as a [`Multiplicity`], causing subsequent [`PipeOp`]s to be executed multiple
#' `reps` times.
#'
#' Note that [`Multiplicity`] is currently an experimental features and the implementation or UI
#' may change.
#'
#' @section Construction:
#' ```
#' PipeOpReplicate$new(id = "replicate", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`
#'   Identifier of the resulting object, default `"replicate"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpReplicate`] has one input channel named `"input"`, taking any input (`"*"`) both during training and prediction.
#'
#' [`PipeOpReplicate`] has one output channel named `"output"` returning the replicated input as a
#' [`Multiplicity`] of type any (`"[*]"`) both during training and prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `reps` :: `numeric(1)`\cr
#'   Integer indicating the number of times the input should be replicated.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @family Experimental Features
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#' task = tsk("iris")
#' po = po("replicate", param_vals = list(reps = 3))
#' po$train(list(task))
#' po$predict(list(task))
PipeOpReplicate = R6Class("PipeOpReplicate",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "replicate", param_vals = list()) {
      ps = ps(
        reps = p_int(lower = 1L, tags = c("train", "predict", "required"))
      )
      ps$values = list(reps = 1L)
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
