#' @title PipeOpProxy
#'
#' @usage NULL
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Behaves like another [`PipeOp`] or [`Graph`] that is encapsulated as a `content` hyperparameter.
#' Input is routed through the `content` and the `content`s' output is returned. Note that the
#' `content` hyperparameter can be changed, this is useful as an alternative to branching.
#'
#' @section Construction:
#' ```
#' PipeOpProxy$new(innum = 0, outnum = 1, id = "proxy", param_vals = list())
#' ```
#' * `innum` :: `numeric(1)\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number of inputs.
#' * `outnum` :: `numeric(1)\cr
#'   Determines the number of output channels.
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`].
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpProxy`] has multiple input channels depending on the `innum` construction argument, named
#' `"input1"`, `"input2"`, ... if `innum` is nonzero; if `innum` is 0, there is only one *vararg*
#' input channel named `"..."`.
#'
#' [`PipeOpProxy`] has multiple output channels depending on the `outnum` construction argument,
#' named `"output1"`, `"output2"`, ...
#' The output is determined by the output of the `content` [`PipeOp`] or [`Graph`].
#'
#' @section State:
#' The `$state` is the `content` [`PipeOp`] or [`Graph`].
#'
#' @section Parameters:
#' * `content` :: [`PipeOp`] or [`Graph`] \cr
#'   The [`PipeOp`] or [`Graph`] that is being proxied (more precisely an object that can be
#'   converted to a [`Graph`], because this is done internally). Defaults to an instance of
#'   [`PipeOpCopy`] (simply copies the input).
#'
#' @section Internals:
#' [`PipeOpProxy`] inherits from [`PipeOp`]. The `content` will internally be coerced to a graph via
#' `as_graph` prior to train and predict.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task1 = tsk("iris")
#' task2 = task1$clone(deep = TRUE)
#'
#' pop = po("pca", param_vals = list(scale. = TRUE))
#' pop$train(list(task1))
#' pop$state
#'
#' proxy = po("proxy", innum = 1, outnum = 1,
#'   param_vals = list(content = po("pca", param_vals = list(scale. = TRUE))))
#' proxy$train(list(task2))
#' proxy$state$state
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpProxy = R6Class("PipeOpProxy",
  inherit = PipeOp,
  public = list(
    initialize = function(innum = 0L, outnum = 1L, id = "proxy", param_vals = list()) {
      assert_int(innum, lower = 0L)
      assert_int(outnum, lower = 1L)
      # input can be a vararg input channel
      inname = if (innum) rep_suffix("input", innum) else "..."
      ps = ParamSet$new(params = list(
        ParamUty$new("content", tags = c("train", "predidct", "required"), custom_check = function(x) {
          # content must be an object that can be coerced to a Graph and the output number must match
          tryCatch({
            graph = as_graph(x)
            if (NROW(graph$output) != outnum) "Graph's output number must match `outnum`" else TRUE
          },
          error = function(error_condition) "`content` must be an object that can be converted to a Graph")
        })
      ))
      ps$values = list(content = PipeOpCopy$new(outnum = outnum))
      super$initialize(id, param_set = ps, param_vals = param_vals,
        input = data.table(name = inname, train = "*", predict = "*"),
        output = data.table(name = rep_suffix("output", outnum), train = "*", predict = "*"),
      )
    },
    train_internal = function(input) {
      content = as_graph(self$param_set$values$content, clone = TRUE)
      output = content$train(unname(input), single_input = FALSE)
      self$state = content
      output
    },
    predict_internal = function(input) {
      content = self$state
      output = content$predict(unname(input), single_input = FALSE)
      self$state = content
      output
    }
  )
)

mlr_pipeops$add("proxy", PipeOpProxy)
