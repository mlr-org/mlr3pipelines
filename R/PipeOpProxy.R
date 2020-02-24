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
#' PipeOpProxy$new(id, param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`].
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpProxy`] has one input channel named `"input"`, taking any input (`"*"`) both during
#' training and prediction.
#'
#' [`PipeOpProxy`] can have multiple output channels depending on the `outnum` construction argument
#' of the `content` [`PipeOp`], named `"output1"`, `"output2"`, ... (one output channel if `content`
#' is a [`Graph`], named `"output"`). The output is determined by the output of the `content`
#' [`PipeOp`] or [`Graph`].
#'
#' @section State:
#' The `$state` is the `content` [`PipeOp`] or [`Graph`].
#'
#' @section Parameters:
#' * `content` :: [`PipeOp`] or [`Graph`] \cr
#'   The [`PipeOp`] or [`Graph`] that is being proxied. Defaults to [`PipeOpNOP`] (simply pushes the
#'   input forward).
#'
#' @section Internals:
#' [`PipeOpProxy`] inherits from [`PipeOp`].
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
#' proxy = po("proxy", param_vals = list(content = po("pca", param_vals = list(scale. = TRUE))))
#' proxy$train(list(task2))
#' proxy$state$state
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpProxy = R6Class("PipeOpProxy",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "proxy", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("content", tags = c("train", "predidct", "required"), custom_check = function(x) {
          cl = class(x)
          ifelse(test_r6(x, classes = "PipeOp") || test_r6(x, classes = "Graph"), yes = TRUE,
            no =  sprintf("Must inherit from class 'PipeOp' or 'Graph', but has class%s '%s'",
              ifelse(length(cl) > 1L, yes = "es", no = ""), paste0(cl, collapse = "','")))
        })
      ))
      ps$values = list(content = PipeOpNOP$new())
      super$initialize(id, param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = ifelse(test_r6(ps$values$content, classes = "PipeOp"),
          yes = rep_suffix("output", n = ps$values$content$outnum), no = "output"), train = "*", predict = "*")
      )
    },
    train_internal = function(input) {
      content = self$param_set$values$content$clone(deep = TRUE)
      if (inherits(content, "Graph")) {
        output = content$train(unname(input), single_input = FALSE)
      } else {
        output = content$train(input)
      }
      self$state = content
      output
    },
    predict_internal = function(input) {
      content = self$state
      if (inherits(content, "Graph")) {
        output = content$predict(unname(input), single_input = FALSE)
      } else {
        output = content$predict(input)
      }
      self$state = content
      output
    }
  )
)

mlr_pipeops$add("proxy", PipeOpProxy)
