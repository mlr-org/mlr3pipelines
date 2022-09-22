#' @title Wrap another PipeOp or Graph as a Hyperparameter
#'
#' @usage NULL
#' @name mlr_pipeops_proxy
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Wraps another [`PipeOp`] or [`Graph`] as determined by the `content` hyperparameter.
#' Input is routed through the `content` and the `content`s' output is returned.
#' The `content` hyperparameter can be changed during tuning, this is useful as an alternative to [`PipeOpBranch`].
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
#' The output is determined by the output of the `content` operation (a [`PipeOp`] or [`Graph`]).
#'
#' @section State:
#' The `$state` is the trained `content` [`PipeOp`] or [`Graph`].
#'
#' @section Parameters:
#' * `content` :: [`PipeOp`] | [`Graph`]\cr
#'   The [`PipeOp`] or [`Graph`] that is being proxied (or an object that is
#'   converted to a [`Graph`] by [`as_graph()`]). Defaults to an instance of
#'   [`PipeOpFeatureUnion`] (combines all input if they are [`Task`][mlr3::Task]s).
#'
#' @section Internals:
#' The `content` will internally be coerced to a graph via
#' [`as_graph()`] prior to train and predict.
#'
#' The default value for `content` is [`PipeOpFeatureUnion`],
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' library("mlr3learners")
#'
#' set.seed(1234)
#' task = tsk("iris")
#'
#' # use a proxy for preprocessing and a proxy for learning, i.e.,
#' # no preprocessing and classif.kknn
#' g = po("proxy", id = "preproc", param_vals = list(content = po("nop"))) %>>%
#'   po("proxy", id = "learner", param_vals = list(content = lrn("classif.kknn")))
#' rr_kknn = resample(task, learner = GraphLearner$new(g), resampling = rsmp("cv", folds = 3))
#' rr_kknn$aggregate(msr("classif.ce"))
#'
#' # use pca for preprocessing and classif.rpart as the learner
#' g$param_set$values$preproc.content = po("pca")
#' g$param_set$values$learner.content = lrn("classif.rpart")
#' rr_pca_rpart = resample(task, learner = GraphLearner$new(g), resampling = rsmp("cv", folds = 3))
#' rr_pca_rpart$aggregate(msr("classif.ce"))
#' @family PipeOps
#' @template seealso_pipeopslist
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
            # graph$output access may be slow, so we cache it here
            graph_outnum = nrow(graph$output)
            graph_input = nrow(graph$input)
            if (graph_outnum != 1 && graph_outnum != outnum) {
              "Graph's output number must either be 1 or match `outnum`"
            } else if (innum > 1 && graph_input != innum && (graph_input > innum || "..." %nin% graph$input$name)) {
              "Graph's input number when `innum` > 1 must either match `innum` or the Graph must contain a '...' (vararg) channel."
            } else {
              TRUE
            }
          },
          error = function(error_condition) "`content` must be an object that can be converted to a Graph")
        })
      ))
      ps$values = list(content = PipeOpFeatureUnion$new(innum = innum))
      super$initialize(id, param_set = ps, param_vals = param_vals,
        input = data.table(name = inname, train = "*", predict = "*"),
        output = data.table(name = rep_suffix("output", outnum), train = "*", predict = "*"),
        tags = "meta"
      )
    }
  ),
  private = list(
    .param_set = NULL,
    .param_set_source = NULL,
    .id = NULL,
    .train = function(input) {
      content = as_graph(self$param_set$values$content)
      prestate = content$state
      on.exit({content$state = prestate})
      if (length(input) > 1) {
        output = content$train(unname(input), single_input = FALSE)
      } else {
        # automatically send input to all graph inputs
        output = content$train(input[[1]])
      }
      self$state = content$state
      output
    },
    .predict = function(input) {
      content = as_graph(self$param_set$values$content)
      prestate = content$state
      on.exit({content$state = prestate})
      content$state = self$state
      if (length(input) > 1) {
        output = content$predict(unname(input), single_input = FALSE)
      } else {
        # automatically send input to all graph inputs
        output = content$predict(input[[1]])
      }
    }
  )
)

mlr_pipeops$add("proxy", PipeOpProxy)
