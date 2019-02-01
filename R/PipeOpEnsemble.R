#' @title PipeOpEnsemble
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Parent class for PipeOps that aggregate a list of predictions.
#' Offers the private method `$merge_predictions()` which does exactly that.
#' @section Methods:
#' * `PipeOpEnsemble$new(innum, id)` \cr
#'   (`numeric(1)`, `character(1)`) -> `self` \cr
#'   Constructor. `innum` determines the number of input channels.
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpEnsemble = R6Class("PipeOpEnsemble",
  inherit = PipeOp,

  public = list(
    initialize = function(innum, id) {
      assert_integerish(innum, lower = 1)
      super$initialize(id,
        input = data.table(name = rep_suffix("input", innum), train = "NULL", predict = "Prediction"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
    },

    train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    predict = function(inputs) stop("abstract")
  ),
  private = list(
    merge_predictions = function(inputs) {
      do.call("rbind", map(inputs, function(x) as.data.table(x)))
    }
  )
)

#' @title PipeOpModelAvg
#'
#' @name mlr_pipeop_modelavg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`].
#'
#' @description
#' Averages its input (a `list` of [`Prediction`]). Returns a
#' single [`Prediction`].
#'
#' @family PipeOps
#' @examples
#' op = PipeOpModelAvg$new(3)
#' @export
PipeOpModelAvg = R6Class("PipeOpModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, id = "PipeOpModelAvg") {
      super$initialize(innum, id)
    },
    predict = function(inputs) {
      prds = private$merge_predictions(inputs)
      prds = prds[, list(response = mean(response, na.rm = TRUE), truth = truth[1]), by = "row_id"]
      # FIXME This is ugly, but currently the best way
      p = PredictionRegr$new()
      p$row_ids = prds$row_id
      p$response = prds$response
      p$truth = prds$truth
      list(p)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpModelAvg", PipeOpModelAvg)


#' @format [R6Class] PipeOpMajorityVote
#'
#' @name mlr_pipeop_majorityvote
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`].
#'
#' @description
#' Computes the mode over different predictions for each row_id.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpMajorityVote$new(3)
#' @export
PipeOpMajorityVote = R6Class("PipeOpMajorityVote",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, id = "majorityvote") {
      super$initialize(innum, id)
    },

    predict = function(inputs) {
      # cbind all predictions and aggregate by "row_id"
      prds = private$merge_predictions(inputs)
      prds = prds[, list(response = compute_mode(response), truth = truth[1]), by = "row_id"]
      # FIXME This is ugly, but currently the best way
      p = PredictionClassif$new()
      p$row_ids = prds$row_id
      p$response = prds$response
      p$truth = prds$truth
      list(p)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpMajorityVote", PipeOpMajorityVote)
