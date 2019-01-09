#' @title PipeOpEnsemble
#' @format [R6Class] PipeOpEnsemble
#'
#' @description
#' Abstract base-class.
#' Aggregates a list of predictions.
#' @section Usage:
#' Inherits from [PipeOpEnsemble]
#' * `f = PipeOpEnsemble$new(outnum, id)` \cr
#'     `integer(1)`, `character(1)` -> [PipeOpEnsemble]
#' @section Details:
#' * `innum`: `integer(1)` Number of inputs.
#' @name PipeOpEnsemble
#' @family PipeOp, PipeOpAggregate, PipeOpEnsemble
#' @export
PipeOpEnsemble = R6Class("PipeOpEnsemble",
  inherit = PipeOp,

  public = list(
    initialize = function(innum, id) {
      assert_integerish(innum)
      super$initialize(id)
    },

    train = function(inputs) {
      self$state = list()
      return(list())
    }
  ),
  private = list(
    merge_predictions = function(inputs) {
      do.call("rbind", map(inputs, function(x) as.data.table(x)))
    }
  )
)


#' @title PipeOpModelAvg
#' @format [R6Class] PipeOpModelAvg
#'
#' @description
#' Averages its input (a list of [mlr3::Prediction]).
#' Returns a [mlr3::Prediction].
#' @section Usage:
#' Inherits from [PipeOpEnsemble]
#' * `f = PipeOpModelAvg$new(innum, id)` \cr
#'     `integer(1)`, `character(1)` -> [PipeOpModelAvg]
#' @section Details:
#' * `innum`: `integer(1)` Number of inputs.
#' @name PipeOpModelAvg
#' @family PipeOp, PipeOpAggregate, PipeOpEnsemble
#' @export
#' @examples
#' op = PipeOpModelAvg$new(3)
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
      return(list(p))
    }
  )
)


#' @title PipeOpMajorityVote
#' @format [R6Class] PipeOpMajorityVote
#'
#' @description
#' Computes the mode over different predictions for each row_id.
#' @section Usage:
#' Inherits from [PipeOpMajorityVote]
#' * `f = PipeOpMajorityVote$new(outnum, id)` \cr
#'     `integer(1)`, `character(1)` -> [PipeOpMajorityVote]
#' @section Details:
#' * `innum`: `integer(1)` Number of inputs.
#' @name PipeOpMajorityVote
#' @family PipeOp, PipeOpAggregate, PipeOpEnsemble
#' @export
#' @examples
#' op = PipeOpMajorityVote$new(3)
PipeOpMajorityVote = R6Class("PipeOpMajorityVote",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, id = "PipeOpMajorityVote") {
      super$initialize(innum, id)
    },
    predict = function(inputs) {
      prds = private$merge_predictions(inputs)
      prds = prds[, list(response = compute_mode(response), truth = truth[1]), by = "row_id"]
      # FIXME This is ugly, but currently the best way
      p = PredictionClassif$new()
      p$row_ids = prds$row_id
      p$response = prds$response
      p$truth = prds$truth
      return(list(p))
    }
  )
)
