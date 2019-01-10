#' @title PipeOpEnsemble
#'
#' @name PipeOpEnsemble
#' @format [R6Class] PipeOpEnsemble
#'
#' @description
#' Abstract base-class.
#' Parent class for PipeOps that aggregate a list of predictions.
#' @section Usage:
#' Inherits from [PipeOpEnsemble]
#' * `f = PipeOpEnsemble$new(outnum, id)` \cr
#'     `integer(1)`, `character(1)` -> [PipeOpEnsemble]
#' @section Details:
#' * `innum`: `integer(1)` Number of inputs.
#' @family PipeOp
#' @family PipeOpAggregate
#' @family PipeOpEnsemble
NULL

#' @include PipeOp.R
#' @export
PipeOpEnsemble = R6Class("PipeOpEnsemble",
  inherit = PipeOp,

  public = list(
    initialize = function(innum, id = "PipeOpEnsemble") {
      assert_integerish(innum)
      super$initialize(id,
        input = data.table(name = rep_suffix("prediction", innum), train = "NULL", predict = "Prediction"),
        output = data.table(name = "prediction", train = "NULL", predict = "Prediction")
      )
    },

    train = function(inputs) {
      self$state = list()
      return(list(NULL))
    },
    predict = function(inputs) {}
  ),
  private = list(
    merge_predictions = function(inputs) {
      do.call("rbind", map(inputs, function(x) as.data.table(x)))
    }
  )
)


#' @title PipeOpModelAvg
#'
#' @name PipeOpModelAvg
#' @format [R6Class] PipeOpModelAvg
#'
#' @description
#' Averages its input (a list of [mlr3::Prediction]).
#' Returns a [Prediction]s.
#' @section Usage:
#' Inherits from [PipeOpEnsemble]
#' * `f = PipeOpModelAvg$new(innum, id)` \cr
#'     `integer(1)`, `character(1)` -> [PipeOpModelAvg]
#' @section Details:
#' * `innum`: `integer(1)` Number of inputs.
#' @name PipeOpModelAvg
#' @family PipeOp
#' @family PipeOpAggregate
#' @family PipeOpEnsemble
#' @examples
#' op = PipeOpModelAvg$new(3)
NULL

#' @include PipeOp.R
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
#' @family PipeOp
#' @family PipeOpAggregate
#' @family PipeOpEnsemble
#' @examples
#' op = PipeOpMajorityVote$new(3)
NULL

#' @include PipeOp.R
#' @export
PipeOpMajorityVote = R6Class("PipeOpMajorityVote",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, id = "PipeOpMajorityVote") {
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
      return(list(p))
    }
  )
)
