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
    weights = NULL,

    initialize = function(innum, id, param_vals) {
      assert_integerish(innum, lower = 1)
      super$initialize(id, param_vals = param_vals,
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
#' Model averager for regression models.
#' Averages its input (a `list` of [`PredictionRegr`]).
#' Returns a single [`PredictionRegr`].
#'
#' @family PipeOps
#' @examples
#' op = PipeOpModelAvg$new(3)
#' @export
PipeOpModelAvg = R6Class("PipeOpModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, id = "PipeOpModelAvg", param_vals = list()) {
      super$initialize(innum, id, param_vals = param_vals)
    },
    predict = function(inputs) {
      assert_true(unique(map_chr(inputs, "task_type")) == "classif")
      prds = private$model_avg(inputs)
      list(private$make_prediction_regr(prds))
    }
  ),

  private = list(
    model_avg = function(inputs) {
      prds = private$merge_predictions(inputs)
      prds[, list(response = mean(response, na.rm = TRUE), truth = truth[1]), by = "row_id"]
    },
    make_prediction_regr = function(prds) {
      p = PredictionRegr$new()
      p$row_ids = prds$row_id
      p$response = prds$response
      p$truth = prds$truth
      return(p)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("modelavg", PipeOpModelAvg)


#' @title PipeOpWtModelAvg
#'
#' @name mlr_pipeop_weightedmodelavg
#' @format [`R6Class`] inheriting from [`PipeOpModelAvg`].
#'
#' @description
#' Averages its input (a `list` of [`PredictionRegr`]).
#' Only used for regression `Prediction`s.
#' Weights can be set by the user.
#' Offers a `$weights` slot to set/get weights for each learner.
#' Returns a single [`PredictionRegr`].
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpWtModelAvg$new(3)
#' @export
PipeOpWtModelAvg = R6Class("PipeOpWtModelAvg",
  inherit = PipeOpModelAvg,

  public = list(
    initialize = function(innum, weights = NULL, id = "PipeOpWtModelAvg", param_vals = list()) {
      super$initialize(innum, id, param_vals = param_vals)
      if (is.null(weights)) weights = rep(1L, innum)
      assert_numeric(weights, len = innum)
      self$weights = weights
    },
    train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    predict = function(inputs) {
      assert_true(unique(map_chr(inputs, "task_type")) == "regr")
      prds = private$weighted_model_avg(inputs, self$weights)
      prds = merge(prds, as.data.table(inputs[[1]])[, c("row_id", "truth")], by = "row_id")
      list(private$make_prediction_regr(prds))
    }
  ),
  private = list(
    weighted_model_avg = function(inputs, weights) {
      assert_numeric(weights, len = length(inputs))
      df = map_dtr(inputs, function(x) {data.frame("row_id" = x$row_ids, "response" = x$response)})
      df[, lapply(.SD, weighted.mean, w = weights), by = "row_id"]
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("wtmodelavg", PipeOpWtModelAvg)


#' @title PipeOpMajorityVote
#'
#' @format [R6Class] PipeOpMajorityVote
#'
#' @name mlr_pipeop_majorityvote
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`].
#'
#' @description
#' Aggregates over different [`PredictionClassif`s].
#' Either computes the mode, if `predict_type` is `"response"`,
#' or averages probabilities if `predict_type` is `"prob"`.
#' Returns a single [`PredictionClassif`].
#' Used only for classification.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpMajorityVote$new(3)
#' @export
PipeOpMajorityVote = R6Class("PipeOpMajorityVote",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, id = "majorityvote", param_vals = list()) {
      super$initialize(innum, id, param_vals = param_vals)
    },
    predict = function(inputs) {
      assert_list(inputs, "PredictionClassif")
      has_probs = all(map_lgl(inputs, function(x) {"prob" %in% x$predict_types}))
      if (has_probs) {
        prds = private$model_avg_probs(inputs)
        p = private$make_prediction_classif(prds, "prob")
      } else {
        prds = private$majority_vote(inputs)
        p = private$make_prediction_classif(prds, "response")
      }
      list(p)
    }
  ),
  private = list(
    model_avg_probs = function(inputs) {
      prds = private$merge_predictions(inputs)
      prds[, list(prob = mean(prob, na.rm = TRUE), truth = truth[1]), by = "row_id"]
    },
    majority_vote = function(inputs) {
      prds = private$merge_predictions(inputs)
      prds[, list(response = compute_mode(response), truth = truth[1]), by = "row_id"]
    },
    # FIXME This is ugly, but currently the best way
    make_prediction_classif = function(prds, type) {
      p = PredictionClassif$new()
      p$row_ids = prds$row_id
      p$truth = prds$truth
      p$predict_types = type
      if ("prob"%in% type) {
        p$prob = as.matrix(prds[, -c("row_id", "response", "truth")])
      }
      if ("response" %in% type) p$response = prds$response
      return(p)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("majorityvote", PipeOpMajorityVote)


#' @title PipeOpWtMajorityVote
#'
#' @format [R6Class] PipeOpWtMajorityVote
#'
#' @name mlr_pipeop_weightedmajorityvote
#' @format [`R6Class`] inheriting from [`PipeOpMajorityVote`].
#'
#' @description
#' Aggregates over different [`PredictionClassif`s].
#' Either computes the mode, if `predict_type` is `"response"`,
#' or averages probabilities if `predict_type` is `"prob"`.
#' Returns a single [`PredictionClassif`].
#' Offers a `$weights` slot to set/get weights for each learner.
#' Used for classification `Prediction`s.
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpWtMajorityVote$new(3)
#' @export
PipeOpWtMajorityVote = R6Class("PipeOpWtMajorityVote",
  inherit = PipeOpMajorityVote,

  public = list(
    initialize = function(innum, weights = NULL, id = "majorityvote", param_vals = list()) {
      super$initialize(innum, id, param_vals = param_vals)
      if(is.null(weights)) weights = rep(1L, innum)
      assert_numeric(weights, len = innum)
      self$weights = weights
    },
    predict = function(inputs) {
      assert_list(inputs, "PredictionClassif")
      has_probs = all(map_lgl(inputs, function(x) {"prob" %in% x$predict_types}))
      if (has_probs) {
        prds = private$weighted_prob_avg(inputs, self$weights)
      } else {
        prds = private$weighted_majority_vote(inputs, self$weights)
      }
      prds = merge(prds, as.data.table(inputs[[1]])[, c("row_id", "truth")], by = "row_id")
      p = private$make_prediction_classif(prds, inputs[[1]]$predict_types)
      list(p)
    }
  ),
  private = list(
    weighted_majority_vote = function(inputs, weights) {
      assert_numeric(weights, len = length(inputs))
      # Unpack predictions, add weights
      df = imap_dtr(inputs, function(x, i) {
        data.frame("row_id" = x$row_ids, "response" = x$response, "weight" = weights[i])
      })
      # Sum weights over response, keep max row.
      df[, weight := sum(weight), by = list(response, row_id)]
      df = unique(df[, maxwt := max(weight), by = "row_id"])[weight == maxwt]
      return(df[, c("row_id", "response")])
    },
    weighted_prob_avg = function(inputs, weights) {
      assert_numeric(weights, len = length(inputs))
      df = map_dtr(inputs, function(x) {data.frame("row_id" = x$row_ids, x$prob)})
      df = unique(df[, lapply(.SD, weighted.mean, w = weights), by = "row_id"])
      df$response = factor(max.col(df[, -"row_id"], ties.method='first'), labels = colnames(df[, -"row_id"]))
      return(df)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("wtmajorityvote", PipeOpWtMajorityVote)



