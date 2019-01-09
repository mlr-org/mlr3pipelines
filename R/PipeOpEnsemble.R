PipeOpEnsemble = R6Class("PipeOpEnsemble",
  inherit = PipeOp,

  public = list(
    initialize = function(innum, id) {
      super$initialize(id)
    },

    train = function(inputs) {
      self$state = list()
      return(list())
    },

    predict = function(inputs) {
    }
  ),
  private = list(
    merge_predictions = function(inputs) {
      do.call("rbind", map(inputs, function(x) as.data.table(x)))
    }
  )
)

PipeOpModelAvg = R6Class("PipeOpModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function() {
      super$initialize("PipeOpModelAvg")
    },
    predict = function(inputs) {
      prds = private$merge_predictions(inputs)
      prds[, list(response = mean(response, na.rm = TRUE), truth = truth[1]), by = "row_id"]
      
      p = PredictionRegr$new(row_id = prds$row_id, response = prds$response,
        truth = prds$truth)
      return(p)
    }
  )
)

PipeOpMajorityVote = R6Class("PipeOpMajorityVote",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function() {
      super$initialize("PipeOpMajorityVote")
    },
    predict = function(inputs) {
      prds = private$merge_predictions(inputs)
      prds[, list(response = compute_mode(response), truth = truth[1]), by = "row_id"]

      p = PredictionRegr$new(row_id = prds$row_id, response = prds$response,
        truth = prds$truth)
      return(p)
    }
  )
)
