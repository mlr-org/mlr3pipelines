#' @title PipeOpOVRSplit
#'
#' @usage NULL
#' @name mlr_pipeop_ovrsplit
#' @include PipeOp.R
#' @export
PipeOpOVRSplit = R6Class("PipeOpOVRSplit",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "ovrsplit", param_vals = list()) {
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "TaskClassif", predict = "TaskClassif"),
        output = data.table(name = "output", train = "[TaskClassif]", predict = "[TaskClassif]"),
        tags = c("target transform", "multiplicity")
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      task = inputs[[1]]
      self$state = list(levels = levels(task$truth()))
      list(as.Multiplicity(private$.splittask(task, self$state$levels)))
    },
    .predict = function(inputs) {
      list(as.Multiplicity(private$.splittask(inputs[[1]], self$state$levels)))
    },
    .splittask = function(task, levels) {
      rest = "rest"
      while (rest %in% levels) {
        rest = paste0(rest, ".")
      }
      sapply(levels, function(l) {
        truthcol = task$data(cols = task$target_names)
        truthcol[[1]] = factor(ifelse(truthcol[[1]] == l, l, rest), levels = c(l, rest))
        backend = task$clone(deep = TRUE)$cbind(truthcol)$backend
        TaskClassif$new(sprintf("%s_%s_vs_rest", task$id, l), backend, task$target_names, positive = l)
      }, simplify = FALSE)
    }
  )
)

mlr_pipeops$add("ovrsplit", PipeOpOVRSplit)



#' @title PipeOpOVRUnite
#'
#' @usage NULL
#' @name mlr_pipeops_ovrunite
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`]/[`PipeOp`].
#' FIXME: document
#' @include PipeOpEnsemble.R
#' @export
PipeOpOVRUnite = R6Class("PipeOpOVRUnite",
  inherit = PipeOpEnsemble,
  public = list(
    initialize = function(id = "ovrunite", param_vals = list()) {
      super$initialize(0, TRUE, id, param_vals = param_vals, prediction_type = "PredictionClassif", tags = "multiplicity")
    }
  ),
  private = list(
    .predict = function(inputs) {
      if (private$.collect) {
        inputs = unclass(inputs[[1]])
      }
      weights = self$param_set$values$weights
      row_ids = inputs[[1]]$row_ids
      map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
      if (length(weights) == 1) weights = rep(1, length(inputs))
      assert_numeric(weights, any.missing = FALSE, len = length(inputs))
      has_probs = every(inputs, function(x) !is.null(x$prob))
      has_classif_response = every(inputs, function(x) !is.null(x$response))

      names(inputs) = map_chr(inputs, function(x) levels(x$truth)[[1]])

      truth = factor(do.call(fcoalesce, map(inputs, function(x) ifelse(as.numeric(x$truth) == 1, levels(x$truth)[[1]], NA_character_))),
        levels = names(inputs))

      if (has_probs) {
        probmat = sapply(inputs, function(x) x$prob[, 1])
      } else if (has_classif_response) {
        probmat = sapply(inputs, function(x) 2 - as.numeric(x$response))
      } else {
        stop("PipeOpOVRUnite input predictions had missing 'prob' and missing 'response' values. At least one of them must be given for all predictions.")
      }
      probmat = sweep(probmat, 2, weights, "*")
      probmat = probmat / rowSums(probmat)
      probmat[is.na(probmat)] = 1 / length(inputs)

      list(PredictionClassif$new(row_ids = row_ids, truth = truth, prob = pmin(pmax(probmat, 0), 1)))
    }
  )
)

mlr_pipeops$add("ovrunite", PipeOpOVRUnite)
