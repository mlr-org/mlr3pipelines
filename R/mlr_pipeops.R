#' @title Dictionary of PipeOps
#'
#' @usage NULL
#' @format [`R6Class`] object inheriting from [`mlr3misc::Dictionary`].
#'
#' @description
#' A simple [`Dictionary`][mlr3misc::Dictionary] storing objects of class [`PipeOp`].
#' Each `PipeOp` has an associated help page, see `mlr_pipeops_[id]`.
#'
#' @section Fields:
#' Fields inherited from [`Dictionary`][mlr3misc::Dictionary], as well as:
#' * `metainf` :: `environment`\cr
#'   Environment that stores the `metainf` argument of the `$add()` method.
#'   Only for internal use.
#'
#' @section Methods:
#' Methods inherited from [`Dictionary`][mlr3misc::Dictionary], as well as:
#' * `add(key, value, metainf = NULL)`\cr
#'   (`character(1)`, `R6ClassGenerator`, `NULL` | `list`)\cr
#'   Adds constructor `value` to the dictionary with key `key`, potentially
#'   overwriting a previously stored item. If `metainf` is not `NULL` (the default),
#'   it must be a `list` of arguments that will be given to the `value` constructor (i.e. `value$new()`)
#'   when it needs to be constructed for `as.data.table` [`PipeOp`] listing.
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [`Dictionary`][mlr3misc::Dictionary] -> [`data.table::data.table`]\cr
#'   Returns a `data.table` with columns `key` (`character`), `packages` (`character`),
#'   `input.num` (`integer`), `output.num` (`integer`), `input.type.train` (`character`),
#'   `input.type.predict` (`character`), `output.type.train` (`character`), `output.type.predict` (`character`).
#'
#' @examples
#' mlr_pipeops$get("pca")
#'
#' mlr_pipeops$get("learner", learner = "classif.rpart")
#'
#' # equivalent:
#' mlr_pipeops$get("learner", "classif.rpart")
#'
#' # unnecessary and silly:
#' mlr_pipeops$add("nop2", PipeOpNOP)
#' mlr_pipeops$get("nop2")
#'
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @export
mlr_pipeops = R6Class("DictionaryPipeOp", inherit = mlr3misc::Dictionary,
  cloneable = FALSE,
  public = list(
    metainf = new.env(parent = emptyenv()),
    add = function(key, value, metainf = NULL) {
      ret = super$add(key, value)
      if (!is.null(metainf)) {
        assign(x = key, value = metainf, envir = self$metainf)
      }
      invisible(self)
    },
    get = function(key, ...) {
      obj = get0(key, envir = self$items, inherits = FALSE, ifnotfound = NULL)
      if (is.null(obj)) {
        stopf("Element with key '%s' not found!%s", key, did_you_mean(key, self$keys()))
      }
      obj$value$new(...)
    }
  )
)$new()

#' @export
as.data.table.DictionaryPipeOp = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    metainf = x$metainf[[key]]
    if (!is.null(metainf)) {
      meta_one = lapply(metainf, function(x) if (identical(x, "N")) 1 else x)
      meta_two = lapply(metainf, function(x) if (identical(x, "N")) 2 else x)
      l1 = do.call(x$get, c(list(key), meta_one))
      l2 = do.call(x$get, c(list(key), meta_two))
    } else {
      l1 = l2 = x$get(key)
    }
    if (nrow(l1$input) == nrow(l2$input) && "..." %nin% l1$input$name) {
      innum = nrow(l1$input)
    } else {
      innum = NA
    }
    if (nrow(l1$output) == nrow(l2$output)) {
      outnum = nrow(l1$output)
    } else {
      outnum = NA
    }
    list(
      key = key,
      packages = list(l1$packages),
      input.num = innum,
      output.num = outnum,
      input.type.train = list(l1$input$train),
      input.type.predict = list(l1$input$predict),
      output.type.train = list(l1$output$train),
      output.type.predict = list(l1$output$predict)
    )
  }), "key")[]
}
