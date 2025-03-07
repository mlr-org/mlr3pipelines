#' @title Dictionary of PipeOps
#'
#' @usage NULL
#' @format [`R6Class`][R6::R6Class] object inheriting from [`mlr3misc::Dictionary`].
#'
#' @description
#' A simple [`Dictionary`][mlr3misc::Dictionary] storing objects of class [`PipeOp`].
#' Each [`PipeOp`] has an associated help page, see `mlr_pipeops_[id]`.
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
#'   Returns a `data.table` with the following columns:
#'   * `key` :: (`character`)\cr
#'     Key with which the [`PipeOp`] was registered to the [`Dictionary`][mlr3misc::Dictionary] using the `$add()` method.
#'   * `label` :: (`character`)\cr
#'     Description of the [`PipeOp`]'s functionality.
#'   * `packages` :: (`character`)\cr
#'     Set of all required packages for the [`PipeOp`]'s train and predict methods.
#'   * `tags` :: (`character`)\cr
#'     A set of tags associated with the [`PipeOp`] describing its purpose.
#'   * `feature_types` :: (`character`)\cr
#'     Feature types the [`PipeOp`] operates on. Is `NA` for [`PipeOp`]s that do not directly operate on a [Task][mlr3::Task].
#'   * `input.num`, `output.num` :: (`integer`)\cr
#'     Number of the [`PipeOp`]'s input and output channels. Is `NA` for [`PipeOp`]s which accept a varying number of input
#'     and/or output channels depending a construction argument.
#'     See `input` and `output` fields of [`PipeOp`].
#'   * `input.type.train`, `input.type.predict`, `output.type.train`, `output.type.predict` :: (`character`)\cr
#'     Types that are allowed as input to or returned as output of the [`PipeOp`]'s `$train()` and `$predict()` methods.\cr
#'     A value of `NULL` means that a null object, e.g. no data, is taken as input or being returned as output.
#'     A value of "`*`" means that any type is possible.\cr
#'     If both `input.type.train` and `output.type.train` or both `input.type.predict` and `output.type.predict` contain
#'     values enclosed by square brackets ("`[`", "`]`"), then the respective input or channel is
#'     [`Multiplicity`]-aware. For more information, see [`Multiplicity`].
#'
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @family Dictionaries
#' @export
#' @examplesIf requireNamespace("rpart")
#' library("mlr3")
#'
#' mlr_pipeops$get("learner", lrn("classif.rpart"))
#'
#' # equivalent:
#' po("learner", learner = lrn("classif.rpart"))
#'
#' # all PipeOps currently in the dictionary:
#' as.data.table(mlr_pipeops)[, c("key", "input.num", "output.num", "packages")]
mlr_pipeops = R6Class("DictionaryPipeOp", inherit = mlr3misc::Dictionary,
  cloneable = FALSE,
  public = list(
    metainf = new.env(parent = emptyenv()),
    add = function(key, value, metainf = NULL) {
      assert_false(grepl("_\\d+$", key))
      ret = super$add(key, value)
      if (!is.null(metainf)) {
        # we save the *expression*, not the value, because we could otherwise get version conflicts from objects.
        assign(x = key, value = substitute(metainf), envir = self$metainf)
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
  result = setkeyv(map_dtr(x$keys(), function(key) {
    metainf = x$metainf[[key]]
    if (!is.null(metainf)) {
      metainfval = eval(metainf, envir = topenv())
      meta_one = lapply(metainfval, function(x) if (identical(x, "N")) 1 else x)
      meta_two = lapply(metainfval, function(x) if (identical(x, "N")) 2 else x)
      l1 = tryCatch(do.call(x$get, c(list(key), meta_one)), error = function(e) ".__error__")
      l2 = tryCatch(do.call(x$get, c(list(key), meta_two)), error = function(e) ".__error__")
    } else {
      l1 = l2 = tryCatch(x$get(key), error = function(e) ".__error__")
    }
    if (identical(l1, ".__error__") || identical(l2, ".__error__")) {
      return(list(
        key = key,
        label = NA_character_,
        packages = list(NA_character_),
        tags = list(NA_character_),
        feature_types = list(NA_character_),
        input.num = NA_integer_,
        output.num = NA_integer_,
        input.type.train = list(NA_character_),
        input.type.predict = list(NA_character_),
        output.type.train = list(NA_character_),
        output.type.predict = list(NA_character_)
      ))
    }
    if (nrow(l1$input) == nrow(l2$input) && "..." %nin% l1$input$name) {
      innum = nrow(l1$input)
    } else {
      innum = NA_integer_
    }
    if (nrow(l1$output) == nrow(l2$output)) {
      outnum = nrow(l1$output)
    } else {
      outnum = NA_integer_
    }
    if (exists("feature_types", envir = l1)) ft = l1$feature_types else ft = NA_character_

    list(
      key = key,
      label = l1$label,
      packages = list(l1$packages),
      tags = list(l1$tags),
      feature_types = list(ft),
      input.num = innum,
      output.num = outnum,
      input.type.train = list(l1$input$train),
      input.type.predict = list(l1$input$predict),
      output.type.train = list(l1$output$train),
      output.type.predict = list(l1$output$predict)
    )
  }), "key")[]

  # I don't trust 'label' to never be NA, but 'packages' is always a `character` (even if often an empty one).
  missings = result$key[map_lgl(result$packages, function(x) any(is.na(x)))]
  if (length(missings)) {
    warningf("The following PipeOps could not be constructed, likely due to missing packages: %s\nTheir corresponding information is incomplete.", paste(missings, collapse = ", "))
  }
  result
}
