#' @title Dictionary of PipeOps
#'
#' @format [`R6Class`] object inheriting from [`mlr3misc::Dictionary`]
#' @description
#' A simple [`Dictionary`][mlr3misc::Dictionary] storing objects of class [`PipeOp`].
#' Each `PipeOp` has an associated help page, see `mlr_pipeop_[id]`.
#'
#' @section Usage:
#'
#' See [`mlr3misc::Dictionary`].
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [Dictionary] -> [data.table::data.table()]\cr
#'   Returns a `data.table()` with fields "key", "packages", and connectors.
#'
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @name mlr_pipeops
NULL

DictionaryPipeOp = R6Class("DictionaryPipeOp", inherit = mlr3misc::Dictionary,
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
)

#' @export
mlr_pipeops = NULL

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


# We would like to have the pipeops in the "mlr_pipeops" Dictionary, but adding
# them at build time is apparently not a good idea. On the other hand we would
# like to register pipeops near their deefinition to prevent confusing
# dependencies throughout the codebase. Therefore we register the pipeops using
# "register_pipeop()" below their definition and call
# "publish_registered_pipeops()" in .onLoad.
mlr_pipeop_register = new.env(parent = emptyenv())

# nocov start
register_pipeop = function(key, value, metainf) {
  m = match.call(expand.dots = FALSE)
  mlr_pipeop_register[[key]] = m
}

publish_registered_pipeops = function() {
  mlr_pipeops <<- DictionaryPipeOp$new()

  for (registercall in as.list(mlr_pipeop_register)) {
    registercall[[1]] = quote(mlr_pipeops$add)
    eval(registercall, envir = parent.env(environment()))
  }
  rm("mlr_pipeop_register", envir = parent.env(environment()))
}
# nocov end
