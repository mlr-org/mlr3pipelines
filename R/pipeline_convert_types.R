#' @include mlr_graphs.R

#' @title Convert Column Types
#' @name mlr_graphs_convert_types
#' @description
#' Converts all columns of type `type_from` to `type_to`, using the corresponding R function (e.g. `as.numeric()`, `as.factor()`).
#' It is possible to further subset the columns that should be affected using the `affect_columns` argument.
#' The resulting [`Graph`] contains a [`PipeOpColApply`], followed, if appropriate, by a [`PipeOpFixFactors`].
#'
#' Unlike R's `as.factor()` function, `ppl("convert_types")` will convert `ordered` types into (unordered) `factor` vectors.
#'
#' @param type_from `character` \cr
#'   Which column types to convert. May be any combination of `"logical"`, `"integer"`, `"numeric"`, `"factor"`, `"ordered"`, `"character"`, or `"POSIXct"`.
#' @param type_to `character(1)` \cr
#'   Which type to convert to. Must be a scalar value, exactly one of the types allowed in `type_from`.
#' @param affect_columns `function` | [`Selector`] | `NULL` \cr
#'   Which columns to affect. This argument can further restrict the columns being converted, beyond the `type_from` argument.
#'   Must be a [`Selector`]-like function, which takes a [`Task`][mlr3::Task] as argument and returns a `character` of features to use.
#' @param id `character(1)` | `NULL` \cr
#'   ID to give to the constructed [`PipeOp`]s.
#'   Defaults to an ID built automatically from `type_from` and `type_to`.
#'   If a [`PipeOpFixFactors`] is appended, its ID will be `paste0(id, "_ff")`.
#' @param fixfactors `logical(1)` | `NULL` \cr
#'   Whether to append a [`PipeOpFixFactors`]. Defaults to `TRUE` if and only if `type_to` is `"factor"` or `"ordered"`.
#' @param more_args `list` \cr
#'   Additional arguments to give to the conversion function. This could e.g. be used to pass the timezone to `as.POSIXct`.
#'
#' @return [`Graph`]
#' @export
#' @examples
#' library("mlr3")
#'
#' data_chr = data.table::data.table(
#'   x = factor(letters[1:3]),
#'   y = letters[1:3],
#'   z = letters[1:3]
#' )
#' task_chr = TaskClassif$new("task_chr", data_chr, "x")
#' str(task_chr$data())
#'
#' graph = ppl("convert_types", "character", "factor")
#' str(graph$train(task_chr)[[1]]$data())
#'
#' graph_z = ppl("convert_types", "character", "factor",
#'   affect_columns = selector_name("z"))
#' graph_z$train(task_chr)[[1]]$data()
#'
#' # `affect_columns` and `type_from` are both applied. The following
#' # looks for a 'numeric' column with name 'z', which is not present;
#' # the task is therefore unchanged.
#' graph_z = ppl("convert_types", "numeric", "factor",
#'   affect_columns = selector_name("z"))
#' graph_z$train(task_chr)[[1]]$data()
pipeline_convert_types = function(type_from, type_to, affect_columns = NULL, id = NULL, fixfactors = NULL, more_args = list()) {
  coltypes = mlr_reflections$task_feature_types


  assert_character(type_from, any.missing = FALSE, unique = TRUE)
  assert_subset(type_from, coltypes)
  assert_choice(type_to, coltypes)
  assert_function(affect_columns, null.ok = TRUE)
  assert_string(id, null.ok = TRUE)
  assert_flag(fixfactors, null.ok = TRUE)
  assert_list(more_args)

  selector = selector_type(type_from)
  if (!is.null(affect_columns)) {
    selector = selector_intersect(selector, affect_columns)
  }
  if (is.null(id)) {
    id = sprintf("convert_%s_to_%s",
      paste(names(coltypes)[match(type_from, coltypes)], collapse = ""),
      names(coltypes)[match(type_to, coltypes)]
    )
  }

  converter = switch(type_to,
    factor = crate(function(x) {
        if (is.ordered(x)) {
          cls <- class(x)
          class(x) <- cls[cls != "ordered"]
        }
        as.factor(x)
      }),
    get(paste0("as.", type_to))
  )
  if (length(more_args)) {
    converter = crate(function(x) {
      mlr3misc::invoke(converter, x = x, .args = more_args)
    }, converter, more_args)
  }
  if (is.null(fixfactors)) {
    fixfactors = type_to %in% c("factor", "ordered")
  }
  po("colapply",
    id = id, applicator = converter, affect_columns = selector
  ) %>>!% if (fixfactors) po("fixfactors", id = paste0(id, "_ff"))
}

mlr_graphs$add("convert_types", pipeline_convert_types)

