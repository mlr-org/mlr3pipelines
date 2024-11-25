#' @title Factor Decoding
#'
#' @usage NULL
#' @name mlr_pipeops_decode
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Description
#'
#' @section Construction:
#' ```
#' PipeOpEncode$new(id = "decode", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"encode"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * ...
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * ...
#'
#' @section Internals:
#' Uses the [`stats::contrasts`] functions. This is relatively inefficient for features with a large number of levels.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#'
PipeOpDecode = R6Class("PipeOpDecode",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "decode", param_vals = list()) {
      ps = ps(
        treatment_encoding = p_lgl(tags = c("train", "predict")),
        group_pattern = p_uty(custom_check = check_string, tags = c("train", "predict"))
      )
      ps$values = list(treatment_encoding = FALSE, group_pattern = "^([^.]*)\\.")
      super$initialize(id, param_set = ps, param_vals = param_vals, tags = "encode", feature_types = c("integer", "numeric"))
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      pv = self$param_set$values
      cols = colnames(dt)

      # If pattern == "", all columns are collapsed into one column
      if (pv$group_pattern == "") {
        return(list(colmaps = list(result = set_names(cols, cols))))
        # should make sure that result is available (or name we chose instead)
      }

      # Drop columns that do contain group_pattern
      # What about cols starting with .? -> probably let user exclude this by changing group_pattern are using affect_columns
      cols = cols[grepl(pv$group_pattern, cols)]

      # Extract factor names
      matches = regmatches(cols, regexec(pv$group_pattern, cols))
      fcts = vapply(matches, function(x) x[[2]], character(1))
      # Extract level names
      lvls = set_names(gsub(pv$group_pattern, "", cols), cols)

      # test that split is consistent for this use case
      s = list(colmaps = split(lvls, fcts))

      if (pv$treatment_encoding) {
        # Set default name for reference level
        ref_name = "ref"
        counter = 1
        while (ref_name %in% cols) {
          ref_name = paste0("ref.", counter)
          counter = counter + 1
        }
        # Append ref_name with empty name to all list entries
        for (i in seq_along(s[["colmaps"]])) {
          s[["colmaps"]][[i]][[length(s[["colmaps"]][[i]]) + 1]] = ref_name
        }
      }

      s
    },

    .transform_dt = function(dt, levels) {
      colmaps = self$state$colmaps

      for (fct in names(colmaps)) {
        old_cols = names(colmaps[[fct]])
        lvls = colmaps[[fct]]

        # Do we check that which.max is unique?
        # Generally what checks should we perform? e.g. that group_pattern contains a capturing group

        # Find the column with the maximal value for each row
        dt[, (fct) := old_cols[apply(.SD, 1, which.max)], .SDcols = old_cols]
        # Assign the corresponding value from the named vector to the new column
        # dt[, (fct) := lvls[get(fct)]]
        dt[, (fct) := lvls[dt[[fct]]]]

        # type conversion to factor?

        # Remove the old columns (can move this to outside the loop)
        dt[, (old_cols) := NULL]
      }

      dt
    }
  )
)

mlr_pipeops$add("decode", PipeOpDecode)

# We don't add columns that have no match with group_pattern to state
# We only remove old_cols in .train_dt -> These columns are just ignored. Good.
#
