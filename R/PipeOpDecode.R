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
        treatment_encoding = p_lgl(tags = c("train", "required")),
        treatment_cutoff = p_dbl(default = 0, tags = "train", requires = quote(treatment_encoding == TRUE)),
        group_pattern = p_uty(custom_check = check_string, tags = c("train", "required")),
        ties_method = p_fct(c("first", "last", "random"), tags = c("train", "required"))
      )
      ps$values = list(treatment_encoding = FALSE, group_pattern = "^([^.]+)\\.", ties_method = "random")
      super$initialize(id, param_set = ps, param_vals = param_vals, tags = "encode", feature_types = c("integer", "numeric"))
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      pv = self$param_set$values
      cols = colnames(dt)

      if (treatment_encoding) {
        # Determine name for reference level
        ref_name = "ref"
        counter = 1
        while (ref_name %in% cols) {
          ref_name = paste0("ref.", counter)
          counter = counter + 1
        }
      }

      # If pattern == "", all columns are collapsed into one column
      # If "pipeop.decoded" is already taken, we overwrite it!
      if (pv$group_pattern == "") {
        if (treatment_encoding) {
          # First add entry ref_name with empty name
          cmap = list(pipeop.decoded = c(set_names(cols, cols)))
          cmap[[pipeop.decoded]][[length(cols) + 1]] = ref_name
          return(list(colmaps = cmap))
        }
        return(list(colmaps = list(pipeop.decoded = set_names(cols, cols))))
      }

      # Drop columns that do contain group_pattern
      # What about cols starting with .? -> probably let user exclude this by changing group_pattern are using affect_columns
      cols = cols[grepl(pv$group_pattern, cols, perl = TRUE)]

      # Extract factor names
      # If group_pattern contains does not contain a capturing group, fcts cannot be generated
      matches = regmatches(cols, regexec(pv$group_pattern, cols, perl = TRUE))
      fcts = map_chr(matches, 2)

      if (any(nchar(fcts) == 0)) {
        stopf("Pattern %s with column %s would produce empty decoded column name", group_pattern, cols[nchar(fcts) == 0])
      }
      # Extract level names
      lvls = set_names(gsub(pv$group_pattern, "", cols, perl = TRUE), cols)

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

      s$cutoff = pv$treatment_cutoff %??% 0
      s$ties_method = pv$ties_method
      s
    },

    .transform_dt = function(dt, levels) {
      colmaps = self$state$colmaps
      cutoff = self$state$cutoff
      ties_method = self$state$ties_method

      for (fct in names(colmaps)) {
        old_cols = names(colmaps[[fct]])
        lvls = colmaps[[fct]]

        old_cols_matrix = as.matrix(dt[, old_cols, with = FALSE])

        # Find the column with the maximal value for each row
        set(dt, , fct, old_cols[apply(old_cols_matrix, 1, which_max, ties_method = ties_method)])

        # If any value in old_cols_matrix are smaller than the cutoff, replace with empty string
        if (treatment_encoding) {
          set(dt, rowSums(old_cols_matrix <= cutoff) > 0, "")
        }

        # Assign the corresponding value from the named vector to the new column
        set(dt, , fct, factor(lvls[match(dt[[fct]], names(lvls))], levels = lvls))
      }

      # Drop old columns
      drop = unlist(lapply(colmaps, names))
      dt[, (drop) := NULL]

      dt
    }
  )
)

mlr_pipeops$add("decode", PipeOpDecode)
