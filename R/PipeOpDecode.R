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
        treatment_cutoff = p_dbl(default = 0, tags = "train", depends = quote(treatment_encoding == TRUE)),
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

      if (pv$treatment_encoding) {
        # Determine name for reference level
        ref_name = "ref"
        counter = 1
        while (ref_name %in% cols) {
          ref_name = paste0("ref.", counter)
          counter = counter + 1
        }
      }

      # If pattern == "", all columns are collapsed into one column.
      # Note, that column "pipeop.decoded" gets overwritten if it already exists.
      if (pv$group_pattern == "") {
        cmap = list(pipeop.decoded = c(set_names(cols, cols)))
        if (pv$treatment_encoding) {
          # Append ref_name with empty name (i.e. "")
          cmap[[pipeop.decoded]][[length(cols) + 1]] = ref_name
        }

        s = list(
          colmaps = cmap,
          treatment_encoding = pv$treatment_encoding,
          cutoff = pv$treatment_cutoff %??% 0,
          ties_method = pv$ties_method
        )

        return(s)
      }

      # Drop columns that do not match group_pattern
      cols = cols[grepl(pv$group_pattern, cols, perl = TRUE)]

      # Extract names for new levels
      lvls = set_names(gsub(pv$group_pattern, "", cols, perl = TRUE), cols)

      # Extract names for new factor columns to be populated with lvls
      matches = regmatches(cols, regexec(pv$group_pattern, cols, perl = TRUE))
      # Error, if nothing was captured.
      if (any(lengths(matches) < 2)) {
        stopf("Pattern %s matches column name %s, but nothing was captured. Make sure group_pattern contains a capturing group.",
              str_collapse(pv$group_pattern, quote = '"'),
              str_collapse(cols[lengths(matches) < 2], quote = '"'))
      }
      fcts = map_chr(matches, 2)

      # Error if no group could be extracted for an entry in col. Thus, we could not create a column name from it.
      if (any(nchar(fcts) == 0)) {
        stopf("Pattern %s with column(s) %s would produce empty string as decoded column name(s). Try using a different pattern.",
              str_collapse(pv$group_pattern, quote = '"'),
              str_collapse(cols[nchar(fcts) == 0], quote = '"'))
      }

      # Create mapping of old column names and derived levels to new column names
      cmap = split(lvls, fcts)
      if (pv$treatment_encoding) {
        # Append ref_name with empty name (i.e. "") to all list entries
        for (map in cmap) {
          map[[length(map) + 1]] = ref_name
        }
      }

      list(
        colmaps = cmap,
        treatment_encoding = pv$treatment_encoding,
        cutoff = pv$treatment_cutoff %??% 0,
        ties_method = pv$ties_method
      )
    },

    .transform_dt = function(dt, levels) {
      colmaps = self$state$colmaps
      # Early exit if no mapping is required
      if (!length(colmaps)) {
        return(dt)
      }
      cutoff = self$state$cutoff
      ties_method = self$state$ties_method
      treatment_encoding = self$state$treatment_encoding

      for (new_col in names(colmaps)) {
        lvls = colmaps[[new_col]]
        old_cols = names(lvls)

        # Create matrix from subset of dt with columns old_cols
        old_cols_matrix = as.matrix(dt[, old_cols, with = FALSE])
        # Populate new column with name of column with maximal value per row
        set(dt, , new_col, old_cols[apply(old_cols_matrix, 1, which_max, ties_method = ties_method)])
        # If any value in old_cols_matrix are smaller than the cutoff, replace with empty string
        # This implies replacement with reference level in next step.
        if (treatment_encoding) {
          set(dt, rowSums(old_cols_matrix <= cutoff) > 0, "")
        }
        # Replace occurrences of old column names with corresponding new level names
        set(dt, , new_col, factor(lvls[match(dt[[new_col]], names(lvls))], levels = lvls))
      }

      # Drop old columns
      drop = unlist(lapply(colmaps, names))
      dt[, (drop) := NULL]

      dt
    }
  )
)

mlr_pipeops$add("decode", PipeOpDecode)
