#' @title Reverse Factor Encoding
#'
#' @usage NULL
#' @name mlr_pipeops_decode
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Reverses one-hot or treatment encoding of columns. It collapses multiple `numeric` or `integer` columns into one `factor`
#' column based on a pre-specified grouping pattern of column names.
#'
#' May be applied to multiple groups of columns, grouped by matching a common naming pattern. The grouping pattern is
#' extracted to form the name of the newly derived `factor` column, and levels are constructed from the previous column
#' names, with parts matching the grouping pattern removed (see examples). The level per row of the new factor column is generally
#' determined as the name of the column with the maximum value in the group.
#'
#' @section Construction:
#' ```
#' PipeOpEncode$new(id = "decode", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"decode"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with encoding columns collapsed into new decoded columns.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `colmaps` :: named `list`\cr
#'   Named list of named character vectors. Each element is named according to the new column name extracted by
#'   `group_pattern`. Each vector contains the level names for the new factor column that should be created, named by
#'   the corresponding old column name. If `treatment_encoding` is `TRUE`, then each vector also contains `ref_name` as the
#'   reference class with an empty string as name.
#' * `treatment_encoding` :: `logical(1)`\cr
#'   Value of `treatment_encoding` hyperparameter.
#' * `cutoff` :: `numeric(1)`\cr
#'   Value of `treatment_encoding` hyperparameter, or `0` if that is not given.
#' * `ties_method` :: `character(1)`\cr
#'   Value of `ties_method` hyperparameter.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `group_pattern` :: `character(1)`\cr
#'   A regular expression to be applied to column names. Should contain a capturing group for the new
#'   column name, and match everything that should not be interpreted as the new factor levels (which are constructed as
#'   the difference between column names and what `group_pattern` matches).
#'   If set to `""`, all columns matching the `group_pattern` are collapsed into one factor column called
#'   `pipeop.decoded`. Use [`PipeOpRenameColumns`] to rename this column.
#'   Initialized to `"^([^.]+)\\."`, which would extract everything up to the first dot as the new column name and
#'   construct new levels as everything after the first dot.
#' * `treatment_encoding` :: `logical(1)`\cr
#'   If `TRUE`, treatment encoding is assumed instead of one-hot encoding. Initialized to `FALSE`.
#' * `treatment_cutoff` :: `numeric(1)`\cr
#'   If `treatment_encoding` is `TRUE`, specifies a cutoff value for identifying the reference level. The reference level
#'   is set to `ref_name` in rows where the value is less than or equal to a specified cutoff value (e.g., `0`) in all
#'   columns in that group. Default is `0`.
#' * `ref_name` :: `character(1)`\cr
#'   If `treatment_encoding` is `TRUE`, specifies the name for reference levels. Default is `"ref"`.
#' * `ties_method` :: `character(1)`\cr
#'   Method for resolving ties if multiple columns have the same value. Specifies the value from which of the columns
#'   with the same value is to be picked. Options are `"first"`, `"last"`, or `"random"`. Initialized to `"random"`.
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
#' # Reverse one-hot encoding
#' df = data.frame(
#'   target = runif(4),
#'   x.1 = rep(c(1, 0), 2),
#'   x.2 = rep(c(0, 1), 2),
#'   y.1 = rep(c(1, 0), 2),
#'   y.2 = rep(c(0, 1), 2),
#'   a = runif(4)
#' )
#' task_one_hot = TaskRegr$new(id = "example", backend = df, target = "target")
#'
#' pop = po("decode")
#'
#' train_out = pop$train(list(task_one_hot))[[1]]
#' # x.1 and x.2 are collapsed into x, same for y; a is ignored.
#' train_out$data()
#'
#' # Reverse treatment encoding from PipeOpEncode
#' df = data.frame(
#'   target = runif(6),
#'   fct = factor(rep(c("a", "b", "c"), 2))
#' )
#' task = TaskRegr$new(id = "example", backend = df, target = "target")
#'
#' po_enc = po("encode", method = "treatment")
#' task_encoded = po_enc$train(list(task))[[1]]
#' task_encoded$data()
#'
#' po_dec = po("decode", treatment_encoding = TRUE)
#' task_decoded = pop$train(list(task))[[1]]
#' # x.1 and x.2 are collapsed into x. All rows where all values
#' # are smaller or equal to 0, the level is set to the reference level.
#' task_decoded$data()
#'
#' # Different group_pattern
#' df = data.frame(
#'   target = runif(4),
#'   x_1 = rep(c(1, 0), 2),
#'   x_2 = rep(c(0, 1), 2),
#'   y_1 = rep(c(2, 0), 2),
#'   y_2 = rep(c(0, 1), 2)
#' )
#' task = TaskRegr$new(id = "example", backend = df, target = "target")
#'
#' # Grouped by first underscore
#' pop = po("decode", group_pattern = "^([^_]+)\\_")
#' train_out = pop$train(list(task))[[1]]
#' # x_1 and x_2 are collapsed into x, same for y
#' train_out$data()
#'
#' # Empty string to collapse all matches into one factor column.
#' pop$param_set$set_values(group_pattern = "")
#' train_out = pop$train(list(task))[[1]]
#' # All columns are combined into a single column.
#' # The level for each row is determined by the column with the largest value in that row.
#' # By default, ties are resolved randomly.
#' train_out$data()
#'
PipeOpDecode = R6Class("PipeOpDecode",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "decode", param_vals = list()) {
      ps = ps(
        group_pattern = p_uty(custom_check = check_string, tags = c("train", "required")),
        treatment_encoding = p_lgl(tags = c("train", "required")),
        treatment_cutoff = p_dbl(default = 0, tags = "train", depends = quote(treatment_encoding == TRUE)),
        ref_name = p_uty(default = "ref", custom_check = crate(function(x) check_string(x, min.chars = 1)), tags = "train", depends = quote(treatment_encoding == TRUE)),
        ties_method = p_fct(c("first", "last", "random"), tags = c("train", "required"))
      )
      ps$values = list(treatment_encoding = FALSE, group_pattern = "^([^.]+)\\.", ties_method = "random")
      super$initialize(id, param_set = ps, param_vals = param_vals, tags = "encode", feature_types = c("integer", "numeric"))
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      pv = self$param_set$values
      ref_name = pv$ref_name %??% "ref"
      cols = colnames(dt)

      # If pattern == "", all columns are collapsed into one column.
      # Note, that column "pipeop.decoded" gets overwritten if it already exists.
      if (pv$group_pattern == "") {
        cmap = list(pipeop.decoded = set_names(cols, cols))

        if (pv$treatment_encoding) {
          # Append reference level with empty name (i.e. "")
          cmap[["pipeop.decoded"]][[length(cols) + 1]] = get_ref_name(ref_name, cmap[["pipeop.decoded"]])
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
        stopf("Pattern %s matches column name %s, but nothing was captured. Make sure \"group_pattern\" contains a capturing group or is an empty string to collapse all colunns into one factor.",
              str_collapse(pv$group_pattern, quote = '"'),
              str_collapse(cols[lengths(matches) < 2], quote = '"'))
      }

      fcts = map_chr(matches, 2)
      # Error, if no group could be extracted for an entry in col so that we could not create a column name from it.
      if (any(nchar(fcts) == 0)) {
        stopf("Pattern %s with column(s) %s would produce empty string as decoded column name(s). Try using a different pattern.",
              str_collapse(pv$group_pattern, quote = '"'),
              str_collapse(cols[nchar(fcts) == 0], quote = '"'))
      }

      # Create mapping of old column names and derived levels to new column names
      cmap = split(lvls, fcts)

      if (pv$treatment_encoding) {
        # Append reference level with empty name (i.e. "") to all list entries
        for (i in seq_along(cmap)) {
          cmap[[i]][[length(cmap[[i]]) + 1]] = get_ref_name(ref_name, cmap[[i]])
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
      if (!length(colmaps)) {
        return(dt)  # Early exit if no mapping is required
      }
      cutoff = self$state$cutoff
      ties_method = self$state$ties_method
      treatment_encoding = self$state$treatment_encoding

      dt_collapsed = data.table()
      lapply(names(colmaps), function(new_col) {
        lvls = colmaps[[new_col]]
        # Get old column names and, ff existent, remove empty string element (for subsetting dt_collapse in next step)
        old_cols = discard(names(lvls), names(lvls) == "")
        # Create matrix from subset of dt with column names given by old_cols
        old_cols_matrix = as.matrix(dt[, old_cols, with = FALSE])
        # Populate new column with name of column with maximal value per row
        set(dt_collapsed, , new_col, old_cols[apply(old_cols_matrix, 1, which_max, ties_method = ties_method)])
        if (treatment_encoding) {
          # If all values in old_cols_matrix are smaller than or equal to the cutoff, replace with empty string
          # This leads to replacement with reference level in next step.
          set(dt_collapsed, which(rowSums(old_cols_matrix > cutoff) == 0), new_col, "")
        }
        # Replace occurrences of old column names with corresponding new level names
        set(dt_collapsed, , new_col, factor(lvls[match(dt_collapsed[[new_col]], names(lvls))], levels = lvls))
      })

      # Drop old columns (if existent, remove empty string elements, to allow subsetting)
      drop = unlist(lapply(colmaps, names))
      drop = discard(drop, drop == "")
      dt[, (drop) := NULL]

      # cbind new columns
      do.call(cbind, list(dt, dt_collapsed))
    }
  )
)

mlr_pipeops$add("decode", PipeOpDecode)

# Ensures the reference level name is unique for a given factor by appending an incrementing suffix if needed.
# * ref_name: name of the reference level by default
# * lvl_names: all other level names for a given factor
get_ref_name = function(ref_name, lvl_names) {
  new_ref_name = ref_name
  counter = 1
  while (new_ref_name %in% lvl_names) {
    new_ref_name = paste0(ref_name, ".", counter)
    counter = counter + 1
  }
  new_ref_name
}
