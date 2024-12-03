#' @title Reverse Encoding
#'
#' @usage NULL
#' @name mlr_pipeops_decode
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Reverses one-hot or treatment encoding of columns. It collapses multiple `numeric` or `integer` columns into one `factor`
#' column based on a specified grouping pattern of column names.
#'
#' May be applied to multiple groups of columns, grouped by matching a common naming pattern. The grouping pattern is
#' extracted to form the name of the newly derived `factor` column, and levels are constructed from the previous column
#' names, with parts matching the grouping pattern removed. The level per row of the new factor column is generally
#' determined as the name of the column with the maximum value in the group.
#' For example, columns `x.1` and `x.2` might be collapsed into a new factor column `x` with levels `1` and `2`, while
#' columns `y.1` and `y.2` might be interpreted as a separate group and collapsed into a new column `y`.
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
#'   the corresponding old column name. If `treatment_encoding` is `TRUE`, then each vector also contains `"ref"` as the
#'   reference class with an empty string as name.
#' * `treatment_encoding` :: `logical(1)`\cr
#'   Indicates whether treatment encoding (`TRUE`) or one-hot encoding (`FALSE`) is assumed.
#' * `cutoff` :: `numeric(1)`\cr
#'   The cutoff value for identifying the reference level in case of treatment encoding.
#' * `ties_method` :: `character(1)`\cr
#'   Method for resolving ties when multiple columns have the same value. Options include `"first"`, `"last"`, or `"random"`.
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
#'   is set to `"ref"` in rows where the value is less than or equal to a specified cutoff value (e.g., `0`) in all
#'   columns in that group To change the name of the reference level, use [`PipeOp???`] (Mutate? ColApply?).
#'   Initialized to `0`.
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
#' # Create example task with one-hot encoding
#' df = data.frame(
#'   target = runif(10),
#'   x.1 = rep(c(1, 0), 5),
#'   x.2 = rep(c(0, 1), 5),
#'   y.1 = rep(c(1, 0), 5),
#'   y.2 = rep(c(0, 1), 5),
#'   a = runif(10)
#' )
#' task = TaskRegr$new(id = "example", backend = df, target = "target")
#'
#' pop = po("decode")
#'
#' # Training
#' train_out = pop$train(list(task))[[1]]
#' # x.1 and x.2 are collapsed into x, same for y; a is ignored.
#' train_out$data()
#'
#' # Create example task with treatment encoding
#' df = data.frame(
#'   target = runif(15),
#'   x.1 = rep(c(1, 0, 0), 5),
#'   x.2 = rep(c(0, 1, 0), 5)
#' )
#' task = TaskRegr$new(id = "example", backend = df, target = "target")
#'
#' pop = po("decode")
#' pop$param_set$set_values(treatment_encoding = TRUE)
#'
#' # Training
#' train_out = pop$train(list(task))[[1]]
#' # x.1 and x.2 are collapsed into x; in rows where all values
#' # are smaller or equal to 0, the reference level is set
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
        cmap = list(pipeop.decoded = set_names(cols, cols))
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

      # Error, if no group could be extracted for an entry in col so that we could not create a column name from it.
      if (any(nchar(fcts) == 0)) {
        stopf("Pattern %s with column(s) %s would produce empty string as decoded column name(s). Try using a different pattern.",
              str_collapse(pv$group_pattern, quote = '"'),
              str_collapse(cols[nchar(fcts) == 0], quote = '"'))
      }

      # Create mapping of old column names and derived levels to new column names
      cmap = split(lvls, fcts)
      if (pv$treatment_encoding) {
        # Append ref_name with empty name (i.e. "") to all list entries
        for (i in seq_along(cmap)) {
          cmap[[i]][[length(cmap[[i]]) + 1]] = ref_name
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
        # If existent, remove empty string element (for subsetting dt in next step)
        old_cols = discard(names(lvls), names(lvls) == "")

        # Create matrix from subset of dt with columns old_cols
        old_cols_matrix = as.matrix(dt[, old_cols, with = FALSE])
        # Populate new column with name of column with maximal value per row
        set(dt, , new_col, old_cols[apply(old_cols_matrix, 1, which_max, ties_method = ties_method)])
        # If all values in old_cols_matrix are smaller than or equal to the cutoff, replace with empty string
        # This leads to replacement with reference level in next step.
        if (treatment_encoding) {
          set(dt, which(rowSums(old_cols_matrix > cutoff) == 0), new_col, "")
        }
        # Replace occurrences of old column names with corresponding new level names
        set(dt, , new_col, factor(lvls[match(dt[[new_col]], names(lvls))], levels = lvls))
      }

      # Drop old columns
      drop = unlist(lapply(colmaps, names))
      # If existent, remove empty string elements
      drop = discard(drop, drop == "")
      dt[, (drop) := NULL]

      dt
    }
  )
)

mlr_pipeops$add("decode", PipeOpDecode)
