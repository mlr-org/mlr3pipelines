#' @title Collapse Factors
#'
#' @usage NULL
#' @name mlr_pipeops_collapsefactors
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Collapses factors of type `factor`, `ordered`: Collapses the rarest factors in the
#' training samples, until `target_level_count` levels remain. Levels that have prevalence above `no_collapse_above_prevalence`
#'  are retained, however. For `factor` variables, these are collapsed to the next larger level, for `ordered` variables,
#' rare variables are collapsed to the neighbouring class, whichever has fewer samples.
#'
#' Levels not seen during training are not touched during prediction; Therefore it is useful to combine this with the
#' [`PipeOpFixFactors`].
#'
#' @section Construction:
#' ```
#' PipeOpCollapseFactors$new(id = "collapsefactors", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"collapsefactors"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with rare affected `factor` and `ordered` feature levels collapsed.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `collapse_map` :: named `list` of named `list` of `character`\cr
#'   List of factor level maps. For each factor, `collapse_map` contains a named `list` that indicates what levels
#'   of the input task get mapped to what levels of the output task. If `collapse_map` has an entry `feat_1` with
#'   an entry `a = c("x", "y")`, it means that levels `"x"` and `"y"` get collapsed to level `"a"` in feature `"feat_1"`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `no_collapse_above_prevalence`  :: `numeric(1)` \cr
#'   Fraction of samples below which factor levels get collapsed. Default is 1, which causes all levels
#'   to be collapsed until `target_level_count` remain.
#' * `target_level_count`  :: `integer(1)` \cr
#'   Number of levels to retain. Default is 2.
#'
#' @section Internals:
#' Makes use of the fact that `levels(fact_var) = list(target1 = c("source1", "source2"), target2 = "source2")` causes
#' renaming of level `"source1"` and `"source2"` both to `"target1"`, and also `"source2"` to `"target2"`.
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
PipeOpCollapseFactors = R6Class("PipeOpCollapseFactors",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "collapsefactors", param_vals = list()) {
      ps = ps(
        no_collapse_above_prevalence = p_dbl(0, 1, tags = c("train", "predict")),
        target_level_count = p_int(2, tags = c("train", "predict"))
      )
      ps$values = list(no_collapse_above_prevalence = 1, target_level_count = 2)
      super$initialize(id, param_set = ps, param_vals = param_vals, feature_types = c("factor", "ordered"))
    }
  ),
  private = list(

    .get_state = function(task) {
      # get the levels of the training task
      dt = task$data(cols = private$.select_cols(task))

      keep_fraction = self$param_set$values$no_collapse_above_prevalence
      target_count = self$param_set$values$target_level_count

      collapse_map = sapply(dt, function(d) {
        if (all(is.na(d))) {
          return(NULL)
        }
        if (length(levels(d)) <= target_count) {
          return(NULL)
        }
        dtable = table(d)
        fractions = sort(dtable, decreasing = TRUE) / sum(!is.na(d))
        keep_fraction = names(fractions)[fractions >= keep_fraction]
        keep_count = names(fractions)[seq_len(target_count)]  # at this point we know there are more levels than target_count
        keep = union(keep_fraction, keep_count)
        dont_keep = setdiff(levels(d), keep)
        if (is.ordered(d)) {
          cmap = stats::setNames(as.list(levels(d)), levels(d))
          for (eliminating in dont_keep) {
            position = match(eliminating, names(cmap))
            if (position == 1) {
              cmap[[2]] = c(cmap[[2]], eliminating)
            } else if (position == length(cmap) || dtable[position - 1] < dtable[position + 1]) {
              cmap[[position - 1]] = c(cmap[[position - 1]], eliminating)
            } else {
              cmap[[position + 1]] = c(cmap[[position + 1]], eliminating)
            }
            dtable = dtable[-position]
            cmap[[position]] = NULL
          }
        } else {
          cmap = stats::setNames(as.list(keep), keep)
          lowest_kept = keep[length(keep)]
          cmap[[lowest_kept]] = c(lowest_kept, dont_keep)
        }
        cmap
      }, simplify = FALSE)

      list(collapse_map = discard(collapse_map, is.null))
    },

    .transform = function(task) {
      cmaplist = self$state$collapse_map
      dt = task$data(cols = names(cmaplist))

      for (n in names(cmaplist)) {
        # don't touch unseen factor levels
        new_lvls = setdiff(levels(dt[[n]]), unlist(cmaplist[[n]], use.names = FALSE))
        all_lvls = c(cmaplist[[n]], stats::setNames(as.list(new_lvls), new_lvls))
        levels(dt[[n]]) = c(
          all_lvls[intersect(levels(dt[[n]]), names(all_lvls))],  # keep all levels in their order, if they were present before
          all_lvls[setdiff(names(all_lvls), levels(dt[[n]]))]     # levels that are missing now get sorted to the back.
        )
      }
      task$select(setdiff(task$feature_names, names(cmaplist)))$cbind(dt)
    }
  )
)

mlr_pipeops$add("collapsefactors", PipeOpCollapseFactors)
