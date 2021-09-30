#' @include mlr_graphs.R

#' @title Robustify a learner
#' @name mlr_graphs_robustify
#' @description
#' Creates a [`Graph`] that can be used to robustify any subsequent learner.
#' Performs the following steps:
#' * Drops empty factor levels using [`PipeOpFixFactors`]
#' * Imputes `numeric` features using [`PipeOpImputeHist`] and [`PipeOpMissInd`]
#' * Imputes `factor` features using [`PipeOpImputeOOR`]
#' * Encodes `factors` using `one-hot-encoding`. Factors with a cardinality > max_cardinality are
#'   collapsed using [`PipeOpCollapseFactors`]
#'
#' The graph is built conservatively, i.e. the function always tries to assure everything works.
#' If a learner is provided, some steps can be left out, i.e. if the learner can deal with
#' factor variables, no encoding is performed.
#'
#' All input arguments are cloned and have no references in common with the returned [`Graph`].
#'
#' @param task [`Task`] \cr
#'   A [`Task`][mlr3::Task] to create a robustifying pipeline for.
#'   Optional, if omitted, the "worst possible" [`Task`][mlr3::Task] is assumed and the full pipeline is created.
#' @param learner [`Learner`][mlr3::Learner] \cr
#'   A learner to create a robustifying pipeline for. Optional, if omitted,
#'   the "worst possible" [`Learner`][mlr3::Learner] is assumed and a more conservative pipeline is built.
#' @param impute_missings `logical(1)` | `NULL` \cr
#'   Should missing values be imputed? Defaults to `NULL`: imputes if the task has
#'   missing values (or factors that are not encoded to numerics) and the learner can not handle them.
#' @param factors_to_numeric `logical(1)` | `NULL` \cr
#'   Should (ordered and unordered) factors be encoded? Defaults to `NULL`: encodes if the task has factors (or character columns that get converted to factor)
#'   and the learner can not handle factors.
#' @param max_cardinality `integer(1)` \cr
#'   Maximum number of factor levels allowed. See above. Default: 1000.
#' @param ordered_action `character(1)`\cr
#'   How to handle `ordered` columns: `"factor"` (default) or `"factor!"`: convert to `factor` columns;
#'   `"numeric"` or `"numeric!"`: convert to `numeric` columns; `"integer"` or `"integer!"`: convert to `integer` columns; `"ignore"` or `"ignore!"`: ignore.
#'   When `task` is given and has no `ordered` columns, or when `learner` is given and can handle `ordered`, then
#'   `"factor"`, `"numeric"` and `"integer"` are treated like `"ignore"`. This means it is necessary to add the exclamation point to
#'   override [`Task`][mlr3::Task] or [`Learner`][mlr3::Learner] properties when given. `"ignore"` and `"ignore!"` therefore
#'   behave completely identically, `"ignore!"` is only present for consistency.\cr
#'   When `ordered` features are converted to `factor`, then they are treated like `factor` features further down in the pipeline,
#'   and are possibly eventually converted to `numeric`s, but in a different way: `factor`s get one-hot encoded, `ordered_action` = `"numeric"`
#'   converts ordered using `as.numeric` to their integer-valued rank.
#' @param character_action `character(1)`\cr
#'   How to handle `character` columns: `"factor"` (default) or `"factor!"`: convert to `factor` columns;
#'   `"matrix"` or `"matrix!"`: Use [`PipeOpTextVectorizer`]. `"ignore"` or `"ignore!"`: ignore.
#'   When `task` is given and has no `character` columns, or when `learner` is given and can handle `character`, then
#'   `"factor"` and `"matrix"` are treated like `"ignore"`. This means it is necessary to add the exclamation point to
#'   override [`Task`][mlr3::Task] or [`Learner`][mlr3::Learner] properties when given. `"ignore"` and `"ignore!"` therefore
#'   behave completely identically, `"ignore!"` is only present for consistency.\cr
#'   When `character` columns are converted to `factor`, then they are treated like `factor` further down in the pipeline,
#'   and are possibly eventually converted to `numeric`s, using one-hot encoding.
#' @param POSIXct_action `character(1)`\cr
#'   How to handle `POSIXct` columns: `"numeric"` (default) or `"numeric!"`: convert to `numeric` columns;
#'   `"datefeatures"` or `"datefeatures!"`: Use [`PipeOpDateFeatures`]. `"ignore"` or `"ignore!"`: ignore.
#'   When `task` is given and has no `POSIXct` columns, or when `learner` is given and can handle `POSIXct`, then
#'   `"numeric"` and `"datefeatures"` are treated like `"ignore"`. This means it is necessary to add the exclamation point to
#'   override [`Task`][mlr3::Task] or [`Learner`][mlr3::Learner] properties when given. `"ignore"` and `"ignore!"` therefore
#'   behave completely identically, `"ignore!"` is only present for consistency.
#' @return [`Graph`]
#' @export
#' @examples
#' \donttest{
#' library(mlr3)
#' lrn = lrn("regr.rpart")
#' task = mlr_tasks$get("boston_housing")
#' gr = pipeline_robustify(task, lrn) %>>% po("learner", lrn)
#' resample(task, GraphLearner$new(gr), rsmp("holdout"))
#' }
pipeline_robustify = function(task = NULL, learner = NULL,
  impute_missings = NULL, factors_to_numeric = NULL, max_cardinality = 1000,
  ordered_action = "factor", character_action = "factor", POSIXct_action = "numeric") {

  # check if `types` are present in `task`, return `default` if `task` is NULL
  has_type_feats = function(types, default = TRUE) {
    if (is.null(task)) default else any(types %in% task$feature_types$type)
  }

  # handle X_action arguments if they don't have a "!": check if task has / learner won't handle X.
  # Also add a "!" if not already present.
  check_action = function(x_action, x_type) {
    if (x_action == "ignore") x_action = "ignore!"
    if (x_action %in% c("matrix", "factor")) {
      # note that learner$properties is NULL when learner is NULL, so is.null(learner) always defaults to properties being absent.
      learner_not_handles_x = x_type %nin% learner$feature_types
      task_has_x = has_type_feats(x_type)
      if (!task_has_x || !learner_not_handles_x) {
        x_action = "ignore!"
      } else {
        x_action = paste0(x_action, "!")
      }
    }
    x_action
  }


  if (!is.null(task)) assert_task(task)

  if (!is.null(learner)) {
    learner = as_learner(learner)
    assert_learner(learner, task = task)
  }
  assert_flag(impute_missings, null.ok = TRUE)
  assert_flag(factors_to_numeric, null.ok = TRUE)
  assert(check_count(max_cardinality, tol = 1e-100), check_true(identical(max_cardinality, Inf)))
  assert_choice(ordered_action, c("factor", "factor!", "numeric", "numeric!", "integer", "integer!", "ignore", "ignore!"))
  assert_choice(character_action, c("factor", "factor!", "matrix", "matrix!", "ignore", "ignore!"))
  assert_choice(POSIXct_action, c("numeric", "numeric!", "datefeatures", "datefeatures!", "ignore", "ignore!"))

  ordered_action = check_action(ordered_action, "ordered")
  character_action = check_action(character_action, "character")
  POSIXct_action = check_action(POSIXct_action, "POSIXct")


  task_has_factors = has_type_feats("factor") || character_action == "factor!" || ordered_action == "factor!"
  # note that learner$properties is NULL when learner is NULL, so is.null(learner) always defaults to properties being absent.
  learner_not_handles_factors = "factor" %nin% learner$feature_types

  if (is.null(factors_to_numeric)) {
    factors_to_numeric = task_has_factors && learner_not_handles_factors
    missind_numeric = learner_not_handles_factors
  } else {
    missind_numeric = factors_to_numeric
  }

  learner_not_handles_missings = "missings" %nin% learner$properties
  if (is.null(impute_missings)) {
    task_has_missings = is.null(task) || any(task$missings())

    impute_missings = task_has_missings && learner_not_handles_missings
  }

  has_numbers = has_type_feats(c("numeric", "integer")) || POSIXct_action != "ignore!" || character_action == "matrix!" || ordered_action %in% c("numeric!", "integer!")
  has_logicals = has_type_feats(c("logical"))
  # assume there are factors when action is 'factor!', even when there are no 'character' or 'ordered' columns
  has_factorials = has_type_feats("factor") || character_action == "factor!" || ordered_action == "factor!" ||
    (has_type_feats("ordered") && ordered_action %nin% c("numeric!", "integer!")) ||
    (has_type_feats("character") && character_action != "matrix!")

  # pos_***-variables contain *lists* of PipeOps, so they can be efficiently chained later. Using the pos()-shorthand in many places, which returns a list.

  pos_removeconstants_1 = pos("removeconstants", id = "removeconstants_prerobustify", na_ignore = FALSE)

  pos_character = switch(character_action,
    `factor!` = pos("colapply", id = "char_to_fct", affect_columns = selector_type("character"), applicator = function(x) as.factor(x)),
    `matrix!` = pos("textvectorizer"),
    `ignore!` = NULL,
    stopf("unexpected value of character_action: %s", character_action)
  )

  pos_POSIXct = switch(POSIXct_action,
    `numeric!` = pos("colapply", id = "POSIXct_to_dbl", affect_columns = selector_type("POSIXct"), applicator = function(x) as.numeric(x)),
    `datefeatures!` = pos("datefeatures"),
    `ignore!` = NULL,
    stopf("unexpected value of POSIXct_action: %s", POSIXct_action)
  )

  pos_ordered = switch(ordered_action,
    `numeric!` = pos("colapply", id = "ord_to_dbl", affect_columns = selector_type("ordered"), applicator = function(x) as.numeric(x)),
    `integer!` = pos("colapply", id = "ord_to_int", affect_columns = selector_type("ordered"), applicator = function(x) as.integer(x)),
    `factor!` = pos("colapply", id = "ord_to_fct", affect_columns = selector_type("ordered"), applicator = function(x) as.factor(x)),
    `ignore!` = NULL,
    stopf("unexpected value of ordered_action: %s", ordered_action)
  )

  imputing = if (has_numbers) po("imputehist") %>>% if (has_logicals) po("imputesample", affect_columns = selector_type("logical"))
  if (is.null(imputing)) imputing = po("nop", id = "missind_bypass")
  pos_impute_1 = if (impute_missings) list(
    if (has_numbers || has_logicals) gunion(list(
      imputing,
      po("missind", affect_columns = selector_type(c("numeric", "integer", "logical"), type = if (missind_numeric) "numeric" else "factor"))
    )),
    if (has_factorials) po("imputeoor")
  )

  pos_fixfactors = if (has_factorials) list(po("fixfactors"))

  pos_impute_2 = if (impute_missings || (task_has_factors && learner_not_handles_missings)) {
    pos("imputesample", affect_columns = selector_type(c("factor", if (ordered_action == "ignore!") "ordered")))
  }

  pos_cardinality = if (is.finite(max_cardinality) && (is.null(task) || length(selector_cardinality_greater_than(max_cardinality)(task)))) {
    pos("collapsefactors", target_level_count = max_cardinality)
  }

  pos_to_numeric = if (factors_to_numeric) pos("encode")
  pos_removeconstants_2 = pos("removeconstants", id = "removeconstants_postrobustify")


  chain_graphs(c(
    pos_removeconstants_1,
    pos_character,
    pos_POSIXct,
    pos_ordered,
    pos_impute_1,
    pos_fixfactors,
    pos_impute_2,
    pos_cardinality,
    pos_to_numeric,
    pos_removeconstants_2
  ), in_place = TRUE)
}

mlr_graphs$add("robustify", pipeline_robustify)
