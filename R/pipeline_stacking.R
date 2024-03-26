#' @include mlr_graphs.R

#' @title Create A Graph to Perform Stacking.
#' @name mlr_graphs_stacking
#' @description
#' Create a new [`Graph`] for stacking. A stacked learner uses predictions of
#' several base learners and fits a super learner using these predictions as
#' features in order to predict the outcome.
#'
#' All input arguments are cloned and have no references in common with the returned [`Graph`].
#'
#' @param base_learners `list` of [`Learner`][mlr3::Learner]\cr
#'   A list of base learners.
#' @param super_learner [`Learner`][mlr3::Learner]\cr
#'   The super learner that makes the final prediction based on the base learners.
#' @param method `character(1)`\cr
#'   `"cv"` (default) for building a super learner using cross-validated predictions of the
#'   base learners or `"insample"` for building a super learner using the
#'   predictions of the base learners trained on all training data.
#' @param folds `integer(1)`\cr
#'   Number of cross-validation folds. Only used for `method = "cv"`. Default 3.
#' @param use_features `logical(1)`\cr
#'   Whether the original features should also be passed to the super learner.
#'   Default `TRUE`.
#' @return [`Graph`]
#'
#' @export
#' @examples
#' if (requireNamespace("kknn")) {
#' library(mlr3)
#' library(mlr3learners)
#'
#' base_learners = list(
#'   lrn("classif.rpart", predict_type = "prob"),
#'   lrn("classif.kknn", predict_type = "prob")
#' )
#' super_learner = lrn("classif.log_reg")
#'
#' graph_stack = pipeline_stacking(base_learners, super_learner)
#' graph_learner = as_learner(graph_stack)
#' graph_learner$train(tsk("german_credit"))
#' }
pipeline_stacking = function(base_learners, super_learner, method = "cv", folds = 3, use_features = TRUE) {
  assert_learners(base_learners)
  assert_learner(super_learner)
  assert_choice(method, c("cv", "insample"))
  assert_flag(use_features)

  base_learners_cv = map(base_learners, po,
    .obj = "learner_cv", resampling.method = method, resampling.folds = folds
  )

  if (use_features) base_learners_cv = c(base_learners_cv, po("nop"))

  gunion(base_learners_cv, in_place = TRUE) %>>!%
     po("featureunion", id = "featureunion_stacking") %>>!%
     super_learner
}

mlr_graphs$add("stacking", pipeline_stacking)
