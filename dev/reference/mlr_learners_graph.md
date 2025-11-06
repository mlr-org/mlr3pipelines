# Encapsulate a Graph as a Learner

A [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) that
encapsulates a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) to
be used in [mlr3](https://mlr3.mlr-org.com/reference/mlr3-package.html)
resampling and benchmarks.

The Graph must return a single
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html) on
its `$predict()` call. The result of the `$train()` call is discarded,
only the internal state changes during training are used.

The `predict_type` of a `GraphLearner` can be obtained or set via it's
`predict_type` active binding. Setting a new predict type will try to
set the `predict_type` in all relevant
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md) /
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)
encapsulated within the
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).
Similarly, the predict_type of a Graph will always be the smallest
denominator in the
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).

A `GraphLearner` is always constructed in an untrained state. When the
`graph` argument has a non-`NULL` `$state`, it is ignored.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html).

## Construction

    GraphLearner$new(graph, id = NULL, param_vals = list(), task_type = NULL, predict_type = NULL)

- `graph` ::
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) \|
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)  
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) to
  wrap. Can be a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
  which is automatically converted to a
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).
  This argument is usually cloned, unless `clone_graph` is `FALSE`; to
  access the
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  inside `GraphLearner` by-reference, use `$graph`.  

- `id` :: `character(1)` Identifier of the resulting
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html).

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings . Default [`list()`](https://rdrr.io/r/base/list.html).

- `task_type` :: `character(1)`  
  What `task_type` the `GraphLearner` should have; usually automatically
  inferred for
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)s
  that are simple enough.

- `predict_type` :: `character(1)`  
  What `predict_type` the `GraphLearner` should have; usually
  automatically inferred for
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)s
  that are simple enough.

- `clone_graph` :: `logical(1)`  
  Whether to clone `graph` upon construction. Unintentionally changing
  `graph` by reference can lead to unexpected behaviour, so `TRUE`
  (default) is recommended. In particular, note that the `$state` of
  `$graph` is set to `NULL` by reference on construction of
  `GraphLearner`, during `$train()`, and during `$predict()` when
  `clone_graph` is `FALSE`.

## Fields

Fields inherited from
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html), as well
as:

- `graph` ::
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)  
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  that is being wrapped. This field contains the prototype of the
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  that is being trained, but does *not* contain the model. Use
  `graph_model` to access the trained
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  after `$train()`. Read-only.

- `graph_model` ::
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)  
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  that is being wrapped. This
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  contains a trained state after `$train()`. Read-only.

- `pipeops` :: named `list` of
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)  
  Contains all
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  in the underlying
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md),
  named by the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  `$id`s. Shortcut for `$graph_model$pipeops`. See
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  for details.

- `edges` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns `src_id` (`character`), `src_channel` (`character`),
  `dst_id` (`character`), `dst_channel` (`character`)  
  Table of connections between the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  in the underlying
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).
  Shortcut for `$graph$edges`. See
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  for details.

- `param_set` ::
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)  
  Parameters of the underlying
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).
  Shortcut for `$graph$param_set`. See
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  for details.

- `pipeops_param_set` :: named
  [`list()`](https://rdrr.io/r/base/list.html)  
  Named list containing the
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)s of
  all
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  in the
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).
  See there for details.

- `pipeops_param_set_values` :: named
  [`list()`](https://rdrr.io/r/base/list.html)  
  Named list containing the set parameter values of all
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  in the
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).
  See there for details.

- `internal_tuned_values` :: named
  [`list()`](https://rdrr.io/r/base/list.html) or `NULL`  
  The internal tuned parameter values collected from all
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.
  `NULL` is returned if the learner is not trained or none of the
  wrapped learners supports internal tuning.

- `internal_valid_scores` :: named
  [`list()`](https://rdrr.io/r/base/list.html) or `NULL`  
  The internal validation scores as retrieved from the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.
  The names are prefixed with the respective IDs of the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.
  `NULL` is returned if the learner is not trained or none of the
  wrapped learners supports internal validation.

- `validate` :: `numeric(1)`, `"predefined"`, `"test"` or `NULL`  
  How to construct the validation data. This also has to be configured
  for the individual
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  such as `PipeOpLearner`, see
  [`set_validate.GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/set_validate.GraphLearner.md).
  For more details on the possible values, see
  [`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html).

- `marshaled` :: `logical(1)`  
  Whether the learner is marshaled.

- `impute_selected_features` :: `logical(1)`  
  Whether to heuristically determine `$selected_features()` as all
  `$selected_features()` of all "base learner" Learners, even if they do
  not have the `"selected_features"` property / do not implement
  `$selected_features()`. If `impute_selected_features` is `TRUE` and
  the base learners do not implement `$selected_features()`, the
  `GraphLearner`'s `$selected_features()` method will return all
  features seen by the base learners. This is useful in cases where
  feature selection is performed inside the `Graph`: The
  `$selected_features()` will then be the set of features that were
  selected by the `Graph`. If `impute_selected_features` is `FALSE`, the
  `$selected_features()` method will throw an error if
  `$selected_features()` is not implemented by the base learners.  
  This is a heuristic and may report more features than actually used by
  the base learners, in cases where the base learners do not implement
  `$selected_features()`. The default is `FALSE`.

## Methods

Methods inherited from
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html), as well
as:

- `ids(sorted = FALSE)`  
  (`logical(1)`) -\> `character`  
  Get IDs of all
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.
  This is in order that
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  were added if `sorted` is `FALSE`, and topologically sorted if
  `sorted` is `TRUE`.

- `plot(html = FALSE, horizontal = FALSE)`  
  (`logical(1)`, `logical(1)`) -\> `NULL`  
  Plot the
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md),
  using either the [igraph](https://CRAN.R-project.org/package=igraph)
  package (for `html = FALSE`, default) or the `visNetwork` package for
  `html = TRUE` producing a
  [`htmlWidget`](https://rdrr.io/pkg/htmlwidgets/man/htmlwidgets-package.html).
  The
  [`htmlWidget`](https://rdrr.io/pkg/htmlwidgets/man/htmlwidgets-package.html)
  can be rescaled using
  [`visOptions`](https://rdrr.io/pkg/visNetwork/man/visOptions.html).
  For `html = FALSE`, the orientation of the plotted graph can be
  controlled through `horizontal`.

- `marshal`  
  (any) -\> `self`  
  Marshal the model.

- `unmarshal`  
  (any) -\> `self`  
  Unmarshal the model.

- `base_learner(recursive = Inf, return_po = FALSE, return_all = FALSE, resolve_branching = TRUE)`  
  (`numeric(1)`, `logical(1)`, `logical(1)`, `character(1)`) -\>
  `Learner` \|
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  \| `list` of `Learner` \| `list` of
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)  
  Return the base learner of the `GraphLearner`. If `recursive` is 0,
  the `GraphLearner` itself is returned. Otherwise, the
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) is
  traversed backwards to find the first `PipeOp` containing a
  `$learner_model` field. If `recursive` is 1, that `$learner_model` (or
  containing `PipeOp`, if `return_po` is `TRUE`) is returned. If
  `recursive` is greater than 1, the discovered base learner's
  `base_learner()` method is called with `recursive - 1`. `recursive`
  must be set to 1 if `return_po` is TRUE, and must be set to at most 1
  if `return_all` is `TRUE`.  
  If `return_po` is `TRUE`, the container-`PipeOp` is returned instead
  of the `Learner`. This will typically be a
  [`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner.md)
  or a
  [`PipeOpLearnerCV`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_cv.md).  
  If `return_all` is `TRUE`, a `list` of `Learner`s or `PipeOp`s is
  returned. If `return_po` is `FALSE`, this list may contain
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/dev/reference/Multiplicity.md)
  objects, which are not unwrapped. If `return_all` is `FALSE` and there
  are multiple possible base learners, an error is thrown. This may also
  happen if only a single
  [`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner.md)
  is present that was trained with a
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/dev/reference/Multiplicity.md).  
  If `resolve_branching` is `TRUE`, and when a
  [`PipeOpUnbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md)
  is encountered, the corresponding
  [`PipeOpBranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_branch.md)
  is searched, and its hyperparameter configuration is used to select
  the base learner. There may be multiple corresponding
  [`PipeOpBranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_branch.md)s,
  which are all considered. If `resolve_branching` is `FALSE`,
  [`PipeOpUnbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md)
  is treated as any other `PipeOp` with multiple inputs; all possible
  branch paths are considered equally.

The following standard extractors as defined by the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) class are
available. Note that these typically only extract information from the
`$base_learner()`. This works well for simple
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)s
that do not modify features too much, but may give unexpected results
for `Graph`s that add new features or move information between features.

As an example, consider a feature `A` with missing values, and a feature
`B` that is used for imputation, using a
[`po("imputelearner")`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputelearner.md).
In a case where the following
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) performs
embedded feature selection and only selects feature `A`, the
`selected_features()` method could return only feature `A`, and
`$importance()` may even report 0 for feature `B`. This would not be
entirely accurate when considering the entire `GraphLearner`, as feature
`B` is used for imputation and would therefore have an impact on
predictions. The following should therefore only be used if the
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) is
known to not have an impact on the relevant properties.

- `importance()`  
  () -\> `numeric`  
  The `$importance()` returned by the base learner, if it has the
  `"importance` property. Throws an error otherwise.

- `selected_features()`  
  () -\> `character`  
  The `$selected_features()` returned by the base learner, if it has the
  `"selected_features` property. If the base learner does not have the
  `"selected_features"` property and `impute_selected_features` is
  `TRUE`, all features seen by the base learners are returned. Throws an
  error otherwise.

- `oob_error()`  
  () -\> `numeric(1)`  
  The `$oob_error()` returned by the base learner, if it has the
  `"oob_error` property. Throws an error otherwise.

- `loglik()`  
  () -\> `numeric(1)`  
  The `$loglik()` returned by the base learner, if it has the `"loglik`
  property. Throws an error otherwise.

## Internals

[`as_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_graph.md)
is called on the `graph` argument, so it can technically also be a
`list` of things, which is automatically converted to a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) via
[`gunion()`](https://mlr3pipelines.mlr-org.com/dev/reference/gunion.md);
however, this will usually not result in a valid
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) that
can work as a
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html). `graph`
can furthermore be a
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html), which is
then automatically wrapped in a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md),
which is then again wrapped in a `GraphLearner` object; this usually
only adds overhead and is not recommended.

## See also

Other Learners:
[`mlr_learners_avg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_avg.md)

## Examples

``` r
library("mlr3")

graph = po("pca") %>>% lrn("classif.rpart")

lr = GraphLearner$new(graph)
lr = as_learner(graph)  # equivalent

lr$train(tsk("iris"))

lr$graph$state  # untrained version!
#> $pca
#> NULL
#> 
#> $classif.rpart
#> NULL
#> 
# The following is therefore NULL:
lr$graph$pipeops$classif.rpart$learner_model$model
#> NULL

# To access the trained model from the PipeOpLearner's Learner, use:
lr$graph_model$pipeops$classif.rpart$learner_model$model
#> n= 150 
#> 
#> node), split, n, loss, yval, (yprob)
#>       * denotes terminal node
#> 
#> 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)  
#>   2) PC1< -1.553145 50   0 setosa (1.00000000 0.00000000 0.00000000) *
#>   3) PC1>=-1.553145 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
#>     6) PC1< 1.142805 44   1 versicolor (0.00000000 0.97727273 0.02272727) *
#>     7) PC1>=1.142805 56   7 virginica (0.00000000 0.12500000 0.87500000) *

# Feature importance (of principal components):
lr$graph_model$pipeops$classif.rpart$learner_model$importance()
#>       PC1       PC2       PC3       PC4 
#> 85.795455 18.016529  5.694731  3.254132 
```
