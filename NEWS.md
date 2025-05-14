# mlr3pipelines 0.7.2-9000

* Added missing error for predicting with untrained `PipeOp`s / `Graph`s.
* Fix: Corrected typo in the hyperparameter name `use_parallel` of `PipeOpVtreat`.

# mlr3pipelines 0.7.2

* New parameter `no_collapse_above_absolute` for `PipeOpCollapseFactors` / `po("collapse_factors")`.
* Fix: `PipeOpCollapseFactors` now correctly collapses levels of ordered factors.
* Fix: `LearnerClassifAvg` and `LearnerRegrAvg` hyperparameters get the `"required"` tag.
* New parameter `use_groups` (default `TRUE`) for `PipeOpSubsampling` to respect grouping (changed default behaviour for grouped data)
* New parameter `new_role_direct` for `PipeOpColRoles` / `po("colroles")` to change column roles by role instead of by column.
* Dictionary sugar functions `po()` / `pos()` / `ppl()` / `ppls()` now make suggestions for entries in both `mlr_pipeops` as well as `mlr_graphs` when an object by the given name could not be found in the respective dictionary.
* New PipeOp `PipeOpDecode` / `po("decode")` to reverse one-hot or treatment encoding.
* Fix: Columns that are `feature` and something else no longer lose the other column role during training or predicting of `PipeOp`s inheriting from `PipeOpTaskPreproc`.
* Fix: Made tests for `PipeOpBLSmote` deterministic.
* Fix: Corrected hash calculation for `PipeOpFilter`.
* New PipeOps `PipeOpEncodePLQuantiles` and `PipeOpEncodePLTree` that implement piecewise linear encoding with two different binning methods.
* Compatibility with new `R6` release.
* Docs: Performed cleanup and standardization.
* Docs: Performed cleanup of reference index page on website.
* Docs: Fixed parsing of examples on website for `PipeOpNMF` and `PipeOpLearnerPICVPlus`.
* Fix: `PipeOpTargetMutate` and `PipeOpTargetTrafoScaleRange` no longer drop unseen factor levels of features or targets during train and predict.
* Simplified parameter checks and added internal type checking for `PipeOpTargetMutate`.

# mlr3pipelines 0.7.1

* Compatibility fix for upcoming `mlr3`
* New down-sampling PipeOps for inbalanced data: `PipeOpTomek` / `po("tomek")` and `PipeOpNearmiss` / `po("nearmiss")`
* New PipeOp `PipeOpLearnerPICVPlus / po("learner_pi_cvplus")`
* New PipeOp for Quantile Regression `PipeOpLearnerQuantiles` / `po(learner_quantiles)`
* `GraphLearner` has new active bindings/methods as shortcuts for active bindings/methods of the underlying `Graph`:
`$pipeops`, `$edges`, `$pipeops_param_set`, and `$pipeops_param_set_values` as well as `$ids()` and `$plot()`.

# mlr3pipelines 0.7.0

* New PipeOp `PipeOpRowApply` / `po("rowapply")`
* Empty `PipeOp` IDs now explicitly forbidden.
* Bugfix: `Graph$tran()` / `Graph$predict()` with `single_input = FALSE` now correctly handles `PipeOp`s with multiple inputs.
* `GraphLearner$base_learner()` now works with `PipeOpBranch`, and is generally more robust.
* `GraphLearner` now supports `$importance`, `$selected_features()`, `$oob_error()`, and `$loglik()`.
  These are computed from the underlying `Learner`.
* `GraphLearner$impute_selected_features` option added:
  `$selected_features()` is reported even if the underlying base learner does not report it; in this case, the full feature set as seen by that learner is returned.
* `GraphLearner$predict_type` handling more robust now.
* `PipeOpThreshold` and `PipeOpTuneThreshold` now have the `$predict_type` `"prob"`.
  They can be set to `"response"`, in which case the probability predictions are discarded, potentially saving memory.
* Bugfix for handling multiplicities in PipeOps with vararg channels.
* Bugfix: `PipeOpImputeOOR` now retains the `.MISSING` level in factors during prediction that were imputed during training, but had no missing values during prediction.
* `as_data_table(po())` now works even when some `PipeOp`s can not be constructed.
  For these `PipeOp`s, `NA` is reported in most columns.
* Compatibility with upcoming `mlr3` release.
* New PipeOps for handling inbalanced data: `PipeOpADAS` / `po("adas")`, `PipeOpBLSmote` / `po("blsmote")` and `PipeOpSmoteNC` / `po("smotenc")`

# mlr3pipelines 0.6.0

* Compatibility with new `bbotk` release.
* Added marshaling support to `GraphLearner`
* Support internal tuning and validation

# mlr3pipelines 0.5.2

* Added new `ppl("convert_types")`.
* Minor documentation fixes.
* Test helpers are now available in `inst/`. These are considered experimental and unstable.

# mlr3pipelines 0.5.1

* Changed the ID of `PipeOpFeatureUnion` used in `ppl("robustify")` and `ppl("stacking")`.
* `pipeline_bagging()` gets the `replace` argument (old behaviour `FALSE` by default).
* Feature: The `$add_pipeop()` method got an argument `clone` (old behaviour `TRUE` by default).
* Bugfix: `PipeOpFeatureUnion` in some rare cases dropped variables called `"x"`.
* Compatibility with upcoming paradox release.

# mlr3pipelines 0.5.0-2

* Avoid unnecessarily large serializations of `ppl("robustify")` pipelines.
* Made tests and examples compatible with mlr3 update.

# mlr3pipelines 0.5.0-1

* Bugfix: `PipeOpTuneThreshold` was not overloading the correct `.train` and `.predict` functions.

# mlr3pipelines 0.5.0

* New way of computing `$hash` and `$phash` for `GraphLearner` and all `PipeOp`s. This could break users that inherit from `PipeOp` and make use of `$hash` in the future (but is ultimately in their interest!).
* Neater plots.
* Bugfix: `phash` of `GraphLearner` now considers content of Graph, not only IDs.
* One vignette removed for version 0.1.3 added back here. Welcome home!
* Bugfix: Make Graph work that have PipeOps with more than one output, where one output was linked to multiple inputs.

# mlr3pipelines 0.4.3
* `po()`, `pos()` can now construct `PipeOp`s with ID postfix `_<number>` to avoid ID clashes.
* `GraphLearner` now has method `$base_learner()` that returns the underlying `Learner`, if it can be found by a simple heuristic.
* Fix S3 function signatures

# mlr3pipelines 0.4.2

* Documentation: Clarified `PipeOpHistBin` operation.
* Documentation: Fixed `PipeOpPCA` documentation of `center` default.
* Added `$label` active binding, setting it to the `help()`-page title by default.
* Made tests compatible with upcoming mlr3misc update.

# mlr3pipelines 0.4.1

* `$help()` function for all PipeOps as well as `Graph`, `GraphLearner` and all Learners.
* `GraphLearner` can be created without cloning `Graph` (for internal use).
* `predict.Graph` throws helpful error when it cannot create a fitting `Task`.
* `PipeOpLearner` `packages` slot is set to the `Learner`'s `packages`.
* Bugfix: `PipeOp` `train()` and `predict()` report correct channel name when output has wrong type.
* Bugfix: More accurate type inference when constructing Graphs.
* Stability fix for interaction with packages such as mlr3spatiotempcv that extend existing Task types.

# mlr3pipelines 0.4.0

* New operator `%>>!%` that modifies Graphs in-place.
* New methods `chain_graphs()`, `concat_graphs()`, `Graph$chain()` as alternatives for `%>>%` and `%>>!%`.
* New methods `pos()` and `ppls()` which create lists of PipeOps/Graphs and can be seen as "plural" forms of `po()` and `ppl()`.
* `po()` S3-method for `PipeOp` class that clones a PipeOp object and optionally modifies its attributes.
* `Graph$add_pipeop()` now clones the PipeOp being added.
* Documentation: Clarified documentation about cloning of input arguments in several places.
* Performance enhancements for Graph concatenation.
* More informative error outputs.
* New attribute `graph_model` in `GraphLearner` class, which gets the trained Graph.
* `as_learner()` S3-method for `PipeOp` class that wraps a `PipeOp` in a `Graph` and turns that into a `Learner`.
* Changed PipeOps:
  - `PipeOpHistBin`: renamed `bins` Param to `breaks`
  - `PipeOpImputeHist`: fix handling of integer features spanning the entire represented integer range
  - `PipeOpImputeOOR`: fix handling of integer features spanning the entire represented integer range
  - `PipeOpProxy`: Avoid unnecessary clone
  - `PipeOpScale`: Performance improvement

# mlr3pipelines 0.3.6-1

* Fix numerics problem in tests

# mlr3pipelines 0.3.6

* Bugfix: Make empty Multiplicities work (unless they are nested)
* Fixed: Compatibility with upcoming `bbotk` version.
* New `mlr_graphs`: `pipeline_stacking`
* Added JMLR-Citation

# mlr3pipelines 0.3.5-1

* Fixed: Compatibility with upcoming `mlr3` version.

# mlr3pipelines 0.3.5

* Changed PipeOp: `PipeOpFilter` gets additional `filter.permuted` hyperparameter.
* Bugfix: Make `add_edge` of Graphs work with Multiplicities.
* Bugfix: Make `GraphLearner` hash depend on `id`.
* Documentation: Clarify documentation of `LearnerAvg`.
* Internals: Using more idiomatic internal helper functions.
* Compatibility with upcoming `mlr3` version.

# mlr3pipelines 0.3.4

* Stability: PipeOps don't crash when they have python/reticulate hyperparameter values.
* Documentation: Titles of PipeOp documentation articles reworked.

# mlr3pipelines 0.3.3

* Bugfix: fix rare issue in randomized test
* Compatibility with `bbotk` 0.3.0

# mlr3pipelines 0.3.2
* Bugfix: Make `as.data.table(mlr_pipeops)` work with `paradox` 0.6
* Changed PipeOps:
  - `PipeOpColApply`: now allows for an applicator function with multiple columns as a return value; also inherits from `PipeOpTaskPreprocSimple` now

# mlr3pipelines 0.3.1

* Changed PipeOps:
  - `PipeOpMissInd` now also allows for setting type = integer
  - `PipeOpNMF`: now exposes all parameters previously in `.options`
* Changed `mlr_graphs`:
  - `pipeline_bagging` now uses multiplicities internally
  - fix how `pipeline_robustify` determines the type of newly created columns when using `PipeOpMissInd`
  - `PipeOpFeatureUnion`: Fixed a minor bug when checking for duplicates
* added an autotest for ParamSets of PipeOps: `expect_valid_pipeop_param_set`
* More informative error message when PipeOp input value has wrong type
* Fix automatic detection of R6 type hierarchy
* Performance improvements for `GraphLearner`
* `GraphLearner` allows custom `id`
* Use parallel tests
* Removed bibtex dependency

# mlr3pipelines 0.3.0

* compatibility with `mlr3` 0.6
* `NULL` input channels accept any kind of input
* `print()` method of Graphs now also allows for printing a DOT representation on the console
* `state` of PipeOps is now reset to `NULL` when training fails
* implemented `as_learner.PipeOp`
* `LearnerClassifAvg`, `LearnerRegrAvg` use `bbotk` now
* Changed PPLs:
  - fix how `ppl_robustify` detects whether a learner can handle factors
* Changed PipeOps:
  - `PipeOpTextVectorizer` can now return an "integer sequence representation".
* New PipeOps:
  - `PipeOpNMF`
  - `PipeOpColRoles`
  - `PipeOpVtreat`
* various bugfixes

# mlr3pipelines 0.2.1

* New feature: Multiplicities: implicit repetition of operations
* new `mlr_graphs`:
  - `pipeline_bagging`
  - `pipeline_branch`
  - `pipeline_greplicate`
  - `pipeline_robustify`
  - `pipeline_targettrafo`
  - `pipeline_ovr`
* New PipeOps:
  - `PipeOpOVRSplit`, `PipeOpOVRUnite`
  - `PipeOpReplicate`
  - `PipeOpMultiplicityExply`, `PipeOpMultiplicityImply`
  - `PipeOpTargetTrafo`, `PipeOpTargetInvert`
  - `PipeOpTargetMutate`
  - `PipeOpTargetTrafoScaleRange`
  - `PipeOpProxy`
  - `PipeOpDateFeatures`
  - `PipeOpImputeConstant`
  - `PipeOpImputeLearner`
  - `PipeOpMode`
  - `PipeOpRandomResponse`
  - `PipeOpRenameColumns`
  - `PipeOpTextVectorizer`
  - `PipeOpThreshold`
* Renamed PipeOps:
  - `PipeOpImputeNewlvl` --> `PipeOpImputeOOR` (with additional functionality for continuous values)
* Changed PipeOps:
  - `PipeOpFeatureUnion`: Bugfix: avoid silently overwriting features when names clash
  - `PipeOpHistBin`: Bugfix: handle test set data out of training set range
  - `PipeOpLearnerCV`: Allow returning trainingset prediction during `train()`
  - `PipeOpMutate`: Allow referencing newly created columns
  - `PipeOpScale`: Allow robust scaling
  - `PipeOpLearner`, `PipeOpLearnerCV`: `learner_models` for access to learner with model slot
* New Selectors:
  - `selector_missing`
  - `selector_cardinality_greater_than`
* NULL is neutral element of `%>>%`
* `PipeOpTaskPreproc` now has `feature_types` slot
* `PipeOpTaskPreproc(Simple)` internal API changed: use `.train_task()`, `.predict_task()`, `.train_dt()`, `.predict_dt()`, `.select_cols()`, `.get_state()`, `.transform()`, `.get_state_dt()`, `.transform_dt()` instead of the old methods without dot prefix
* PipeOp now has tags slot
* PipeOp internal API changed: use `.train()`, `.predict()` instead of `train_internal()`, `predict_internal()`
* `Graph` new method `update_ids()`
* `Graph` methods `train(single_input = FALSE)` and `predict(single_input = FALSE)` now handle vararg channels correctly.
* Obsoleted `greplicate()`; use `pipeline_greplicate` / `ppl("greplicate")` instead.
* `po()` now automatically converts `Selector` to `PipeOpSelect`
* `po()` prints available `mlr_pipeops` dictionary content
* `mlr_graphs` dictionary of useful Graphs, with short form accessor `ppl()`
* Work with new `mlr3` version 0.4.0

# mlr3pipelines 0.1.3

* small test fix for R 4.0 (necessary for `stringsAsFactors` option default change in 3.6 -> 4.0)
* `predict()` generic for Graph
* Migrated last vignette to "mlr3 Book"
* Compact in-memory representation of R6 objects to save space when saving objects via `saveRDS()`, `serialize()` etc.

# mlr3pipelines 0.1.2

* Work with new `mlr3` version 0.1.5 (handling of character columns changed)

# mlr3pipelines 0.1.1

* Better html graphics for linear Graphs
* New PipeOps:
  - `PipeOpEncodeImpact`
* Changed PipeOp Behaviour:
  - `PipeOpEncode`: handle NAs

# mlr3pipelines 0.1.0

* Initial upload to CRAN.

