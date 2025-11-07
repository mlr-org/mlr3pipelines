# PipeOp Base Class

A `PipeOp` represents a transformation of a given "input" into a given
"output", with two stages: "training" and "prediction". It can be
understood as a generalized function that not only has multiple inputs,
but also multiple outputs (as well as two stages). The "training" stage
is used when training a machine learning pipeline or fitting a
statistical model, and the "predicting" stage is then used for making
predictions on new data.

To perform training, the `$train()` function is called which takes
inputs and transforms them, while simultaneously storing information in
its `$state` slot. For prediction, the `$predict()` function is called,
where the `$state` information can be used to influence the
transformation of the new data.

A `PipeOp` is usually used in a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) object,
a representation of a computational graph. It can have multiple **input
channels**—think of these as multiple arguments to a function, for
example when averaging different models—, and multiple **output
channels**—a transformation may return different objects, for example
different subsets of a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html). The purpose of
the [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) is
to connect different outputs of some `PipeOp`s to inputs of other
`PipeOp`s.

Input and output channel information of a `PipeOp` is defined in the
`$input` and `$output` slots; each channel has a *name*, a required type
during training, and a required type during prediction. The `$train()`
and `$predict()` functions are called with a `list` argument that has
one entry for each declared channel (with one exception, see next
paragraph). The `list` is automatically type-checked for each channel
against `$input` and then passed on to the `private$.train()` or
`private$.predict()` functions. There the data is processed and a result
`list` is created. This `list` is again type-checked for declared output
types of each channel. The length and types of the result `list` is as
declared in `$output`.

A special input channel name is `"..."`, which creates a *vararg*
channel that takes arbitrarily many arguments, all of the same type. If
the `$input` table contains an `"..."`-entry, then the input given to
`$train()` and `$predict()` may be longer than the number of declared
input channels.

This class is an abstract base class that all `PipeOp`s being used in a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) should
inherit from, and is not intended to be instantiated.

## Format

Abstract [`R6Class`](https://r6.r-lib.org/reference/R6Class.html).

## Construction

    PipeOp$new(id, param_set = ps(), param_vals = list(), input, output, packages = character(0), tags = character(0))

- `id` :: `character(1)`  
  Identifier of resulting object. See `$id` slot.

- `param_set` ::
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html) \|
  `list` of `expression`  
  Parameter space description. This should be created by the subclass
  and given to `super$initialize()`. If this is a
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html), it
  is used as the `PipeOp`'s
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)
  directly. Otherwise it must be a `list` of expressions e.g. created by
  [`alist()`](https://rdrr.io/r/base/list.html) that evaluate to
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)s.
  These
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html) are
  combined using a
  [`ParamSetCollection`](https://paradox.mlr-org.com/reference/ParamSetCollection.html).

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings given in `param_set`. The subclass should have its own
  `param_vals` parameter and pass it on to `super$initialize()`. Default
  [`list()`](https://rdrr.io/r/base/list.html).

- `input` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns `name` (`character`), `train` (`character`), `predict`
  (`character`)  
  Sets the `$input` slot of the resulting object; see description there.

- `output` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns `name` (`character`), `train` (`character`), `predict`
  (`character`)  
  Sets the `$output` slot of the resulting object; see description
  there.

- `packages` :: `character`  
  Set of all required packages for the `PipeOp`'s `$train` and
  `$predict` methods. See `$packages` slot. Default is `character(0)`.

- `tags` ::`character`  
  A set of tags associated with the `PipeOp`. Tags describe a PipeOp's
  purpose. Can be used to filter `as.data.table(mlr_pipeops)`. Default
  is `"abstract"`, indicating an abstract `PipeOp`.

## Internals

`PipeOp` is an abstract class with abstract functions `private$.train()`
and `private$.predict()`. To create a functional `PipeOp` class, these
two methods must be implemented. Each of these functions receives a
named `list` according to the `PipeOp`'s input channels, and must return
a `list` (names are ignored) with values in the order of output channels
in `$output`. The `private$.train()` and `private$.predict()` function
should not be called by the user; instead, a `$train()` and `$predict()`
should be used. The most convenient usage is to add the `PipeOp` to a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)
(possibly as singleton in that
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)), and
using the
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)'s
`$train()` / `$predict()` methods.

`private$.train()` and `private$.predict()` should treat their inputs as
read-only. If they are
[`R6`](https://r6.r-lib.org/reference/R6Class.html) objects, they should
be cloned before being manipulated in-place. Objects, or parts of
objects, that are not changed, do not need to be cloned, and it is legal
to return the same identical-by-reference objects to multiple outputs.

## Fields

- `id` :: `character`  
  ID of the `PipeOp`. IDs are user-configurable, and IDs of `PipeOp`s
  must be unique within a
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md). IDs
  of `PipeOp`s must not be changed once they are part of a
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md),
  instead the
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)'s
  `$set_names()` method should be used.

- `packages` :: `character`  
  Packages required for the `PipeOp`. Functions that are not in base R
  should still be called using `::` (or explicitly attached using
  [`require()`](https://rdrr.io/r/base/library.html)) in
  `private$.train()` *and* `private$.predict()`, but packages declared
  here are checked before any (possibly expensive) processing has
  started within a
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).

- `param_set` ::
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)  
  Parameters and parameter constraints. Parameter values that influence
  the functioning of `$train` and / or `$predict` are in the
  `$param_set$values` slot; these are automatically checked against
  parameter constraints in `$param_set`.

- `state` :: `any` \| `NULL`  
  Method-dependent state obtained during training step, and usually
  required for the prediction step. This is `NULL` if and only if the
  `PipeOp` has not been trained. The `$state` is the *only* slot that
  can be reliably modified during `$train()`, because `private$.train()`
  may theoretically be executed in a different `R`-session (e.g. for
  parallelization). `$state` should furthermore always be set to
  something with copy-semantics, since it is never cloned. This is a
  limitation not of `PipeOp` or `mlr3pipelines`, but of the way the
  system as a whole works, together with
  [`GraphLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.md)
  and [mlr3](https://CRAN.R-project.org/package=mlr3).

- `input` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns `name` (`character`), `train` (`character`), `predict`
  (`character`)  
  Input channels of `PipeOp`. Column `name` gives the names (and order)
  of values in the list given to `$train()` and `$predict()`. Column
  `train` is the (S3) class that an input object must conform to during
  training, column `predict` is the (S3) class that an input object must
  conform to during prediction. Types are checked by the `PipeOp` itself
  and do not need to be checked by `private$.train()` /
  `private$.predict()` code.  
  A special name is `"..."`, which creates a *vararg* input channel that
  accepts a variable number of inputs.  
  If a row has both `train` and `predict` values enclosed by square
  brackets ("`[`", "`]`"), then this channel is
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)-aware.
  If the `PipeOp` receives a
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
  value on these channels, this
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
  is given to the `.train()` and `.predict()` functions directly.
  Otherwise, the
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
  is transparently unpacked and the `.train()` and `.predict()`
  functions are called multiple times, once for each
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
  element. The type enclosed by square brackets indicates that only a
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
  containing values of this type are accepted. See
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
  for more information.

- `output` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns `name` (`character`), `train` (`character`), `predict`
  (`character`)  
  Output channels of `PipeOp`, in the order in which they will be given
  in the list returned by `$train` and `$predict` functions. Column
  `train` is the (S3) class that an output object must conform to during
  training, column `predict` is the (S3) class that an output object
  must conform to during prediction. The `PipeOp` checks values returned
  by `private$.train()` and `private$.predict()` against these types
  specifications.  
  If a row has both `train` and `predict` values enclosed by square
  brackets ("`[`", "`]`"), then this signals that the channel emits a
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
  of the indicated type. See
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
  for more information.

- `innum` :: `numeric(1)`  
  Number of input channels. This equals `nrow($input)`.

- `outnum` :: `numeric(1)`  
  Number of output channels. This equals `nrow($output)`.

- `is_trained` :: `logical(1)`  
  Indicate whether the `PipeOp` was already trained and can therefore be
  used for prediction.

- `tags` ::`character`  
  A set of tags associated with the `PipeOp`. Tags describe a PipeOp's
  purpose. Can be used to filter
  [`as.data.table(mlr_pipeops)`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.md).
  `PipeOp` tags are inherited and child classes can introduce additional
  tags.

- `hash` :: `character(1)`  
  Checksum calculated on the `PipeOp`, depending on the `PipeOp`'s
  `class` and the slots `$id` and `$param_set$values`. If a `PipeOp`'s
  functionality may change depending on more than these values, it
  should inherit the `$hash` active binding and calculate the hash as
  `digest(list(super$hash, <OTHER THINGS>), algo = "xxhash64")`.

- `phash` :: `character(1)`  
  Checksum calculated on the `PipeOp`, depending on the `PipeOp`'s
  `class` and the slots `$id` but ignoring `$param_set$values`. If a
  `PipeOp`'s functionality may change depending on more than these
  values, it should inherit the `$hash` active binding and calculate the
  hash as `digest(list(super$hash, <OTHER THINGS>), algo = "xxhash64")`.

- `.result` :: `list`  
  If the
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)'s
  `$keep_results` flag is set to `TRUE`, then the intermediate Results
  of `$train()` and `$predict()` are saved to this slot, exactly as they
  are returned by these functions. This is mainly for debugging purposes
  and done, if requested, by the
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)
  backend itself; it should *not* be done explicitly by
  `private$.train()` or `private$.predict()`.

- `man` :: `character(1)`  
  Identifying string of the help page that shows with
  [`help()`](https://rdrr.io/r/utils/help.html).

- `label` :: `character(1)`  
  Description of the `PipeOp`'s functionality. Derived from the title of
  its help page.

- `properties` ::
  [`character()`](https://rdrr.io/r/base/character.html)  
  The properties of the `PipeOp`. Currently supported values are:

  - `"validation"`: the `PipeOp` can make use of the
    `$internal_valid_task` of an
    [`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html). This
    is for example used for
    [`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md)s
    that wrap a
    [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) with
    this property, see
    [`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html).
    `PipeOp`s that have this property, also have a `$validate` field,
    which controls whether to use the validation task, as well as a
    `$internal_valid_scores` field, which allows to access the internal
    validation scores after training.

  - `"internal_tuning"`: the `PipeOp` is able to internally optimize
    hyperparameters. This works analogously to the internal tuning
    implementation for
    [`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html).
    `PipeOp`s with that property also implement the standardized
    accessor `$internal_tuned_values` and have at least one parameter
    tagged with `"internal_tuning"`. An example for such a `PipeOp` is a
    [`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md)
    that wraps a
    [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) with
    the `"internal_tuning"` property.

Programatic access to all available properties is possible via
`mlr_reflections$pipeops$properties`.

## Methods

- [`print()`](https://rdrr.io/r/base/print.html)  
  () -\> `NULL`  
  Prints the `PipeOp`s most salient information: `$id`, `$is_trained`,
  `$param_set$values`, `$input` and `$output`.

- `help(help_type)`  
  (`character(1)`) -\> help file  
  Displays the help file of the concrete `PipeOp` instance. `help_type`
  is one of `"text"`, `"html"`, `"pdf"` and behaves as the `help_type`
  argument of R's [`help()`](https://rdrr.io/r/utils/help.html).

The following public `$train()` and `$predict()` methods are the primary
user-facing functions intended for direct use:

- `train(input)`  
  (`list`) -\> named `list`  
  Train `PipeOp` on `input`s, transform it to output and store the
  learned `$state`. If the `PipeOp` is already trained, already present
  `$state` is overwritten. Input list is typechecked against the
  `$input` `train` column. Return value is a list with as many entries
  as `$output` has rows, with each entry named after the `$output`
  `name` column and class according to the `$output` `train` column. The
  workhorse function for training each `PipeOp` is the
  `private$.train()` function.

- `predict(input)`  
  (`list`) -\> named `list`  
  Predict on new data in `input`, possibly using the stored `$state`.
  Input and output are specified by `$input` and `$output` in the same
  way as for `$train()`, except that the `predict` column is used for
  type checking. The workhorse function for predicting in each `PipeOp`
  is the `private$.predict()` function.

To implement a `PipeOp` the following abstract private functions should
be overloaded in the inheriting `PipeOp`. Note that these should not be
called by a user; instead the public `$train()` and `$predict()` method
should be used.

- `.train(input)`  
  (named `list`) -\> `list`  
  Abstract function that must be implemented by concrete subclasses.
  `private$.train()` is called by `$train()` after typechecking. It must
  change the `$state` value to something non-`NULL` and return a list of
  transformed data according to the `$output` `train` column. Names of
  the returned list are ignored.  

- `.predict(input)`  
  (named `list`) -\> `list`  
  Abstract function that must be implemented by concrete subclasses.
  `private$.predict()` is called by `$predict()` after typechecking and
  works analogously to `private$.train()`. Unlike `private$.train()`,
  `private$.predict()` should not modify the `PipeOp` in any way.  

## Inheriting

To create your own `PipeOp`, you need to overload the `private$.train()`
and `private$.predict()` functions. It is most likely also necessary to
overload the `$initialize()` function to do additional initialization.
The `$initialize()` method should have at least the arguments `id` and
`param_vals`, which should be passed on to `super$initialize()`
unchanged. `id` should have a useful default value, and `param_vals`
should have the default value
[`list()`](https://rdrr.io/r/base/list.html), meaning no initialization
of hyperparameters.

If the `$initialize()` method has more arguments, then it is necessary
to also overload the `private$.additional_phash_input()` function. This
function should return either all objects, or a hash of all objects,
that can change the function or behavior of the `PipeOp` and are
independent of the class, the id, the `$state`, and the
`$param_set$values`. The last point is particularly important: changing
the `$param_set$values` should *not* change the return value of
`private$.additional_phash_input()`.

When you are implementing a `PipeOp` that operates a task (and is not a
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md)),
you also need to handle the `$internal_valid_task` field of the input
task, if there is one.

## See also

https://mlr-org.com/pipeops.html

Other mlr3pipelines backend related:
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreprocSimple.md),
[`mlr_graphs`](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_updatetarget.md)

Other PipeOps:
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md),
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreprocSimple.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.md),
[`mlr_pipeops_adas`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_adas.md),
[`mlr_pipeops_blsmote`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_blsmote.md),
[`mlr_pipeops_boxcox`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_boxcox.md),
[`mlr_pipeops_branch`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_branch.md),
[`mlr_pipeops_chunk`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_chunk.md),
[`mlr_pipeops_classbalancing`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classbalancing.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_classweights`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classweights.md),
[`mlr_pipeops_colapply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_colapply.md),
[`mlr_pipeops_collapsefactors`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_collapsefactors.md),
[`mlr_pipeops_colroles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_colroles.md),
[`mlr_pipeops_copy`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_copy.md),
[`mlr_pipeops_datefeatures`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_datefeatures.md),
[`mlr_pipeops_decode`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_decode.md),
[`mlr_pipeops_encode`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encode.md),
[`mlr_pipeops_encodeimpact`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodeimpact.md),
[`mlr_pipeops_encodelmer`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodelmer.md),
[`mlr_pipeops_encodeplquantiles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodeplquantiles.md),
[`mlr_pipeops_encodepltree`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodepltree.md),
[`mlr_pipeops_featureunion`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_featureunion.md),
[`mlr_pipeops_filter`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_filter.md),
[`mlr_pipeops_fixfactors`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_fixfactors.md),
[`mlr_pipeops_histbin`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_histbin.md),
[`mlr_pipeops_ica`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ica.md),
[`mlr_pipeops_imputeconstant`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputeconstant.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputesample.md),
[`mlr_pipeops_info`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_info.md),
[`mlr_pipeops_isomap`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_isomap.md),
[`mlr_pipeops_kernelpca`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_kernelpca.md),
[`mlr_pipeops_learner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md),
[`mlr_pipeops_learner_pi_cvplus`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner_pi_cvplus.md),
[`mlr_pipeops_learner_quantiles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner_quantiles.md),
[`mlr_pipeops_missind`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_missind.md),
[`mlr_pipeops_modelmatrix`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_modelmatrix.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_mutate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_mutate.md),
[`mlr_pipeops_nearmiss`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nearmiss.md),
[`mlr_pipeops_nmf`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nmf.md),
[`mlr_pipeops_nop`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nop.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_pca`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_pca.md),
[`mlr_pipeops_proxy`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_proxy.md),
[`mlr_pipeops_quantilebin`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_quantilebin.md),
[`mlr_pipeops_randomprojection`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_randomprojection.md),
[`mlr_pipeops_randomresponse`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_randomresponse.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_regravg.md),
[`mlr_pipeops_removeconstants`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_removeconstants.md),
[`mlr_pipeops_renamecolumns`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_renamecolumns.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_replicate.md),
[`mlr_pipeops_rowapply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_rowapply.md),
[`mlr_pipeops_scale`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_scale.md),
[`mlr_pipeops_scalemaxabs`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_scalemaxabs.md),
[`mlr_pipeops_scalerange`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_scalerange.md),
[`mlr_pipeops_select`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_select.md),
[`mlr_pipeops_smote`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_smote.md),
[`mlr_pipeops_smotenc`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_smotenc.md),
[`mlr_pipeops_spatialsign`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_spatialsign.md),
[`mlr_pipeops_subsample`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_subsample.md),
[`mlr_pipeops_targetinvert`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetinvert.md),
[`mlr_pipeops_targetmutate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetmutate.md),
[`mlr_pipeops_targettrafoscalerange`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targettrafoscalerange.md),
[`mlr_pipeops_textvectorizer`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_textvectorizer.md),
[`mlr_pipeops_threshold`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_threshold.md),
[`mlr_pipeops_tomek`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_tomek.md),
[`mlr_pipeops_tunethreshold`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_tunethreshold.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_yeojohnson.md)

## Examples

``` r
# example (bogus) PipeOp that returns the sum of two numbers during $train()
# as well as a letter of the alphabet corresponding to that sum during $predict().

PipeOpSumLetter = R6::R6Class("sumletter",
  inherit = PipeOp,  # inherit from PipeOp
  public = list(
    initialize = function(id = "posum", param_vals = list()) {
      super$initialize(id, param_vals = param_vals,
        # declare "input" and "output" during construction here
        # training takes two 'numeric' and returns a 'numeric';
        # prediction takes 'NULL' and returns a 'character'.
        input = data.table::data.table(name = c("input1", "input2"),
          train = "numeric", predict = "NULL"),
        output = data.table::data.table(name = "output",
          train = "numeric", predict = "character")
      )
    }
  ),
  private = list(
    # PipeOp deriving classes must implement .train and
    # .predict; each taking an input list and returning
    # a list as output.
    .train = function(input) {
      sum = input[[1]] + input[[2]]
      self$state = sum
      list(sum)
    },
    .predict = function(input) {
      list(letters[self$state])
    }
  )
)
posum = PipeOpSumLetter$new()

print(posum)
#> 
#> ── PipeOp <posum>: not trained ─────────────────────────────────────────────────
#> Values: list()
#> 
#> ── Input channels: 
#>    name   train predict
#>  <char>  <char>  <char>
#>  input1 numeric    NULL
#>  input2 numeric    NULL
#> 
#> ── Output channels: 
#>    name   train   predict
#>  <char>  <char>    <char>
#>  output numeric character

posum$train(list(1, 2))
#> $output
#> [1] 3
#> 
# note the name 'output' is the name of the output channel specified
# in the $output data.table.

posum$predict(list(NULL, NULL))
#> $output
#> [1] "c"
#> 
```
