# Shorthand PipeOp Constructor

Create

- a `PipeOp` from `mlr_pipeops` from given ID

- a `PipeOpLearner` from a `Learner` object

- a `PipeOpFilter` from a `Filter` object

- a `PipeOpSelect` from a `Selector` object

- a clone of a `PipeOp` from a given `PipeOp` (possibly with changed
  settings)

The object is initialized with given parameters and `param_vals`.

`po()` taks a single `obj` (`PipeOp` id,
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html), ...) and
converts it to a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).
`pos()` (with plural-s) takes either a `character`-vector, or a list of
objects, and creates a `list` of
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.

## Usage

``` r
po(.obj, ...)

pos(.objs, ...)
```

## Arguments

- .obj:

  `[any]`  
  The object from which to construct a `PipeOp`. If this is a
  `character(1)`, it is looked up in the
  [`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops.md)
  dictionary. Otherwise, it is converted to a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

- ...:

  `any`  
  Additional parameters to give to constructed object. This may be an
  argument of the constructor of the `PipeOp`, in which case it is given
  to this constructor; or it may be a parameter value, in which case it
  is given to the `param_vals` argument of the constructor.

- .objs:

  `character` \| `list`  
  Either a `character` of
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  to look up in
  [`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops.md),
  or a list of other objects to be converted to a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).
  If this is a named `list`, then the names are used as `$id` slot for
  the resulting
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.

## Value

A [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
(for `po()`), or a `list` of
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
(for `pos()`).

## Examples

``` r
library("mlr3")

po("learner", lrn("classif.rpart"), cp = 0.3)
#> PipeOp: <classif.rpart> (not trained)
#> values: <cp=0.3, xval=0>
#> Input channels <name [train type, predict type]>:
#>   input [TaskClassif,TaskClassif]
#> Output channels <name [train type, predict type]>:
#>   output [NULL,PredictionClassif]

po(lrn("classif.rpart"), cp = 0.3)
#> PipeOp: <classif.rpart> (not trained)
#> values: <cp=0.3, xval=0>
#> Input channels <name [train type, predict type]>:
#>   input [TaskClassif,TaskClassif]
#> Output channels <name [train type, predict type]>:
#>   output [NULL,PredictionClassif]

# is equivalent with:
mlr_pipeops$get("learner", lrn("classif.rpart"),
  param_vals = list(cp = 0.3))
#> PipeOp: <classif.rpart> (not trained)
#> values: <cp=0.3, xval=0>
#> Input channels <name [train type, predict type]>:
#>   input [TaskClassif,TaskClassif]
#> Output channels <name [train type, predict type]>:
#>   output [NULL,PredictionClassif]

mlr3pipelines::pos(c("pca", original = "nop"))
#> $pca
#> PipeOp: <pca> (not trained)
#> values: <list()>
#> Input channels <name [train type, predict type]>:
#>   input [Task,Task]
#> Output channels <name [train type, predict type]>:
#>   output [Task,Task]
#> 
#> $original
#> PipeOp: <original> (not trained)
#> values: <list()>
#> Input channels <name [train type, predict type]>:
#>   input [*,*]
#> Output channels <name [train type, predict type]>:
#>   output [*,*]
#> 
```
