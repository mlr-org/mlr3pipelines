# Configure Validation for a GraphLearner

Configure validation for a graph learner.

In a
[`GraphLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.md),
validation can be configured on two levels:

1.  On the
    [`GraphLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.md)
    level, which specifies **how** the validation set is constructed
    before entering the graph.

2.  On the level of the individual `PipeOp`s (such as `PipeOpLearner`),
    which specifies which pipeops actually make use of the validation
    data (set its `$validate` field to `"predefined"`) or not (set it to
    `NULL`). This can be specified via the argument `ids`.

## Usage

``` r
# S3 method for class 'GraphLearner'
set_validate(
  learner,
  validate,
  ids = NULL,
  args_all = list(),
  args = list(),
  ...
)
```

## Arguments

- learner:

  ([`GraphLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.md))  
  The graph learner to configure.

- validate:

  (`numeric(1)`, `"predefined"`, `"test"`, or `NULL`)  
  How to set the `$validate` field of the learner. If set to `NULL` all
  validation is disabled, both on the graph learner level, but also for
  all pipeops.

- ids:

  (`NULL` or [`character()`](https://rdrr.io/r/base/character.html))  
  For which pipeops to enable validation. This parameter is ignored when
  `validate` is set to `NULL`. By default, validation is enabled for the
  final `PipeOp` in the `Graph`.

- args_all:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Rarely needed. A named list of parameter values that are passed to all
  subsequet
  [`set_validate()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html)
  calls on the individual `PipeOp`s.

- args:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Rarely needed. A named list of lists, specifying additional argments
  to be passed to
  [`set_validate()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html)
  when calling it on the individual `PipeOp`s.

- ...:

  (any)  
  Currently unused.

## Examples

``` r
library(mlr3)

glrn = as_learner(po("pca") %>>% lrn("classif.debug"))
set_validate(glrn, 0.3)
glrn$validate
#> [1] 0.3
glrn$graph$pipeops$classif.debug$learner$validate
#> [1] "predefined"

set_validate(glrn, NULL)
glrn$validate
#> NULL
glrn$graph$pipeops$classif.debug$learner$validate
#> NULL

set_validate(glrn, 0.2, ids = "classif.debug")
glrn$validate
#> [1] 0.2
glrn$graph$pipeops$classif.debug$learner$validate
#> [1] "predefined"
```
