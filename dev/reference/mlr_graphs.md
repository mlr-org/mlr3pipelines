# Dictionary of (sub-)graphs

A simple
[`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md). The
dictionary contains a collection of often-used graph structures, and
it's aim is solely to make often-used functions more accessible. Each
`Graph` has an associated help page, which can be accessed via
`?mlr_graphs_<key>`, i.e.
[`?mlr_graphs_bagging`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_graphs_bagging.md).

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`mlr3misc::Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

Methods inherited from
[`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html),
as well as:

- `add(key, value)`  
  (`character(1)`, `function`)  
  Adds constructor `value` to the dictionary with key `key`, potentially
  overwriting a previously stored item.

## S3 methods

- `as.data.table(dict)`  
  [`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  -\>
  [`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Returns a `data.table` with column `key` (`character`).

## See also

Other mlr3pipelines backend related:
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md),
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreprocSimple.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md)

Other Dictionaries:
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops.md)

## Examples

``` r
library(mlr3)
lrn = lrn("regr.rpart")
task = mlr_tasks$get("boston_housing")

# Robustify the learner for the task.
gr = pipeline_robustify(task, lrn) %>>% po("learner", lrn)
# or equivalently
gr = mlr_graphs$get("robustify", task = task, learner = lrn) %>>% po(lrn)
# or equivalently
gr = ppl("robustify", task, lrn) %>>% po("learner", lrn)

# all Graphs currently in the dictionary:
as.data.table(mlr_graphs)
#> Key: <key>
#>              key
#>           <char>
#> 1:       bagging
#> 2:        branch
#> 3: convert_types
#> 4:    greplicate
#> 5:           ovr
#> 6:     robustify
#> 7:      stacking
#> 8:   targettrafo
```
