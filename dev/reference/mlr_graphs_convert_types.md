# Convert Column Types

Converts all columns of type `type_from` to `type_to`, using the
corresponding R function (e.g.
[`as.numeric()`](https://rdrr.io/r/base/numeric.html),
[`as.factor()`](https://rdrr.io/r/base/factor.html)). It is possible to
further subset the columns that should be affected using the
`affect_columns` argument. The resulting
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
contains a
[`PipeOpColApply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_colapply.md),
followed, if appropriate, by a
[`PipeOpFixFactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_fixfactors.md).

Unlike R's [`as.factor()`](https://rdrr.io/r/base/factor.html) function,
`ppl("convert_types")` will convert `ordered` types into (unordered)
`factor` vectors.

## Usage

``` r
pipeline_convert_types(
  type_from,
  type_to,
  affect_columns = NULL,
  id = NULL,
  fixfactors = NULL,
  more_args = list()
)
```

## Arguments

- type_from:

  `character`  
  Which column types to convert. May be any combination of `"logical"`,
  `"integer"`, `"numeric"`, `"factor"`, `"ordered"`, `"character"`, or
  `"POSIXct"`.

- type_to:

  `character(1)`  
  Which type to convert to. Must be a scalar value, exactly one of the
  types allowed in `type_from`.

- affect_columns:

  `function` \|
  [`Selector`](https://mlr3pipelines.mlr-org.com/dev/reference/Selector.md)
  \| `NULL`  
  Which columns to affect. This argument can further restrict the
  columns being converted, beyond the `type_from` argument. Must be a
  [`Selector`](https://mlr3pipelines.mlr-org.com/dev/reference/Selector.md)-like
  function, which takes a
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) as argument and
  returns a `character` of features to use.

- id:

  `character(1)` \| `NULL`  
  ID to give to the constructed
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.
  Defaults to an ID built automatically from `type_from` and `type_to`.
  If a
  [`PipeOpFixFactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_fixfactors.md)
  is appended, its ID will be `paste0(id, "_ff")`.

- fixfactors:

  `logical(1)` \| `NULL`  
  Whether to append a
  [`PipeOpFixFactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_fixfactors.md).
  Defaults to `TRUE` if and only if `type_to` is `"factor"` or
  `"ordered"`.

- more_args:

  `list`  
  Additional arguments to give to the conversion function. This could
  e.g. be used to pass the timezone to `as.POSIXct`.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)

## Examples

``` r
library("mlr3")

data_chr = data.table::data.table(
  x = factor(letters[1:3]),
  y = letters[1:3],
  z = letters[1:3]
)
task_chr = TaskClassif$new("task_chr", data_chr, "x")
str(task_chr$data())
#> Classes ‘data.table’ and 'data.frame':   3 obs. of  3 variables:
#>  $ x: Factor w/ 3 levels "a","b","c": 1 2 3
#>  $ y: chr  "a" "b" "c"
#>  $ z: chr  "a" "b" "c"
#>  - attr(*, ".internal.selfref")=<externalptr> 

graph = ppl("convert_types", "character", "factor")
str(graph$train(task_chr)[[1]]$data())
#> Classes ‘data.table’ and 'data.frame':   3 obs. of  3 variables:
#>  $ x: Factor w/ 3 levels "a","b","c": 1 2 3
#>  $ y: Factor w/ 3 levels "a","b","c": 1 2 3
#>  $ z: Factor w/ 3 levels "a","b","c": 1 2 3
#>  - attr(*, ".internal.selfref")=<externalptr> 

graph_z = ppl("convert_types", "character", "factor",
  affect_columns = selector_name("z"))
graph_z$train(task_chr)[[1]]$data()
#>         x      z      y
#>    <fctr> <fctr> <char>
#> 1:      a      a      a
#> 2:      b      b      b
#> 3:      c      c      c

# `affect_columns` and `type_from` are both applied. The following
# looks for a 'numeric' column with name 'z', which is not present;
# the task is therefore unchanged.
graph_z = ppl("convert_types", "numeric", "factor",
  affect_columns = selector_name("z"))
graph_z$train(task_chr)[[1]]$data()
#>         x      y      z
#>    <fctr> <char> <char>
#> 1:      a      a      a
#> 2:      b      b      b
#> 3:      c      c      c
```
