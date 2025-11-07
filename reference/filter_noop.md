# Remove NO_OPs from a List

Remove all
[`NO_OP`](https://mlr3pipelines.mlr-org.com/reference/NO_OP.md) elements
from a `list`.

## Usage

``` r
filter_noop(x)
```

## Arguments

- x:

  `list`  
  List to filter.

## Value

`list`: The input list, with all `NO_OP` elements removed.

## See also

Other Path Branching:
[`NO_OP`](https://mlr3pipelines.mlr-org.com/reference/NO_OP.md),
[`is_noop()`](https://mlr3pipelines.mlr-org.com/reference/is_noop.md),
[`mlr_pipeops_branch`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_branch.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_unbranch.md)
