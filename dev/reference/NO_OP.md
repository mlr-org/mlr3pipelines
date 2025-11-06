# No-Op Sentinel Used for Alternative Branching

Special data type for no-ops. Distinct from `NULL` for easier debugging
and distinction from unintentional `NULL` returns.

## Usage

``` r
NO_OP
```

## Format

[`R6`](https://r6.r-lib.org/reference/R6Class.html) object.

## See also

Other Path Branching:
[`filter_noop()`](https://mlr3pipelines.mlr-org.com/dev/reference/filter_noop.md),
[`is_noop()`](https://mlr3pipelines.mlr-org.com/dev/reference/is_noop.md),
[`mlr_pipeops_branch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_branch.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md)

Other Special Graph Messages:
[`Multiplicity()`](https://mlr3pipelines.mlr-org.com/dev/reference/Multiplicity.md)
