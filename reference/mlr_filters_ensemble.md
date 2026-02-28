# Filter Ensemble

`FilterEnsemble` aggregates several
[`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html)s by
averaging their scores (or ranks) with user-defined weights. Each
wrapped filter is evaluated on the supplied task, and the resulting
feature scores are combined feature-wise by a convex combination
determined through the `weights` parameter. This allows leveraging
complementary inductive biases of multiple filters without committing to
a single criterion. The concept was introduced by Binder et al. (2020).
This implementation follows the idea but leaves the exact choice of
weights to the user.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html).

## Construction

    FilterEnsemble$new(filters)

- `filters` :: `list` of
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html)  
  Filters that are evaluated and aggregated. Each filter must be
  cloneable and support the task type and feature types of the ensemble.
  The ensemble identifier defaults to the wrapped filter ids
  concatenated by `"."`.

## Parameters

- `weights` :: [`numeric()`](https://rdrr.io/r/base/numeric.html)  
  Required non-negative weights, one for each wrapped filter, with at
  least one strictly positive value. Values are used as given when
  calculating the weighted mean. If named, names must match the wrapped
  filter ids.

- `rank_transform` :: `logical(1)`  
  If `TRUE`, ranks of individual filter scores are used instead of the
  raw scores. Initialized to `FALSE`.

- `filter_score_transform` :: `function`  
  Function to be applied to the vector of individual filter scores after
  they were potentially transformed by `rank_transform` but before
  weighting and aggregation. Initialized to `identity`.

- `aggregator` :: `function`  
  Function to aggregate the (potentially transformed) and weighted
  filter scores across filters. Must take arguments `w` for weights and
  `na.rm`, the latter of which is always set to `TRUE`. Defaults to
  [`stats::weighted.mean`](https://rdrr.io/r/stats/weighted.mean.html).

- `result_score_transform` :: `function`  
  Function to be applied to the vector of aggregated scores after they
  were potentially transformed by `rank_transform` and/or
  `filter_score_transform`. Initialized to `identity`.

Parameters of wrapped filters are available via `$param_set` and can be
referenced using the wrapped filter id followed by `"."`, e.g.
`"variance.na.rm"`.

## Fields

- `$wrapped` :: named `list` of
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html)  
  Read-only access to the wrapped filters.

## Methods

- `get_weights_search_space(weights_param_name = "weights", normalize_weights = "uniform", prefix = "w")`  
  (`character(1)`, `character(1)`, `character(1)`) -\>
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)  
  Construct a
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)
  describing a weight search space.

- `get_weights_tunetoken(normalize_weights = "uniform")`  
  (`character(1)`) -\>
  [`TuneToken`](https://paradox.mlr-org.com/reference/to_tune.html)  
  Shortcut returning a
  [`TuneToken`](https://paradox.mlr-org.com/reference/to_tune.html) for
  tuning the weights.

- `set_weights_to_tune(normalize_weights = "uniform")`  
  (`character(1)`) -\> `self`  
  Convenience wrapper that stores the `TuneToken` returned by
  `get_weights_tunetoken()` in `$param_set$values$weights`.

## Internals

All wrapped filters are called with `nfeat` equal to the number of
features to ensure that complete score vectors are available for
aggregation. Scores are combined per feature by computing a weighted
aggregation of transformed (default: `identity`) scores or ranks.
Additionally, the final scores may also be transformed (default:
`identity`).

The order of transformations is as follows:

1.  `$calculate` the filter's scores for all features;

2.  If `rank_transform` is `TRUE`, convert filter scores to ranks;

3.  Apply `filter_score_transform` to the scores / ranks;

4.  Calculate the weighted aggregation across all filters using
    `aggregator`;

5.  Potentially apply `result_score_transform` to the vector of scores
    for each feature aggreagted across filters.

## References

Binder M, Moosbauer J, Thomas J, Bischl B (2020). “Multi-objective
hyperparameter tuning and feature selection using filter ensembles.” In
*Proceedings of the 2020 Genetic and Evolutionary Computation
Conference*, 471–479.
[doi:10.1145/3377930.3389815](https://doi.org/10.1145/3377930.3389815) .

## Examples

``` r
library("mlr3")
library("mlr3filters")

task = tsk("sonar")

filter = flt("ensemble",
  filters = list(FilterVariance$new(), FilterAUC$new()))
filter$param_set$values$weights = c(variance = 0.5, auc = 0.5)
filter$calculate(task)
head(as.data.table(filter))
#>    feature     score
#>     <char>     <num>
#> 1:     V11 0.1493737
#> 2:     V12 0.1312692
#> 3:     V10 0.1253847
#> 4:      V9 0.1224299
#> 5:     V36 0.1174703
#> 6:     V49 0.1162774

# Weighted median as aggregator
filter$param_set$set_values(aggregator = function(x, w, na.rm) {
  if (na.rm) x <- x[!is.na(x)]
  o <- order(x)
  x <- x[o]
  w <- w[o]
  x[match(TRUE, which(cumsum(w) >= sum(w) / 2))]
})
filter$calculate(task)
head(as.data.table(filter))
#>    feature      score
#>     <char>      <num>
#> 1:     V36 0.06975989
#> 2:     V20 0.06898649
#> 3:     V35 0.06714950
#> 4:     V19 0.06655799
#> 5:     V21 0.06647019
#> 6:     V22 0.06547617

# Aggregate reciprocal ranking
filter$param_set$set_values(rank_transform = TRUE, 
  filter_score_transform = function(x) 1 / x, 
  result_score_transform = function(x) rank(1 / x, ties.method = "average"))
filter$calculate(task)
head(as.data.table(filter))
#>    feature score
#>     <char> <num>
#> 1:     V36  59.5
#> 2:     V11  59.5
#> 3:     V12  57.5
#> 4:     V17  57.5
#> 5:     V10  55.5
#> 6:     V20  55.5
```
