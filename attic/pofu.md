# POFU design document

## Issues

### 126: POFU could drop all targets except first task's one

Instead of checking that all targets are the same and throwing an error if they are not.

### 216: POFU with differing row IDs

PipeOpFeatureUnion could under some circumstances want to unite tasks that have differing row IDs, e.g. after PipeOpSubsample on two different paths sampled (and `$filter()`ed) different sets of rows.

```r
graph = greplicate(PipeOpSubsample$new() %>>%
    PipeOpLearnerCV$new("classif.rpart"), 2) %>>%
  PipeOpFeatureUnion$new()
graph$plot()  # this is what it looks like

graph$train("iris")  # assertion error
```
mlr-org/mlr3#309 could solve part of this, but the problem goes deeper:
* what if we do sampling with replacement?
* what if PipeOpLearnerCV has a resampling that predicts some entries multiple times, e.g. RepCV or bootstrapping?

### 271: Use DataBackend info to avoid unnecessary data comparison

Using col_hashes

### 388: POFU should use DataBackend cbind

but backends do not have info about col roles

### 390: POFU assert_targets_equal parameter should go

### 570: assertion on 'rows' failed

branch with subsample on one end gives error

```r
library(mlr3)
library(mlr3pipelines)

task = tsk("iris")
resampling = rsmp("holdout")

graph = gunion(
  list(
    po("pca") %>>% po("learner_cv", id = "featureless", lrn("classif.featureless")),
    po("subsample") %>>% po("learner_cv", id = "rpart", lrn("classif.rpart")))
  ) %>>%
  po("featureunion") %>>%
  po("learner", lrn("classif.rpart"))

resample(task, graph, resampling)

```

- possibly improve error message
- what are the options here? fill with NAs? aggregate?

### 571: POFU seems more broken now

this does not seem to give an error any more:

```r
  pos = PipeOpSubsample$new()
  pos$param_set$values$frac = 0.5
  g = pipeline_greplicate(
    pos %>>% PipeOpPCA$new(),
    2
  ) %>>% PipeOpFeatureUnion$new(c("a", "b"))
  task = mlr_tasks$get("iris")
  expect_error(g$train(task), "Assertion on 'rows'")
```


### 607: PipeOpPredictionUnion

https://github.com/mlr-org/miesmuschel/blob/smashy_ex/R/PipeOpPredictionUnion.R

name is confusing, since it is rbinding, not cbinding

### 634:  New Pipeop: Split data by row_ids / logical arg

https://gist.github.com/pfistfl/6b190f0612535817bdd33fe8f8bd6548

- how do we combine this with zero-inflated things?

### 646: Bootstrap resampling

Apparently the problem is that bootstrapping uses some rows repeatedly, which somehow breaks with mlr3's assumption that row_ids are unique values.

```r
library("mlr3")
library("mlr3pipelines")
options(mlr3.debug=TRUE)
resample(tsk("iris"), po("pca") %>>% lrn("classif.featureless"), rsmp("bootstrap"))
```


### 696: PipeOpFeatureUnion breaks predict_newdata when all features of original task aver overwritten


```r
gr <- list(po("select", selector = selector_none()), po("nop")) %>>!% po("featureunion", innum = c("a", "")) %>>!%
  { l <- lrn("classif.rpart") ; l$properties <- setdiff(l$properties, "missings") ; l }
gr$train(tsk("iris"))
#> $classif.rpart.output
#> NULL
#>
gr$predict(tsk("iris"))
#> $classif.rpart.output
#> <PredictionClassif> for 150 observations:
#>     row_ids     truth  response
#>           1    setosa    setosa
#>           2    setosa    setosa
#>           3    setosa    setosa
#> ---
#>         148 virginica virginica
#>         149 virginica virginica
#>         150 virginica virginica

lr <- as_learner(gr)
lr$train(tsk("iris"))
lr$predict_newdata(iris[1:4])
#> Error in map_values(names(x), self$old, self$new) :
#>   Assertion on 'x' failed: Must be of type 'atomic vector', not 'NULL'.
#> This happened PipeOp classif.rpart's $predict()
```

### 697: POFU should use feature_types as reported by input tasks and not the datatypes it gets from $data() (mlr-org/mlr3#685).

- how does data conversion work between task and backend?

### cbind backend simplification


## Notes

- task filter: integer ids as reported by backend
- backend$data
- duplicated IDs, possibly problem with resampling
- col conversion
  - "conversion should hapen"
  - setequal factorlevel: convert, otherwise kA
  - maybe happens when there are fewer levels than before (? -- check)
  - predict-newdata

## Synthesis

### Mostly independent of data backend

- Handling Tasks with different Targets (126)
- Data Comparison when merging (271)
- Error not triggered (571)
- predict_newdata issues (696: all cols replaced, 697: feature types)

### POFU behaviour

- POFU should use DataBackend cbind (388)
- POFU assert_targets_equal parameter should go (390)

### "merging" with different row IDs

- 216, 570, 646
- what if multiple rows are generated?
  - fill with NAs?
  - aggregate?
    - might be relevant for PipeOpLearenerCV with RepCV or bootstrapping
- left, right, outer, inner join?
- train() vs predict()
- predictions
  - see how missing predictions / NAs are handled

### Predictions

- PipeOpFeatureUnion (607)

### Splitting

- by row_ids / logical arg (634)
- zero inflated zeug

### What else?

- predict cols that are then used as input
- auto-simplification
