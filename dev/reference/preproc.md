# Simple Pre-processing

Function that offers a simple and direct way to train or predict
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
and [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)s
on [`Task`](https://mlr3.mlr-org.com/reference/Task.html)s,
[`data.frame`](https://rdrr.io/r/base/data.frame.html)s or
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)s.

Training happens if `predict` is set to `FALSE` and no `state` is passed
to this function. Prediction happens if `predict` is set to `TRUE` and
if the passed `Graph` or `PipeOp` is either trained or a `state` is
explicitly passed to this function.

The passed `PipeOp` or `Graph` gets modified by-reference.

## Usage

``` r
preproc(indata, processor, state = NULL, predict = !is.null(state))
```

## Arguments

- indata:

  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html) \|
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) \|
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) )  
  Data to be pre-processed.

- processor:

  ([`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  \|
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md))  
  `Graph` or `PipeOp` accepting a
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) that has one
  output channel.  
  Whenever `indata` is passed a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) or
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html),
  the output channel must return a `Task` to be converted back into a
  `data.frame` or `data.table`. Additionally, `processor`s which only
  work on sub-classes of
  [`TaskSupervised`](https://mlr3.mlr-org.com/reference/TaskSupervised.html)
  will not accept [`data.frame`](https://rdrr.io/r/base/data.frame.html)
  or [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html),
  as it would be unclear which column was the `target`.  
  Be aware that the `processor` gets modified by-reference both during
  training, and if a `state` is passed to this function. This especially
  means that the state of a trained `processor` will get overwritten
  when `state` is passed.  
  You may want to use dictionary sugar functions to select a `processor`
  and to set its hyperparameters, e.g.
  [`po()`](https://mlr3pipelines.mlr-org.com/dev/reference/po.md) or
  [`ppl()`](https://mlr3pipelines.mlr-org.com/dev/reference/ppl.md).

- state:

  (named `list` \| `NULL`)  
  Optional state to be used for prediction, if the `processor` is
  untrained or if the current `state` of the `processor` should be
  overwritten. Must be a complete and correct state for the respective
  `processor`. Default `NULL` (do not overwrite `processor`'s `state`).

- predict:

  (`logical(1)`)  
  Whether to predict (`TRUE`) or train (`FALSE`). By default, this is
  `FALSE` if `state` is `NULL` (`state`'s default), and `TRUE`
  otherwise.

## Value

`any` \| [`data.frame`](https://rdrr.io/r/base/data.frame.html) \|
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html): If
`indata` is a `Task`, whatever is returned by the `processor`'s single
output channel is returned. If `indata` is a
[`data.frame`](https://rdrr.io/r/base/data.frame.html) or
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html), an
object of the same class is returned, or if the `processor`'s output
channel does not return a `Task`, an error is thrown.

## Internals

If `processor` is a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
the S3 method `preproc.PipeOp` gets called first, converting the
`PipeOp` into a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) and
wrapping the `state` appropriately, before calling the S3 method
`preproc.Graph` with the modified objects.

If `indata` is a [`data.frame`](https://rdrr.io/r/base/data.frame.html)
or [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html), a
[`TaskUnsupervised`](https://mlr3.mlr-org.com/reference/TaskUnsupervised.html)
is constructed internally. This implies that `processor`s which only
work on sub-classes of
[`TaskSupervised`](https://mlr3.mlr-org.com/reference/TaskSupervised.html)
will not work with these input types for `indata`.

## Examples

``` r
library("mlr3")

task = tsk("iris")
pop = po("pca")

# Training
preproc(task, pop)
#> 
#> ── <TaskClassif> (150x5): Iris Flowers ─────────────────────────────────────────
#> • Target: Species
#> • Target classes: setosa (33%), versicolor (33%), virginica (33%)
#> • Properties: multiclass
#> • Features (4):
#>   • dbl (4): PC1, PC2, PC3, PC4
# Note that the PipeOp gets trained through this
pop$is_trained
#> [1] TRUE

# Predicting a trained PipeOp (trained through previous call to preproc)
preproc(task, pop, predict = TRUE)
#> 
#> ── <TaskClassif> (150x5): Iris Flowers ─────────────────────────────────────────
#> • Target: Species
#> • Target classes: setosa (33%), versicolor (33%), virginica (33%)
#> • Properties: multiclass
#> • Features (4):
#>   • dbl (4): PC1, PC2, PC3, PC4

# Predicting using a given state
# We use the state of the PipeOp from the last example and then reset it
state = pop$state
pop$state = NULL
preproc(task, pop, state)
#> 
#> ── <TaskClassif> (150x5): Iris Flowers ─────────────────────────────────────────
#> • Target: Species
#> • Target classes: setosa (33%), versicolor (33%), virginica (33%)
#> • Properties: multiclass
#> • Features (4):
#>   • dbl (4): PC1, PC2, PC3, PC4

# Note that the PipeOp's state may get overwritten inadvertently during
# training or if a state is given
pop$state$sdev
#> [1] 2.0562689 0.4926162 0.2796596 0.1543862
preproc(tsk("wine"), pop)
#> 
#> ── <TaskClassif> (178x14): Wine Regions ────────────────────────────────────────
#> • Target: type
#> • Target classes: 2 (40%), 1 (33%), 3 (27%)
#> • Properties: multiclass
#> • Features (13):
#>   • dbl (13): PC1, PC10, PC11, PC12, PC13, PC2, PC3, PC4, PC5, PC6, PC7, PC8,
#>   PC9
pop$state$sdev
#>  [1] 314.9631558  13.1352680   3.0721513   2.2340946   1.1085329   0.9170953
#>  [7]   0.5281794   0.3890775   0.3348085   0.2677734   0.1938452   0.1451632
#> [13]   0.0905743

# Piping multiple preproc() calls, using dictionary sugar to set parameters
tsk("penguins") |>
  preproc(po("imputemode", affect_columns = selector_name("sex"))) |>
  preproc(po("imputemean"))
#> 
#> ── <TaskClassif> (344x8): Palmer Penguins ──────────────────────────────────────
#> • Target: species
#> • Target classes: Adelie (44%), Gentoo (36%), Chinstrap (20%)
#> • Properties: multiclass
#> • Features (7):
#>   • int (3): body_mass, flipper_length, year
#>   • dbl (2): bill_depth, bill_length
#>   • fct (2): island, sex

# Use preproc with a Graph
gr = po("pca", rank. = 4) %>>% po("learner", learner = lrn("classif.rpart"))
preproc(tsk("sonar"), gr)  # returns NULL because of the learner
#> NULL
preproc(tsk("sonar"), gr, predict = TRUE)
#> 
#> ── <PredictionClassif> for 208 observations: ───────────────────────────────────
#>  row_ids truth response
#>        1     R        R
#>        2     R        R
#>        3     R        R
#>      ---   ---      ---
#>      206     M        M
#>      207     M        M
#>      208     M        M

# Training with a data.table input
# Note that `$data()` drops the information that "Species" is the target.
# It gets handled like an ordinary feature here.
dt = tsk("iris")$data()
preproc(dt, pop)
#>        Species       PC1         PC2         PC3          PC4
#>         <fctr>     <num>       <num>       <num>        <num>
#>   1:    setosa -2.684126 -0.31939725  0.02791483 -0.002262437
#>   2:    setosa -2.714142  0.17700123  0.21046427 -0.099026550
#>   3:    setosa -2.888991  0.14494943 -0.01790026 -0.019968390
#>   4:    setosa -2.745343  0.31829898 -0.03155937  0.075575817
#>   5:    setosa -2.728717 -0.32675451 -0.09007924  0.061258593
#>  ---                                                         
#> 146: virginica  1.944110 -0.18753230 -0.17782509 -0.426195940
#> 147: virginica  1.527167  0.37531698  0.12189817 -0.254367442
#> 148: virginica  1.764346 -0.07885885 -0.13048163 -0.137001274
#> 149: virginica  1.900942 -0.11662796 -0.72325156 -0.044595305
#> 150: virginica  1.390189  0.28266094 -0.36290965  0.155038628

# Predicting with a data.table input
preproc(dt, pop)
#>        Species       PC1         PC2         PC3          PC4
#>         <fctr>     <num>       <num>       <num>        <num>
#>   1:    setosa -2.684126 -0.31939725  0.02791483 -0.002262437
#>   2:    setosa -2.714142  0.17700123  0.21046427 -0.099026550
#>   3:    setosa -2.888991  0.14494943 -0.01790026 -0.019968390
#>   4:    setosa -2.745343  0.31829898 -0.03155937  0.075575817
#>   5:    setosa -2.728717 -0.32675451 -0.09007924  0.061258593
#>  ---                                                         
#> 146: virginica  1.944110 -0.18753230 -0.17782509 -0.426195940
#> 147: virginica  1.527167  0.37531698  0.12189817 -0.254367442
#> 148: virginica  1.764346 -0.07885885 -0.13048163 -0.137001274
#> 149: virginica  1.900942 -0.11662796 -0.72325156 -0.044595305
#> 150: virginica  1.390189  0.28266094 -0.36290965  0.155038628
```
