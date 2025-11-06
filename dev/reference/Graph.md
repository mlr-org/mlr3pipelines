# Graph Base Class

A `Graph` is a representation of a machine learning pipeline graph. It
can be *trained*, and subsequently used for *prediction*.

A `Graph` is most useful when used together with
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) objects
encapsulated as
[`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner.md).
In this case, the `Graph` produces
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html) data
during its `$predict()` phase and can be used as a
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) itself
(using the
[`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md)
wrapper). However, the `Graph` can also be used without
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) objects to
simply perform preprocessing of data, and, in principle, does not even
need to handle data at all but can be used for general processes with
dependency structure (although the
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
for this would need to be written).

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html).

## Construction

    Graph$new()

## Internals

A `Graph` is made up of a list of
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s,
and a
[`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
of edges. Both for training and prediction, the `Graph` performs
topological sorting of the
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
and executes their respective `$train()` or `$predict()` functions in
order, moving the
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
results along the edges as input to other
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.

## Fields

- `pipeops` :: named `list` of
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)  
  Contains all
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  in the `Graph`, named by the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  `$id`s.

- `edges` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns `src_id` (`character`), `src_channel` (`character`),
  `dst_id` (`character`), `dst_channel` (`character`)  
  Table of connections between the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.
  A
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
  `src_id` and `dst_id` are `$id`s of
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  that must be present in the `$pipeops` list. `src_channel` and
  `dst_channel` must respectively be `$output` and `$input` channel
  names of the respective
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.

- `is_trained` :: `logical(1)`  
  Is the `Graph`, i.e. are all of its
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s,
  trained, and can the `Graph` be used for prediction?

- `lhs` :: `character`  
  Ids of the 'left-hand-side'
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  that have some unconnected input channels and therefore act as `Graph`
  input layer.

- `rhs` :: `character`  
  Ids of the 'right-hand-side'
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  that have some unconnected output channels and therefore act as
  `Graph` output layer.

- `input` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns `name` (`character`), `train` (`character`), `predict`
  (`character`), `op.id` (`character`), `channel.name` (`character`)  
  Input channels of the `Graph`. For each channel lists the name, input
  type during training, input type during prediction,
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  `$id` of the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  the channel pertains to, and channel name as the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  knows it.

- `output` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns `name` (`character`), `train` (`character`), `predict`
  (`character`), `op.id` (`character`), `channel.name` (`character`)  
  Output channels of the `Graph`. For each channel lists the name,
  output type during training, output type during prediction,
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  `$id` of the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  the channel pertains to, and channel name as the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  knows it.

- `packages` :: `character`  
  Set of all required packages for the various methods in the `Graph`, a
  set union of all required packages of all contained
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  objects.

- `state` :: named `list`  
  Get / Set the `$state` of each of the members of
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

- `param_set` ::
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)  
  Parameters and parameter constraints. Parameter values are in
  `$param_set$values`. These are the union of `$param_set`s of all
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  in the `Graph`. Parameter names as seen by the `Graph` have the naming
  scheme `<PipeOp$id>.<PipeOp original parameter name>`. Changing
  `$param_set$values` also propagates the changes directly to the
  contained
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  and is an alternative to changing a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  `$param_set$values` directly.

- `hash` :: `character(1)`  
  Stores a checksum calculated on the `Graph` configuration, which
  includes all
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  hashes (and therefore their `$param_set$values`) and a hash of
  `$edges`.

- `phash` :: `character(1)`  
  Stores a checksum calculated on the `Graph` configuration, which
  includes all
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  hashes *except* their `$param_set$values`, and a hash of `$edges`.

- `keep_results` :: `logical(1)`  
  Whether to store intermediate results in the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  `$.result` slot, mostly for debugging purposes. Default `FALSE`.

- `man` :: `character(1)`  
  Identifying string of the help page that shows with
  [`help()`](https://rdrr.io/r/utils/help.html).

## Methods

- `ids(sorted = FALSE)`  
  (`logical(1)`) -\> `character`  
  Get IDs of all
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.
  This is in order that
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  were added if `sorted` is `FALSE`, and topologically sorted if
  `sorted` is `TRUE`.

- `add_pipeop(op, clone = TRUE)`  
  ([`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  \| [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) \|
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html) \|
  `...`, `logical(1)`) -\> `self`  
  Mutates `Graph` by adding a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  to the `Graph`. This does not add any edges, so the new
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  will not be connected within the `Graph` at first.  
  Instead of supplying a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  directly, an object that can naturally be converted to a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  can also be supplied, e.g. a
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) or a
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html); see
  [`as_pipeop()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_pipeop.md).
  The argument given as `op` is cloned if `clone` is `TRUE` (default);
  to access a `Graph`'s
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  by-reference, use `$pipeops`.  
  Note that `$add_pipeop()` is a relatively low-level operation, it is
  recommended to build graphs using `%>>%`.

- `add_edge(src_id, dst_id, src_channel = NULL, dst_channel = NULL)`  
  (`character(1)`, `character(1)`, `character(1)` \| `numeric(1)` \|
  `NULL`, `character(1)` \| `numeric(1)` \| `NULL`) -\> `self`  
  Add an edge from
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  `src_id`, and its channel `src_channel` (identified by its name or
  number as listed in the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  `$output`), to
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  `dst_id`'s channel `dst_channel` (identified by its name or number as
  listed in the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  `$input`). If source or destination
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  have only one input / output channel and `src_channel` / `dst_channel`
  are therefore unambiguous, they can be omitted (i.e. left as `NULL`).

- `chain(gs, clone = TRUE)`  
  (`list` of `Graph`s, `logical(1)`) -\> `self`  
  Takes a list of `Graph`s or
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  (or objects that can be automatically converted into `Graph`s or
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s,
  see
  [`as_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_graph.md)
  and
  [`as_pipeop()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_pipeop.md))
  as inputs and joins them in a serial `Graph` coming after `self`, as
  if connecting them using `%>>%`.

- `plot(html = FALSE, horizontal = FALSE)`  
  (`logical(1)`, `logical(1)`) -\> `NULL`  
  Plot the `Graph`, using either the
  [igraph](https://CRAN.R-project.org/package=igraph) package (for
  `html = FALSE`, default) or the `visNetwork` package for `html = TRUE`
  producing a
  [`htmlWidget`](https://rdrr.io/pkg/htmlwidgets/man/htmlwidgets-package.html).
  The
  [`htmlWidget`](https://rdrr.io/pkg/htmlwidgets/man/htmlwidgets-package.html)
  can be rescaled using
  [`visOptions`](https://rdrr.io/pkg/visNetwork/man/visOptions.html).
  For `html = FALSE`, the orientation of the plotted graph can be
  controlled through `horizontal`.

- `print(dot = FALSE, dotname = "dot", fontsize = 24L)`  
  (`logical(1)`, `character(1)`, `integer(1)`) -\> `NULL`  
  Print a representation of the `Graph` on the console. If `dot` is
  `FALSE`, output is a table with one row for each contained
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  and columns `ID` (`$id` of `PipeOp`), `State` (short representation of
  `$state` of
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)),
  `sccssors`
  ([`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  that take their input directly from the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  on this line), and `prdcssors` (the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  that produce the data that is read as input by the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  on this line). If `dot` is `TRUE`, print a DOT representation of the
  `Graph` on the console. The DOT output can be named via the argument
  `dotname` and the `fontsize` can also be specified.

- `set_names(old, new)`  
  (`character`, `character`) -\> `self`  
  Rename
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s:
  Change ID of each
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  as identified by `old` to the corresponding item in `new`. This should
  be used instead of changing a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  `$id` value directly!

- `update_ids(prefix = "", postfix = "")`  
  (`character`, `character`) -\> `self`  
  Pre- or postfix
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  existing ids. Both `prefix` and `postfix` default to `""`, i.e. no
  changes.

- `train(input, single_input = TRUE)`  
  (`any`, `logical(1)`) -\> named `list`  
  Train `Graph` by traversing the `Graph`s' edges and calling all the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  `$train` methods in turn. Return a named `list` of outputs for each
  unconnected
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  out-channel, named according to the `Graph`'s `$output` `name` column.
  During training, the `$state` member of each
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  will be set and the `$is_trained` slot of the `Graph` (and each
  individual `PipeOp`) will consequently be set to `TRUE`.  
  If `single_input` is `TRUE`, the `input` value will be sent to each
  unconnected
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  input channel (as listed in the `Graph`'s `$input`). Typically,
  `input` should be a
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html), although this
  is dependent on the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  in the `Graph`. If `single_input` is `FALSE`, then `input` should be a
  `list` with the same length as the `Graph`'s `$input` table has rows;
  each list item will be sent to a corresponding input channel of the
  `Graph`. If `input` is a named `list`, names must correspond to input
  channel names (`$input$name`) and inputs will be sent to the channels
  by name; otherwise they will be sent to the channels in order in which
  they are listed in `$input`.

- `predict(input, single_input = TRUE)`  
  (`any`, `logical(1)`) -\> `list` of `any`  
  Predict with the `Graph` by calling all the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  `$train` methods. Input and output, as well as the function of the
  `single_input` argument, are analogous to `$train()`.

- `help(help_type)`  
  (`character(1)`) -\> help file  
  Displays the help file of the concrete `PipeOp` instance. `help_type`
  is one of `"text"`, `"html"`, `"pdf"` and behaves as the `help_type`
  argument of R's [`help()`](https://rdrr.io/r/utils/help.html).

## See also

Other mlr3pipelines backend related:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreprocSimple.md),
[`mlr_graphs`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_graphs.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md)

## Examples

``` r
library("mlr3")

g = Graph$new()$
  add_pipeop(PipeOpScale$new(id = "scale"))$
  add_pipeop(PipeOpPCA$new(id = "pca"))$
  add_edge("scale", "pca")
g$input
#>           name  train predict  op.id channel.name
#>         <char> <char>  <char> <char>       <char>
#> 1: scale.input   Task    Task  scale        input
g$output
#>          name  train predict  op.id channel.name
#>        <char> <char>  <char> <char>       <char>
#> 1: pca.output   Task    Task    pca       output

task = tsk("iris")
trained = g$train(task)
trained[[1]]$data()
#>        Species        PC1         PC2         PC3         PC4
#>         <fctr>      <num>       <num>       <num>       <num>
#>   1:    setosa -2.2571412 -0.47842383  0.12727962 -0.02408751
#>   2:    setosa -2.0740130  0.67188269  0.23382552 -0.10266284
#>   3:    setosa -2.3563351  0.34076642 -0.04405390 -0.02828231
#>   4:    setosa -2.2917068  0.59539986 -0.09098530  0.06573534
#>   5:    setosa -2.3818627 -0.64467566 -0.01568565  0.03580287
#>  ---                                                         
#> 146: virginica  1.8642579 -0.38567404 -0.25541818 -0.38795715
#> 147: virginica  1.5593565  0.89369285  0.02628330 -0.21945690
#> 148: virginica  1.5160915 -0.26817075 -0.17957678 -0.11877324
#> 149: virginica  1.3682042 -1.00787793 -0.93027872 -0.02604141
#> 150: virginica  0.9574485  0.02425043 -0.52648503  0.16253353

task$filter(1:10)
predicted = g$predict(task)
predicted[[1]]$data()
#>     Species       PC1         PC2         PC3          PC4
#>      <fctr>     <num>       <num>       <num>        <num>
#>  1:  setosa -2.257141 -0.47842383  0.12727962 -0.024087508
#>  2:  setosa -2.074013  0.67188269  0.23382552 -0.102662845
#>  3:  setosa -2.356335  0.34076642 -0.04405390 -0.028282305
#>  4:  setosa -2.291707  0.59539986 -0.09098530  0.065735340
#>  5:  setosa -2.381863 -0.64467566 -0.01568565  0.035802870
#>  6:  setosa -2.068701 -1.48420530 -0.02687825 -0.006586116
#>  7:  setosa -2.435868 -0.04748512 -0.33435030  0.036652767
#>  8:  setosa -2.225392 -0.22240300  0.08839935  0.024529919
#>  9:  setosa -2.326845  1.11160370 -0.14459247  0.026769540
#> 10:  setosa -2.177035  0.46744757  0.25291827  0.039766068
```
