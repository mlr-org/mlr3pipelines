# Multiplicity

A `Multiplicity` class S3 object.

The function of multiplicities is to indicate that
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
should be executed multiple times with multiple values.

A `Multiplicity` is a container, like a
[`list()`](https://rdrr.io/r/base/list.html), that contains multiple
values. If the message that is passed along the edge of a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) is a
`Multiplicity`-object, then the
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
that receives this object will *usually* be called once for each
contained value. The result of each of these calls is then, again,
packed in a `Multiplicity` and sent along the outgoing edge(s) of that
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).
This means that a `Multiplicity` can cause multiple
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
in a row to be run multiple times, where the run for each element of the
`Multiplicity` is independent from the others.

Most
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
only return a `Multiplicity` if their input was a `Multiplicity` (and
after having run their code multiple times, once for each entry).
However, there are a few special
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
that are "aware" of `Multiplicity` objects. These may either *create* a
`Multiplicity` even though not having a `Multiplicity` input (e.g.
[`PipeOpReplicate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_replicate.md)
or
[`PipeOpOVRSplit`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrsplit.md))
– causing the subsequent
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
to be run multiple times – or *collect* a `Multiplicity`, being called
only once even though their input is a `Multiplicity` (e.g.
[`PipeOpOVRUnite`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrunite.md)
or
[`PipeOpFeatureUnion`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_featureunion.md)
if constructed with the `collect_multiplicity` argument set to `TRUE`).
The combination of these mechanisms makes it possible for parts of a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) to
be called variably many times if "sandwiched" between `Multiplicity`
creating and collecting
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s.

Whether a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
creates or collects a `Multiplicity` is indicated by the `$input` or
`$output` slot (which indicate names and types of in/out channels). If
the `train` and `predict` types of an input or output are surrounded by
square brackets ("`[`", "`]`"), then this channel handles a
`Multiplicity` explicitly. Depending on the function of the
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
it will usually collect (input channel) or create (output channel) a
`Multiplicity`.
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
without this indicator are `Multiplicity` agnostic and blindly execute
their function multiple times when given a `Multiplicity`.

If a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md) is
trained on a `Multiplicity`, the `$state` slot is set to a
`Multiplicity` as well; this `Multiplicity` contains the "original"
`$state` resulting from each individual call of the
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
with the input `Multiplicity`'s content. If a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
was trained with a `Multiplicity`, then the
[`predict()`](https://rdrr.io/r/stats/predict.html) argument must be a
`Multiplicity` with the same number of elements.

## Usage

``` r
Multiplicity(...)
```

## Arguments

- ...:

  `any`  
  Can be anything.

## Value

`Multiplicity`

## See also

Other Special Graph Messages:
[`NO_OP`](https://mlr3pipelines.mlr-org.com/dev/reference/NO_OP.md)

Other Experimental Features:
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_replicate.md)

Other Multiplicity PipeOps:
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_featureunion`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_featureunion.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_regravg.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_replicate.md)
