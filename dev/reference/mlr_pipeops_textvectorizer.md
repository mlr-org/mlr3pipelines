# Bag-of-word Representation of Character Features

Computes a bag-of-word representation from a (set of) columns. Columns
of type `character` are split up into words. Uses the
[`quanteda::dfm()`](https://quanteda.io/reference/dfm.html) and
[`quanteda::dfm_trim()`](https://quanteda.io/reference/dfm_trim.html)
functions. TF-IDF computation works similarly to
[`quanteda::dfm_tfidf()`](https://quanteda.io/reference/dfm_tfidf.html)
but has been adjusted for train/test data split using
[`quanteda::docfreq()`](https://quanteda.io/reference/docfreq.html) and
[`quanteda::dfm_weight()`](https://quanteda.io/reference/dfm_weight.html).

In short:

- Per default, produces a bag-of-words representation

- If `n` is set to values \> 1, ngrams are computed

- If `df_trim` parameters are set, the bag-of-words is trimmed.

- The `scheme_tf` parameter controls term-frequency (per-document, i.e.
  per-row) weighting

- The `scheme_df` parameter controls the document-frequency (per token,
  i.e. per-column) weighting.

Parameters specify arguments to `quanteda`'s `dfm`, `dfm_trim`,
`docfreq` and `dfm_weight`. What belongs to what can be obtained from
each parameter's `tags` where `tokenizer` are arguments passed on to
[`quanteda::dfm()`](https://quanteda.io/reference/dfm.html). Defaults to
a bag-of-words representation with token counts as matrix entries.

In order to perform the *default* `dfm_tfidf` weighting, set the
`scheme_df` parameter to `"inverse"`. The `scheme_df` parameter is
initialized to `"unary"`, which disables document frequency weighting.

The
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
works as follows:

1.  Words are tokenized using
    [`quanteda::tokens`](https://quanteda.io/reference/tokens.html).

2.  Ngrams are computed using
    [`quanteda::tokens_ngrams`](https://quanteda.io/reference/tokens_ngrams.html).

3.  A document-frequency matrix is computed using
    [`quanteda::dfm`](https://quanteda.io/reference/dfm.html).

4.  The document-frequency matrix is trimmed using
    [`quanteda::dfm_trim`](https://quanteda.io/reference/dfm_trim.html)
    during train-time.

5.  The document-frequency matrix is re-weighted (similar to
    [`quanteda::dfm_tfidf`](https://quanteda.io/reference/dfm_tfidf.html))
    if `scheme_df` is not set to `"unary"`.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpTextVectorizer$new(id = "textvectorizer", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"textvectorizer"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with all affected
features converted to a bag-of-words representation.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as:

- `colmodels` :: named `list`  
  Named list with one entry per extracted column. Each entry has two
  further elements:

  - `tdm`: sparse document-feature matrix resulting from
    [`quanteda::dfm()`](https://quanteda.io/reference/dfm.html)

  - `docfreq`: (weighted) document frequency resulting from
    [`quanteda::docfreq()`](https://quanteda.io/reference/docfreq.html)

## Parameters

The parameters are the parameters inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as:

- `return_type` :: `character(1)`  
  Whether to return an integer representation (`"integer-sequence"`) or
  a Bag-of-words (`"bow"`). If set to `"integer_sequence"`, tokens are
  replaced by an integer and padded/truncated to `sequence_length`. If
  set to `"factor_sequence"`, tokens are replaced by a factor and
  padded/truncated to `sequence_length`. If set to `"bow"`, a possibly
  weighted bag-of-words matrix is returned. Defaults to `bow`.

- `stopwords_language` :: `character(1)`  
  Language to use for stopword filtering. Needs to be either `"none"`, a
  language identifier listed in
  `stopwords::stopwords_getlanguages("snowball")` (`"de"`, `"en"`, ...)
  or `"smart"`. `"none"` disables language-specific stopwords. `"smart"`
  coresponds to `stopwords::stopwords(source = "smart")`, which contains
  *English* stopwords and also removes one-character strings.
  Initialized to `"smart"`.

- `extra_stopwords` :: `character`  
  Extra stopwords to remove. Must be a `character` vector containing
  individual tokens to remove. When `n` is set to values greater than
  `1`, this can also contain stop-ngrams. Initialized to `character(0)`.

- `tolower` :: `logical(1)`  
  Whether to convert to lower case. See
  [`quanteda::dfm`](https://quanteda.io/reference/dfm.html). Default is
  `TRUE`.

- `stem` :: `logical(1)`  
  Whether to perform stemming. See
  [`quanteda::dfm`](https://quanteda.io/reference/dfm.html). Default is
  `FALSE`.

- `what` :: `character(1)`  
  Tokenization splitter. See
  [`quanteda::tokens`](https://quanteda.io/reference/tokens.html).
  Default is `"word"`.

- `remove_punct` :: `logical(1)`  
  See [`quanteda::tokens`](https://quanteda.io/reference/tokens.html).
  Default is `FALSE`.

- `remove_url` :: `logical(1)`  
  See [`quanteda::tokens`](https://quanteda.io/reference/tokens.html).
  Default is `FALSE`.

- `remove_symbols` :: `logical(1)`  
  See [`quanteda::tokens`](https://quanteda.io/reference/tokens.html).
  Default is `FALSE`.

- `remove_numbers` :: `logical(1)`  
  See [`quanteda::tokens`](https://quanteda.io/reference/tokens.html).
  Default is `FALSE`.

- `remove_separators` :: `logical(1)`  
  See [`quanteda::tokens`](https://quanteda.io/reference/tokens.html).
  Default is `TRUE`.

- `split_hypens` :: `logical(1)`  
  See [`quanteda::tokens`](https://quanteda.io/reference/tokens.html).
  Default is `FALSE`.

- `n` :: `integer`  
  Vector of ngram lengths. See
  [`quanteda::tokens_ngrams`](https://quanteda.io/reference/tokens_ngrams.html).
  Initialized to `1`, deviating from the base function's default. Note
  that this can be a *vector* of multiple values, to construct ngrams of
  multiple orders.

- `skip` :: `integer`  
  Vector of skips. See
  [`quanteda::tokens_ngrams`](https://quanteda.io/reference/tokens_ngrams.html).
  Default is `0`. Note that this can be a *vector* of multiple values.

- `sparsity` :: `numeric(1)`  
  Desired sparsity of the 'tfm' matrix. See
  [`quanteda::dfm_trim`](https://quanteda.io/reference/dfm_trim.html).
  Default is `NULL`.

- `max_termfreq` :: `numeric(1)`  
  Maximum term frequency in the 'tfm' matrix. See
  [`quanteda::dfm_trim`](https://quanteda.io/reference/dfm_trim.html).
  Default is `NULL`.

- `min_termfreq` :: `numeric(1)`  
  Minimum term frequency in the 'tfm' matrix. See
  [`quanteda::dfm_trim`](https://quanteda.io/reference/dfm_trim.html).
  Default is `NULL`.

- `termfreq_type` :: `character(1)`  
  How to asess term frequency. See
  [`quanteda::dfm_trim`](https://quanteda.io/reference/dfm_trim.html).
  Default is `"count"`.

- `scheme_df` :: `character(1)`  
  Weighting scheme for document frequency: See
  [`quanteda::docfreq`](https://quanteda.io/reference/docfreq.html).
  Initialized to `"unary"` (`1` for each document, deviating from base
  function default).

- `smoothing_df` :: `numeric(1)`  
  See [`quanteda::docfreq`](https://quanteda.io/reference/docfreq.html).
  Default is `0`.

- `k_df` :: `numeric(1)`  
  `k` parameter given to
  [`quanteda::docfreq`](https://quanteda.io/reference/docfreq.html) (see
  there). Default is `0`.

- `threshold_df` :: `numeric(1)`  
  See [`quanteda::docfreq`](https://quanteda.io/reference/docfreq.html).
  Default is `0`. Only considered if `scheme_df` is set to `"count"`.

- `base_df` :: `numeric(1)`  
  The base for logarithms in
  [`quanteda::docfreq`](https://quanteda.io/reference/docfreq.html) (see
  there). Default is `10`.

- `scheme_tf` :: `character(1)`  
  Weighting scheme for term frequency: See
  [`quanteda::dfm_weight`](https://quanteda.io/reference/dfm_weight.html).
  Default is `"count"`.

- `k_tf` :: `numeric(1)`  
  `k` parameter given to
  [`quanteda::dfm_weight`](https://quanteda.io/reference/dfm_weight.html)
  (see there). Default is `0.5`.

- `base_df` :: `numeric(1)`  
  The base for logarithms in
  [`quanteda::dfm_weight`](https://quanteda.io/reference/dfm_weight.html)
  (see there). Default is `10`.

- `sequence_length` :: `integer(1)`  
  The length of the integer sequence. Defaults to `Inf`, i.e. all texts
  are padded to the length of the longest text. Only relevant for
  `return_type` is set to `"integer_sequence"`.

## Internals

See Description. Internally uses the `quanteda` package. Calls
[`quanteda::tokens`](https://quanteda.io/reference/tokens.html),
[`quanteda::tokens_ngrams`](https://quanteda.io/reference/tokens_ngrams.html)
and [`quanteda::dfm`](https://quanteda.io/reference/dfm.html). During
training,
[`quanteda::dfm_trim`](https://quanteda.io/reference/dfm_trim.html) is
also called. Tokens not seen during training are dropped during
prediction.

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## See also

https://mlr-org.com/pipeops.html

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreprocSimple.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops.md),
[`mlr_pipeops_adas`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_adas.md),
[`mlr_pipeops_blsmote`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_blsmote.md),
[`mlr_pipeops_boxcox`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_boxcox.md),
[`mlr_pipeops_branch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_branch.md),
[`mlr_pipeops_chunk`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_chunk.md),
[`mlr_pipeops_classbalancing`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classbalancing.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_classweights`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classweights.md),
[`mlr_pipeops_colapply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_colapply.md),
[`mlr_pipeops_collapsefactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_collapsefactors.md),
[`mlr_pipeops_colroles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_colroles.md),
[`mlr_pipeops_copy`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_copy.md),
[`mlr_pipeops_datefeatures`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_datefeatures.md),
[`mlr_pipeops_decode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_decode.md),
[`mlr_pipeops_encode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encode.md),
[`mlr_pipeops_encodeimpact`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodeimpact.md),
[`mlr_pipeops_encodelmer`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodelmer.md),
[`mlr_pipeops_encodeplquantiles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodeplquantiles.md),
[`mlr_pipeops_encodepltree`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodepltree.md),
[`mlr_pipeops_featureunion`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_featureunion.md),
[`mlr_pipeops_filter`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_filter.md),
[`mlr_pipeops_fixfactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_fixfactors.md),
[`mlr_pipeops_histbin`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_histbin.md),
[`mlr_pipeops_ica`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ica.md),
[`mlr_pipeops_imputeconstant`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeconstant.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputesample.md),
[`mlr_pipeops_kernelpca`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_kernelpca.md),
[`mlr_pipeops_learner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner.md),
[`mlr_pipeops_learner_pi_cvplus`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_pi_cvplus.md),
[`mlr_pipeops_learner_quantiles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_quantiles.md),
[`mlr_pipeops_missind`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_missind.md),
[`mlr_pipeops_modelmatrix`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_modelmatrix.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_mutate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_mutate.md),
[`mlr_pipeops_nearmiss`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_nearmiss.md),
[`mlr_pipeops_nmf`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_nmf.md),
[`mlr_pipeops_nop`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_nop.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_pca`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_pca.md),
[`mlr_pipeops_proxy`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_proxy.md),
[`mlr_pipeops_quantilebin`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_quantilebin.md),
[`mlr_pipeops_randomprojection`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_randomprojection.md),
[`mlr_pipeops_randomresponse`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_randomresponse.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_regravg.md),
[`mlr_pipeops_removeconstants`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_removeconstants.md),
[`mlr_pipeops_renamecolumns`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_renamecolumns.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_replicate.md),
[`mlr_pipeops_rowapply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_rowapply.md),
[`mlr_pipeops_scale`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_scale.md),
[`mlr_pipeops_scalemaxabs`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_scalemaxabs.md),
[`mlr_pipeops_scalerange`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_scalerange.md),
[`mlr_pipeops_select`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_select.md),
[`mlr_pipeops_smote`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_smote.md),
[`mlr_pipeops_smotenc`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_smotenc.md),
[`mlr_pipeops_spatialsign`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_spatialsign.md),
[`mlr_pipeops_subsample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_subsample.md),
[`mlr_pipeops_targetinvert`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetinvert.md),
[`mlr_pipeops_targetmutate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetmutate.md),
[`mlr_pipeops_targettrafoscalerange`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targettrafoscalerange.md),
[`mlr_pipeops_threshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_threshold.md),
[`mlr_pipeops_tomek`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tomek.md),
[`mlr_pipeops_tunethreshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tunethreshold.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_yeojohnson.md)

## Examples

``` r
library("mlr3")
library("data.table")
# create some text data
dt = data.table(
  txt = replicate(150, paste0(sample(letters, 3), collapse = " "))
)
task = tsk("iris")$cbind(dt)

pos = po("textvectorizer", param_vals = list(stopwords_language = "en"))

pos$train(list(task))[[1]]$data()
#> 'as(<dgCMatrix>, "dgTMatrix")' is deprecated.
#> Use 'as(., "TsparseMatrix")' instead.
#> See help("Deprecated") and help("Matrix-deprecated").
#>        Species Petal.Length Petal.Width Sepal.Length Sepal.Width txt.n txt.f
#>         <fctr>        <num>       <num>        <num>       <num> <num> <num>
#>   1:    setosa          1.4         0.2          5.1         3.5     1     1
#>   2:    setosa          1.4         0.2          4.9         3.0     0     0
#>   3:    setosa          1.3         0.2          4.7         3.2     0     1
#>   4:    setosa          1.5         0.2          4.6         3.1     0     0
#>   5:    setosa          1.4         0.2          5.0         3.6     0     0
#>  ---                                                                        
#> 146: virginica          5.2         2.3          6.7         3.0     0     0
#> 147: virginica          5.0         1.9          6.3         2.5     0     0
#> 148: virginica          5.2         2.0          6.5         3.0     0     0
#> 149: virginica          5.4         2.3          6.2         3.4     0     0
#> 150: virginica          5.1         1.8          5.9         3.0     0     0
#>      txt.v txt.m txt.p txt.r txt.d txt.w txt.x txt.q txt.b txt.l txt.t txt.o
#>      <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#>   1:     1     0     0     0     0     0     0     0     0     0     0     0
#>   2:     0     1     1     1     0     0     0     0     0     0     0     0
#>   3:     0     0     0     0     1     0     0     0     0     0     0     0
#>   4:     0     0     0     0     0     1     1     1     0     0     0     0
#>   5:     0     0     0     0     0     1     0     0     1     1     0     0
#>  ---                                                                        
#> 146:     1     0     0     0     0     0     0     0     0     0     0     0
#> 147:     0     0     0     0     0     0     0     0     0     0     0     0
#> 148:     0     0     0     1     0     0     0     0     0     0     0     0
#> 149:     0     0     1     0     0     0     0     0     0     0     0     0
#> 150:     0     0     0     1     1     0     0     0     0     0     0     0
#>      txt.j txt.e txt.g txt.h txt.s txt.y txt.c txt.u txt.k txt.z
#>      <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#>   1:     0     0     0     0     0     0     0     0     0     0
#>   2:     0     0     0     0     0     0     0     0     0     0
#>   3:     0     0     0     0     0     0     0     0     0     0
#>   4:     0     0     0     0     0     0     0     0     0     0
#>   5:     0     0     0     0     0     0     0     0     0     0
#>  ---                                                            
#> 146:     0     0     0     0     0     0     0     1     1     0
#> 147:     0     0     1     1     0     0     1     0     0     0
#> 148:     1     0     1     0     0     0     0     0     0     0
#> 149:     0     0     0     0     1     0     0     1     0     0
#> 150:     0     0     0     0     0     0     1     0     0     0

one_line_of_iris = task$filter(13)

one_line_of_iris$data()
#>    Species Petal.Length Petal.Width Sepal.Length Sepal.Width    txt
#>     <fctr>        <num>       <num>        <num>       <num> <char>
#> 1:  setosa          1.4         0.1          4.8           3  s y p

pos$predict(list(one_line_of_iris))[[1]]$data()
#>    Species Petal.Length Petal.Width Sepal.Length Sepal.Width txt.n txt.f txt.v
#>     <fctr>        <num>       <num>        <num>       <num> <num> <num> <num>
#> 1:  setosa          1.4         0.1          4.8           3     0     0     0
#>    txt.m txt.p txt.r txt.d txt.w txt.x txt.q txt.b txt.l txt.t txt.o txt.j
#>    <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#> 1:     0     1     0     0     0     0     0     0     0     0     0     0
#>    txt.e txt.g txt.h txt.s txt.y txt.c txt.u txt.k txt.z
#>    <num> <num> <num> <num> <num> <num> <num> <num> <num>
#> 1:     0     0     0     1     1     0     0     0     0
```
