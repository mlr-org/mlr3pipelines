# Impute Features by a Constant

Impute features by a constant value.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Construction

    PipeOpImputeConstant$new(id = "imputeconstant", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"imputeconstant"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with all affected
features missing values imputed by the value of the `constant`
parameter.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md).

The `$state$model` contains the value of the `constant` parameter that
is used for imputation.

## Parameters

The parameters are the parameters inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md),
as well as:

- `constant` :: `atomic(1)`  
  The constant value that should be used for the imputation, atomic
  vector of length `1`. The atomic mode must match the type of the
  features that will be selected by the `affect_columns` parameter and
  this will be checked during imputation. This is a required
  hyperparameter and needs to be set by the user.

- `check_levels` :: `logical(1)`  
  Should be checked whether the `constant` value is a valid level of
  factorial features (i.e., it already is a level)? Raises an error if
  unsuccessful. This check is only performed for factorial features
  (i.e., `factor`, `ordered`; skipped for `character`). Initialized to
  `TRUE`.  
  Note that empty factor levels can be a problem for many
  [`Learners`](https://mlr3.mlr-org.com/reference/Learner.html). Thus,
  [`PipeOpImputeOOR`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputeoor.md)
  is the preferred choice for creating new levels, since it is designed
  to impute out-of-range values and offers a more explicit control for
  handling potentially problematic behavior.

## Internals

The constructor is called with `empty_level_control` set to `"always"`,
to allow the creation of a new empty level for `factor` and `ordered`
(but not `character`) features during training, if `constant` is not an
already existing level and `check_levels` is set to `FALSE`. This has no
impact if `check_levels` is `TRUE`, since in that case an error would be
raised before imputation.

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## See also

https://mlr-org.com/pipeops.html

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md),
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreprocSimple.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.md),
[`mlr_pipeops_adas`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_adas.md),
[`mlr_pipeops_blsmote`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_blsmote.md),
[`mlr_pipeops_boxcox`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_boxcox.md),
[`mlr_pipeops_branch`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_branch.md),
[`mlr_pipeops_chunk`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_chunk.md),
[`mlr_pipeops_classbalancing`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classbalancing.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_classweights`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classweights.md),
[`mlr_pipeops_classweightsex`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classweightsex.md),
[`mlr_pipeops_colapply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_colapply.md),
[`mlr_pipeops_collapsefactors`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_collapsefactors.md),
[`mlr_pipeops_colroles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_colroles.md),
[`mlr_pipeops_copy`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_copy.md),
[`mlr_pipeops_datefeatures`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_datefeatures.md),
[`mlr_pipeops_decode`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_decode.md),
[`mlr_pipeops_encode`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encode.md),
[`mlr_pipeops_encodeimpact`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodeimpact.md),
[`mlr_pipeops_encodelmer`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodelmer.md),
[`mlr_pipeops_encodeplquantiles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodeplquantiles.md),
[`mlr_pipeops_encodepltree`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodepltree.md),
[`mlr_pipeops_featureunion`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_featureunion.md),
[`mlr_pipeops_filter`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_filter.md),
[`mlr_pipeops_fixfactors`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_fixfactors.md),
[`mlr_pipeops_histbin`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_histbin.md),
[`mlr_pipeops_ica`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ica.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputesample.md),
[`mlr_pipeops_info`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_info.md),
[`mlr_pipeops_isomap`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_isomap.md),
[`mlr_pipeops_kernelpca`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_kernelpca.md),
[`mlr_pipeops_learner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md),
[`mlr_pipeops_learner_pi_cvplus`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner_pi_cvplus.md),
[`mlr_pipeops_learner_quantiles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner_quantiles.md),
[`mlr_pipeops_materialize`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_materialize.md),
[`mlr_pipeops_missind`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_missind.md),
[`mlr_pipeops_modelmatrix`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_modelmatrix.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_mutate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_mutate.md),
[`mlr_pipeops_nearmiss`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nearmiss.md),
[`mlr_pipeops_nmf`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nmf.md),
[`mlr_pipeops_nop`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nop.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_pca`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_pca.md),
[`mlr_pipeops_proxy`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_proxy.md),
[`mlr_pipeops_quantilebin`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_quantilebin.md),
[`mlr_pipeops_randomprojection`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_randomprojection.md),
[`mlr_pipeops_randomresponse`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_randomresponse.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_regravg.md),
[`mlr_pipeops_removeconstants`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_removeconstants.md),
[`mlr_pipeops_renamecolumns`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_renamecolumns.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_replicate.md),
[`mlr_pipeops_rowapply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_rowapply.md),
[`mlr_pipeops_scale`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_scale.md),
[`mlr_pipeops_scalemaxabs`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_scalemaxabs.md),
[`mlr_pipeops_scalerange`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_scalerange.md),
[`mlr_pipeops_select`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_select.md),
[`mlr_pipeops_smote`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_smote.md),
[`mlr_pipeops_smotenc`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_smotenc.md),
[`mlr_pipeops_spatialsign`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_spatialsign.md),
[`mlr_pipeops_splines`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_splines.md),
[`mlr_pipeops_subsample`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_subsample.md),
[`mlr_pipeops_targetinvert`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetinvert.md),
[`mlr_pipeops_targetmutate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetmutate.md),
[`mlr_pipeops_targettrafoscalerange`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targettrafoscalerange.md),
[`mlr_pipeops_textvectorizer`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_textvectorizer.md),
[`mlr_pipeops_threshold`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_threshold.md),
[`mlr_pipeops_tomek`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_tomek.md),
[`mlr_pipeops_tunethreshold`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_tunethreshold.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_yeojohnson.md)

Other Imputation PipeOps:
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputesample.md)

## Examples

``` r
library("mlr3")

task = tsk("diabetes")
task$missings()
#> diabetes      age  glucose  insulin     mass pedigree pregnant pressure 
#>        0        0        5      405       13        0        0       35 
#>  triceps 
#>      251 

# impute missing values of the numeric feature "glucose" by the constant value -999
po = po("imputeconstant", param_vals = list(
  constant = -999, affect_columns = selector_name("glucose"))
)
new_task = po$train(list(task = task))[[1]]
new_task$missings()
#> diabetes      age  insulin     mass pedigree pregnant pressure  triceps 
#>        0        0      405       13        0        0       35      251 
#>  glucose 
#>        0 
new_task$data(cols = "glucose")[[1]]
#>   [1]  194  129  132  184  108  117  100   88   73  126  122   87   99  112   80
#>  [16]  111   82  144  126   97  142   87  145   88   88  102  106  106   87  169
#>  [31]  111  179  126   87  122  101  158  111  165   87  112   72  175   78  112
#>  [46]  142  100   96  113  147  141  167  127  122  107  111  118  119  199  128
#>  [61]  144  194  116  124  165  134  106   88   88  128  129  112  115   88  115
#>  [76]   99  129  127  124  103  108  165  137  112  126  137  173  165  128  126
#>  [91]   87   85  129  128  191   80  115  122  111  138  165  111   88  106  135
#> [106]  117  111  146  150   90   96  111  100  160  119  142  163  101   80   88
#> [121]  158  137   57  147  104  147  191  124  128  171   57   82  137  134  113
#> [136]  163  168  107  126  181  111  101   97  165  170   99  112  117  112  134
#> [151]  111   97  165  146  129   97  112  105  119  165  165  184  130  128   99
#> [166]  131  173  103  169   97  124  165  112  191   93  160  106  129  100  103
#> [181]  103  111   97  128  194  104  103  147  134  145  180   87  106  144  181
#> [196]  158  181  175   99  173   87  156   99  117  184  165  129   93  117  117
#> [211]  119  167  131  191   99   88  134   88  122  194  181  137  196  126   94
#> [226]  180  158  144  103  126  112  128   95 -999  117  107  119  121  112  182
#> [241]  158  100  134   83  191  108  106  132  160  117  128   99  136   93  165
#> [256]  127  128  194  128  119  111  106  106  141  111  106  191  124  113   91
#> [271]   82   86   97  155  137  115  120  107  102  117   91  103  132  134  182
#> [286]  165  160   97  194  108  135  144  131  144   86  106  111  109   92  112
#> [301]   80   87  137  148   99  101  124  112  146  131  191  101  131  167  112
#> [316]   88  165  107  102  119  136  134   93   97   90   91   92  152  194  128
#> [331]  122  147  142   96  132  134  129  128  175   91  129  124  121   88  112
#> [346]  101   89  147   73  144  124  167   97  104   83   82   78  169  181  102
#> [361]   97   88  169  111  144  147  102   95  189  152  128  126  175  108   76
#> [376]  112  165   87  181  129   88  112   92  129   95   91  121  128  189  124
#> [391]  126  135   73   96  116  112  169  127  129  168  101  144  170  194  113
#> [406]  165  122  128  181  137  173   88  181   87   93  138   88  173  131   99
#> [421]   95  128  194  189  128   78  116   85  119  142  113   90  147  111  141
#> [436]  181  106  191   97  135  121   99  111  129  129  106   96  106   65  130
#> [451]   82 -999   97  126  165  144  128  107  111  102   88  131  169   78  134
#> [466]  104  106  107  112  100  114  101  109  113  156   85  155   95  102  173
#> [481]  104  191  144  115  106   87  199   87  132  101   97  196  126  119  194
#> [496]   88  184  160  181  111  173  147  181   88  194   84  117  168  191  189
#> [511]  181  112  117   88  154  195  128  119  129  115   97  152  112 -999  128
#> [526]  162  167  179   99   97  128  111   99  119   99  119  165   95  106  112
#> [541]   88  103  106  196  122   80  108  119   88  106   89  112   85  119   87
#> [556]  119   73  146   83  122   92  112  103  165   99  181  131   88   88  170
#> [571]   97   98  109  102  106  119  180  156  137  129  107  116  116  119  144
#> [586]  191  126  115   86  191  104  126  162  119  184  128  137  119   71  134
#> [601]  111  156  147  100   88   99   85   82  160  122  116   93  181   91  126
#> [616]  106  126  134   90  134  147   88  184   97  137 -999   80   76   57  112
#> [631]  137  191  181   86   81  195  147  162  142  160  160  147   82  128  103
#> [646]  184  165  124  117  113  101  181   87  126  131  112  129  154   99  150
#> [661]  124  113  147  138  138  112   91  137  169  137  162   91  100  129  132
#> [676]  103   65  112  106  111  165  181   88  130  181  128   84   96  170  106
#> [691]  112  160  144   88  156  116  154  140  121  106  134  127   95  106  126
#> [706]  160   99  101  113  115  119  119  175  194  104  147  128  180  132   87
#> [721]  172  112  120   92  115  173   98   87   96  172   97   95  184  169  131
#> [736]   96  129  179   91  112   99  124  128   87   97  111   78  107  137  101
#> [751]  134  127   99  116  160  181  100   99 -999   88  113  120  179   92  181
#> [766]  130  146  115
```
