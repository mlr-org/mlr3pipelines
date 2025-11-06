# Impute Features by a Constant

Impute features by a constant value.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

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
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with all affected
features missing values imputed by the value of the `constant`
parameter.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md).

The `$state$model` contains the value of the `constant` parameter that
is used for imputation.

## Parameters

The parameters are the parameters inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
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
  [`PipeOpImputeOOR`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md)
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
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

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
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputesample.md),
[`mlr_pipeops_info`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_info.md),
[`mlr_pipeops_isomap`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_isomap.md),
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
[`mlr_pipeops_textvectorizer`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_textvectorizer.md),
[`mlr_pipeops_threshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_threshold.md),
[`mlr_pipeops_tomek`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tomek.md),
[`mlr_pipeops_tunethreshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tunethreshold.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_yeojohnson.md)

Other Imputation PipeOps:
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputesample.md)

## Examples

``` r
library("mlr3")

task = tsk("pima")
task$missings()
#> diabetes      age  glucose  insulin     mass pedigree pregnant pressure 
#>        0        0        5      374       11        0        0       35 
#>  triceps 
#>      227 

# impute missing values of the numeric feature "glucose" by the constant value -999
po = po("imputeconstant", param_vals = list(
  constant = -999, affect_columns = selector_name("glucose"))
)
new_task = po$train(list(task = task))[[1]]
new_task$missings()
#> diabetes      age  insulin     mass pedigree pregnant pressure  triceps 
#>        0        0      374       11        0        0       35      227 
#>  glucose 
#>        0 
new_task$data(cols = "glucose")[[1]]
#>   [1]  148   85  183   89  137  116   78  115  197  125  110  168  139  189  166
#>  [16]  100  118  107  103  115  126   99  196  119  143  125  147   97  145  117
#>  [31]  109  158   88   92  122  103  138  102   90  111  180  133  106  171  159
#>  [46]  180  146   71  103  105  103  101   88  176  150   73  187  100  146  105
#>  [61]   84  133   44  141  114   99  109  109   95  146  100  139  126  129   79
#>  [76] -999   62   95  131  112  113   74   83  101  137  110  106  100  136  107
#>  [91]   80  123   81  134  142  144   92   71   93  122  163  151  125   81   85
#> [106]  126   96  144   83   95  171  155   89   76  160  146  124   78   97   99
#> [121]  162  111  107  132  113   88  120  118  117  105  173  122  170   84   96
#> [136]  125  100   93  129  105  128  106  108  108  154  102   57  106  147   90
#> [151]  136  114  156  153  188  152   99  109   88  163  151  102  114  100  131
#> [166]  104  148  120  110  111  102  134   87   79   75  179   85  129  143  130
#> [181]   87  119 -999   73  141  194  181  128  109  139  111  123  159  135   85
#> [196]  158  105  107  109  148  113  138  108   99  103  111  196  162   96  184
#> [211]   81  147  179  140  112  151  109  125   85  112  177  158  119  142  100
#> [226]   87  101  162  197  117  142  134   79  122   74  171  181  179  164  104
#> [241]   91   91  139  119  146  184  122  165  124  111  106  129   90   86   92
#> [256]  113  111  114  193  155  191  141   95  142  123   96  138  128  102  146
#> [271]  101  108  122   71  106  100  106  104  114  108  146  129  133  161  108
#> [286]  136  155  119   96  108   78  107  128  128  161  151  146  126  100  112
#> [301]  167  144   77  115  150  120  161  137  128  124   80  106  155  113  109
#> [316]  112   99  182  115  194  129  112  124  152  112  157  122  179  102  105
#> [331]  118   87  180  106   95  165  117  115  152  178  130   95 -999  122   95
#> [346]  126  139  116   99 -999   92  137   61   90   90  165  125  129   88  196
#> [361]  189  158  103  146  147   99  124  101   81  133  173  118   84  105  122
#> [376]  140   98   87  156   93  107  105  109   90  125  119  116  105  144  100
#> [391]  100  166  131  116  158  127   96  131   82  193   95  137  136   72  168
#> [406]  123  115  101  197  172  102  112  143  143  138  173   97  144   83  129
#> [421]  119   94  102  115  151  184   94  181  135   95   99   89   80  139   90
#> [436]  141  140  147   97  107  189   83  117  108  117  180  100   95  104  120
#> [451]   82  134   91  119  100  175  135   86  148  134  120   71   74   88  115
#> [466]  124   74   97  120  154  144  137  119  136  114  137  105  114  126  132
#> [481]  158  123   85   84  145  135  139  173   99  194   83   89   99  125   80
#> [496]  166  110   81  195  154  117   84 -999   94   96   75  180  130   84  120
#> [511]   84  139   91   91   99  163  145  125   76  129   68  124  114  130  125
#> [526]   87   97  116  117  111  122  107   86   91   77  132  105   57  127  129
#> [541]  100  128   90   84   88  186  187  131  164  189  116   84  114   88   84
#> [556]  124   97  110  103   85  125  198   87   99   91   95   99   92  154  121
#> [571]   78  130  111   98  143  119  108  118  133  197  151  109  121  100  124
#> [586]   93  143  103  176   73  111  112  132   82  123  188   67   89  173  109
#> [601]  108   96  124  150  183  124  181   92  152  111  106  174  168  105  138
#> [616]  106  117   68  112  119  112   92  183   94  108   90  125  132  128   94
#> [631]  114  102  111  128   92  104  104   94   97  100  102  128  147   90  103
#> [646]  157  167  179  136  107   91  117  123  120  106  155  101  120  127   80
#> [661]  162  199  167  145  115  112  145  111   98  154  165   99   68  123   91
#> [676]  195  156   93  121  101   56  162   95  125  136  129  130  107  140  144
#> [691]  107  158  121  129   90  142  169   99  127  118  122  125  168  129  110
#> [706]   80  115  127  164   93  158  126  129  134  102  187  173   94  108   97
#> [721]   83  114  149  117  111  112  116  141  175   92  130  120  174  106  105
#> [736]   95  126   65   99  102  120  102  109  140  153  100  147   81  187  162
#> [751]  136  121  108  181  154  128  137  123  106  190   88  170   89  101  122
#> [766]  121  126   93
```
