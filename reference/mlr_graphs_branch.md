# Branch Between Alternative Paths

Create a multiplexed graph.

All input arguments are cloned and have no references in common with the
returned
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).

## Usage

``` r
pipeline_branch(graphs, prefix_branchops = "", prefix_paths = FALSE)
```

## Arguments

- graphs:

  `list` of
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)  
  Multiple graphs, possibly named. They all must have exactly one
  output. If any of the arguments are named, then all must have unique
  names.

- prefix_branchops:

  `character(1)`  
  Optional id prefix to prepend to
  [`PipeOpBranch`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_branch.md)
  and
  [`PipeOpUnbranch`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_unbranch.md)
  id. Their resulting IDs will be `"[prefix_branchops]branch"` and
  `"[prefix_branchops]unbranch"`. Default is `""`.

- prefix_paths:

  `logical(1)` \| `character(1)`  
  Whether to add prefixes to graph IDs when performing gunion. Can be
  helpful to avoid ID clashes in resulting graph. Default `FALSE`. If
  this is `TRUE`, the prefixes are taken from the names of the input
  arguments if present or `"poX"` where X counts up. If this is a
  `character(1)`, it is a prefix that is added to the `PipeOp` IDs
  *additionally* to the input argument list.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)

## Examples

``` r
library("mlr3")

po_pca = po("pca")
po_nop = po("nop")

branches = pipeline_branch(list(pca = po_pca, nothing = po_nop))
# gives the same as
branches = c("pca", "nothing")
po("branch", branches) %>>%
  gunion(list(po_pca, po_nop)) %>>%
  po("unbranch", branches)
#> 
#> ── Graph with 4 PipeOps: ───────────────────────────────────────────────────────
#>        ID         State sccssors prdcssors
#>    <char>        <char>   <char>    <char>
#>    branch <<UNTRAINED>>  pca,nop          
#>       pca <<UNTRAINED>> unbranch    branch
#>       nop <<UNTRAINED>> unbranch    branch
#>  unbranch <<UNTRAINED>>            pca,nop
#> 
#> ── Pipeline: non-sequential 

pipeline_branch(list(pca = po_pca, nothing = po_nop),
  prefix_branchops = "br_", prefix_paths = "xy_")
#> 
#> ── Graph with 4 PipeOps: ───────────────────────────────────────────────────────
#>              ID         State               sccssors              prdcssors
#>          <char>        <char>                 <char>                 <char>
#>       br_branch <<UNTRAINED>> xy_pca.pca,xy_nothi...                       
#>      xy_pca.pca <<UNTRAINED>>            br_unbranch              br_branch
#>  xy_nothing.nop <<UNTRAINED>>            br_unbranch              br_branch
#>     br_unbranch <<UNTRAINED>>                        xy_pca.pca,xy_nothi...
#> 
#> ── Pipeline: non-sequential 
# gives the same as
po("branch", branches, id = "br_branch") %>>%
  gunion(list(xy_pca = po_pca, xy_nothing = po_nop)) %>>%
  po("unbranch", branches, id = "br_unbranch")
#> 
#> ── Graph with 4 PipeOps: ───────────────────────────────────────────────────────
#>              ID         State               sccssors              prdcssors
#>          <char>        <char>                 <char>                 <char>
#>       br_branch <<UNTRAINED>> xy_pca.pca,xy_nothi...                       
#>      xy_pca.pca <<UNTRAINED>>            br_unbranch              br_branch
#>  xy_nothing.nop <<UNTRAINED>>            br_unbranch              br_branch
#>     br_unbranch <<UNTRAINED>>                        xy_pca.pca,xy_nothi...
#> 
#> ── Pipeline: non-sequential 
```
