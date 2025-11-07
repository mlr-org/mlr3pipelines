# Selector Functions

A `Selector` function is used by different
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s, most
prominently
[`PipeOpSelect`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_select.md)
and many
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s
inheriting from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
to determine a subset of
[`Task`](https://mlr3.mlr-org.com/reference/Task.html)s to operate on.

Even though a `Selector` is a `function` that can be written itself, it
is preferable to use the `Selector` constructors shown here. Each of
these can be called with its arguments to create a `Selector`, which can
then be given to the
[`PipeOpSelect`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_select.md)
`selector` parameter, or many
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md)s'
`affect_columns` parameter. See there for examples of this usage.

## Usage

``` r
selector_all()

selector_none()

selector_type(types)

selector_grep(pattern, ignore.case = FALSE, perl = FALSE, fixed = FALSE)

selector_name(feature_names, assert_present = FALSE)

selector_invert(selector)

selector_intersect(selector_x, selector_y)

selector_union(selector_x, selector_y)

selector_setdiff(selector_x, selector_y)

selector_missing()

selector_cardinality_greater_than(min_cardinality)
```

## Arguments

- types:

  (`character`)  
  Type of feature to select

- pattern:

  (`character(1)`)  
  grep pattern

- ignore.case:

  (`logical(1)`)  
  ignore case

- perl:

  (`logical(1)`)  
  perl regex

- fixed:

  (`logical(1)`)  
  fixed pattern instead of regex

- feature_names:

  (`character`)  
  Select features by exact name match.

- assert_present:

  (`logical(1)`)  
  Throw an error if `feature_names` are not all present in the task
  being operated on.

- selector:

  (`Selector`)  
  `Selector` to invert.

- selector_x:

  (`Selector`)  
  First `Selector` to query.

- selector_y:

  (`Selector`)  
  Second `Selector` to query.

- min_cardinality:

  (`integer`)  
  Minimum number of levels required to be selected.

## Value

`function`: A `Selector` function that takes a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) and returns the
feature names to be processed.

## Functions

- `selector_all()`: `selector_all` selects all features.

- `selector_none()`: `selector_none` selects none of the features.

- `selector_type()`: `selector_type` selects features according to type.
  Legal types are listed in `mlr_reflections$task_feature_types`.

- `selector_grep()`: `selector_grep` selects features with names
  matching the [`grep()`](https://rdrr.io/r/base/grep.html) pattern.

- `selector_name()`: `selector_name` selects features with names
  matching exactly the names listed.

- `selector_invert()`: `selector_invert` inverts a given `Selector`: It
  always selects the features that would be *dropped* by the other
  `Selector`, and drops the features that would be kept.

- `selector_intersect()`: `selector_intersect` selects the intersection
  of two `Selector`s: Only features selected by both `Selector`s are
  selected in the end.

- `selector_union()`: `selector_union` selects the union of two
  `Selector`s: Features selected by either `Selector` are selected in
  the end.

- `selector_setdiff()`: `selector_setdiff` selects the setdiff of two
  `Selector`s: Features selected by `selector_x` are selected, unless
  they are also selected by `selector_y`.

- `selector_missing()`: `selector_missing` selects features with missing
  values.

- `selector_cardinality_greater_than()`:
  `selector_cardinality_greater_than` selects categorical features with
  cardinality greater then a given threshold.

## Details

A `Selector` is a `function` that has one input argument (commonly named
`task`). The function is called with the
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) that a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) is
operating on. The return value of the function must be a `character`
vector that is a subset of the feature names present in the
[`Task`](https://mlr3.mlr-org.com/reference/Task.html).

For example, a `Selector` that selects all columns is

    function(task) {
      task$feature_names
    }

(this is the `selector_all()`-`Selector`.) A `Selector` that selects all
columns that have names shorter than four letters would be:

    function(task) {
      task$feature_names[
        nchar(task$feature_names) < 4
      ]
    }

A `Selector` that selects only the column `"Sepal.Length"` (as in the
[iris task](https://mlr3.mlr-org.com/reference/mlr_tasks_iris.html)), if
present, is

    function(task) {
      intersect(task$feature_names, "Sepal.Length")
    }

It is preferable to use the `Selector` construction functions like
`select_type`, `select_grep` etc. if possible, instead of writing custom
`Selector`s.

## See also

Other Selectors:
[`mlr_pipeops_select`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_select.md)

## Examples

``` r
library("mlr3")

iris_task = tsk("iris")
bh_task = tsk("boston_housing")

sela = selector_all()
sela(iris_task)
#> [1] "Petal.Length" "Petal.Width"  "Sepal.Length" "Sepal.Width" 
sela(bh_task)
#>  [1] "age"     "b"       "chas"    "crim"    "dis"     "indus"   "lat"    
#>  [8] "lon"     "lstat"   "nox"     "ptratio" "rad"     "rm"      "tax"    
#> [15] "town"    "tract"   "zn"     

self = selector_type("factor")
self(iris_task)
#> character(0)
self(bh_task)
#> [1] "chas" "town"

selg = selector_grep("a.*i")
selg(iris_task)
#> [1] "Petal.Width" "Sepal.Width"
selg(bh_task)
#> [1] "ptratio"

selgi = selector_invert(selg)
selgi(iris_task)
#> [1] "Petal.Length" "Sepal.Length"
selgi(bh_task)
#>  [1] "age"   "b"     "chas"  "crim"  "dis"   "indus" "lat"   "lon"   "lstat"
#> [10] "nox"   "rad"   "rm"    "tax"   "town"  "tract" "zn"   

selgf = selector_union(selg, self)
selgf(iris_task)
#> [1] "Petal.Width" "Sepal.Width"
selgf(bh_task)
#> [1] "ptratio" "chas"    "town"   
```
