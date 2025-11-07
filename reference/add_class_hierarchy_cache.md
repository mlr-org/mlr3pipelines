# Add a Class Hierarchy to the Cache

Add a class hierarchy to the class hierarchy cache. This is necessary
whenever an S3 class's class hierarchy is important when inferring
compatibility between types.

## Usage

``` r
add_class_hierarchy_cache(hierarchy)
```

## Arguments

- hierarchy:

  `character` the class hierarchy to add; should correspond to the
  [`class()`](https://rdrr.io/r/base/class.html) of the lowest object in
  the hierarchy.

## Value

`NULL`

## See also

Other class hierarchy operations:
[`register_autoconvert_function()`](https://mlr3pipelines.mlr-org.com/reference/register_autoconvert_function.md),
[`reset_autoconvert_register()`](https://mlr3pipelines.mlr-org.com/reference/reset_autoconvert_register.md),
[`reset_class_hierarchy_cache()`](https://mlr3pipelines.mlr-org.com/reference/reset_class_hierarchy_cache.md)

## Examples

``` r
# This lets mlr3pipelines handle "data.table" as "data.frame".
# This is an example and not necessary, because mlr3pipelines adds it by default.

add_class_hierarchy_cache(c("data.table", "data.frame"))
```
