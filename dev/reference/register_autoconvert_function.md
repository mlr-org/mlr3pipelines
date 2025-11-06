# Add Autoconvert Function to Conversion Register

Add functions that perform conversion to a desired class.

Whenever a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) or a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md) is
called with an object that does not conform to its declared input type,
the "autoconvert register" is queried for functions that may turn the
object into a desired type.

Conversion functions should try to avoid cloning.

## Usage

``` r
register_autoconvert_function(cls, fun, packages = character(0))
```

## Arguments

- cls:

  `character(1)` The class that `fun` converts to.

- fun:

  `function` The conversion function. Must take one argument and return
  an object of class `cls`, or possibly a sub-class as recognized by
  `are_types_compatible()`.

- packages:

  `character` The packages required to be loaded for fun to operate.

## Value

`NULL`.

## See also

Other class hierarchy operations:
[`add_class_hierarchy_cache()`](https://mlr3pipelines.mlr-org.com/dev/reference/add_class_hierarchy_cache.md),
[`reset_autoconvert_register()`](https://mlr3pipelines.mlr-org.com/dev/reference/reset_autoconvert_register.md),
[`reset_class_hierarchy_cache()`](https://mlr3pipelines.mlr-org.com/dev/reference/reset_class_hierarchy_cache.md)

## Examples

``` r
# This lets mlr3pipelines automatically try to convert a string into
# a `PipeOp` by querying the [`mlr_pipeops`] [`Dictionary`][mlr3misc::Dictionary].
# This is an example and not necessary, because mlr3pipelines adds it by default.
register_autoconvert_function("PipeOp", function(x) as_pipeop(x), packages = "mlr3pipelines")
```
