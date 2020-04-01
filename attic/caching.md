# Caching

These docs describe `oportunistic caching`, i.e. caching after a first function call.
If the same function is executed twice in parallel, this does not save any time/cores.
The example currently uses the `R.cache` package by Henrik Bengtsson for caching.
This is just a very simple caching package, that provides a clean, simple API, could
theoretically be replaced by other packages.


## Implementation Details

Ideally we would like to do caching on an abstract level,
perhaps within the PipeOp base-classes `$train` function.
A very nice point would be to wrap the call to `private$.train`.
This would make complexity very manageable.

`R.cache::evalWithMemoization` memoizes the provided expression.
The `hash` is computed from its `key` argument.


Possible solution: adjust `PipeOp` in `PipeOp.R`
```
train = function(input) {
      ...
      t = check_types(self, input, "input", "train")
      # caching
      R.cache::evalWithMemoization({
        result = list(private$.train(input), self$state) #(see A below)
        }, key = list(map_chr(input, get_hash), self$hash)
      )
      if (is.null(self$state)) state = result$state #(see A below)
      output = check_types(self, result$output, "output", "train")
      output
    },
    predict = function(input) {
      ...
      input = check_types(self, input, "input", "predict")
      R.cache::evalWithMemoization({
        output = private$.predict(input)
        },
         key = list(map_chr(input, get_hash), self$hash)
      )
      output = check_types(self, output, "output", "predict")
      output
    }
  ),
```

where `get_hash` is:
```
get_hash = function(x) {
  if (!is.null(x$hash)) return(x$hash)
    digest(x, algo = "xxhash64")
}
```

## Possible problems:

A) Unfortunately `private$.train()` is not a pure function, but
   instead has side-effects:
    - sets a `$state`
    - ... (others?)

If we can ensure that the only side-effect of `$.train` is a modified state, 
we could also memoize the state during `$train` (see above).

If other fields are updated, we need to have a list of fields that are updated or go a different route.

## Further Issues:

F) Should caching be optional? 
   Probably yes!

G) How do we globally enable/disable caching?
    1. global option
    < ugly, might not work with parallelization. >

    2. caching can be turned on in `Graph` | `GraphLearner`
    ```
    Graph = R6Class(
     ...
     caching = TRUE,
     ...
    )
    ```
    `GraphLearner` gets an active binding to turn caching of it's graph on/off.
    Could also be added as an arg to the `GraphLearner`s constructor.

    The caching of individual steps is then done by adjusting calls to `graph_reduce`:
    `graph_reduce(..., caching = self$caching)`

H) Should caching be disabled for some `PipeOp`s?
   Yes, possible solution: New field in each `PipeOp`: `cached`.
   Caching for a pipeop only happens if `cached = TRUE`.
   Can also be manually changed to disable caching for any pipeop.

Open Questions:
  - How do `$train` and `$predict` know whether to do caching or not?
    Add a second argument `caching`?
  - How do caching and `parallelization` interact?
  - Does `R.cache::evalWithMemoization`s `key` arg need anything else?
