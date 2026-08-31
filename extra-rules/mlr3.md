### Architecture

#### Dictionary system

Objects are registered in dictionaries and accessed via sugar functions:

| Dictionary            | Sugar                | Example                          |
|-----------------------|----------------------|----------------------------------|
| `mlr_learners`        | `lrn()` / `lrns()`   | `lrn("classif.rpart", cp = 0.1)` |
| `mlr_pipeops`           | `po()` / `pos()`   | `po("pca")`                    |

etc. New objects must be registered in the respective dictionary; this works differently for external (e.g. `mlr_learners`) than for package-owned (`mlr_pipeops`) dictionaries.

#### Collation order

Derived classes must declare `#' @include ParentClass.R` in their roxygen header. This controls the `Collate:` field in DESCRIPTION so base classes load before derived classes.

#### Hyperparameters (paradox)

Parameters are defined with `paradox::ps()` and should usually be tagged `"train"`, `"predict"`, or both.

In `.train()` / `.predict()`, retrieve values with `self$param_set$get_values(tags = "train")`.
Use additional tags for additional grouping (e.g. parameters that get passed to different library functions.

There is a distinction between `default` and `init` values:
- `default` describes the behavior when a parameter is not set at all (i.e., the upstream function's default). It is informational only.
- `init` (via `p_xxx(init = ...)`) sets the parameter to a value upon construction. Use this when our own default should differ from the upstream default, or when upstream is `"required"` but there is a reasonable default. For functions that we implement in our own package, this is typically the way we go. The `init()` functionality is new, some old code does the initialization differently, via `ps$values = ...`; do not copy the old style.
- A parameter tagged `"required"` causes an error if not set. A required parameter cannot have a `default` (that would be contradictory). For functionality that we implement ourselves, we usually use the `"required"` route.
- paradox does type-checking and range-checking automatically; `get_values()` checks that required params are present. Additional feasibility checks are rarely needed.

#### Public fields as active bindings

Public fields on R6 classes are exposed as active bindings backed by a private `.field`.

For mutable fields, the binding returns the private value when called without arguments and validates the new value with an `assert_*()` call when set. For read-only fields, call `assert_ro_binding(rhs)` to raise an error on any assignment attempt:


#### Core dependencies

`data.table`, `checkmate`, `mlr3misc`, `paradox`, `R6`, and `cli` are imported wholesale. Use their functions directly without `::`. Key mlr3misc utilities: `map()`, `map_chr()`, `invoke()`, `calculate_hash()`, `str_collapse()`, `%nin%`, `%??%`.

### Testing

- The full test suite takes a long time. Only run tests relevant to your changes with `devtools::test(filter = '^{name}')`.
- New PipeOps must pass `expect_pipeop_class`, or, in case of preprocessing PipeOps that inherit from `PipeOpTaskPreproc[Simple]`,  `expect_datapreproc_pipeop_class`. The latter calls the former, and both call `expect_pipeop`, which should not be necessary by itself in most cases.
- Use shared assertion helpers: `expect_learner()`, `expect_task()`, `expect_resampling()`, `expect_measure()`, `expect_prediction()`.
- Shared test infrastructure lives in `inst/testthat/` and is sourced by extension packages too.
- Use `skip_if_not_installed(<package_name>)` to skip tests that require suggested packages.

### Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Wrap roxygen comments at 120 characters.
- Write one sentence per line.
- If a sentence exceeds the limit, break at a comma, "and", "or", "but", or other appropriate point.
- Internal functions should not have roxygen documentation.
- Whenever you add a new (non-internal) documentation topic, also add the topic to `_pkgdown.yml`.
- Always rerefresh documentation via `roxygen2::roxygenize()` after changing a roxygen2 comment.
- Use `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.
- Roxygen templates live in `man-roxygen/` (e.g., `@template learner`, `@template param_id`). Use `@templateVar` to pass values.
- Bibliographic references go in `R/bibentries.R` and are cited with `` `r format_bib("key")` ``.
- Man page names for dictionary objects follow `mlr_learners_classif.rpart`, `mlr_tasks_iris`, etc.
- Wrap parts of examples that use suggested packages in `if (mlr3misc::require_namespaces(<package_names_vector>, quietly = TRUE)) {..}` blocks. When essentially the entire example needs the package, use `#' @examplesIf mlr3misc::require_namespaces(<package_names_vector>, quietly = TRUE)` instead of `#' @examples`, which inserts the `if` automatically.

### `NEWS.md`

- Every user-facing change should be given a bullet in `NEWS.md`. Do not add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describe the change to the end user and mention the related issue in parentheses.
- A bullet can consist of multiple sentences but should not contain any new lines (i.e. DO NOT line wrap).
- If the change is related to a function, put the name of the function early in the bullet.
- Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.
