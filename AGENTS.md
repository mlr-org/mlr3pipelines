# R package development

## Key commands

```
# To run code
Rscript -e "devtools::load_all(); code"

# To run all tests for files starting with {name}; omit filter to run all tests (expensive).
Rscript -e "devtools::test(filter = '^{name}')"

# To run all tests for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R')"

# To run a single test "blah" for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R', desc = 'blah')"

# To redocument the package
Rscript -e "devtools::document()"

# To check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"

# To check the package with R CMD check
Rscript -e "devtools::check()"
```

## Code Style

* Always use `=` for assignment, never `<-`.
* 2-space indentation, 120-character line limit.
* Block-indent, never visual-indent, i.e. code aligned with opening parentheses.
* `snake_case` for functions and variables, `CamelCase` for R6 classes.
* When calling a function from imported package `foo` do not write `foo::bar()` but `bar()`
* Double quotes for strings, explicit `TRUE`/`FALSE` (never `T`/`F`), explicit `1L` for integers.
* Use implicit return values for functions.
* Prefer `result = if (...) ... else ...` over `if (...) { result = ... } else { result = ... }`
  when the only difference between branches is the assigned value.
* User-facing API (exported functions, public R6 methods) must have `checkmate` `assert_*()` argument checks.
  For internal code, match the existing level of defensiveness.
* Use these mlr3misc utilities when appropriate:
  `map()`, `map_chr()`, `invoke()`, `calculate_hash()`, `str_collapse()`, `%nin%`, `%??%`.
* Before implementing something, read similar existing files first to match the established patterns.
* Always use `# nolint next` to disable linters for the next line instead of `# nolint` on the same line.

## File naming

* Name the file as the most important contained function / class
* Usually one large function / class, per file; if auxiliary functions pertain almost exclusively to that, it they should go in the same file, not an auxiliary file.

## Collation order

* Derived classes must declare `#' @include ParentClass.R` in their roxygen header.
  This controls the `Collate:` field in DESCRIPTION so base classes load before derived classes.

## Testing

* Tests for `R/{name}.R` go in `tests/testthat/test_{name}.R`.
* All new code should have an accompanying test.
* If there are existing tests, place new tests next to similar existing tests.
* Strive to keep your tests minimal with few comments.
* Read the additional important helpers in `inst/testthat/helper_functions.R` to understand our `PipeOpTaskPreproc` auto-test framework.
* The full test suite takes a long time. Only run tests relevant to your changes with `devtools::test(filter = '^{name}')`.
* Shared test infrastructure lives in `inst/testthat/` and is sourced by extension packages too.
* New PipeOps must pass `expect_pipeop_class`, or, in case of preprocessing PipeOps that inherit from `PipeOpTaskPreproc[Simple]`,  `expect_datapreproc_pipeop_class`. The latter calls the former, and both call `expect_pipeop`, which should not be necessary by itself in most cases.
* Use `skip_if_not_installed(<package_name>)` to skip tests that require suggested packages.
* Use shared assertion helpers where sensible: `expect_learner()`, `expect_task()`, `expect_resampling()`, `expect_measure()`, `expect_prediction()`.
* Tests involving the `$man` field, and tests involving parallelization, do not work well when the package is loaded with `devtools::load_all()`, because of conflicts with the installed version. Ignore these failures, CI will take care of this.

## Documentation

* Every user-facing function should be exported and have `roxygen2` documentation.
* Wrap roxygen comments at 120 characters.
* Write one sentence per line.
* If a sentence exceeds the limit, break at a comma, "and", "or", "but", or other appropriate point.
* Internal functions should not have roxygen documentation.
* Always re-document the package after changing a `roxygen2` comment.
* Don’t hand-edit generated artifacts: `man/`, or `NAMESPACE`.
* Never edit `README.md` directly -- it is generated from `README.Rmd`. Always edit `README.Rmd` and then run `devtools::build_readme()` to regenerate `README.md`.
* When adding a new S3 method (such as `print.<ClassName>`), always run `devtools::document()` afterwards to re-generate the NAMESPACE.
* Environment variables and options are documented in package-level documentation (typically `R/package.R`).
* Use `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.
* For functions, always document the return value (section `#' @return`).
* Bibliographic references go in `R/bibentries.R` and are cited with `` `r format_bib("key")` ``.
* Man page names for dictionary objects follow `mlr_learners_classif.rpart`, `mlr_tasks_iris`, etc.
* Wrap parts of examples that use suggested packages in `if (mlr3misc::require_namespaces(<package_names_vector>, quietly = TRUE)) {..}` blocks. When essentially the entire example needs the package, use `#' @examplesIf mlr3misc::require_namespaces(<package_names_vector>, quietly = TRUE)` instead of `#' @examples`, which inserts the `if` automatically.
* Roxygen templates live in `man-roxygen/` (e.g., `@template learner`, `@template param_id`). Use `@templateVar` to pass values.
* If `roxygenize()` / `document()` produce warnings that are unrelated to the code you wrote, ignore them. Do not fix code or formatting that is unrelated to what you are working on, but *do* mention bugs or problems that you noticed it in your final report.
* A very small number of packages listed in `Suggests:` used by some tests / examples is missing; ignore warnings in that regard. You will never be asked to work on things that require these packages.

## Pkgdown

* When adding a new exported function, ensure it's in the `_pkgdown.yml` file.

## `NEWS.md`

* Every user-facing change should be given a bullet in `NEWS.md`.
  Do not add bullets for small documentation changes or internal refactorings.
* Each bullet should briefly describe the change to the end user and mention the related issue in parentheses.
* A bullet can consist of multiple sentences but should not contain any new lines (i.e. DO NOT line wrap).
* If the change is related to a function, put the name of the function early in the bullet.
* Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.


# `mlr3pipelines` architecture

## Dictionary system

Objects are registered in dictionaries and accessed via sugar functions:

| Dictionary            | Sugar                | Example                          |
|-----------------------|----------------------|----------------------------------|
| `mlr_learners`        | `lrn()` / `lrns()`   | `lrn("classif.rpart", cp = 0.1)` |
| `mlr_pipeops`         | `po()` / `pos()`     | `po("pca")`                      |
| `mlr_graphs`          | `ppl()` / `ppls()`   | `ppl("robustify")`               |

etc. New objects must be registered in the respective dictionary; this works differently for external (e.g. `mlr_learners`) than for package-owned (`mlr_pipeops`, `mlr_graphs`) dictionaries.

## Hyperparameters (paradox)

Parameters are defined with `paradox::ps()` and should usually be tagged `"train"`, `"predict"`, or both.

In `.train()` / `.predict()`, retrieve values with `self$param_set$get_values(tags = "train")`.
Use additional tags for additional grouping (e.g. parameters that get passed to different library functions.

There is a distinction between `default` and `init` values:
* `default` describes the behavior when a parameter is not set at all (i.e., the upstream function's default). It is informational only.
* `init` (via `p_xxx(init = ...)`) sets the parameter to a value upon construction. Use this when our own default should differ from the upstream default, or when upstream is `"required"` but there is a reasonable default. For functions that we implement in our own package, this is typically the way we go. The `init` functionality is new, some old code does the initialization differently, via `ps$values = ...`; do not copy the old style.
* A parameter tagged `"required"` causes an error if not set. A required parameter cannot have a `default` (that would be contradictory). For functionality that we implement ourselves, we usually use the `"required"` route.
* paradox does type-checking and range-checking automatically; `get_values()` checks that required params are present. Additional feasibility checks are rarely needed.

## Public fields as active bindings

Public fields on `R6` classes are exposed as active bindings backed by a private `.field`.

For mutable fields, the binding returns the private value when called without arguments and validates the new value with an `assert_*()` call when set. For read-only fields, call `assert_ro_binding(rhs)` to raise an error on any assignment attempt.

## Core dependencies

`data.table`, `checkmate`, `mlr3misc`, `paradox`, `R6`, and `cli` are imported wholesale. Use their functions directly without `::`. Key mlr3misc utilities: `map()`, `map_chr()`, `invoke()`, `calculate_hash()`, `str_collapse()`, `%nin%`, `%??%`.

# Further resources

* Read `R/Graph.R` and `R/GraphLearner.R` to understand the Graph architecture.
* When working on PipeOps, read `R/PipeOp.R` beforehand.
* When working on PipeOps inheriting from `PipeOpTaskPreproc` or `PipeOpTaskPreprocSimple`, read `R/PipeOpTaskPreproc.R` beforehand.
* When commiting changes via git, make sure to read @extra-rules/commit-messages.md and follow its instructions.

