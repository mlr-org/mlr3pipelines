# Documentation style guide

## Style guide for entities:

* **slot description in `@section Public Members / Active Bindings`**: name in backticks, followed by ` :: ` (with spaces; possibly multiple spaces for vertical alignment), followed by type description, followed by `\cr`.

  `` `id` :: `character(1)` \cr``
  
  `` `output` :: [`data.table`] with columns `name`, `train`, `predict` \cr``

* **method description in `@section Methods`**: function's formals in backticks, with  default values if applicable, followed by `\cr`. Class methods (usually `Class$new`) prepended with class name and dollar sign.

  `` `Graph$new()` \cr``
  
  `` `add_edge(src_id, dst_id, src_channel = NULL, dst_channel = NULL)` \cr``
  
* **method type description in `@section Methods`**: classes in backticks (linked if not default R package classes; possibly with length given in parentheses if vector type), alternative classes separated by vertical bars (`|`), different arguments comma-separated, all surrounded by parentheses. Spaces before and after `|`, after `,`, but not after `(` or before `)` or `,`. Followed by arrow (` -> ` with spaces), followed by return type, followed by `\cr`. Functions without input ("nullary functions") have `()` in-type, functions with `invisible(NULL)` (e.g. `print`, `plot`) have `` `NULL` `` return type.

  ``() -> `NULL` \cr``
  
  ``(`character(1)` | `NULL`, `character`, named `list`) -> [`data.table`] with columns `id`, `state` \cr``

* **R6, S3 class names**: Backtick-quoted, possibly linked if mentioned for the first time *or* in a member var type description / function type description, but *not* linked if in base R or in an R default package.

  ``Many [`Graph`]s``
  
  ``The `Graph`'s size``
  
  ``This is a [`data.table`], not a `data.frame` ``

* **member variable / AB slot names**: Backtick-quoted. Prepended by dollar sign *except* in their description line in `@section Public Members / Active Bindings`.

  ``The `Graph`'s `$id` ``
  
* **method names**: Backtick-quoted. Prepended by dollar sign, followed by `()`, exception is their description line in `@section Methods`.

  ``Use `$add_pipeop()` to...``

* **self** when referring to return value of a mutator: Backtick quoted. Otherwise use the class name instead.

  ``(`character`) -> `self` ``
  
  ``The `Graph`'s `$id` ``, *not* `` `self$id` ``

* **any** when referring to any possible type: Backtick quoted.

  `` `any` ``
  
* **literal strings**: Backtick *and* double quoted:

  ``The default ID is `"pca"` ``

* **column names** of `data.table`s / `data.frame`s: Backtick quoted, comma separated, without "and" if in a member var type description / function type description.

  `` `output` :: `data.table` with columns `name`, `train`, `predict` \cr``

  ``The table's `name` column contains...``
  
* **functions** that are not R6 class methods: Followed by `()`, and in  brackets (`[]`).

  ``Use [print()] to...``, but ``Use `$add_pipeop()` to...``

The following should *not* be backtick-quoted:

* 'named' in `` named `list` `` and similar
* parentheses, vertical bars, arrows, equal signs in function type description

The following should be linked (i.e. put in []):

* Every function that is not an R6 method.
* First mention of any type / class that is not in R default packages (The "first mention" *can* be an occurrence in a type description; if the class is mentioned later it shouldn't be linked any more).
* Every type, class, function that is not in R default packages in a member var type description or function type description.

Don't link things like "character(1)" or "character" because (1) it would be silly and (2) it would lead to an inconsistent typeface.

## @family

The `@family` tag creates a group of documentation pages that mutually link each other. Writing `@family <TEXT>` will create the line "Other \<TEXT\>: \[link\] \[link\] \[link\]". The following rules for this:

* Family \<TEXT\> should be short but is allowed to, and should probably, contain spaces. It should make a natural sentence when written as "Other \<TEXT\>:".
* A page can be member of multiple families if that is natural.
* Do not create families with only one member.

## Example

Syntax

* `<>`: object dependent strings.
* `{{}}`: optional strings

```
#' @title <classname>
#' @format [R6Class] <classname>{{[. Inherits from [<superclass>]}}
#'
#' @description
#' Long form description. Mentions [`PipeOp`] once. Mentions `PipeOp` twice.
#'
#' Another paragraph in long form description.
#'
#' @section Public Members / Active Bindings:
#' * `<varname>`    :: named `list` of [`Graph`] \cr
#'   Description of `$<varname>`. Mentions `Graph`, but doesn't link it, because
#'   this is not the first mention of `Graph`.
#' * `<varname2>`   :: `any` \cr
#'   Description of `$<varname2>`. Notice how a dollar-sign is used when referring
#'   to slots.
#'
#' @section Methods:
#' * `<classname>$new(x, y)` \cr
#'   (`character` | `NULL`, `numeric(1)`) -> `self` \cr
#'   The `$new()` method always returns `self`.
#' * `mutate(x = default)` \cr
#'   (`any`) -> `self` \cr
#'   When referring to `$mutate()` and other methods, always use the dollar sign
#'   and an empty pair of parentheses.
#' * `produce()` \cr
#'   () -> [`PipeOp`] \cr
#'   Description of `$produce()` function.
#'
#' @examples
#' g = Graph$new()
#'
#' @family mlr3pipelines backend related
#' @export
```
