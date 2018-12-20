#' @title ClassFoo
#' @description This is the description of class foo.
#' @section Usage:
#' Inherits from [BaseClass]
#' \tabular{ll}{
#'   Usage                        \tab Data Types \cr
#'   `$new(a_very_long_argument_ssssssssssssssssssssssssssssss, nr)`              \tab `character(1)`, `logical(1)` -> [ClassFoo]  \cr
#'   `$member_var`                \tab `character(1)` \cr
#'   `$compute_stuff(foo)`        \tab `logical(1)` -> `numeric(1)` \cr
#'   `$mutate(foo, paramset)`     \tab `[Foo]`, [Foo], [`Foo`], [paradox::paramset] -> `self` \cr
#' }
#'
#' @section Usage2:
#' Inherits from [BaseClass]
#' * f = ClassFoo$new(nr, name) \cr
#'    `logical(1)`, `numeric(1)` -> [ClassFoo]
#' * `f$compute_stuff(foo = 123)` \cr
#'    `logical(1)` -> `numeric(1)`
#' * `f$compute_stuff(foo)` \cr
#'    `logical(1)` -> `numeric(1)`
#'
#' @section Details:
#' * `new()`: Constructs the object from a name string and a number.
#' * `member_var`: A really nice descriptive string.
#' * `compute_stuff()`: Computes a nice number.
#' * `mutate()`: Changes our object to something better.
#'
#' @section Arguments:
#' * `bla`: A string name, which must have more than 3 chars and can only be lowercase.
#'
#'
#' @name ClassFoo
#' @family foo_and_bar_stuff
#'
#' @examples
#' f = ClassFoo$new("hello", 123)
#' f$compute_stuff(10)
NULL

