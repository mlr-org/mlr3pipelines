#' @title Selector Functions
#'
#' @name Selector
#'
#' @description
#' A [`Selector`] function is used by different [`PipeOp`]s, most prominently [`PipeOpSelect`] and many [`PipeOp`]s inheriting
#' from [`PipeOpTaskPreproc`], to determine a subset of [`Task`][mlr3::Task]s to operate on.
#'
#' Even though a [`Selector`] is a `function` that can be written itself, it is preferable to use the [`Selector`] constructors
#' shown here. Each of these can be called with its arguments to create a [`Selector`], which can then be given to the [`PipeOpSelect`]
#' `selector` parameter, or many [`PipeOpTaskPreproc`]s' `affect_columns` parameter. See there for examples of this usage.
#'
#' @section Details:
#' A [`Selector`] is a `function`
#' that has one input argument (commonly named `task`). The function is called with the [`Task`][mlr3::Task] that a [`PipeOp`]
#' is operating on. The return value of the function must be a `character` vector that is a subset of the feature names present
#' in the [`Task`][mlr3::Task].
#'
#' For example, a [`Selector`] that selects all columns is
#' ```r
#' function(task) {
#'   task$feature_names
#' }
#' ```
#' (this is the `selector_all()`-[`Selector`].) A [`Selector`] that selects
#' all columns that have names shorter than four letters would be:
#' ```r
#' function(task) {
#'   task$feature_names[
#'     nchar(task$feature_names) < 4
#'   ]
#' }
#' ```
#' A [`Selector`] that selects only the column `"Sepal.Length"` (as in the [iris task][mlr3::mlr_tasks_iris]), if present, is
#' ```r
#' function(task) {
#'   intersect(task$feature_names, "Sepal.Length")
#' }
#' ```
#'
#' It is preferable to use the [`Selector`] construction functions like `select_type`, `select_grep` etc. if possible, instead of writing custom [`Selector`]s.
#'
#' @return `function`: A [`Selector`] function that takes a [`Task`][mlr3::Task] and returns the feature names to be processed.
#'
#' @family Selectors
#' @examples
#' library("mlr3")
#'
#' iris_task = tsk("iris")
#' bh_task = tsk("boston_housing")
#'
#' sela = selector_all()
#' sela(iris_task)
#' sela(bh_task)
#'
#' self = selector_type("factor")
#' self(iris_task)
#' self(bh_task)
#'
#' selg = selector_grep("a.*i")
#' selg(iris_task)
#' selg(bh_task)
#'
#' selgi = selector_invert(selg)
#' selgi(iris_task)
#' selgi(bh_task)
#'
#' selgf = selector_union(selg, self)
#' selgf(iris_task)
#' selgf(bh_task)
NULL

make_selector = function(fun, description, ...) {
  structure(fun,
    repr = sprintf(description, ...),
    class = c("Selector", "function")
  )
}

# Representation of character vector
# letters[1]   --> '"a"'
# letters[1:2] --> 'c("a", "b")'
char_repr = function(x) {
  output = str_collapse(x, sep = ", ", quote = '"')
  if (length(x) == 0) {
    "character(0)"
  } else if (length(x) == 1) {
    output
  } else {
    sprintf("c(%s)", output)
  }
}

# Representation for a function that may or may not be a `Selector`.
# If it is not, we just use deparse(), otherwise we use the repr as
# reported by that selector.
selector_repr = function(selector) {
  if (test_string(attr(selector, "repr"))) {
    attr(selector, "repr")
  } else {
    str_collapse(deparse(selector), sep = "\n")
  }
}

#' @export
print.Selector = function(x, ...) {
  cat(paste0(attr(x, "repr"), "\n"))
}

#' @describeIn Selector `selector_all` selects all features.
#' @export
selector_all = function() make_selector(function(task) {
    task$feature_names
  }, "selector_all()")

#' @describeIn Selector `selector_none` selects none of the  features.
#' @export
selector_none = function() make_selector(function(task) {
  character(0)
}, "selector_none()")

#' @describeIn Selector `selector_type` selects features according to type. Legal types are listed in `mlr_reflections$task_feature_types`.
#' @param types (`character`) \cr
#'   Type of feature to select
#' @export
selector_type = function(types) {
  assert_character(types, any.missing = FALSE)
  assert_subset(types, mlr_reflections$task_feature_types)
  make_selector(function(task) {
    task$feature_types[get("type") %in% types, get("id")]
  }, "selector_type(%s)", char_repr(types))
}

#' @describeIn Selector `selector_grep` selects features with names matching the `grep()` pattern.
#' @param pattern (`character(1)`) \cr
#'   grep pattern
#' @param ignore.case (`logical(1)`) \cr
#'   ignore case
#' @param perl (`logical(1)`) \cr
#'   perl regex
#' @param fixed (`logical(1)`) \cr
#'   fixed pattern instead of regex
#' @export
selector_grep = function(pattern, ignore.case = FALSE, perl = FALSE, fixed = FALSE) {
  assert_string(pattern)
  assert_flag(ignore.case)
  assert_flag(perl)
  assert_flag(fixed)
  str_ignore_case = if (ignore.case) ", ignore.case = TRUE" else ""
  str_perl = if (perl) ", perl = TRUE" else ""
  str_fixed = if (fixed) ", fixed = TRUE" else ""
  make_selector(function(task) {
    grep(pattern, task$feature_names, ignore.case = ignore.case, perl = perl, fixed = fixed, value = TRUE)
  }, "selector_grep(%s%s%s%s)", pattern, str_ignore_case, str_perl, str_fixed)
}

#' @describeIn Selector `selector_name` selects features with names matching exactly the names listed.
#' @param feature_names (`character`)\cr
#'   Select features by exact name match.
#' @param assert_present (`logical(1)`)\cr
#'   Throw an error if `feature_names` are not all present in the task being operated on.
#' @export
selector_name = function(feature_names, assert_present = FALSE) {
  assert_character(feature_names, any.missing = FALSE)
  assert_flag(assert_present)
  str_assert_present = if (assert_present) ", assert_present = TRUE" else ""
  make_selector(function(task) {
    if (assert_present) {
      assert_subset(feature_names, task$feature_names)
    }
    intersect(task$feature_names, feature_names)
  }, "selector_name(%s%s)", char_repr(feature_names), str_assert_present)
}

#' @describeIn Selector `selector_invert` inverts a given [`Selector`]: It always selects the features
#' that would be *dropped* by the other [`Selector`], and drops the features that
#' would be kept.
#' @param selector ([`Selector`])\cr
#'   [`Selector`] to invert.
#' @export
selector_invert = function(selector) {
  assert_function(selector)
  make_selector(function(task) {
    setdiff(task$feature_names, selector(task))
  }, "selector_invert(%s)", selector_repr(selector))
}

#' @describeIn Selector `selector_intersect` selects the intersection of two [`Selector`]s: Only features
#' selected by both [`Selector`]s are selected in the end.
#' @param selector_x ([`Selector`])\cr
#'   First [`Selector`] to query.
#' @param selector_y ([`Selector`])\cr
#'   Second [`Selector`] to query.
#' @export
selector_intersect = function(selector_x, selector_y) {
  assert_function(selector_x)
  assert_function(selector_y)
  make_selector(function(task) {
    intersect(selector_x(task), selector_y(task))
  }, "selector_intersect(%s, %s)", selector_repr(selector_x), selector_repr(selector_y))
}

#' @describeIn Selector `selector_union` selects the union of two [`Selector`]s: Features
#' selected by either [`Selector`] are selected in the end.
#' @export
selector_union = function(selector_x, selector_y) {
  assert_function(selector_x)
  assert_function(selector_y)
  make_selector(function(task) {
    union(selector_x(task), selector_y(task))
  }, "selector_union(%s, %s)", selector_repr(selector_x), selector_repr(selector_y))
}

#' @describeIn Selector `selector_setdiff` selects the setdiff of two [`Selector`]s: Features
#' selected by `selector_x` are selected, unless they are also selected
#' by `selector_y`.
#' @export
selector_setdiff = function(selector_x, selector_y) {
  assert_function(selector_x)
  assert_function(selector_y)
  make_selector(function(task) {
    setdiff(selector_x(task), selector_y(task))
  }, "selector_setdiff(%s, %s)", selector_repr(selector_x), selector_repr(selector_y))
}

#' @describeIn Selector `selector_missing` selects features with missing values.
#' @export
selector_missing = function() make_selector(function(task) {
  missings = task$missings()
  names(missings)[missings != 0]
}, "selector_missing()")

#' @describeIn Selector `selector_cardinality_greater_than` selects categorical features with cardinality
#' greater then a given threshold.
#' @param min_cardinality (`integer`) \cr
#'   Minimum number of levels required to be selected.
#' @export
selector_cardinality_greater_than = function(min_cardinality) {
  assert_int(min_cardinality)
  make_selector(function(task) {
    levlens = map_int(task$clone(deep = TRUE)$droplevels()$levels(), length)
    names(levlens[levlens > min_cardinality])
  }, "selector_cardinality_greater_than(%s)", min_cardinality)
}
