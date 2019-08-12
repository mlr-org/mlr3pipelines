#' @title Selector Functions
#'
#' @name Selector
#'
#' @description
#' Selector functions.
#'
#' @family Selectors
NULL

#' @export
#' @rdname Selector
selector_all = function() function(task) {
    task$feature_names
  }

#' @export
#' @rdname Selector
#'
#' @param types (`character`) \cr
#'   Type of feature to select
selector_type = function(types) {
  assert_character(types, any.missing = FALSE)
  function(task) {
    task$feature_types[get("type") %in% types, get("id")]
  }
}

#' @export
#' @rdname Selector
#'
#' @param pattern (`character(1)`) \cr
#'   grep pattern
#' @param ignore.case (`logical(1)`) \cr
#'   ignore case
#' @param perl (`logical(1)`) \cr
#'   perl regex
#' @param fixed (`logical(1)`) \cr
#'   fixed pattern instead of regex
selector_grep = function(pattern, ignore.case = FALSE, perl = FALSE, fixed = FALSE) {
  assert_string(pattern)
  assert_flag(ignore.case)
  assert_flag(perl)
  assert_flag(fixed)
  function(task) {
    grep(pattern, task$feature_names, ignore.case = ignore.case, perl = perl, fixed = fixed, value = TRUE)
  }
}

#' @export
#' @rdname Selector
#'
#' @param feature_names (`character`) \cr
#'   select features by exact name match
#' @param assert_present (`logical(1)`) \cr
#'   error if `feature_names` are not all present
selector_name = function(feature_names, assert_present = FALSE) {
  assert_character(feature_names, any.missing = FALSE)
  assert_flag(assert_present)
  function(task) {
    if (assert_present) {
      assert_subset(feature_names, task$feature_names)
    }
    intersect(task$feature_names, feature_names)
  }
}
