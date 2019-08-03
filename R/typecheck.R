

# Check whether Classes are Compatible
#
# This is a permissible test to check whether `$input` type of one
# `PipeOp` and `$output` type of another `PipeOp` could in principle
# agree. It is a symmetric check if `class1` and `class2` have overlap
# in their class hierarchies.
#
# Class hierarchies are checked using the "`class_hierarchy_cache`",
# which in particular contains the class hierarchies in `default_chc`,
# and the R6 class hierarchy using `get_r6_inheritance()`.
#
# If any of the arguments are `"*"`, this returns `TRUE`. It furthermore
# returns `TRUE` if there is any overlap in the class hierarchy. Otherwise
# it returns `FALSE`.
#
# It may be useful to check that the input class of one `PipeOp` is a strict
# superclass of the output class of the other `PipeOp`, but that is not done
# here (yet).
#
# @param class1 `character(1)` One class to check
# @param class2 `character(1)` Other class to check
# @return `logical(1)`
are_types_compatible = function(class1, class2) {
  assert_string(class1)
  assert_string(class2)
  if ("*" %in% c(class1, class2)) {
    return(TRUE)
  }
  ch1 = unique(get_class_hierarchy(class1))
  ch2 = unique(get_class_hierarchy(class2))
  cpi = intersect(ch1, ch2)

  length(cpi) == min(length(ch1), length(ch2))
}

# Try to find the `character` vector of superclass classnames
# @param classname [character(1)] the class name; the search path is searched for
#   an R6ClassGenerator by this name, and its super-classes are
get_r6_inheritance = function(classname) {
  if (!exists(classname, mode = "environment")) {
    return(NULL)
  }
  gen = get(classname, mode = "environment")
  if ("R6ClassGenerator" %nin% class(gen)) {
    return(NULL)
  }
  recurse_classname = function(gen) {
    if (is.null(gen)) {
      return(NULL)
    }
    c(gen$classname, recurse_classname(gen$get_inherit()))
  }
  recurse_classname(gen)
}

get_class_hierarchy = function(classname) {
  if (exists(classname, envir = class_hierarchy_cache, inherits = FALSE)) {
    return(get(classname, envir = class_hierarchy_cache, inherits = FALSE))
  }
  r6class = get_r6_inheritance(classname)
  if (!is.null(r6class)) {
    add_class_hierarchy_cache(r6class)
  }
  r6class %??% classname
}

#' @title Add a Class Hierarchy to the Cache
#'
#' @description
#' Add a class hierarchy to the class hierarchy cache.
#'
#' @param hierarchy `character(1)` the class hieararchy to add
#' @family class hierarchy operations
#' @export
add_class_hierarchy_cache = function(hierarchy) {
  assert_character(hierarchy, any.missing = FALSE, min.len = 1)
  class_hierarchy_cache[[hierarchy[1]]] = hierarchy
  if (length(hierarchy) > 1) {
    add_class_hierarchy_cache(hierarchy[-1])
  }
}

#' @title Reset the Class Hierarchy Cache
#'
#' @description
#' Reset the class hierarchy cache to factory default.
#'
#' @family class hierarchy operations
#' @export
reset_class_hierarchy_cache = function() {
  rm(list = names(class_hierarchy_cache), envir = class_hierarchy_cache)
  for (item in default_chc) {
    add_class_hierarchy_cache(item)
  }
}

default_chc = list(
  c("data.table", "data.frame")
)

class_hierarchy_cache = new.env(parent = emptyenv())
reset_class_hierarchy_cache()
