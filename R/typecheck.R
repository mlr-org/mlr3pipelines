

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
  rm(names(class_hierarchy_cache), envir = class_hierarchy_cache)
  for (item in default_chc) {
    add_class_hierarchy_cache(item)
  }
}

default_chc = list(
  c("data.table", "data.frame")
)

class_hierarchy_cache = new.env(parent = emptyenv())
