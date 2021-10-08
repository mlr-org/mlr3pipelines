

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
# If `class2` contains `"NULL"` then this returns `TRUE` because any
# class can be converted to `"NULL"`.
#
# `class1` is the class of the "output" of one `PipeOp`, and `class2`
# is the "input" of the following `PipeOp`.
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
  if ("NULL" %in% class2) {
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
  unique(unlist(map(utils::getAnywhere(classname)$objs, function(gen) {
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
  })))
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
#' Add a class hierarchy to the class hierarchy cache. This is necessary
#' whenever an S3 class's class hierarchy is important when inferring
#' compatibility between types.
#'
#' @param hierarchy `character` the class hierarchy to add; should
#'   correspond to the `class()` of the lowest object in the hierarchy.
#' @return `NULL`
#' @family class hierarchy operations
#' @export
#' @examples
#' # This lets mlr3pipelines handle "data.table" as "data.frame".
#' # This is an example and not necessary, because mlr3pipelines adds it by default.
#'
#' add_class_hierarchy_cache(c("data.table", "data.frame"))
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
#' Reset the class hierarchy cache to factory default, thereby undoing
#' any calls to [add_class_hierarchy_cache()] by the user.
#'
#' @return `NULL`
#' @family class hierarchy operations
#' @export
reset_class_hierarchy_cache = function() {
  rm(list = names(class_hierarchy_cache), envir = class_hierarchy_cache)
  for (item in default_chc) {
    # fill hierarchy of things that are not R6 that we should
    # be aware of, e.g. that data.table inherits data.frame.
    add_class_hierarchy_cache(item)
  }
  for (cname in names(autoconvert_register)) {
    # fill hierarchy of things that are R6 and that have an
    # autoconvert function
    get_class_hierarchy(cname)
  }
}

default_chc = list(
  c("data.table", "data.frame")
)

#' @title Add Autoconvert Function to Conversion Register
#'
#' @description
#' Add functions that perform conversion to a desired class.
#'
#' Whenever a [`Graph`] or a [`PipeOp`] is called with an object
#' that does not conform to its declared input type, the "autoconvert
#' register" is queried for functions that may turn the object into
#' a desired type.
#'
#' Conversion functions should try to avoid cloning.
#'
#' @param cls `character(1)` The class that `fun` converts to.
#' @param fun `function` The conversion function. Must take one
#'   argument and return an object of class `cls`, or possibly
#'   a sub-class as recognized by `are_types_compatible()`.
#' @param packages `character` The packages required to be loaded for fun to operate.
#' @return `NULL`.
#' @family class hierarchy operations
#' @export
#' @examples
#' # This lets mlr3pipelines automatically try to convert a string into
#' # a `PipeOp` by querying the [`mlr_pipeops`] [`Dictionary`][mlr3misc::Dictionary].
#' # This is an example and not necessary, because mlr3pipelines adds it by default.
#' register_autoconvert_function("PipeOp", function(x) as_pipeop(x), packages = "mlr3pipelines")
register_autoconvert_function = function(cls, fun, packages = character(0)) {
  assert_string(cls)
  assert_function(fun)
  assert_character(packages, any.missing = FALSE)
  autoconvert_register[[cls]] = list(fun = fun, packages = packages)
}

#' @title Reset Autoconvert Register
#'
#' @description
#' Reset autoconvert register to factory default, thereby undoing
#' any calls to [register_autoconvert_function()] by the user.
#'
#' @return `NULL`
#'
#' @family class hierarchy operations
#' @export
reset_autoconvert_register = function() {
  rm(list = names(autoconvert_register), envir = autoconvert_register)
  for (item in default_acr) {
    # fill autoconvert register with given items
    do.call(register_autoconvert_function, item)
    # fill class hierarchy cache with info about autoconvertible classes
    get_class_hierarchy(item[[1]])
  }
}

default_acr = list(
  # need to put mlr3::assert_X inside functions because we shouldn't refer to mlr3 funs directly at build time
  list("Task", function(x) as_task(x), packages = "mlr3"),
  list("Measure", function(x) as_measure(x), packages = "mlr3"),
  list("Learner", function(x) as_learner(x), packages = "mlr3"),
  list("Resampling", function(x) as_resampling(x), packages = "mlr3"),
  list("PipeOp", function(x) as_pipeop(x), packages = "mlr3pipelines"),
  list("NULL", function(x) NULL)
)

# see add_class_hierarchy_cache()
class_hierarchy_cache = new.env(parent = emptyenv())

# see register_autoconvert_function()
autoconvert_register = new.env(parent = emptyenv())

# the following two must be called after *both* class_hierarchy_cache and autoconvert_register were created
reset_class_hierarchy_cache()
reset_autoconvert_register()

# Get a convert function to target, or one of its super- or subclasses.
# This is needed if there is an object of class `target` required, but
# `object` is not of that class.
#
# If `target` is in autoconvert_register, the autoconversion is applied directly.
# Otherwise, if there is a subclass in the autoconvert_register, the subclass conversion
# is applied. If multiple subclasses are present the one with the shortest path is applied.
# Finally, fi there is a superclass in the autoconvert register it is used.
# @param target `character(1)`
# @return a function with one argument that converts an object to one of class `target`,
#   a subclass, or a superclass, if possible. `NULL` if no such function is found.
get_autoconverter = function(target) {
  # check first if 'target' is in the autoconvert register
  if (target %in% names(autoconvert_register)) {
    return(autoconvert_register[[target]])
  }

  # now check if there is a subclass of `target` that we can convert to.
  # for this we first get the class hierarchy cache entries corresponding to all possible autoconverters
  # convertible_hierarchies is a `list` of `character`s;
  # e.g. if there was a `TaskClassif` converter it could be c("TaskClassif", "Task")
  convertible_hierarchies = mget(names(autoconvert_register), class_hierarchy_cache, ifnotfound = list(NULL))
  # then we filter by all class hierarchies that actually contain `target`
  superclass_hierarchies = Filter(function(x) {
    target %in% x
  }, convertible_hierarchies)

  if (length(superclass_hierarchies)) {
    # if we found a class hierarchy entry we determine the one where `target` is at the earliest position,
    # indicating the shortest class "distance" between conversion result and target
    pathlengths = map_int(superclass_hierarchies, match, x = target)
    return(autoconvert_register[[superclass_hierarchies[[which.min(pathlengths)]][1]]])
  }

  # now check if there is a superclass of `target` that we can convert to.
  target_hierarchy = get_class_hierarchy(target)

  # hierarchy_match contains a number for each autoconvert register entry
  # that is a superclass of `target`, and that number indicates the position
  # in `target_hierarchy`, i.e. the class distance from `target`.
  hierarchy_match = match(names(autoconvert_register), target_hierarchy)

  # superclass_index is the index in `target_hierarchy` for which we found a autoconvert entry
  superclass_index = min(c(hierarchy_match, Inf), na.rm = TRUE)

  if (is.finite(superclass_index)) {
    return(autoconvert_register[[target_hierarchy[superclass_index]]])
  }

  # nothing found

  NULL
}
