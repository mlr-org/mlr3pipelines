rep_suffix = function(x, n) {
  # priority here is "easy to enter by hand", not "can reasonably be sorted alphabetically" which NEVER happens
  sprintf("%s%s", x, seq_len(n))
}

calculate_collimit = function(colwidths, outwidth) {
  margin = length(colwidths) + 4  # columns are separated by one space, with some breathing room
  numcols = length(colwidths)  # number of columns that we expect to limit
  repeat {
    # collimit: the width at which we limit data.table column output. If some columns are very
    # small, we can be more generous for other columns.
    collimit = floor((outwidth - margin) / numcols)
    violating = colwidths > collimit - 2
    if (sum(violating) >= numcols) {
      break
    }
    margin = length(colwidths) + 4 + sum(colwidths[!violating])
    numcols = sum(violating)
    if (numcols == 0) {
      collimit = outwidth
      break
    }
  }
  collimit - 3  # subtracting 3 here because data.table adds "..." whenever it truncates a string
}

# same as task$filter(), but allows duplicate row IDs
# @param task [Task] the task
# @param row_ids [numeric] the row IDs to select
# @return [Task] the modified task
task_filter_ex = function(task, row_ids) {

  addedrows = row_ids[duplicated(row_ids)]

  newrows = task$nrow + seq_along(addedrows)

  if (length(addedrows)) {
    task$rbind(task$data(rows = addedrows))
  }

  # row ids can be anything, we just take what mlr3 happens to assign.
  row_ids[duplicated(row_ids)] = task$row_ids[newrows]

  task$filter(row_ids)
}

# these must be at the root and can not be anonymous functions because all.equal fails otherwise.
check_function_or_null = function(x) check_function(x, null.ok = TRUE)
check_numeric_valid_threshold = function(x) check_numeric(x, any.missing = FALSE, min.len = 1, lower = 0, upper = 1)
# function that checks whether something is either of class `cls` or in the
# dictionary `dict` (in which case it is assumed the right class is created).
check_class_or_character <- function(cls, dict) {
  function(x) {
    if (is.character(x)) {
      check_choice(x, dict$keys())
    } else {
      check_measure(x, class = cls)
    }
  }
}
curry = function(fn, ..., varname = "x") {
  arguments = list(...)
  function(x) {
    arguments[[varname]] = x
    do.call(fn, arguments)
  }
}


# 'and' operator for checkmate check_*-functions
# example:
# check_numeric(x) %check&&% check_true(all(x < 0))
`%check&&%` = function(lhs, rhs) {
  if (!isTRUE(lhs) && !isTRUE(rhs)) return(paste0(lhs, ", and ", rhs))
  if (isTRUE(lhs)) rhs else lhs
}
# check_numeric(x) %check||% check_character(x)
`%check||%` = function(lhs, rhs) {
  if (!isTRUE(lhs) && !isTRUE(rhs)) return(paste0(lhs, ", or ", rhs))
  TRUE
}


# perform gsub on names of list
# `...` are given to `gsub()`
rename_list = function(x, ...) {
  names(x) = gsub(x = names(x), ...)
  x
}

# clone a learner and set a state
clone_with_state = function(learner, state) {
  lrn = learner$clone(deep = TRUE)
  lrn$state = state
  lrn
}

#' @include multiplicity.R
multiplicity_recurse = function(.multip, .fun, ...) {
  if (is.Multiplicity(.multip)) {
    as.Multiplicity(lapply(.multip, function(m) multiplicity_recurse(.multip = m, .fun = .fun, ...)))
  } else {
    .fun(.multip, ...)
  }
}

#' @title A Quick Way to Initialize Objects from Dictionaries with Incremented ID
#' @description
#' Covenience wrapper around [mlr3misc::dictionary_sugar_get] and [mlr3misc::dictionary_sugar_inc_mget]
#' to allow easier avoidance of of ID clashes which is for example useful,
#' when multiple instances of the [PipeOp] are in one [Graph].
#' Let `<key>` be the key of the objet to retrieve. When passing the `<key>_<n>` to this
#' function, where `<n>` is any natural numer, the object with key <key> is retrieved and the
#' suffix `_<n>` is appended to the id after the object is constructed.
#'
#' @param dict ([Dictionary])\cr
#'   Dictionary from which to retrieve an element.
#' @param .key (`character(1)`)\cr
#'   Key of the object to construct - possibly with a suffix like `_n` to modify its id.
#' @param .keys (`character()`)\cr
#'   Keys of the objects to construct - possibly with a suffix like `_n` to modify its id.
#' @param ... (any)\cr
#'   See description of [mlr3misc::dictionary_sugar].
#'
#' @return An element from the dictionary.
#'
#' @examples
#' mlr3misc::dictionary_sugar_get(mlr_pipeops, "pca", id = "pca_1")
#' # is the same as
#' dictionary_sugar_inc_get(mlr_pipeops, "pca_1")
#' # multiple pipeops
#' dictionary_sugar_inc_mget(mlr_pipeops, c("pca_1", "pca_2"))
#'
#' @rdname inc_get
#' @export
dictionary_sugar_inc_get = function(dict, .key, ...) {
  newkey = gsub("_\\d+$", "", .key)
  add_suffix = .key != newkey
  if (add_suffix) {
    assert_true(!hasArg("id"))
    suffix = gsub(newkey, "", .key)
  }
  obj = mlr3misc::dictionary_sugar_get(dict = dict, .key = newkey, ...)

  if (add_suffix) {
    obj$id = paste0(obj$id, suffix)
  }
  obj

}

#' @name inc_get
#' @export
dictionary_sugar_inc_mget = function(dict, .keys, ...) {
  objs = lapply(.keys, dictionary_sugar_inc_get, dict = dict, ...)
  if (!is.null(names(.keys))) {
    nn = names2(.keys)
    ii = which(!is.na(nn))
    for (i in ii) {
      objs[[i]]$id = nn[i]
    }
  }
  names(objs) = map_chr(objs, "id")
  objs
}
