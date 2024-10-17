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

  dupids = row_ids[duplicated(row_ids)]
  newrows = task$nrow + seq_along(dupids)

  cols = unique(unlist(task$col_roles, use.names = FALSE))

  # Save original grp_sizes
  if (!is.null(task$groups)) {
    grp_sizes = table(task$groups$group)
  }

  if (length(dupids)) {
    task$rbind(task$data(rows = dupids, cols = cols))
  }
  # For column with role "group", create new groups for duplicates by adding a suffix
  if (!is.null(task$groups)) {
    group = NULL  # for binding

    t_grps = table(task$groups[dupids]$group)
    n_grps = t_grps / grp_sizes[names(t_grps)]

    for (grp in names(n_grps)) {
      n = n_grps[[grp]]
      grp_size = grp_sizes[[grp]]
      # This relies on correct ordering of rbinded rows ...
      new_group = rep(paste0(grp, "_", seq_len(n)), each = grp_size)
      task$groups[row_id %in% newrows & group == grp, group := new_group]
    }
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

multiplicity_flatten = function(.multip) {
  # returns list(.multip) if .multip is not a Multiplicity
  # Otherwise, it returns a list with all the elements contained in .multip, independent of their nesting level
  if (!is.Multiplicity(.multip)) {
    return(list(.multip))
  }
  unlist(map(.multip, multiplicity_flatten), recursive = FALSE, use.names = FALSE)
}

pos_with_property = function(x, property) {
  x = if (test_class(x, "GraphLearner")) {
    x$graph$pipeops
  } else if(test_class(x, "Graph")) {
    x$pipeops
  } else {
    x
  }
  keep(x, function(po) property %in% po$properties)
}

assert_po_validate = function(rhs) {
  assert_choice(rhs, "predefined", null.ok = TRUE)
}

test_po_validate = function(x) {
  test_choice(x, "predefined", null.ok = TRUE)
}
