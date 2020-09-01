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
curry = function(fn, ..., varname = "x") {
  arguments = list(...)
  function(x) {
    arguments[[varname]] = x
    do.call(fn, arguments)
  }
}

# 'and' operator for checkmate check_*-functions
# example:
# check_numeric(x) %&&% check_true(all(x < 0))
`%check&&%` = function(lhs, rhs) {
  if (isTRUE(lhs)) rhs else lhs
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

# Check whether an object is a measure or convertable to one via `msr()`
check_measure = function(x, class = "Measure") {
  if (is.character(x)) {
    if (x %nin% mlr_measures$keys()) "Is a `character` but not a known measure" else TRUE
  } else {
    check_r6(x, class)
  }
}


# Check whether an object is an Optimizer or in a fixed set of optimizers.
check_optimizer = function(x, class = "Optimizer") {
  if (is.character(x)) {
    if (!(x %in% c("gensa", "nloptr", "random_search"))) {
      paste0("optimizer must be convertable to a bbotk::Optimizer via bbotk::opt().")
    } else {
      TRUE
    }
  } else {
    check_r6(optimizer)
  }
}