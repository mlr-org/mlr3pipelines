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

# Same as task$filter(), but allows duplicate row IDs.
# Handles duplicate rows in tasks with col_role "group" by renaming groups
# following pattern grp_name_1, grp_name_1, ... per each duplication of a group.
# @param task [Task] the task
# @param row_ids [numeric] the row IDs to select
# @return [Task] the modified task
task_filter_ex = function(task, row_ids) {

  # Get vector of duplicate row IDs
  dup_ids = row_ids[duplicated(row_ids)]
  # Generate vector with new row IDs
  newrows = task$nrow + seq_along(dup_ids)
  # Get all columns of task for subsetting using task$data()
  cols = unique(unlist(task$col_roles, use.names = FALSE))

  # Rbind duplicated rows to task
  if (length(dup_ids)) {
    task$rbind(task$data(rows = dup_ids, cols = cols))
  }

  # For column with role "group", create new groups for duplicates by adding a suffix.
  if (!is.null(task$groups)) {
    group = NULL  # for binding
    row_id = NULL  # for binding

    # Grps for rows that were added as duplicates
    # could iterate through these
    # subset dt_grps with grp and dup_ids ... meh

    # This works because we only care about which groups were sampled at this point
    # not how often they were sampled. Otherwise, we'd need to use join.
    # grps = unique(task$groups[row_id %in% dup_ids]$group)

    # dup_ids contains the information about which groups belong together
    # can we get a mapping between dupids and the new row_id that was assigned to it?
    # by default: dupids -> newrows maps exactly to the same element

    browser()

    grp_sizes = table(task$groups[-newrows]$group)

    # Find out how many times a group was added
    n_dups_per_grp = table(task$groups[data.table(row_id = dup_ids), on = "row_id", allow.cartesian = TRUE]$group)
    n_grps = n_dups_per_grp / grp_sizes[names(n_dups_per_grp)]

    # Generate data.table of row_ids and groups with updated group names.
    dt_grps = task$groups[newrows]
    for (grp in names(n_grps)) {
      n_add = n_grps[[grp]]
      # This assumes that we always filter the whole group. We probably shouldn't assume
      # that in this general function.
      grp_size = grp_sizes[[grp]]
      # This relies on correct ordering of rbinded rows ...
      # This breaks if task has column with role "order"
      # Could correct for this by sorting dup_ids before task$rbind if col_role order exists ...
      # This doesn't seem very future-proof.
      new_group = rep(paste0(grp, "_", seq_len(n_add)), each = grp_size)
      dt_grps[group == grp, group := new_group]
    }

    # Update column in task
    dt = rbind(task$groups[seq(1, task$nrow - length(newrows))], dt_grps)
    setnames(dt, c(task$backend$primary_key, task$col_roles$group))
    task$cbind(dt)

    # Correct that cbind automatically sets col_roles for new columns to feature.
    task$col_roles$feature = setdiff(task$col_roles$feature, task$col_roles$group)
  }

  # Row ids can be anything, we just take what mlr3 happens to assign to filter the task.
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
