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
    # First, get a data.table with all duplicated rows.
    new_data = task$data(rows = dup_ids, cols = cols)

    # Second, if task has a column with role "group", create new groups for duplicate rows by adding a suffix to the group entry.
    if (!is.null(task$groups)) {
      group = NULL  # for binding
      row_id = NULL  # for binding

      # row_ids:
      # - might have more occurances of an ID than in task$row_roles$use which does not have to be an exact multiple
      # - might have less occurances of an ID than in task$row_roles$use
      # -> We decide to ignore this.
      row_counts = table(task$row_roles$use)
      # Original group names
      grps = unique(task$groups$group)

      # by = "row_id" for faster computation since groups are implied by row_id in task$groups
      new_groups = unique(task$groups, by = "row_id")[list(dup_ids), on = "row_id"]
      new_groups[, group := {
        # Number of how often the same group name should occur for this row ID
        target_count = row_counts[as.character(row_id)]
        # Get default group name target_count - 1 times since default group already exists in task once
        groups = group[seq_len(target_count - 1)]
        # Initialize suffix to be appended to group name if it is otherwise already taken
        suffix = 0

        while (length(groups) < .N) {
          suffix = suffix + 1
          # Otherwise, create a new group with a suffix
          new_group = paste0(group[[1]], "_", suffix)
          # Add it if the suffixed name is not already taken; If it is, increment suffix.
          if (new_group %in% grps) {
            next
          }
          groups[length(groups) + seq_len(target_count)] = new_group
        }
        if (length(groups) != .N) {
          stopf("Called task_filter_ex() but constructed incomplete group '%s'. Try removing column with role 'group'.", group[[1]])
        }
        groups
      }, by = row_id]

      # Use "new_groups" to update the group entries.
      new_data[, (task$col_roles$group) := new_groups$group]
    }

    # Lastly, new data is rbinded to the original task.
    task$rbind(new_data)
  }

  # row_ids can be anything, we just take what mlr3 happens to assign to filter the task.
  row_ids[duplicated(row_ids)] = task$row_ids[newrows]

  # Update row_ids, effectively filtering the task
  task$row_roles$use = row_ids
  task
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
