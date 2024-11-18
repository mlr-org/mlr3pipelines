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

      # If there are duplicates in task$row_roles$use, we change dup_ids ... (but also need to change assignemnt later)
      if (length(task$row_roles$use) > length(unique(task$row_roles$use))) {
        # row_ids:
        # - might have IDs that are not in task$row_roles$use
        # - might have more occurances of an ID than in task$row_roles$use which does not have to be an exact multiple
        # - might have less occurances of an ID than in task$row_roles$use (how do we handle that?)
        id_counts = table(row_ids)
        row_counts = table(task$row_roles$use)

        # Restrict row_counts to only include the same row IDs as dup_counts (only ones of interest) to get same dimension
        row_counts = row_counts[names(id_counts)]
        count_diff = id_counts - row_counts
        # These are the only ids which need updated group names
        count_diff = count_diff[count_diff >= 1]

        # creates new dup_ids (however, not in the same order!)
        dup_ids = rep(as.integer(names(count_diff)), count_diff)
      }

      # We create a data.table "new_groups" with the corresponding group to each duplicated ID.
      #   1. Remove duplicates from task$groups which could exist in case of duplicates in task$row_roles$use.
      #   2. Get correct number and positioning of rows through dup_ids.
      # We then change the group entry based on how often the ID occurs. E.g. row_id = 1 occurs
      # two times has the group entry "g". Then we rename the group entries to "g_1" and "g_2".
      # If a group with a suffix (e.g. "_1") already exists, we add another suffix to it (i.e. "_1_1").
      grps = unique(task$groups$group)
      new_groups = unique(task$groups, by = "row_id")[list(dup_ids), on = "row_id"][, group := {
        groups = character(0)
        i = 1
        while (length(groups) < .N) {
          new_group = paste0(group[[1]], "_", i)
          if (new_group %nin% grps) groups[[length(groups) + 1]] = new_group
          i = i + 1
        }
        groups
      }, by = row_id]

      # Use "new_groups" to update the group entries.
      if (length(task$row_roles$use) > length(unique(task$row_roles$use))) {

      } else {
        new_data[, (task$col_roles$group) := new_groups$group]
      }
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
