rep_suffix = function(x, n) {
  # priority here is "easy to enter by hand", not "can reasonably be sorted alphabetically" which NEVER happens
  paste0(x, seq_len(n))
}

calculate_collimit = function(colwidths, outwidth) {
  margin = length(colwidths) + 4  # columns are separated by one space, with some breathing room
  numcols = length(colwidths)     # number of columns that we expect to limit
  repeat {
    # collimit: the width at which we limit data.table column output. If some columns are very
    # small, we can be more generous for other columns.
    collimit = floor((outwidth - margin) / numcols)
    violating = colwidths > collimit - 2
    if (sum(violating) >= numcols)
      break
    margin = length(colwidths) + 4 + sum(colwidths[!violating])
    numcols = sum(violating)
    if (numcols == 0) {
      collimit = outwidth
      break
    }
  }
  collimit - 3  # subtracting 3 here because data.table adds "..." whenever it truncates a string
}

# Get 'levels' of task columns as named list [feature name] -> [levels]
# If a feature has no levels, the entry is NULL
# @param task [Task] the task
# @param cols [character] the columns to query
# @return named [list]
task_levels = function(task, cols) {
  structure(task$col_info[cols, get("levels"), on = "id"], names = cols)
}

# same as task$filter(), but allows duplicate row IDs
# @param task [Task] the task
# @param row_ids [numeric] the row IDs to select
# @return [Task] the modified task
task_filter_ex = function(task, row_ids) {
  addedrows = row_ids[duplicated(row_ids)]

  row_ids[duplicated(row_ids)] = task$nrow + seq_along(addedrows)

  if (length(addedrows)) {
    task$rbind(task$data(rows = addedrows))
  }
  task$filter(row_ids)
}
