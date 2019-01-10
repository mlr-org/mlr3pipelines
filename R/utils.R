rep_suffix = function(x, n) {
  sprintf("%s_%04i", x, seq_len(n))
}

# replace columns with new data. A bit hacky because mlr3 doesn't supply this out of the box
# and makes it a bit harder than it needs to be. if no primary key column is present, it is
# added. In that case the number of rows must be identical.
#
# task will be cloned.

#FIXME: make this an issue, this seems pretty central. this should not be hacky and weird
task_update_data = function(task, newdata) {
  rowids = task$row_ids
  keyname = names(rowids)
  assert(length(keyname) == 1)
  if (keyname %nin% rownames(newdata)) {
    assert(nrow(newdata) == task$nrow)
    newdata = cbind(newdata, rowids)
  }
  overwriting = setdiff(intersect(colnames(newdata), task$col_info$id), keyname)  # must take all col_info$id columns
  adding = setdiff(colnames(newdata), c(overwriting, keyname))

  task = task$clone(deep = TRUE)$
    select(overwriting)$
    replace_columns(newdata[, c(overwriting, keyname), with = FALSE])$
    cbind(newdata[, c(adding, keyname), with = FALSE])

  task$set_col_role(setdiff(colnames(newdata), keyname), "feature")
}
