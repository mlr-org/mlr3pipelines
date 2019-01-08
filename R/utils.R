collapse = function(x) {
  paste(x, collapse = ",")
}


# specify an R6 active binding as readonly that can still have its member variables changed.
#
# E.g. ```obj$readonlyvar$member = newval``` should work if `readonlyvar` is an R6 object,
# but will usually fail if it is a naively implemented active binding.
#
# '<name>' should be the name of the active binding, and a private member variable named `.<name>`:
#
# Example:
#
# Class = R6Class("Class",
#   active = list(
#     var = readonly("var")
#   ),
#   private = list(
#     .var = OtherClass$new()
#   )
# )
readonly = function(name) {
  body = substitute({
    var = private[[paste0(".", name)]]
    if (!missing(x) && !identical(x, var)) stopf("Variable %s is read-only", name)
    var
  })
  eval(call("function", as.pairlist(alist(x = )), body))
}


# FIXME: This should be in mlr3misc
# Chunk elements of vectors into blocks of nearly equal size.
#
# In case of shuffling and vectors that cannot be chunked evenly,
# it is chosen randomly which levels / chunks will receive 1 element less.
# If you do not shuffle, always the last chunks will receive 1 element less.
#
# @param x [ANY]\cr
#   Vector, list or other type supported by \code{\link[base]{split}}.
# @param chunk.size [\code{integer(1)}]\cr
#   Requested number of elements in each chunk.
#   Cannot be used in combination with \code{n.chunks} or \code{props}.
#   If \code{x} cannot be evenly chunked, some chunks will have less elements.
# @param n.chunks [\code{integer(1)}]\cr
#   Requested number of chunks.
#   If more chunks than elements in \code{x} are requested, empty chunks are
#   dropped.
#   Can not be used in combination with \code{chunks.size} or \code{props}.
# @param props [\code{numeric}]\cr
#   Vector of proportions for chunk sizes.
#   Empty chunks may occur, depending on the length of \code{x} and the given
#   proportions.
#   Cannot be used in combination with \code{chunks.size} or \code{n.chunks}.
# @param shuffle [\code{logical(1)}]\cr
#   Shuffle \code{x}?
#   Default is \code{FALSE}.
# @return [unnamed \code{list}] of chunks.
chunk = function(x, chunk.size, n.chunks, props, shuffle = FALSE) {
  assertFlag(shuffle)
  method = c("chunk.size", "n.chunks", "props")
  method = method[!c(missing(chunk.size), missing(n.chunks), missing(props))]
  if (length(method) != 1L)
    stop("You must provide exactly one of 'chunk.size', 'n.chunks' or 'props'")
  nx = length(x)

  ch = switch(method,
    n.chunks = {
      assertCount(n.chunks, positive = TRUE)
      getNChunks(nx, n.chunks, shuffle)
    },
    props = {
      assertNumeric(props, min.len = 1L, any.missing = FALSE, lower = 0)
      props = props / sum(props)
      ch = factor(rep.int(seq_along(props), round(props * nx, digits = 0L)),
        levels = seq_along(props))
      if (shuffle) sample(ch) else ch
    })

  unname(split(x, ch))
}

getNChunks = function(nx, n.chunks, shuffle) {
  n.chunks = min(n.chunks, nx)
  if (shuffle) {
    c(sample(seq(0L, (nx %/% n.chunks) * n.chunks - 1L) %% n.chunks),
      sample(n.chunks, nx %% n.chunks) - 1L)
  } else {
    sort(seq.int(0L, nx - 1L) %% n.chunks)
  }
}

# replace columns with new data. A bit hacky because mlr3 doesn't supply this out of the box
# and makes it a bit harder than it needs to be. if no primary key column is present, it is
# added. In that case the number of rows must be identical.
#
# task will be cloned.
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
