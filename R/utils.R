collapse = function(x) {
  paste(x, collapse = ",")
}


readonly = function(name) {
  body = substitute({
    var = private[[paste0(".", name)]]
    if (!missing(x) && !identical(x, var)) stopf("Variable %s is read-only", name)
    var
  })
  eval(call("function", as.pairlist(alist(x = )), body))
}


# FIXME: This should be in mlr3misc
#' Chunk elements of vectors into blocks of nearly equal size.
#'
#' In case of shuffling and vectors that cannot be chunked evenly,
#' it is chosen randomly which levels / chunks will receive 1 element less.
#' If you do not shuffle, always the last chunks will receive 1 element less.
#'
#' @param x [ANY]\cr
#'   Vector, list or other type supported by \code{\link[base]{split}}.
#' @param chunk.size [\code{integer(1)}]\cr
#'   Requested number of elements in each chunk.
#'   Cannot be used in combination with \code{n.chunks} or \code{props}.
#'   If \code{x} cannot be evenly chunked, some chunks will have less elements.
#' @param n.chunks [\code{integer(1)}]\cr
#'   Requested number of chunks.
#'   If more chunks than elements in \code{x} are requested, empty chunks are
#'   dropped.
#'   Can not be used in combination with \code{chunks.size} or \code{props}.
#' @param props [\code{numeric}]\cr
#'   Vector of proportions for chunk sizes.
#'   Empty chunks may occur, depending on the length of \code{x} and the given
#'   proportions.
#'   Cannot be used in combination with \code{chunks.size} or \code{n.chunks}.
#' @param shuffle [\code{logical(1)}]\cr
#'   Shuffle \code{x}?
#'   Default is \code{FALSE}.
#' @return [unnamed \code{list}] of chunks.
#' @export
#' @examples
#' xs = 1:10
#' chunk(xs, chunk.size = 3)
#' chunk(xs, n.chunks = 2)
#' chunk(xs, n.chunks = 2, shuffle = TRUE)
#' chunk(xs, props = c(7, 3))
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