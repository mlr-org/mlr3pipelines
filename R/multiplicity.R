#' @title Multiplicity
#'
#' @description
#' Construct an object of the S3 class [`Multiplicity`], which is a named or
#' unnamed lists of data.
#'
#' @param \dots \cr
#'   Can be anything.
#' @return [`Multiplicity`]
#' @export
Multiplicity = function(...) {
  structure(list(...), class = "Multiplicity")
}

#' @export
print.Multiplicity = function(x, ...) {
  if (!length(x)) {
    cat("Empty Multiplicity.\n")
  } else {
    cat("Multiplicity:\n")
    print(unclass(x), ...)
  }
}

#' @title Check if an object is a Multiplicity
#'
#' @description
#' Check if an object is a [`Multiplicity`].
#'
#' @param x (`any`) \cr
#'   Object to check.
#' @return `logical(1)`
#' @export
is.Multiplicity = function(x) {
  inherits(x, "Multiplicity")
}

#' @title Convert an object to a Multiplicity
#'
#' @description
#' Convert an object to a [`Multiplicity`].
#' @param x (`any`) \cr
#'   Object to convert.
#' @return [`Multiplicity`]
#' @export
as.Multiplicity = function(x) {
  structure(as.list(x), class = "Multiplicity")
}

# Check well-formedness of a multiplicity x
assert_multiplicity = function(x, .var.name, check_nesting = FALSE) {
  assert_class(x, "Multiplicity", .var.name = .var.name)
  assert(check_list(x, names = "unnamed"), check_list(x, names = "unique"))
  checkmnl = function(x) {
    if (!is.Multiplicity(x)) return(0L)
    levels = unique(unlist(map(x, checkmnl)))  # don't map_int because Multiplicity() exists which would return NULL or integer(0)
    if (length(levels) > 1) {
      stopf("Inconsistent multiplicity nesting level in %s", .var.name)
    }
    levels + 1L
  }
  if (check_nesting) checkmnl(x)
  invisible(x)
}

# Check whether multiplicity x has a deeper nesting level than cutoff
# Assumes multiplicity is well-formed. Use assert_multiplicity to check well-formedness
multiplicity_nests_deeper_than = function(x, cutoff) {
  cutoff = assert_count(cutoff)
  if (!is.Multiplicity(x)) return(FALSE)
  if (!cutoff) return(TRUE)
  ret = NA
  for (element in x) {
    ret = multiplicity_nests_deeper_than(element, cutoff - 1L)
    if (!is.na(ret)) break
  }
  ret
}
