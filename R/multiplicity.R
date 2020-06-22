
#' @title Multiplicity
#' @export
Multiplicity = function(...) {
  structure(list(...), class = "Multiplicity")
}

print.Multiplicity = function(x, ...) {
  if (!length(x)) {
    cat("Empty Multiplicity.\n")
  } else {
    cat("Multiplicity:\n")
    print(unclass(x), ...)
  }
}

#' @title Check if a Value is a Multiplicity
#' @export
is.Multiplicity = function(x) {
  inherits(x, "Multiplicity")
}

#' @title Convert to Multiplicity
#' @export
as.Multiplicity = function(x) {
  structure(as.list(x), class = "Multiplicity")
}

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

# check whether multiplicity x has a deeper nesting level than cutoff.
# assumes multiplicity is well-formed. Use assert_multiplicity to check well-formedness
#
# multiplicity_nests_deeper_than(Multiplicity(Multiplicity(1)), 1) --> TRUE
# multiplicity_nests_deeper_than(Multiplicity(Multiplicity(1)), 2) --> FALSE
# multiplicity_nests_deeper_than(Multiplicity(Multiplicity()), 2) --> NA  # don't know
# multiplicity_nests_deeper_than(Multiplicity(Multiplicity(), Multiplicity(1)), 2) --> FALSE
# multiplicity_nests_deeper_than(Multiplicity(Multiplicity(), Multiplicity(Multiplicity())), 2) --> TRUE
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
