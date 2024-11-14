#' @title Multiplicity
#'
#' @description
#' A [`Multiplicity`] class S3 object.
#'
#' The function of multiplicities is to indicate that [`PipeOp`]s should be executed
#' multiple times with multiple values.
#'
#' A [`Multiplicity`] is a container, like a
#' `list()`, that contains multiple values. If the message that is passed along the
#' edge of a [`Graph`] is a [`Multiplicity`]-object, then the [`PipeOp`] that receives
#' this object will *usually* be called once for each contained value. The result of
#' each of these calls is then, again, packed in a [`Multiplicity`] and sent along the
#' outgoing edge(s) of that [`PipeOp`]. This means that a [`Multiplicity`] can cause
#' multiple [`PipeOp`]s in a row to be run multiple times, where the run for each element
#' of the [`Multiplicity`] is independent from the others.
#'
#' Most [`PipeOp`]s only return a [`Multiplicity`] if their input was a [`Multiplicity`]
#' (and after having run their code multiple times, once for each entry). However,
#' there are a few special [`PipeOp`]s that are "aware" of [`Multiplicity`] objects. These
#' may either *create* a [`Multiplicity`] even though not having a [`Multiplicity`] input
#' (e.g. [`PipeOpReplicate`] or [`PipeOpOVRSplit`]) -- causing the subsequent [`PipeOp`]s
#' to be run multiple times -- or *collect* a [`Multiplicity`], being called only once
#' even though their input is a [`Multiplicity`] (e.g. [`PipeOpOVRUnite`] or [`PipeOpFeatureUnion`]
#' if constructed with the `collect_multiplicity` argument set to `TRUE`). The combination
#' of these mechanisms makes it possible for parts of a [`Graph`] to be called variably
#' many times if "sandwiched" between [`Multiplicity`] creating and collecting [`PipeOp`]s.
#'
#' Whether a [`PipeOp`] creates or collects a [`Multiplicity`] is indicated by the `$input`
#' or `$output` slot (which indicate names and types of in/out channels). If the `train` and
#' `predict` types of an input or output are surrounded by square brackets ("`[`", "`]`"), then
#' this channel handles a [`Multiplicity`] explicitly. Depending on the function of the [`PipeOp`],
#' it will usually collect (input channel) or create (output channel) a [`Multiplicity`].
#' [`PipeOp`]s without this indicator are [`Multiplicity`] agnostic and blindly execute their
#' function multiple times when given a [`Multiplicity`].
#'
#' If a [`PipeOp`] is trained on a [`Multiplicity`], the `$state` slot is set to a [`Multiplicity`]
#' as well; this [`Multiplicity`] contains the "original" `$state` resulting from each individual
#' call of the [`PipeOP`] with the input [`Multiplicity`]'s content. If a [`PipeOp`] was trained
#' with a [`Multiplicity`], then the `predict()` argument must be a [`Multiplicity`] with the same
#' number of elements.
#'
#' @param \dots `any`\cr
#'   Can be anything.
#' @return [`Multiplicity`]
#' @family Special Graph Messages
#' @family Experimental Features
#' @family Multiplicity PipeOps
#' @export
Multiplicity = function(...) {
  structure(list(...), class = "Multiplicity")
}

#' @export
print.Multiplicity = function(x, ...) {
  if (!length(x)) {
    cat("Empty Multiplicity.\n")
  } else {
    cli_h2("Multiplicity:")
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
  ret = FALSE  # TODO: this is currently a hack to make empty multiplicities work, but *nested* empty multiplicities don't work, currently.
  for (element in x) {
    ret = multiplicity_nests_deeper_than(element, cutoff - 1L)
    if (!is.na(ret)) break
  }
  ret
}

#' @export
marshal_model.Multiplicity = function(model, inplace = FALSE, ...) {
  maybe_marshaled = multiplicity_recurse(model, marshal_model, inplace = inplace, ...)
  was_marshaled = unlist(multiplicity_recurse(maybe_marshaled, is_marshaled_model))
  if (!any(was_marshaled)) {
    return(model)
  }
  structure(list(
    marshaled = maybe_marshaled,
    packages = "mlr3pipelines"
  ), class = c("Multiplicity_marshaled", "marshaled"))
}

#' @export
unmarshal_model.Multiplicity_marshaled = function(model, inplace = FALSE, ...) {
  multiplicity_recurse(model$marshaled, unmarshal_model, inplace = inplace, ...)
}
