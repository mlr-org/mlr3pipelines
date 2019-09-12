
#' @export
as_pipeop.character = function(x, clone = FALSE) {
  assert_string(x)
  if (x %nin% c(mlr_pipeops$keys(), mlr_learners$keys())) {
    stopf("'%s' is neither in mlr_pipeops nor in mlr_learners.%s%s",
      x, did_you_mean(x, mlr_pipeops$keys()), did_you_mean(x, mlr_learners$keys()))
  }
  if (x %in% mlr_pipeops$keys()) {
    x = mlr_pipeops$get(x)
  } else {
    as_pipeop(mlr_learners$get(x))
  }
}

