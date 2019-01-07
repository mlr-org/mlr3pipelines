PipeOpTest1 = R6::R6Class("PipeOpTest1", inherit = PipeOp,
  public = list(
    initialize = function() {
      ps = paradox::ParamSet$new(list(paradox::ParamDbl$new("dbl", lower = 1, upper = 10)))
      super$initialize("th_po", param_set = ps, param_vals = list(dbl = 1))
      self$packages = "package1"
    },
    train = function(inputs) {
      self$state = 1
      return(1)
    },
    predict = function() {return(2)}
  )
)

test_basic_pipeop_props = function(po) {
  expect_class(po, "PipeOp")
  expect_character(po$id)
  expect_class(po$param_set, "ParamSet")
  expect_list(po$param_vals, names = "unique")
  expect_output(print(po), "PipeOp:")
  expect_character(po$packages)
  expect_null(po$state)
  expect_null(po$result)
}
