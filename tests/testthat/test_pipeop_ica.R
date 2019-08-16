context("PipeOpICA")

task = mlr_tasks$get("iris")
op = PipeOpICA$new()
expect_pipeop(op)
set.seed(1234)
result = op$train(list(task))

test_that("PipeOpICA - basic properties", {
  expect_datapreproc_pipeop_class(PipeOpICA, task = task,
    deterministic_train = FALSE)
  expect_task(result[[1]])
  expect_equal(result[[1]]$data(), op$predict(list(task))[[1]]$data())
}
)

test_that("PipeOpICA - compare to fastICA", {
  # Default parameters
  dt = task$data()[, 2:5]
  n.comp = ncol(dt)
  method = "C"
  set.seed(1234)
  fica = fastICA::fastICA(dt, n.comp = n.comp, method = method)
  expect_true(all.equal(dim(fica$S), dim(result[[1]]$data()[, 2:5])))
  expect_true(all(c("K", "W", "A", "center") %in% names(op$state)))
  expect_identical(op$state[c("K", "W", "A")], fica[c("K", "W", "A")])
  dtres = as.matrix(result[[1]]$data()[, 2:5])
  dimnames(dtres) = NULL
  expect_equal(dtres, fica$S)

  # Change some parameters
  op2 = PipeOpICA$new(param_vals = list(method = "R", alpha = 2))
  expect_pipeop(op2)
  set.seed(1234)
  result2 = op2$train(list(task))
  set.seed(1234)
  fica2 = fastICA::fastICA(dt, n.comp = n.comp, fun = fun,
    alpha = 2, method = "R")
  expect_true(all.equal(dim(fica2$S), dim(result2[[1]]$data()[, 2:5])))
  expect_true(all(c("K", "W", "A", "center") %in% names(op2$state)))
  expect_identical(op2$state[c("K", "W", "A")], fica2[c("K", "W", "A")])
  dtres = as.matrix(result2[[1]]$data()[, 2:5])
  dimnames(dtres) = NULL
  expect_equal(dtres, fica2$S)
})
