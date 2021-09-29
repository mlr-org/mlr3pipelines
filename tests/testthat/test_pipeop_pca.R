context("PipeOpPCA")

test_that("PipeOpPCA - basic properties", {
  op = PipeOpPCA$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)

  expect_datapreproc_pipeop_class(PipeOpPCA, task = task)

  # FIXME: either we dont need this, as this is already ensured in PipeOp, or we should test this in expect_pipeop
  result = train_pipeop(op, inputs = list(task))
  expect_task(result[[1]])

  result = predict_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
})


test_that("PipeOpPCA works as expected", {

  op = PipeOpPCA$new()
  task = mlr_tasks$get("iris")

  ip = op$train(list(task))

  ret = op$predict(list(task))

  expect_equal(ip, ret)

  hip = task$clone(deep = TRUE)$filter(1:10)
  expect_equal(op$predict(list(hip))[[1]]$data(), ret[[1]]$clone(deep = TRUE)$filter(1:10)$data())

  prc = prcomp(iris[1:4])

  expect_equal(as.data.table(abs(op$state$rotation))[order(PC1)], as.data.table(abs(prc$rotation))[order(PC1)])

  op$param_set$values$center = FALSE
  ip = op$train(list(task))[[1]]
  expect_equal(abs(ip$data(cols = ip$feature_names)),
               as.data.table(abs(prcomp(iris[1:4], center = FALSE, scale. = FALSE)$x)))

  trueval = prcomp(iris[1:4], center = FALSE, scale. = TRUE, rank. = 3)
  op$param_set$values$scale. = TRUE
  op$param_set$values$rank. = 3
  opval = op$train(list(task))[[1]]

  ctrl = op$state
  expect_subset(c("sdev", "rotation", "scale", "center"), names(ctrl))

  expect_equal(ctrl$sdev, trueval$sdev)
  expect_equal(as.data.table(abs(ctrl$rotation))[order(PC1)], as.data.table(abs(trueval$rotation))[order(PC1)])
  expect_equal(sort(ctrl$scale), sort(trueval$scale))
  expect_equal(sort(ctrl$center), sort(trueval$center))

})
