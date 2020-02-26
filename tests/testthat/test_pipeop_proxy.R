context("PipeOpProxy")

test_that("PipeOpProxy - basic properties", {
  task = mlr_tasks$get("iris")
  pop = PipeOpProxy$new(param_vals = list(content = PipeOpNOP$new()))
  expect_pipeop(pop)
  expect_equal(train_pipeop(pop, inputs = list(task))[[1L]], task)
  expect_graph(pop$state)
  expect_equal(predict_pipeop(pop, inputs = list(task))[[1L]], task)
})

test_that("PipeOpProxy - datapreproc", {
  pop = PipeOpScale$new()
  #FIXME: deep cloning seems to have problems with the content
  expect_datapreproc_pipeop_class(PipeOpProxy, constargs = list(param_vals = list(content = pop)), task = mlr_tasks$get("iris"))
})

test_that("PipeOpProxy - content error handling", {
  expect_error(PipeOpProxy$new(param_vals = list(content = "error")), regexp = "`content` must be an object that can be converted to a Graph")
  expect_error(PipeOpProxy$new(param_vals = list(content = PipeOpCopy$new(outnum = 2L))), regexp = "Graph's output number must match `outnum`")
})

test_that("PipeOpProxy - several inputs via featureunion", {
  task1 = mlr_tasks$get("iris")
  task2 = mlr_tasks$get("iris")
  gr1 = PipeOpCopy$new(outnum = 2L) %>>% gunion(list(PipeOpNOP$new(), PipeOpPCA$new())) %>>% PipeOpProxy$new(param_vals = list(content = PipeOpFeatureUnion$new()))
  gr2 = PipeOpCopy$new(outnum = 2L) %>>% gunion(list(PipeOpNOP$new(), PipeOpPCA$new())) %>>% PipeOpFeatureUnion$new()
  tout1 = gr1$train(task1)
  tout2 = gr2$train(task2)
  expect_equal(tout1[[1L]], tout2[[1L]])
})

test_that("PipeOpProxy - several outputs", {
  task1 = mlr_tasks$get("iris")
  task2 = mlr_tasks$get("iris")
  pop = PipeOpProxy$new(outnum = 2L, param_vals = list(content = PipeOpCopy$new(outnum = 2L)))
  pop_copy = PipeOpCopy$new(outnum = 2L)
  tout1 = pop$train(list(task1))
  tout2 = pop_copy$train(list(task2))
  expect_equal(tout1[[1L]], tout2[[1L]])
})

test_that("PipeOpProxy - PCA proxied", {
  task1 = mlr_tasks$get("iris")
  task2 = mlr_tasks$get("iris")
  pop = PipeOpProxy$new(param_vals = list(content =
    PipeOpPCA$new(param_vals = list(center = TRUE, scale. = TRUE, rank. = 1L))))
  pop_pca = PipeOpPCA$new(param_vals = list(center = TRUE, scale. = TRUE, rank. = 1L))
  task1 = mlr_tasks$get("iris")
  task2 = task1$clone(deep = TRUE)
  tout = train_pipeop(pop, list(task1))
  tout_pca = train_pipeop(pop_pca, list(task2))
  expect_equal(tout[[1L]], tout_pca[[1L]])
})

test_that("PipeOpProxy - Graph proxied", {
  pop = PipeOpProxy$new(param_vals = list(content =
    PipeOpScale$new() %>>%
    PipeOpPCA$new(param_vals = list(center = TRUE, scale. = TRUE, rank. = 1L))))
  gr = PipeOpScale$new() %>>%
    PipeOpPCA$new(param_vals = list(center = TRUE, scale. = TRUE, rank. = 1L))
  task1 = mlr_tasks$get("iris")
  task2 = task1$clone(deep = TRUE)
  tout = train_pipeop(pop, list(task1))
  tout_gr = gr$train(task2)
  expect_equal(tout[[1L]], tout_gr[[1L]])
})
