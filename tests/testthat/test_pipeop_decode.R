context("PipeOpDecode")

test_that("PipeOpDecode - basic properties", {
  # check what expect_datapreproc_pipeop_class does, whether we need custom task here
  df = data.frame(
    target = runif(120),
    x.1 = rep(c(1, 0, 0), 40),
    x.2 = rep(c(0, 1, 0), 40),
    x.3 = rep(c(0, 0, 1), 40),
    y.1 = rep(c(1, 0, 0), 40),
    y.2 = rep(c(0, 1, 0), 40),
    y.3 = rep(c(0, 0, 1), 40),
    a = runif(120)
  )
  task = TaskRegr$new(id = "decode", backend = df, target = "target")

  expect_datapreproc_pipeop_class(PipeOpDecode, task = task)
})

test_that("PipeOpDecode - assertions", {
  # test that edge cases are caught
})

test_that("PipeOpDecode - one-hot-encoding", {
  po = PipeOpDecode$new()

  df = data.frame(
    target = runif(120),
    x.1 = rep(c(1, 0, 0), 40),
    x.2 = rep(c(0, 1, 0), 40),
    x.3 = rep(c(0, 0, 1), 40),
    y.1 = rep(c(1, 0, 0), 40),
    y.2 = rep(c(0, 1, 0), 40),
    y.3 = rep(c(0, 0, 1), 40),
    a = runif(120)
  )
  task = TaskRegr$new(id = "decode", backend = df, target = "target")


})

test_that("PipeOpDecode - treatment encoding", {

})

test_that("PipOpDecode - different regex patterns", {

})
