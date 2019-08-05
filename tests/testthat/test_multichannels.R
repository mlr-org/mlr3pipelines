context("multichannels")

test_that("adding multiple edges to output channels works", {

  graph = Graph$new()$add_pipeop("scale")$add_pipeop("pca")$add_pipeop("subsample")

  graph$add_edge("scale", "subsample")$add_edge("scale", "pca")

  expect_output(print(graph), c("scale.*subsample,pca.*\n.*subsample.*scale.*\n.*pca.*scale"))

  pdf(file = NULL) # don't show plot. It is annoying.
  graph$plot()
  dev.off()

})

test_that("doublearrow with one output to many input works as expected", {

  g1 = "scale" %>>% gunion(list("pca", "subsample"))
  g2 = Graph$new()$add_pipeop("scale")$add_pipeop("pca")$add_pipeop("subsample")$
    add_edge("scale", "pca")$add_edge("scale", "subsample")

  expect_equal(g1, g2)

  expect_error(gunion(list("scale", "pca")) %>>% gunion(list("nop", "subsample", "select")),
    "mismatching number of inputs / outputs")

})

test_that("multiple edges on output channel copies, as expected", {
  graph = Graph$new()$add_pipeop("scale")$add_pipeop("pca")$add_pipeop("subsample")

  graph$add_edge("scale", "subsample")$add_edge("scale", "pca")

  expect_list(graph$train(mlr_tasks$get("iris")), types = "Task", any.missing = FALSE, len = 2)

  nullgraph = mlr_pipeops$get("nop", id = "nop1") %>>%
    gunion(list(mlr_pipeops$get("nop", id = "nop2"), mlr_pipeops$get("nop", id = "nop3")))

  tsk0 = mlr_tasks$get("iris")
  tsk_clone = tsk0$clone(deep = TRUE)

  tmp = nullgraph$train(tsk0)

  tsk1 = tmp[[1]]
  tsk2 = tmp[[2]]

  expect_identical(tsk0, tsk1)
  expect_identical(tsk0, tsk2)

  tmp = nullgraph$predict(tsk0)

  tsk1 = tmp[[1]]
  tsk2 = tmp[[2]]

  expect_identical(tsk0, tsk1)
  expect_identical(tsk0, tsk2)

  graph = "nop" %>>% gunion(list(
    mlr_pipeops$get("scale", id = "s1", param_vals = list(scale = TRUE, center = FALSE)),
    mlr_pipeops$get("scale", id = "s2", param_vals = list(scale = FALSE, center = TRUE))))

  tmp = graph$train(tsk0)

  stsk = tmp[[1]]
  ctsk = tmp[[2]]

  expect_equal(sapply(ctsk$data(cols = ctsk$feature_names), mean),
    c(Petal.Length = 0, Petal.Width = 0, Sepal.Length = 0, Sepal.Width = 0))

  expect_equal(sapply(stsk$data(cols = stsk$feature_names), function(x) sum(x^2) / (length(x) - 1)),
    c(Petal.Length = 1, Petal.Width = 1, Sepal.Length = 1, Sepal.Width = 1))

  tmp = graph$predict(tsk0)

  stsk = tmp[[1]]
  ctsk = tmp[[2]]

  expect_equal(sapply(ctsk$data(cols = ctsk$feature_names), mean),
    c(Petal.Length = 0, Petal.Width = 0, Sepal.Length = 0, Sepal.Width = 0))

  expect_equal(sapply(stsk$data(cols = stsk$feature_names), function(x) sum(x^2) / (length(x) - 1)),
    c(Petal.Length = 1, Petal.Width = 1, Sepal.Length = 1, Sepal.Width = 1))

  expect_identical(tsk0, tsk1)  # input task not changed

  expect_deep_clone(tsk0, tsk_clone)

})

test_that("adding multiple edges to vararg input channel works", {

  graph = Graph$new()$add_pipeop("scale")$add_pipeop("pca")$add_pipeop(VarargPipeop$new())

  graph$add_edge("scale", "vararg")$add_edge("pca", "vararg")

  expect_output(print(graph), c("scale.*vararg.*\n.*pca.*vararg.*\n.*vararg.*scale,pca"))

  pdf(file = NULL) # don't show plot. It is annoying.
  graph$plot()
  dev.off()

  graph = Graph$new()$
    add_pipeop("scale")$add_pipeop("pca")$
    add_pipeop("subsample")$add_pipeop("select")$
    add_pipeop(VarargPipeop$new(innum = 2))

  expect_error(graph$add_edge("scale", "vararg"), "dst_channel must not be NULL")

  graph$add_edge("scale", "vararg", dst_channel = "...")$
    add_edge("pca", "vararg", dst_channel = "...")$
    add_edge("subsample", "vararg", dst_channel = "input1")$
    add_edge("select", "vararg", dst_channel = "input2")


  expect_output(print(graph), c("scale.*vararg.*\n.*pca.*vararg.*\n.*subsample.*vararg.*\n.*select.*vararg.*\n.*vararg.*scale,pca,subsample,select"))

  pdf(file = NULL) # don't show plot. It is annoying.
  graph$plot()
  dev.off()

})

test_that("doublearrow with many output to vararg input works as expected", {

  g1 = gunion(list("scale", "pca")) %>>% VarargPipeop$new()
  g2 = Graph$new()$add_pipeop("scale")$add_pipeop("pca")$add_pipeop(VarargPipeop$new())$
    add_edge("scale", "vararg")$add_edge("pca", "vararg")

  expect_equal(g1, g2)

  g1 = gunion(list("scale", "pca")) %>>% VarargPipeop$new(innum = 1)
  g2 = Graph$new()$add_pipeop("scale")$add_pipeop("pca")$add_pipeop(VarargPipeop$new(innum = 1))$
    add_edge("scale", "vararg", dst_channel = "...")$add_edge("pca", "vararg", dst_channel = "input1")

  expect_equal(g1, g2)

  expect_error(gunion(list("scale", "pca", "select", "subsample")) %>>% VarargPipeop$new(innum = 2),
    "mismatching number of inputs / outputs")

})

test_that("vararg passes args through as it should", {

  nullgraph = gunion(list(mlr_pipeops$get("nop", id = "nop1"), mlr_pipeops$get("nop", id = "nop2"))) %>>% VarargPipeop$new()

  expect_equal(nullgraph$train(1)[[1]], list(`...` = 1, `...` = 1))

  graph = gunion(list(
    mlr_pipeops$get("scale", id = "s1", param_vals = list(scale = TRUE, center = FALSE)),
    mlr_pipeops$get("scale", id = "s2", param_vals = list(scale = FALSE, center = TRUE)))) %>>%
    VarargPipeop$new()

  tsk0 = tsk1 = mlr_tasks$get("iris")
  tsk_clone = tsk0$clone(deep = TRUE)

  tmp = graph$train(tsk0)[[1]]

  stsk = tmp[[1]]
  ctsk = tmp[[2]]

  expect_equal(sapply(ctsk$data(cols = ctsk$feature_names), mean),
    c(Petal.Length = 0, Petal.Width = 0, Sepal.Length = 0, Sepal.Width = 0))

  expect_equal(sapply(stsk$data(cols = stsk$feature_names), function(x) sum(x^2) / (length(x) - 1)),
    c(Petal.Length = 1, Petal.Width = 1, Sepal.Length = 1, Sepal.Width = 1))

  tmp = graph$predict(tsk0)[[1]]

  stsk = tmp[[1]]
  ctsk = tmp[[2]]

  expect_equal(sapply(ctsk$data(cols = ctsk$feature_names), mean),
    c(Petal.Length = 0, Petal.Width = 0, Sepal.Length = 0, Sepal.Width = 0))

  expect_equal(sapply(stsk$data(cols = stsk$feature_names), function(x) sum(x^2) / (length(x) - 1)),
    c(Petal.Length = 1, Petal.Width = 1, Sepal.Length = 1, Sepal.Width = 1))

  expect_identical(tsk0, tsk1)  # input task not changed
  expect_deep_clone(tsk0, tsk_clone)


  nullgraph = gunion(list(
    mlr_pipeops$get("nop", id = "nop1"),
    mlr_pipeops$get("nop", id = "nop2"),
    mlr_pipeops$get("nop", id = "nop3")))$
    add_pipeop(VarargPipeop$new(innum = 1))$
    add_edge("nop1", "vararg", dst_channel = "...")$
    add_edge("nop3", "vararg", dst_channel = "...")$
    add_edge("nop2", "vararg", dst_channel = "input1")

  expect_equal(nullgraph$train(list(1, 2, 3), single_input = FALSE)[[1]], list(`...` = 1, `...` = 3, input1 = 2))

})
