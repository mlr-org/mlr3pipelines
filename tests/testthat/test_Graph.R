context("Graph")

test_that("linear graph", {

  g = Graph$new()
  expect_equal(g$ids(sorted = TRUE), character(0))

  # FIXME: we should  "dummy" ops, so we can change properties of the ops at will
  # we should NOT use PipeOpNULL, because we want to check that $train/$predict actually does something.
  # FIXME: we should packages of the graph
  op_ds = PipeOpDownsample$new()
  op_pca = PipeOpPCA$new()
  op_lrn = PipeOpLearner$new(mlr_learners$get("classif.rpart"))
  g$add_pipeop(op_ds)
  g$add_pipeop(op_pca)

  expect_graph(g)

  g$add_edge("downsample", "pca")

  expect_graph(g)

  expect_output(print(g), "Graph with 2 PipeOps.*downsample.*UNTRAINED.*pca.*UNTRAINED")



  inputs = mlr_tasks$get("iris")
  x = g$train(inputs)
  expect_task(x[[1]])

  expect_output(print(g), "Graph with 2 PipeOps.*downsample.*list.*pca.*prcomp")

  out = g$predict(inputs)
  expect_task(x[[1]])

  g$add_pipeop(op_lrn)

  expect_graph(g)

  expect_error(g$add_edge("downsample", "rpart"),
    "Channel.*output.*of node.*downsample.*already connected to channel.*input.*of node pca")

  expect_error(g$add_pipeop(op_lrn), "PipeOp with id.*rpart.*already in Graph")

  expect_deep_clone(g, g$clone(deep = TRUE))

})

test_that("complex graph", {
  # test that debug pipeops exist
  expect_pipeop(PipeOpDebugBasic$new())
  expect_pipeop(PipeOpDebugMulti$new(1, 1))
  expect_pipeop(PipeOpDebugMulti$new(3, 2))

  expect_graph(PipeOpDebugBasic$new() %>>% PipeOpDebugMulti$new(1, 2) %>>% PipeOpDebugMulti$new(2, 1, "debug2"))

  expect_graph(PipeOpDebugBasic$new() %>>% PipeOpDebugMulti$new(1, 2) %>>%
    greplicate(PipeOpDebugMulti$new(1, 2, "debug2"), 2))

  biggraph = PipeOpDebugBasic$new() %>>%
    PipeOpDebugMulti$new(1, 2) %>>%
    greplicate(PipeOpDebugMulti$new(1, 2, "debug2"), 2) %>>%
    gunion(list(PipeOpDebugBasic$new("basictop"),
      PipeOpDebugMulti$new(2, 1, "debug2"),
      PipeOpDebugBasic$new("basicbottom"))) %>>%
    PipeOpDebugMulti$new(3, 1, "debug3")

  # it's a beauty: biggraph$plot()

  lines = strsplit(capture_output(biggraph$train(1)), "\n")[[1]]

  expect_set_equal(lines,
    c("Training debug.basic",
      "Training debug.multi with input list(input_1 = 1)",
      "Training debug2_001 with input list(input_1 = 2)",
      "Training debug2_002 with input list(input_1 = 3)",
      "Training basictop",
      "Training basicbottom",
      "Training debug2 with input list(input_1 = 4, input_2 = 4)",
      "Training debug3 with input list(input_1 = 3, input_2 = 5, input_3 = 5)"))


  biggraph$plot()

})


test_that("input / output lists and naming", {

  gr = Graph$new()$add_pipeop(PipeOpDebugMulti$new(2, 2))

  expect_equal(csvify(gr$input),
    c("debug.multi.input_1,*,*,debug.multi,input_1",
      "debug.multi.input_2,*,*,debug.multi,input_2"))
  expect_equal(csvify(gr$output),
    c("debug.multi.output_1,*,*,debug.multi,output_1",
      "debug.multi.output_2,*,*,debug.multi,output_2"))

  expect_output(gr$train(9), "list\\(input_1 = 9, input_2 = 9\\)")

  expect_output(gr$train(-9), "list\\(input_1 = -9, input_2 = -9\\)")

  expect_output(gr$train(list(debug.multi.input_1 = 1, debug.multi.input_2 = 2), single_input = FALSE),
    "list\\(input_1 = 1, input_2 = 2\\)")

  expect_output(gr$train(list(debug.multi.input_2 = 2, debug.multi.input_1 = 1), single_input = FALSE),
    "list\\(input_1 = 1, input_2 = 2\\)")


  expect_error(gr$train(list(debug.multi.input_2 = 2, debug.multi.input_99 = 1), single_input = FALSE),
    "debug.multi.input_1,debug.multi.input_2")

  expect_error(gr$train(list(), single_input = FALSE), "have length 2")
  expect_error(gr$train(list(1, 2, 3), single_input = FALSE), "have length 2")

  gr$train(list(1, 2), single_input = FALSE)

})
