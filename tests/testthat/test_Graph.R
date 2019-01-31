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

  expect_output(gr$train(list(1, 2), single_input = FALSE),
    "list\\(input_1 = 1, input_2 = 2\\)")

  expect_output(gr$train(list(debug.multi.input_1 = 1, debug.multi.input_2 = 2), single_input = FALSE),
    "list\\(input_1 = 1, input_2 = 2\\)")

  expect_output(gr$train(list(debug.multi.input_2 = 2, debug.multi.input_1 = 1), single_input = FALSE),
    "list\\(input_1 = 1, input_2 = 2\\)")


  expect_error(gr$train(list(debug.multi.input_2 = 2, debug.multi.input_99 = 1), single_input = FALSE),
    "debug.multi.input_1,debug.multi.input_2")

  expect_error(gr$train(list(), single_input = FALSE), "have length 2")
  expect_error(gr$train(list(1, 2, 3), single_input = FALSE), "have length 2")

  expect_equal(gr$train(1), list(debug.multi.output_1 = 2, debug.multi.output_2 = 3))

  gr$add_pipeop(PipeOpDebugMulti$new(2, 3, "debug2"))
  gr$add_pipeop(PipeOpDebugMulti$new(3, 2, "debug3"))

  gr$add_edge("debug.multi", "debug3", 1, 2)
  gr$add_edge("debug.multi", "debug3", "output_2", "input_1")

  gr$add_edge("debug2", "debug.multi", 2, 1)
  gr$add_edge("debug2", "debug.multi", "output_1", "input_2")

  # layout now:
  #
  # |     1\    2/
  # |     <debug2>
  # |     1/ 2| 3\
  # |     /   |   \
  # |   2|   1|    \
  # |   <multi>    |
  # |   1|   2|    |
  # \    |   /     |
  # 3\  2| 1/      |
  #   <debug3>     |
  #    1/   2\     |
  #
  # input should be debug2.1, debug2.2, debug3.3
  # output should be debug2.3, debug3.1, debug3.2
  # (inputs and outputs in PipeOp order first, in channel order second)

  # test output 1: debug.multi was already trained above
  expect_output(print(gr), "debug2.*<<UNTRAINED>>.*debug.multi.*<list>.*debug3.*<<UNTRAINED>>")
  gr$pipeops$debug.multi$state = NULL
  expect_output(print(gr), "debug2.*<<UNTRAINED>>.*debug.multi.*<<UNTRAINED>>.*debug3.*<<UNTRAINED>>")


  expect_equal(csvify(gr$input),
    c("debug2.input_1,*,*,debug2,input_1",
      "debug2.input_2,*,*,debug2,input_2",
      "debug3.input_3,*,*,debug3,input_3"))

  expect_equal(csvify(gr$output),
    c("debug2.output_3,*,*,debug2,output_3",
      "debug3.output_1,*,*,debug3,output_1",
      "debug3.output_2,*,*,debug3,output_2"))

  lines = strsplit(capture_output(
      {trained = gr$train(list(debug2.input_2 = 10, debug3.input_3 = 100, debug2.input_1 = 1000), single_input = FALSE)}),
    "\n")[[1]]

  expect_equal(lines,
    c("Training debug2 with input list(input_1 = 1000, input_2 = 10)",
      "Training debug.multi with input list(input_1 = 1002, input_2 = 1001)",
      "Training debug3 with input list(input_1 = 1004, input_2 = 1003, input_3 = 100)"))

  expect_equal(trained, list(debug2.output_3 = 1003, debug3.output_1 = 1005, debug3.output_2 = 1006))

  # test output II
  expect_output(print(gr), "debug2.*<list>.*debug.multi.*<list>.*debug3.*<list>")


})

test_that("edges that introduce loops cannot be added", {


  g = Graph$new()$
    add_pipeop(PipeOpNULL$new("p1"))$
    add_pipeop(PipeOpNULL$new("p2"))

  gclone = g$clone(deep = TRUE)
  expect_deep_clone(g, gclone)

  expect_error(g$add_edge("p1", "p1", 1, 1), "Cycle detected")

  expect_deep_clone(g, gclone) # check that edges did not change

  g$add_edge("p1", "p2")

  gclone = g$clone(deep = TRUE)
  expect_deep_clone(g, gclone)

  expect_error(g$add_edge("p2", "p1", 1, 1), "Cycle detected")

  expect_deep_clone(g, gclone) # check that edges did not change

})

