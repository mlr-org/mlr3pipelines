context("Graph")

test_that("linear graph", {
  g = Graph$new()
  expect_equal(g$ids(sorted = TRUE), character(0))

  # FIXME: we should  "dummy" ops, so we can change properties of the ops at will
  # we should NOT use PipeOpNOP, because we want to check that $train/$predict actually does something.
  # FIXME: we should packages of the graph
  op_ds = PipeOpSubsample$new()
  op_pca = PipeOpPCA$new()
  op_lrn = PipeOpLearner$new(mlr_learners$get("classif.rpart"))
  g$add_pipeop(op_ds)
  g$add_pipeop(op_pca)

  expect_graph(g)

  g$add_edge("subsample", "pca")

  expect_graph(g)

  expect_output(print(g), "Graph with 2 PipeOps.*subsample.*UNTRAINED.*pca.*UNTRAINED")



  inputs = mlr_tasks$get("iris")
  x = g$train(inputs)
  expect_task(x[[1]])

  expect_output(print(g), "Graph with 2 PipeOps.*subsample.*list.*pca.*prcomp")

  out = g$predict(inputs)
  expect_task(x[[1]])

  g$add_pipeop(op_lrn)

  expect_graph(g)

  g$add_edge("subsample", "classif.rpart")

  expect_error(g$add_edge("pca", "classif.rpart"),
    "Channel.*output.*of node.*subsample.*already connected to channel.*input.*of node classif.rpart")

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

  lines = strsplit(capture_output(biggraph$train(1)), "\n")[[1]]

  expect_set_equal(lines,
    c("Training debug.basic",
      "Training debug.multi with input list(input_1 = 1)",
      "Training debug2_1 with input list(input_1 = 2)",
      "Training debug2_2 with input list(input_1 = 3)",
      "Training basictop",
      "Training basicbottom",
      "Training debug2 with input list(input_1 = 4, input_2 = 4)",
      "Training debug3 with input list(input_1 = 3, input_2 = 5, input_3 = 5)"),
    info = paste0("'", lines, "'", collapse = "', '"))

  pdf(file = NULL)  # don't show plot. It is annoying.
  biggraph$plot()
  dev.off()

  p = biggraph$plot(TRUE)
  expect_class(p, "htmlwidget")
  expect_class(p, "visNetwork")
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

  pdf(file = NULL)  # don't show plot. It is annoying.
  gr$plot()
  dev.off()

  p = gr$plot(TRUE)
  expect_class(p, "htmlwidget")
  expect_class(p, "visNetwork")

  # test output 1: debug.multi was already trained above
  expect_output(print(gr),
    "debug2.*<<UNTRAINED>>.*debug.multi.*\n.*debug.multi.*<list>.*debug3.*debug2.*\n.*debug3.*<<UNTRAINED>>.*debug.multi")

  gr$pipeops$debug.multi$state = NULL
  expect_output(print(gr),
    "debug2.*<<UNTRAINED>>.*debug.multi.*\n.*debug.multi.*<<UNTRAINED>>.*debug3.*debug2.*\n.*debug3.*<<UNTRAINED>>.*debug.multi")

  expect_equal(csvify(gr$input),
    c("debug2.input_1,*,*,debug2,input_1",
      "debug2.input_2,*,*,debug2,input_2",
      "debug3.input_3,*,*,debug3,input_3"))

  expect_equal(csvify(gr$output),
    c("debug2.output_3,*,*,debug2,output_3",
      "debug3.output_1,*,*,debug3,output_1",
      "debug3.output_2,*,*,debug3,output_2"))

  lines = strsplit(capture_output({
    trained = gr$train(list(debug2.input_2 = 10, debug3.input_3 = 100, debug2.input_1 = 1000), single_input = FALSE)
  }),
  "\n")[[1]]

  expect_equal(lines,
    c("Training debug2 with input list(input_1 = 1000, input_2 = 10)",
      "Training debug.multi with input list(input_1 = 1002, input_2 = 1001)",
      "Training debug3 with input list(input_1 = 1004, input_2 = 1003, input_3 = 100)"))

  expect_equal(trained, list(debug2.output_3 = 1003, debug3.output_1 = 1005, debug3.output_2 = 1006))

  # test output II
  expect_output(print(gr),
    "debug2.*<list>.*debug.multi.*\n.*debug.multi.*<list>.*debug3.*debug2.*\n.*debug3.*<list>.*debug.multi")
})

test_that("edges that introduce loops cannot be added", {
  g = Graph$new()$
    add_pipeop(PipeOpNOP$new("p1"))$
    add_pipeop(PipeOpNOP$new("p2"))

  gclone = g$clone(deep = TRUE)
  expect_deep_clone(g, gclone)

  expect_error(g$add_edge("p1", "p1", 1, 1), "Cycle detected")

  expect_deep_clone(g, gclone)  # check that edges did not change

  g$add_edge("p1", "p2")

  gclone = g$clone(deep = TRUE)
  expect_deep_clone(g, gclone)

  expect_error(g$add_edge("p2", "p1", 1, 1), "Cycle detected")

  expect_deep_clone(g, gclone)  # check that edges did not change
})


test_that("assert_graph test", {
  gr = PipeOpNOP$new() %>>% PipeOpDebugMulti$new(1, 1)

  gr2 = assert_graph(gr)

  expect_error(expect_deep_clone(gr, gr2), class = "error", "addresses differ.*isn't true")

  gr2 = as_graph(gr, clone = TRUE)

  expect_deep_clone(gr, gr2)

  expect_error(assert_graph(PipeOpNOP$new()), "inherit from class.*Graph")

  assert_graph(as_graph(PipeOpNOP$new()))

  po = PipeOpNOP$new()

  expect_error(expect_deep_clone(po, po), class = "error", "addresses differ.*isn't true")

  po2 = as_graph(po, clone = TRUE)$pipeops[[1]]

  expect_deep_clone(po, po2)
})

test_that("Empty Graph", {
  expect_equal(length(Graph$new()$pipeops), 0)

  expect_equal(nrow(Graph$new()$edges), 0)

  expect_output(print(Graph$new()), "^Empty Graph\\.$")

  expect_output(Graph$new()$plot(), "^Empty Graph, not plotting\\.$")

  expect_equal(gunion(list()), Graph$new())

  expect_equal(gunion(list(Graph$new())), Graph$new())

  expect_equal(gunion(list(Graph$new(), Graph$new())), Graph$new())

  expect_equal(greplicate(Graph$new(), 100), Graph$new())

  expect_error(Graph$new()$add_edge("a", "b"), "Cannot add edge to empty Graph")

  expect_equal(Graph$new()$state, list())

})

test_that("Graph printer aux function calculates col widths well", {
  skip_on_cran()
  set.seed(8008135)

  effective_outwidth = function(colwidths, collimit) {
    colwidths[colwidths > collimit] = collimit + 3  # this is how data.table does it
    sum(colwidths + 1) + 4  # add 1 spacer between columns, and an extra margin of 4
  }

  test_outlimit = function(colwidths, outwidth) {
    collimit = calculate_collimit(colwidths, outwidth)
    if (effective_outwidth(colwidths, collimit) > outwidth) {
      # expectations are *really* slow, so only do them if they fail in this high volume test
      expect_lte(effective_outwidth(colwidths, collimit), outwidth)
    }
  }

  test_all_outlimits = function(colwidths) {
    for (outwidth in seq(max(length(colwidths), length(colwidths) * min(colwidths) - 2),
      length(colwidths) * max(colwidths) + 2)) {
      test_outlimit(colwidths, outwidth)
    }
  }

  for (numcols in 1:10) {
    replicate(1000, test_all_outlimits(rgeom(numcols, 0.1)))
  }

  expect_string("spurious expectation to show that we didn't skip this test")
})

test_that("Intermediate results are saved to Graph if requested", {
  g = PipeOpPCA$new() %>>% PipeOpCopy$new(2) %>>%
    gunion(list(PipeOpScale$new(), PipeOpNOP$new()))

  task = mlr_tasks$get("iris")

  res = g$train(task)

  expect_null(g$pipeops$pca$.result)

  g$keep_results = TRUE

  res = g$predict(task)

  restask = PipeOpPCA$new()$train(list(task))

  expect_equal(g$pipeops$pca$.result, restask)

  restask2 = PipeOpScale$new()$train(restask)

  expect_equal(g$pipeops$scale$.result, restask2)

  expect_equal(g$pipeops$nop$.result, restask)
})

test_that("Namespaces get loaded", {
  g = PipeOpPCA$new() %>>% PipeOpCopy$new(2) %>>%
    gunion(list(PipeOpScale$new(), PipeOpNOP$new()))

  g$train(mlr_tasks$get("iris"))

  g$pipeops$scale$packages = c("4rfjfw", "324r32")
  g$pipeops$nop$packages = c("4rfjfw", "9422228u")

  res = try(g$train(mlr_tasks$get("iris")), silent = TRUE)

  expect_class(res, "try-error")
  expect_subset(c(
    "  Error loading package 9422228u (required by nop):",
    "  Error loading package 4rfjfw (required by scale, nop):",
    "  Error loading package 324r32 (required by scale):"),
  strsplit(res, "\n")[[1]])
})

test_that("Graph State", {
  g = PipeOpPCA$new() %>>% PipeOpCopy$new(2) %>>%
    gunion(list(PipeOpScale$new(), PipeOpNOP$new()))

  g_clone = g$clone(deep = TRUE)
  task = mlr_tasks$get("iris")

  res = g$train(task)

  g_clone$state = g$state
  expect_deep_clone(g_clone, g$clone(deep = TRUE))

  expect_equal(g$predict(task), g_clone$predict(task))

})

test_that("Graph with vararg input", {
  t1 = tsk("iris")
  t2 = PipeOpPCA$new()$train(list(t1))[[1]]
  tcombined = PipeOpFeatureUnion$new()$train(list(t1, t2))[[1]]
  gr = as_graph(PipeOpFeatureUnion$new())
  expect_equal(tcombined, gr$train(list(t1, t2), single_input = FALSE)[[1]])
  expect_equal(tcombined, gr$predict(list(t1, t2), single_input = FALSE)[[1]])

  gr = gunion(list(PipeOpNOP$new(), gr, PipeOpNOP$new(id = "nop2"), PipeOpNOP$new(id = "nop3")))

  expect_equal(list(nop.output = 1, featureunion.output = tcombined, nop2.output = 2, nop3.output = 3),
               gr$train(list(1, t1, t2, 2, 3), single_input = FALSE))

})

test_that("Caching ABs", {
  gr = as_graph(po("scale"))
  expect_true(!gr$cache)
  gr$cache = TRUE
  expect_true(gr$cache)
})
