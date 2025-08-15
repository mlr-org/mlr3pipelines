context("ppl - pipeline_branch")

test_that("Branching Pipeline", {
  skip_if_not_installed("rpart")
  lrns = map(list(lrn("classif.rpart"), lrn("classif.featureless")), po)
  task = mlr_tasks$get("boston_housing_classic")
  gr = pipeline_branch(lrns)

  expect_graph(gr)
  expect_data_table(gr$param_set$deps)
  par_ids = unlist(map(lrns, function(x) as_graph(x)$param_set$ids()))
  expect_subset(par_ids, gr$param_set$ids())

  gr$train(tsk("iris"))
  out = gr$predict(tsk("iris"))$unbranch.output
  assert_prediction(out)

  gr = pipeline_branch(lrns, prefix_branchops = "foo_", prefix_paths = TRUE)
  assert_true(gr$input$name == "foo_branch.input")
  assert_true(gr$output$name == "foo_unbranch.output")
  assert_true(all(grepl("foo", gr$param_set$params[["foo_branch.selection"]]$levels)))
})


test_that("Branching Pipeline extended tests", {
  skip_on_cran()  # takes too long
  po1 = po("scale")
  po2 = po("scale", id = "scale2")
  po3 = po("pca")
  po4 = po("subsample")

  pofu = po("featureunion", 2)
  pofu2 = po("featureunion", 3)

  poco = po("copy", 2)

  # bring graphs into comparable form: sort $pipeops
  canonical = function(graph) {
    graph$pipeops = graph$pipeops[order(names(graph$pipeops))]
    graph$edges = graph$edges[chorder(graph$edges)]
    graph
  }
  expect_graph_equal = function(g1, g2) {
    expect_equal(canonical(g1), canonical(g2))
  }

  # single input/output
  expect_graph_equal(
    pipeline_branch(list(po1, po2)),
    po("branch", 2) %>>% gunion(list(po1, po2)) %>>% po("unbranch", 2)
  )

  # single input/output, named
  expect_graph_equal(
    pipeline_branch(list(a = po1, b = po2)),
    po("branch", c("a", "b")) %>>% gunion(list(po1, po2)) %>>% po("unbranch", c("a", "b"))
  )

  # single input/output, using .graph
  expect_graph_equal(
    pipeline_branch(graphs = list(po1, po2)),
    po("branch", 2) %>>% gunion(list(po1, po2)) %>>% po("unbranch", 2)
  )

  ## # single input/output, using both .graph and argument
  ## expect_graph_equal(
  ##   pipeline_branch(po1, .graphs = list(po2)),
  ##   po("branch", 2) %>>% gunion(list(po1, po2)) %>>% po("unbranch", 2)
  ## )

  ## # single input/output, using both .graph and argument, named
  ## expect_graph_equal(
  ##   pipeline_branch(a = po1, .graphs = list(b = po2)),
  ##   po("branch", c("a", "b")) %>>% gunion(list(po1, po2)) %>>% po("unbranch", c("a", "b"))
  ## )

  ## # error if some args named, some not named
  ## expect_error(pipeline_branch(a = po1, .graphs = list(po2)), "One of the following")

  # prefix branch operations
  expect_graph_equal(
    pipeline_branch(list(po1, po2), prefix_branchops = "xy_"),
    po("branch", 2, id = "xy_branch") %>>% gunion(list(po1, po2)) %>>% po("unbranch", 2, id = "xy_unbranch")
  )

  # prefix branch operations, named
  expect_graph_equal(
    pipeline_branch(list(a = po1, b = po2), prefix_branchops = "xy_"),
    po("branch", c("a", "b"), id = "xy_branch") %>>%
      gunion(list(po1, po2)) %>>% po("unbranch", c("a", "b"), id = "xy_unbranch")
  )

  # prefix branch operations and paths
  expect_graph_equal(
    pipeline_branch(list(po1, po2), prefix_branchops = "xy_", prefix_paths = TRUE),
    po("branch", 2, id = "xy_branch") %>>%
      gunion(list(po1 = po1, po2 = po2)) %>>% po("unbranch", 2, id = "xy_unbranch")
  )

  # prefix branch operations and paths, named
  expect_graph_equal(
    pipeline_branch(list(a = po1, b = po2), prefix_branchops = "xy_", prefix_paths = TRUE),
    po("branch", c("a", "b"), id = "xy_branch") %>>%
      gunion(list(a = po1, b = po2)) %>>% po("unbranch", c("a", "b"), id = "xy_unbranch")
  )

  # more than one input
  expect_graph_equal(
    pipeline_branch(list(gunion(list(po1, po3)) %>>% pofu, po2)),
    gunion(list(
        po("branch", 2),
        gunion(list(
            gunion(list(po1, po3)) %>>% pofu,
            po2)) %>>%
          po("unbranch", 2)))$
      add_edge("branch", "scale", src_channel = "output1")$
      add_edge("branch", "pca", src_channel = "output1")$
      add_edge("branch", "scale2", src_channel = "output2")
  )

  # more than one input, named
  expect_graph_equal(
    pipeline_branch(list(b = po2, a = gunion(list(po1, po3)) %>>% pofu)),
    gunion(list(
      po("branch", c("b", "a")),
      gunion(list(
        po2,
        gunion(list(po1, po3)) %>>% pofu
      )) %>>%
        po("unbranch", c("b", "a"))))$
      add_edge("branch", "scale", src_channel = "a")$
      add_edge("branch", "pca", src_channel = "a")$
      add_edge("branch", "scale2", src_channel = "b")
  )

  # more than one output: error
  expect_error(pipeline_branch(list(po1, poco)), "Graph 2 must have exactly one output channel")
  expect_error(pipeline_branch(list(a = po1, b = poco)), "Graph b must have exactly one output channel")

  # more than one input, named, prefix branches and paths, named
  expect_graph_equal(
    pipeline_branch(list(a = gunion(list(po1, po3)) %>>% pofu, b = pofu2), prefix_branchops = "xy_", prefix_paths = TRUE),
    gunion(list(
      po("branch", c("a", "b"), id = "xy_branch"),
      gunion(list(
        a = gunion(list(po1, po3)) %>>% pofu,
        b = pofu2
      )) %>>%
        po("unbranch", c("a", "b"), id = "xy_unbranch")))$
      add_edge("xy_branch", "a.scale", src_channel = "a")$
      add_edge("xy_branch", "a.pca", src_channel = "a")$
      add_edge("xy_branch", "b.featureunion", src_channel = "b", dst_channel = "input1")$
      add_edge("xy_branch", "b.featureunion", src_channel = "b", dst_channel = "input2")$
      add_edge("xy_branch", "b.featureunion", src_channel = "b", dst_channel = "input3")
  )
})
