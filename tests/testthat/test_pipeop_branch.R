context("PipeOpBranch")

test_that("PipeOpBranch - basic properties", {
  expect_pipeop_class(PipeOpBranch, list(1))
  expect_pipeop_class(PipeOpBranch, list(3))
  expect_error(PipeOpBranch$new(0))


  po = PipeOpBranch$new(3)
  expect_pipeop(po)


  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 3)
})


test_that("PipeOpBranch - train and predict", {
  # Define PipeOp's
  t1 = mlr_tasks$get("iris")

  branch = PipeOpBranch$new(2)
  tout = train_pipeop(branch, list(t1))
  expect_true(length(tout) == branch$outnum)
  expect_list(tout, len = branch$outnum)
  expect_class(tout[[branch$param_set$values$selection]], "Task")
  expect_equal(tout[[branch$param_set$values$selection]], t1)
  expect_class(tout[[2]], "NO_OP")
  expect_true(is_noop(tout[[2]]))

  branch2 = PipeOpBranch$new(2)
  branch2$param_set$values$selection = 2L
  tout = train_pipeop(branch2, list(t1))
  # expect_true(length(tout) == branch2$outnum)
  # expect_list(tout, len = branch2$outnum)
  expect_class(tout[[branch2$param_set$values$selection]], "Task")
  expect_equal(tout[[branch2$param_set$values$selection]], t1)
  expect_class(tout[[1]], "NO_OP")
  expect_true(is_noop(tout[[1]]))

  pout = predict_pipeop(branch, list(t1))
  # expect_true(length(pout) == branch$outnum)
  expect_equal(pout[[1]], t1)
  expect_class(pout[[2]], "NO_OP")
  expect_true(is_noop(pout[[2]]))
})

test_that("branch function", {

  po1 = PipeOpScale$new()
  po2 = PipeOpScale$new("scale2")
  po3 = PipeOpPCA$new()
  po4 = PipeOpSubsample$new()

  pofu = PipeOpFeatureUnion$new(2)
  pofu2 = PipeOpFeatureUnion$new(3)

  poco = PipeOpCopy$new(2)

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
    branch(po1, po2),
    PipeOpBranch$new(2) %>>% gunion(list(po1, po2)) %>>% PipeOpUnbranch$new(2)
  )

  # single input/output, named
  expect_graph_equal(
    branch(a = po1, b = po2),
    PipeOpBranch$new(c("a", "b")) %>>% gunion(list(po1, po2)) %>>% PipeOpUnbranch$new(c("a", "b"))
  )

  # single input/output, using .graph
  expect_graph_equal(
    branch(.graphs = list(po1, po2)),
    PipeOpBranch$new(2) %>>% gunion(list(po1, po2)) %>>% PipeOpUnbranch$new(2)
  )

  # single input/output, using both .graph and argument
  expect_graph_equal(
    branch(po1, .graphs = list(po2)),
    PipeOpBranch$new(2) %>>% gunion(list(po1, po2)) %>>% PipeOpUnbranch$new(2)
  )

  # single input/output, using both .graph and argument, named
  expect_graph_equal(
    branch(a = po1, .graphs = list(b = po2)),
    PipeOpBranch$new(c("a", "b")) %>>% gunion(list(po1, po2)) %>>% PipeOpUnbranch$new(c("a", "b"))
  )

  # error if some args named, some not named
  expect_error(branch(a = po1, .graphs = list(po2)), "One of the following")

  # prefix branch operations
  expect_graph_equal(
    branch(po1, po2, .prefix_branchops = "xy_"),
    PipeOpBranch$new(2, id = "xy_branch") %>>% gunion(list(po1, po2)) %>>% PipeOpUnbranch$new(2, id = "xy_unbranch")
  )

  # prefix branch operations, named
  expect_graph_equal(
    branch(a = po1, b = po2, .prefix_branchops = "xy_"),
    PipeOpBranch$new(c("a", "b"), id = "xy_branch") %>>%
      gunion(list(po1, po2)) %>>% PipeOpUnbranch$new(c("a", "b"), id = "xy_unbranch")
  )

  # prefix branch operations and paths
  expect_graph_equal(
    branch(po1, po2, .prefix_branchops = "xy_", .prefix_paths = TRUE),
    PipeOpBranch$new(2, id = "xy_branch") %>>%
      gunion(list(po1 = po1, po2 = po2)) %>>% PipeOpUnbranch$new(2, id = "xy_unbranch")
  )

  # prefix branch operations and paths, named
  expect_graph_equal(
    branch(a = po1, b = po2, .prefix_branchops = "xy_", .prefix_paths = TRUE),
    PipeOpBranch$new(c("a", "b"), id = "xy_branch") %>>%
      gunion(list(a = po1, b = po2)) %>>% PipeOpUnbranch$new(c("a", "b"), id = "xy_unbranch")
  )

  # more than one input
  expect_graph_equal(
    branch(gunion(list(po1, po3)) %>>% pofu, po2),
    gunion(list(
        PipeOpBranch$new(2),
        gunion(list(
            gunion(list(po1, po3)) %>>% pofu,
            po2)) %>>%
          PipeOpUnbranch$new(2)))$
      add_edge("branch", "scale", src_channel = "output1")$
      add_edge("branch", "pca", src_channel = "output1")$
      add_edge("branch", "scale2", src_channel = "output2")
  )

  # more than one input, named
  expect_graph_equal(
    branch(b = po2, a = gunion(list(po1, po3)) %>>% pofu),
    gunion(list(
      PipeOpBranch$new(c("b", "a")),
      gunion(list(
        po2,
        gunion(list(po1, po3)) %>>% pofu
      )) %>>%
        PipeOpUnbranch$new(c("b", "a"))))$
      add_edge("branch", "scale", src_channel = "a")$
      add_edge("branch", "pca", src_channel = "a")$
      add_edge("branch", "scale2", src_channel = "b")
  )

  # more than one output: error
  expect_error(branch(po1, poco), "Graph 2 must have exactly one output channel")
  expect_error(branch(a = po1, b = poco), "Graph b must have exactly one output channel")

  # more than one input, named, prefix branches and paths, named
  expect_graph_equal(
    branch(a = gunion(list(po1, po3)) %>>% pofu, b = pofu2, .prefix_branchops = "xy_", .prefix_paths = TRUE),
    gunion(list(
      PipeOpBranch$new(c("a", "b"), id = "xy_branch"),
      gunion(list(
        a = gunion(list(po1, po3)) %>>% pofu,
        b = pofu2
      )) %>>%
        PipeOpUnbranch$new(c("a", "b"), id = "xy_unbranch")))$
      add_edge("xy_branch", "a.scale", src_channel = "a")$
      add_edge("xy_branch", "a.pca", src_channel = "a")$
      add_edge("xy_branch", "b.featureunion", src_channel = "b", dst_channel = "input1")$
      add_edge("xy_branch", "b.featureunion", src_channel = "b", dst_channel = "input2")$
      add_edge("xy_branch", "b.featureunion", src_channel = "b", dst_channel = "input3")
  )
})
