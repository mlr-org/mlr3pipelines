context("double-arrow")

test_that("Simple ops do what we expect", {
  p1 = PipeOpNULL$new("p1")
  p2 = PipeOpNULL$new("p2")
  p3 = PipeOpNULL$new("p3")
  p4 = PipeOpNULL$new("p4")
  p5 = PipeOpNULL$new("p5")
  p6 = PipeOpNULL$new("p6")
  p7 = PipeOpNULL$new("p7")

  g = p1 %>>% p2 %>>% p3
  expect_class(g, "Graph")
  expect_equal(g$rhs[[1]], p3$id)

  expect_error(p1 %>>% gunion(list(p2, p3)), "mismatching number of inputs / outputs")

  g = p1 %>>% PipeOpBranch$new(2) %>>% gunion(list(p2, p3))
  expect_class(g, "Graph")
  expect_set_equal(g$rhs, c(p2$id, p3$id))

  expect_error(gunion(list(p2, p3)) %>>% p4, "mismatching number of inputs / outputs")

  g = p1 %>>% PipeOpBranch$new(2) %>>% gunion(list(p2, p3)) %>>% PipeOpUnbranch$new(2) %>>% p4

  expect_equal(g$rhs, p4$id)

  # check that edges are as they should be, but don't make assumptions about order of edges:
  # convert edges DT into comma separated values
  edges_csv = csvify(g$edges)
  expect_set_equal(edges_csv,
    c("p1,output,branch,input", "branch,output1,p2,input", "branch,output2,p3,input",
      "p2,output,unbranch,input1", "p3,output,unbranch,input2", "unbranch,output,p4,input"))

  g = p1 %>>% PipeOpBranch$new(3) %>>% gunion(list(p2, p3, p4)) %>>% gunion(list(p5, p6, p7))

  edges_csv = csvify(g$edges)
  expect_set_equal(edges_csv,
    c("p1,output,branch,input", "branch,output1,p2,input", "branch,output2,p3,input", "branch,output3,p4,input",
      "p2,output,p5,input", "p3,output,p6,input", "p4,output,p7,input"))

  g = gunion(list(p2, p3, p4)) %>>% gunion(list(p5, p6, p7))

  edges_csv = csvify(g$edges)
  expect_set_equal(edges_csv,
    c("p2,output,p5,input", "p3,output,p6,input", "p4,output,p7,input"))
})

test_that("operations make deep copies", {
  p1 = PipeOpNULL$new("p1")
  p2 = PipeOpNULL$new("p2")

  g3 = Graph$new()$add_pipeop(PipeOpNULL$new("p3"))
  g4 = Graph$new()$add_pipeop(PipeOpNULL$new("p4"))

  g = p1 %>>% p2
  expect_deep_clone(g$pipeops$p1, p1)
  expect_deep_clone(g$pipeops$p2, p2)

  g = g3 %>>% g4
  expect_deep_clone(g$pipeops$p3, g3$pipeops$p3)
  expect_deep_clone(g$pipeops$p4, g4$pipeops$p4)
})
