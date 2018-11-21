context("Sugar")

test_that("Simple ops", {

  p1 = PipeOpNULL$new("p1")
  p2 = PipeOpNULL$new("p2")
  p3 = PipeOpNULL$new("p3")
  p4 = PipeOpNULL$new("p4")
  p5 = PipeOpNULL$new("p5")

  ### 1-to-1
  g = p1 %>>% p2 %>>% p3
  expect_class(g, "Graph")
  expect_equal(g$rhs[[1]]$pipeop, p3)

  ### 1-to-2
  g = p1 %>>% list(p2, p3)
  expect_class(g, "Graph")
  expect_equal(g$rhs[[1]]$pipeop, p2)
  expect_equal(g$rhs[[2]]$pipeop, p3)

  ### 1-to-2-to-1
  g = p1 %>>% list(p2, p3) %>>%  p4
  expect_equal(g$rhs[[1]]$pipeop, p4)
  expect_equal(g$rhs$p4$prev_nodes[[1]]$pipeop, p2)
  expect_equal(g$rhs$p4$prev_nodes[[2]]$pipeop, p3)

  ### 1-to-2-to-2 - error n-to-m not supported
  expect_error(p1 %>>% list(p2, p3) %>>%  list(p4, p5))

})
