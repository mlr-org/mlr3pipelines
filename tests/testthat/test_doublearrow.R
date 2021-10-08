context("double-arrow")

test_that("Simple ops do what we expect", {
  p1 = PipeOpNOP$new("p1")
  p2 = PipeOpNOP$new("p2")
  p3 = PipeOpNOP$new("p3")
  p4 = PipeOpNOP$new("p4")
  p5 = PipeOpNOP$new("p5")
  p6 = PipeOpNOP$new("p6")
  p7 = PipeOpNOP$new("p7")

  g = p1 %>>% p2 %>>% p3
  expect_class(g, "Graph")
  expect_equal(g$rhs[[1]], p3$id)

  g2 = concat_graphs(concat_graphs(p1, p2), p3)
  expect_equal(g, g2)

  expect_graph(p1 %>>% gunion(list(p2, p3)))
  expect_error(gunion(list(p2, p3)) %>>% p1, "mismatching number of inputs / outputs")

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
  p1 = PipeOpNOP$new("p1")
  p2 = PipeOpNOP$new("p2")

  g3 = Graph$new()$add_pipeop(PipeOpNOP$new("p3"))
  g4 = Graph$new()$add_pipeop(PipeOpNOP$new("p4"))

  g = p1 %>>% p2
  expect_deep_clone(g$pipeops$p1, p1)
  expect_deep_clone(g$pipeops$p2, p2)

  g = g3 %>>% g4
  expect_deep_clone(g$pipeops$p3, g3$pipeops$p3)
  expect_deep_clone(g$pipeops$p4, g4$pipeops$p4)
})

test_that("neutral elements", {
  p = PipeOpNOP$new("p1")
  g1 = p %>>% NULL
  expect_graph(g1)
  expect_true((length(g1$pipeops) == 1L) && (names(g1$pipeops) == "p1"))
  g2 = NULL %>>% p
  expect_equal(g1$pipeops, g2$pipeops)
})

test_that("triple-arrow", {

  p1 = PipeOpNOP$new("p1")
  p2 = PipeOpNOP$new("p2")
  p3 = PipeOpNOP$new("p3")

  gr = as_graph(p1)

  expect_equal(gr$pipeops, list(p1 = p1))

  expect_deep_clone(gr$pipeops$p1, p1)

  p2graph1 = as_graph(p2)

  gr2 = gr %>>>% p2graph1

  expect_equal(gr2$pipeops, list(p1 = p1, p2 = p2))

  expect_deep_clone(p2graph1, as_graph(p2))


  expect_identical(gr2, gr)

  expect_deep_clone(gr2$pipeops$p2, p2)

  gr3 = gr %>>>% p3

  expect_identical(gr3, gr)
  expect_identical(gr2, gr)

  expect_equal(gr3$pipeops, list(p1 = p1, p2 = p2, p3 = p3))

  expect_equal(gr3$edges, data.table(src_id = c("p1", "p2"), src_channel = "output", dst_id = c("p2", "p3"), dst_channel = "input"))

  gr = as_graph(p1)

  gr2_2 = concat_graphs(gr, p2graph1, in_place = TRUE)

  expect_identical(gr2_2, gr)
  expect_deep_clone(p2graph1, as_graph(p2))

  gr3_2 = concat_graphs(gr, p3, in_place = TRUE)

  expect_identical(gr3_2, gr)
  expect_identical(gr3_2, gr2_2)

  expect_deep_clone(gr3_2, gr3)

  gr = as_graph(p1)

  gr3_3 = chain_graphs(list(gr, p2graph1, p3))

  expect_deep_clone(gr, as_graph(p1))

  expect_deep_clone(gr3, gr3_3)

  expect_deep_clone(p2graph1, as_graph(p2))

  gr3_3 = chain_graphs(list(gr, p2graph1, p3), in_place = TRUE)

  expect_identical(gr, gr3_3)
  expect_deep_clone(gr3_3, gr3_2)

  expect_deep_clone(p2graph1, as_graph(p2))


  # not mutable in-place

  gr = p1 %>>>% p2graph1

  expect_deep_clone(p1, PipeOpNOP$new("p1"))
  expect_deep_clone(p2graph1, as_graph(p2))
  expect_deep_clone(gr, p1 %>>% p2)


  gr = chain_graphs(list(p1, p2graph1), in_place = TRUE)

  expect_deep_clone(p1, PipeOpNOP$new("p1"))
  expect_deep_clone(p2graph1, as_graph(p2))
  expect_deep_clone(gr, p1 %>>% p2)

})

