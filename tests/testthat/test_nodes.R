

test_that("pipe concatenation works", {
  gr = Graph$new()
  gr$add_node(BasicPO$new("testa"))
  gr$add_node(BasicPO$new("testb"))
  gr$add_node(BasicPO$new("testc"))
  freegr = Graph$new(gr)

  expect_equal(freegr, gr)  # breaks because of issue #149 in paradox

  gr[["testa"]]$next_node_channels[[1]] = gr[["testb"]]$in_channels[[1]]
  expect_equal(gr$node_list$testa$next_node_channels, gr$node_list$testb$in_channels)
  expect_equal(gr$node_list$testb$prev_node_channels, gr$node_list$testa$out_channels)
  expect_equal(gr$node_list$testc$next_node_channels, list(NULL))
  expect_equal(gr$node_list$testc$prev_node_channels, list(NULL))
  expect_equal(gr$node_list$testb$next_node_channels, list(NULL))
  expect_equal(gr$node_list$testa$prev_node_channels, list(NULL))
  expect_identical(gr$lhs$testa, gr$node_list$testa)
  expect_identical(gr$lhs$testc, gr$node_list$testc)
  expect_identical(gr$rhs$testb, gr$node_list$testb)
  expect_identical(gr$rhs$testc, gr$node_list$testc)
  expect_set_equal(names(gr$lhs), c("testa", "testc"))
  expect_set_equal(names(gr$rhs), c("testb", "testc"))

  gr[["testb"]]$prev_node_channels[[1]] = gr[["testa"]]$out_channels[[1]]  # same connection as before
  expect_equal(gr$node_list$testa$next_node_channels, gr$node_list$testb$in_channels)
  expect_equal(gr$node_list$testb$prev_node_channels, gr$node_list$testa$out_channels)
  expect_equal(gr$node_list$testc$next_node_channels, list(NULL))
  expect_equal(gr$node_list$testc$prev_node_channels, list(NULL))
  expect_equal(gr$node_list$testb$next_node_channels, list(NULL))
  expect_equal(gr$node_list$testa$prev_node_channels, list(NULL))
  expect_identical(gr$lhs$testa, gr$node_list$testa)
  expect_identical(gr$lhs$testc, gr$node_list$testc)
  expect_identical(gr$rhs$testb, gr$node_list$testb)
  expect_identical(gr$rhs$testc, gr$node_list$testc)
  expect_set_equal(names(gr$lhs), c("testa", "testc"))
  expect_set_equal(names(gr$rhs), c("testb", "testc"))

  expect_error({gr[["testb"]]$prev_node_channels[[2]] = gr[["testc"]]$out_channels[[1]]})

  gr[["testc"]]$prev_node_channels[[1]] = gr[["testa"]]$out_channels[[1]]  # reform connection
  expect_equal(gr$node_list$testa$next_node_channels, gr$node_list$testc$in_channels)
  expect_equal(gr$node_list$testc$prev_node_channels, gr$node_list$testa$out_channels)
  expect_equal(gr$node_list$testb$next_node_channels, list(NULL))
  expect_equal(gr$node_list$testb$prev_node_channels, list(NULL))
  expect_equal(gr$node_list$testc$next_node_channels, list(NULL))
  expect_equal(gr$node_list$testa$prev_node_channels, list(NULL))
  expect_identical(gr$lhs$testa, gr$node_list$testa)
  expect_identical(gr$lhs$testb, gr$node_list$testb)
  expect_identical(gr$rhs$testc, gr$node_list$testc)
  expect_identical(gr$rhs$testb, gr$node_list$testb)
  expect_set_equal(names(gr$lhs), c("testa", "testb"))
  expect_set_equal(names(gr$rhs), c("testc", "testb"))

  expect_error({gr$node_list$testc$prev_node_channels[[1]] = NULL})

  gr$node_list$testc$prev_node_channels[1] = list(NULL)  # sever connection
  expect_equal(gr, freegr)

  gr[["testc"]]$prev_node_channels[[1]] = gr[["testa"]]$out_channels[[1]]  # reform connection
  gr$add_node(BasicPO$new("testa2"))
  gr$add_node(BasicPO$new("testb2"))
  gr$add_node(BasicPO$new("testc2"))

  expect_set_equal(names(gr$node_list), c("testa2", "testb2", "testc2", "testa", "testb", "testc"))
  expect_set_equal(names(gr$lhs), c("testa2", "testb2", "testc2", "testa", "testb"))
  expect_set_equal(names(gr$rhs), c("testa2", "testb2", "testc2", "testc", "testb"))

  bpo = BasicPO$new("testid")
  bpo2 = BasicPO$new("testid2")
  bpo3 = BasicPO$new("testid3")

  ccgraph = bpo %>>% bpo2 %>>% bpo3

  gr = Graph$new()
  node = GraphNode$new(bpo, gr)
  node2 = GraphNode$new(bpo2, gr)
  node3 = GraphNode$new(bpo3, gr)
  node$next_node_channels[[1]] = node2$in_channels[[1]]
  node3$prev_node_channels[[1]] = node2$out_channels[[1]]

  expect_equal(gr$node_list, ccgraph$node_list)
  expect_equal(gr$lhs, ccgraph$lhs)
  expect_equal(gr$rhs, ccgraph$rhs)
  expect_equal(gr$in_channels, ccgraph$in_channels)
  expect_equal(gr$out_channels, ccgraph$out_channels)
  expect_equal(gr, ccgraph)

})
