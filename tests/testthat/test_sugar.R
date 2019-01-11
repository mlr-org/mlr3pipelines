
# FIXME: unit test file for the arrow-op, needs to be cleaned up and only test that connecting properties of the aarow op work

context("Sugar")

# test_that("Simple ops", {

#   p1 = PipeOpNULL$new("p1")
#   p2 = PipeOpNULL$new("p2")
#   p3 = PipeOpNULL$new("p3")
#   p4 = PipeOpNULL$new("p4")
#   p5 = PipeOpNULL$new("p5")

#   ### 1-to-1
#   g = p1 %>>% p2 %>>% p3
#   expect_class(g, "Graph")
#   expect_equal(g$rhs[[1]]$pipeop, p3)

#   ### 1-to-2
#   g = p1 %>>% list(p2, p3)
#   expect_class(g, "Graph")
#   expect_equal(g$rhs[[1]]$pipeop, p2)
#   expect_equal(g$rhs[[2]]$pipeop, p3)

#   ### 1-to-2-to-1
#   g = p1 %>>% list(p2, p3) %>>%  p4
#   expect_equal(g$rhs[[1]]$pipeop, p4)
#   expect_equal(g$rhs$p4$prev_nodes[[1]]$pipeop, p2)
#   expect_equal(g$rhs$p4$prev_nodes[[2]]$pipeop, p3)

#   ### 1-to-2-to-2 - error n-to-m not supported
#   expect_error(p1 %>>% list(p2, p3) %>>%  list(p4, p5))

# })

# test_that("%>=>% - n-to-n", {

#   p1 = PipeOpNULL$new("p1")
#   p2 = PipeOpNULL$new("p2")
#   p3 = PipeOpNULL$new("p3")
#   p4 = PipeOpNULL$new("p4")
#   p5 = PipeOpNULL$new("p5")
#   p6 = PipeOpNULL$new("p6")
#   p7 = PipeOpNULL$new("p7")

#   g = p1 %>=>% p2 %>=>% p3

#   g = p1 %>>% list(p2, p3, p4) %>=>% list(p5, p6, p7)
#   rhs_pipeops = lapply(g$rhs, function(x) x$pipeop)

#   # correct rhs
#   expect_equal(rhs_pipeops[[1]], p5)
#   expect_equal(rhs_pipeops[[2]], p6)
#   expect_equal(rhs_pipeops[[3]], p7)

#   # only one prev node for each rhs
#   expect_true(
#     all(vapply(g$rhs, function(x) length(x$prev_nodes) == 1, FUN.VALUE = TRUE))
#   )

#   # correct prev node - orders matter!
#   expect_equal(g$rhs$p5$prev_nodes[[1]]$pipeop, p2)
#   expect_equal(g$rhs$p6$prev_nodes[[1]]$pipeop, p3)
#   expect_equal(g$rhs$p7$prev_nodes[[1]]$pipeop, p4)

# })

# test_that("%>x>% - n-to-m", {

#   p1 = PipeOpNULL$new("p1")
#   p2 = PipeOpNULL$new("p2")
#   p3 = PipeOpNULL$new("p3")
#   p4 = PipeOpNULL$new("p4")
#   p5 = PipeOpNULL$new("p5")
#   p6 = PipeOpNULL$new("p6")
#   p7 = PipeOpNULL$new("p7")

#   g = p1 %>x>% p2 %>x>% p3

#   g = p1 %>x>% list(p2, p3, p4) %>x>% list(p5, p6, p7)
#   rhs_pipeops = lapply(g$rhs, function(x) x$pipeop)

#   # correct rhs
#   expect_equal(rhs_pipeops[[1]], p5)
#   expect_equal(rhs_pipeops[[2]], p6)
#   expect_equal(rhs_pipeops[[3]], p7)

#   # each element of rhs has 3 predecessors
#   expect_true(
#     all(vapply(g$rhs, function(x) length(x$prev_nodes) == 3, FUN.VALUE = TRUE))
#   )

#   expect_equal(g$rhs$p5$prev_nodes[[1]]$pipeop, p2)
#   expect_equal(g$rhs$p6$prev_nodes[[1]]$pipeop, p2)
#   expect_equal(g$rhs$p7$prev_nodes[[1]]$pipeop, p2)

#   expect_equal(g$rhs$p5$prev_nodes[[2]]$pipeop, p3)
#   expect_equal(g$rhs$p6$prev_nodes[[2]]$pipeop, p3)
#   expect_equal(g$rhs$p7$prev_nodes[[2]]$pipeop, p3)

#   expect_equal(g$rhs$p5$prev_nodes[[3]]$pipeop, p4)
#   expect_equal(g$rhs$p6$prev_nodes[[3]]$pipeop, p4)
#   expect_equal(g$rhs$p7$prev_nodes[[3]]$pipeop, p4)

#   g = list(p1, p2) %>x>% list(p3, p4, p5) %>x>% list(p6, p7)

#   expect_false(g[["p1"]]$has_lhs)
#   expect_false(g[["p2"]]$has_lhs)
#   expect_equal(length(g[["p3"]]$prev_nodes), 2)
#   expect_equal(length(g[["p4"]]$prev_nodes), 2)
#   expect_equal(length(g[["p5"]]$prev_nodes), 2)
#   expect_equal(length(g[["p6"]]$prev_nodes), 3)
#   expect_equal(length(g[["p7"]]$prev_nodes), 3)

# })
