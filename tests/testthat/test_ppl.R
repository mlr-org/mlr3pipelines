context("mlr_graphs")

test_that("mlr_graphs access works", {

  expect_equal(
    ppl("robustify"),
    mlr_graphs$get("robustify")
  )


  expect_equal(
    ppl("robustify", char_to_fct.affect_columns = selector_all()),
    {
      graph = mlr_graphs$get("robustify")
      graph$param_set$values$char_to_fct.affect_columns = selector_all()
      graph
    }
  )

  expect_equal(
    ppl("bagging", graph = po("nop")),
    mlr_graphs$get("bagging", graph = po("nop"))
  )

  expect_equal(ppl(), mlr_graphs)
})


test_that("mlr_pipeops multi-access works", {

  expect_equal(
    ppls("robustify"),
    list(mlr_graphs$get("robustify"))
  )


  expect_equal(
    ppls("robustify", char_to_fct.affect_columns = selector_all()),
    {
      graph = mlr_graphs$get("robustify")
      graph$param_set$values$char_to_fct.affect_columns = selector_all()
      list(graph)
    }
  )

  expect_equal(
    ppls("bagging", graph = po("nop")),
    list(mlr_graphs$get("bagging", graph = po("nop")))
  )

  expect_equal(
    ppls(c(x = "robustify")),
    list(x = mlr_graphs$get("robustify"))
  )


  expect_equal(ppls(), mlr_graphs)

})
