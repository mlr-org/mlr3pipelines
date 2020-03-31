context("ppl - pipeline_branch")

test_that("Branching Pipeline", {
  lrns = map(list(lrn("classif.rpart"), lrn("classif.featureless")), po)
  task = mlr_tasks$get("boston_housing")
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
