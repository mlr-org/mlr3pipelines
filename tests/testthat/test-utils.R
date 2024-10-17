context("utils")

test_that("task_filter_ex", {
  task = mlr_tasks$get("iris")

  rowidx = as.integer(c(1, 2, 3, 2, 1, 2, 3, 2, 1))  # annoying and unnecessary mlr3 type strictness

  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))

  task$select(c("Petal.Length", "Petal.Width"))
  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))

  task$set_col_roles("Petal.Length", "group")
  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))
  expect_equal(tfiltered$groups$group, task$groups[rowidx]$group)

  task$select(character(0))
  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))
  expect_equal(tfiltered$groups$group, task$groups[rowidx]$group)
})
