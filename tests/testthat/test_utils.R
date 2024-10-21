context("utils")

# test changed row_roles$use (some removed, duplicates)
# test with task with trailing rows filtered out

test_that("task_filter_ex - Basic functionality", {
  task = mlr_tasks$get("iris")

  rowidx = as.integer(c(1, 2, 3, 2, 1, 2, 3, 2, 1))  # annoying and unnecessary mlr3 type strictness

  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))

  task$select(c("Petal.Length", "Petal.Width"))
  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))
})

test_that("task_filter_ex - task with col role group", {
  task = mlr_tasks$get("iris")
  task$cbind(data.frame(grp = rep(c("A", "A", "B", "C", "D"), 30)))
  task$set_col_roles("grp", "group")

  rowidx = as.integer(c(1, 2, 3, 2, 1, 2, 3, 2, 1, 4))

  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))
  expect_equal(
    table(tfiltered$groups$group),
    table(c("A", "A", "A_1", "A_1", "A_2", "A_2", "A_3", "B", "B_1", "C"))
  )
})

test_that("task_filter_ex - changed row_roles$use", {

})
