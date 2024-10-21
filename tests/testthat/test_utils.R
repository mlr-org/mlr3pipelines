context("utils")

# test on normal task
# test after select
# test with group
# test renaming with group
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

  task$select(character(0))
  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))
  expect_equal(tfiltered$groups$group, task$groups[rowidx]$group)
})

test_that("task_filter_ex - task with col role group", {
  task = mlr_tasks$get("iris")
  task$cbind(data.frame(grp = sample(LETTERS[1:8], size = task$nrow, replace = TRUE)))
  task$set_col_roles("grp", "group")

  rowidx = as.integer(c(1, 2, 3, 2, 1, 2, 3, 2, 1))

  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))
  expect_equal(tfiltered$groups$group, task$groups[rowidx]$group)

})

test_that("task_filter_ex - changed row_roles$use", {

})
