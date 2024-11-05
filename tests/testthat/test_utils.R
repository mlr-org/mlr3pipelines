context("utils")

test_that("task_filter_ex - Basic functionality", {
  task = mlr_tasks$get("iris")

  rowidx = as.integer(c(1, 2, 3, 2, 1, 2, 3, 2, 1))  # annoying and unnecessary mlr3 type strictness

  # Equal to task$filter() in case of no duplicates
  tfiltered_ex = task_filter_ex(task$clone(), unique(rowidx))
  tfiltered = task$clone()$filter(unique(rowidx))
  expect_equal(tfiltered_ex$data(), tfiltered$data())

  # With duplicates
  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))

  # After selecting columns
  task$select(c("Petal.Length", "Petal.Width"))
  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))
})

test_that("task_filter_ex - filtered trailing rows", {
  task = as_task_classif(rbind(iris, iris, iris), target = "Species", id = "test")
  task$filter(301:450)

  rowidx = as.integer(300 + c(1, 2, 3, 2, 1, 2, 3, 2, 1, 4))

  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))
})

test_that("task_filter_ex - task with column role group", {
  task = mlr_tasks$get("iris")
  task$cbind(data.frame(grp = rep(c("A", "A", "B", "C", "D"), 30)))
  task$set_col_roles("grp", "group")

  rowidx = as.integer(c(1, 2, 3, 2, 1, 2, 3, 2, 1, 4))

  # Basic test
  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))
  expect_equal(
    table(tfiltered$groups$group),
    table(c("A", "A", "B", "A_1", "A_1", "A_2", "B_1", "A_3", "A_2", "C"))
  )

  # Name collision
  task$cbind(data.frame(grp = rep(c("A", "A_1", "B", "C", "D"), 30)))
  task$set_col_roles("grp", "group")

  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))
  expect_equal(
    tfiltered$groups$group,
    c("A", "A_1", "B", "A_1_1", "A_2", "A_1_2", "B_1", "A_1_3", "A_3", "C")
  )
})

test_that("task_filter_ex - changed row_roles$use", {
  task = mlr_tasks$get("iris")

  rowidx = as.integer(c(1, 2, 3, 2, 1, 2, 3, 2, 1))

  task$row_roles$use = seq(1, 50)
  tfiltered = task_filter_ex(task$clone(), rowidx)
  expect_equal(tfiltered$data(), task$data(rows = rowidx))

  task$row_roles$use = c(seq(1, 50), seq(1, 20))
  tfiltered = task_filter_ex(task$clone(), 50L + rowidx)
  expect_equal(tfiltered$data(), task$data(rows = 50L + rowidx))
})
