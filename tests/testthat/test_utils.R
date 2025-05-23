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
})

test_that("task_filter_ex - group renaming in changed row_roles$use", {
  df = data.frame(
    target = 1:3,
    x = 1:3,
    grp = c("a", "b", "c")
  )
  task = TaskRegr$new(id = "test", backend = df, target = "target")
  task$set_col_roles("grp", "group")
  task$row_roles$use = c(1, 1, 2, 2, 3)

  tfiltered = task_filter_ex(task$clone(), c(1L, 1L, 2L, 2L, 3L))
  expect_equal(tfiltered$row_ids, c(1, 4, 2, 5, 3))
  expect_equal(tfiltered$groups$group, c("a", "a", "b", "b", "c"))

  tfiltered = task_filter_ex(task$clone(), c(1L, 1L, 1L, 1L, 2L, 2L, 3L))
  expect_equal(tfiltered$row_ids, c(1, 4, 5, 6, 2, 7, 3))
  expect_equal(tfiltered$groups$group, c("a", "a", "a_1", "a_1", "b", "b", "c"))

  tfiltered = task_filter_ex(task$clone(), c(1L, 1L, 1L, 1L, 3L))
  expect_equal(tfiltered$row_ids, c(1, 4, 5, 6, 3))
  expect_equal(tfiltered$groups$group, c("a", "a", "a_1", "a_1", "c"))

  tfiltered = task_filter_ex(task$clone(), 3L)
  expect_equal(tfiltered$row_ids, 3)
  expect_equal(tfiltered$groups$group, "c")

  tfiltered = task_filter_ex(task$clone(), c(1L, 1L, 1L, 1L, 3L, 1L, 1L))
  expect_equal(tfiltered$row_ids, c(1, 4, 5, 6, 3, 7, 8))
  expect_equal(tfiltered$groups$group, c("a", "a", "a_1", "a_1", "c", "a_2", "a_2"))

  expect_error(task_filter_ex(task$clone(), c(1, 1, 1, 2, 2, 3)), "constructed incomplete group")

  # Name collision
  df = data.frame(
    target = 1:3,
    x = 1:3,
    grp = c("a", "a_1", "a_3")
  )
  task = TaskRegr$new(id = "test", backend = df, target = "target")
  task$set_col_roles("grp", "group")
  task$row_roles$use = c(1, 1, 2, 2, 3)

  tfiltered = task_filter_ex(task, c(1L, 1L, 1L, 1L, 2L, 2L, 3L, 1L, 1L, 2L, 2L))
  expect_equal(tfiltered$row_ids, c(1, 4, 5, 6, 2, 7, 3, 8, 9, 10, 11))
  expect_equal(tfiltered$groups$group, c("a", "a", "a_2", "a_2", "a_1", "a_1", "a_3", "a_4", "a_4", "a_1_1", "a_1_1"))
})
