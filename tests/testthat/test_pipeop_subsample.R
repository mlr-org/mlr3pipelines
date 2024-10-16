context("PipeOpSubsample")

test_that("PipeOpSubsample - basic properties", {
  op = PipeOpSubsample$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))

  expect_datapreproc_pipeop_class(PipeOpSubsample, task = task,
    predict_like_train = FALSE, deterministic_train = FALSE)
})

test_that("PipeOpSubsample works unstratified", {
  task = mlr_tasks$get("iris")
  po = PipeOpSubsample$new()

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(po$param_set$values$frac * task$nrow))

  po = PipeOpSubsample$new()
  po$param_set$values$frac = 0.7

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 0.7))
  expect_subset(tnew[[1]]$row_roles$use, task$row_roles$use)

  pnew = predict_pipeop(po, list(task))
  expect_true(pnew[[1]]$nrow == task$nrow)
  expect_equal(pnew[[1]], task)

  task = mlr_tasks$get("iris")$filter(1L)  # actually has to be an int m(
  po = PipeOpSubsample$new()
  tnew = train_pipeop(po, list(task))

  task = mlr_tasks$get("boston_housing_classic")$filter(1L)  # actually has to be an int m(
  po = PipeOpSubsample$new()
  po$param_set$values = list(stratify = TRUE, frac = 0.6)
  expect_error(train_pipeop(po, list(task)))
})

test_that("PipeOpSubsample works stratified", {
  task = mlr_tasks$get("iris")

  po = PipeOpSubsample$new()
  po$param_set$values = list(stratify = TRUE, use_groups = FALSE, frac = 0.6, replace = FALSE)
  expect_class(po, "PipeOpSubsample")

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 0.6))
  # Proportions as expected
  expect_equal(table(tnew[[1]]$data(cols = tnew[[1]]$target_names)),
    table(list(Species = rep(c("setosa", "versicolor", "virginica"), 30))))

  po = PipeOpSubsample$new()
  po$param_set$values = list(stratify = TRUE, use_groups = FALSE, frac = 0.6, replace = TRUE)
  expect_class(po, "PipeOpSubsample")

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 0.6))
  # Proportions as expected
  expect_equal(table(tnew[[1]]$data(cols = tnew[[1]]$target_names)),
    table(list(Species = rep(c("setosa", "versicolor", "virginica"), 30))))

  po = PipeOpSubsample$new()
  po$param_set$values = list(stratify = TRUE, use_groups = FALSE, frac = 2, replace = TRUE)
  expect_class(po, "PipeOpSubsample")

  tnew = train_pipeop(po, list(task))
  expect_true(tnew[[1]]$nrow == ceiling(task$nrow * 2))
  # Proportions as expected
  expect_equal(table(tnew[[1]]$data(cols = tnew[[1]]$target_names)),
    table(list(Species = rep(c("setosa", "versicolor", "virginica"), 100))))
})

test_that("PipeOpSubsample - Grouped Data - Equal group sizes", {
  op = PipeOpSubsample$new()

  # Data with groups of the same size
  test_df = data.frame(
    target = runif(3000),
    x1 = runif(3000),
    x2 = runif(3000),
    grp = rep(paste0("g", 1:100), 30)
  )
  task = TaskRegr$new(id = "test", backend = test_df, target = "target")
  task$set_col_roles("grp", "group")

  # TESTCASE: default frac, replace = FALSE
  train_out = op$train(list(task))[[1]]
  # Grouped data are kept together
  grps_out = table(train_out$groups$group)
  expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
  # Frac is adhered to as best as possible
  expect_equal(train_out$nrow / task$nrow, op$param_set$values$frac, tolerance = 0.025)
  # Test for uniformity?

  # TESTCASE: changed frac, replace = TRUE
  op$param_set$set_values(frac = 2, replace = TRUE)
  train_out = op$train(list(task))[[1]]
  # Grouped data are kept together
  grps_out = table(train_out$groups$group)
  expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
  # Frac is adhered to as best as possible
  #expect_equal(train_out$nrow / task$nrow, op$param_set$values$frac, tolerance = 0.025)

  # TESTCASE: Exclude some rows from row_roles$use and one group completely
  task$row_roles$use = setdiff(seq(1, 2800), task$groups[group == "g1", row_id])
  train_out = op$train(list(task))[[1]]
  # Test that all sampled rows are in row_roles$use
  expect_in(train_out$row_ids, task$row_roles$use)
  # # Grouped data are kept together
  # grps_out = table(train_out$groups$group)
  # expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
  # # Frac is adhered to as best as possible
  # expect_equal(train_out$nrow / task$nrow, op$param_set$values$frac, tolerance = 0.025)


  # TESTCASE: Set some rows to be included multiple times in row_roles$use
  task$row_roles$use = c(task$row_roles$use, seq(1:50))
  expect_no_error(op$train(list(task)))
  # train_out = op$train(list(task))[[1]]
  # # Grouped data are kept together
  # grps_out = table(train_out$groups$group)
  # expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
  # # Frac is adhered to as best as possible
  # expect_equal(train_out$nrow / task$nrow, op$param_set$values$frac, tolerance = 0.025)


  # TESTCASE: use_groups = TRUE and stratify = TRUE should throw an error
  op$param_set$set_values(stratify = TRUE, use_groups = TRUE)
  expect_error(op$train(list(task)))

})

test_that("PipeOpSubsample - Grouped data - Large variance in group sizes", {
  op = PipeOpSubsample$new()

  # Data with one very large group
  test_df = data.frame(
    target = runif(3000),
    x1 = runif(3000),
    x2 = runif(3000),
    grp = c(
      sample(paste0("g", 1:50), 1500, replace = TRUE),  # small groups
      sample(paste0("G", 1:5), 1500, replace = TRUE)  # large groups
    )
  )
  task = TaskRegr$new(id = "test", backend = test_df, target = "target")
  task$set_col_roles("grp", "group")

  # TESTCASE: default frac, replace = FALSE
  train_out = op$train(list(task))[[1]]
  # Grouped data are kept together
  grps_out = table(train_out$groups$group)
  expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
  # Frac is adhered to as best as possible
  expect_equal(train_out$nrow / task$nrow, op$param_set$values$frac, tolerance = 0.1)

  # TESTCASE: changed frac, replace = TRUE
  op$param_set$set_values(frac = 2, replace = TRUE)
  train_out = op$train(list(task))[[1]]
  # grouped data are kept together
  grps_out = table(train_out$groups$group)
  expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
  # Frac is adhered to as best as possible
  expect_equal(train_out$nrow / task$nrow, op$param_set$values$frac, tolerance = 0.1)

})

test_that("task filter utility function", {
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
