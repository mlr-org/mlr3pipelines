context("PipeOpSubsample")

test_that("PipeOpSubsample - basic properties", {
  op = PipeOpSubsample$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  train_pipeop(op, inputs = list(task))
  predict_pipeop(op, inputs = list(task))

  expect_datapreproc_pipeop_class(PipeOpSubsample, task = task, predict_like_train = FALSE, deterministic_train = FALSE)
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

test_that("PipeOpSubsample - use_groups - Sanity Checks", {
  op = PipeOpSubsample$new()
  op$param_set$set_values(frac = 0.5)

  df_two = data.frame(
    target = runif(200),
    x = rnorm(200),
    grp = rep(c("g1", "g2"), each = 100)
  )
  task_two = TaskRegr$new(id = "two", backend = df_two, target = "target")
  task_two$set_col_roles("grp", "group")

  df_three = data.frame(
    target = runif(400),
    x = rnorm(400),
    grp = c(rep(c("g1", "g2"), each = 100), rep("g3", 200))
  )
  task_three = TaskRegr$new(id = "three", backend = df_three, target = "target")
  task_three$set_col_roles("grp", "group")

  set.seed(2024)
  sampled_groups = list(10)
  for (i in seq_len(10)) {
    sampled_groups[[i]] = unique(op$train(list(task_two))[[1]]$groups$group)
  }
  expect_setequal(sampled_groups, list("g1", "g2"))

  set.seed(2024)
  sampled_groups = list(20)
  for (i in seq_len(20)) {
    sampled_groups[[i]] = unique(op$train(list(task_three))[[1]]$groups$group)
  }
  expect_setequal(sampled_groups, list("g1", "g2", "g3", c("g1", "g2"), c("g2", "g1")))

  # Tests with replace = TRUE
  op$param_set$set_values(replace = TRUE)

  set.seed(2024)
  sampled_groups = list(10)
  for (i in seq_len(10)) {
    sampled_groups[[i]] = unique(op$train(list(task_two))[[1]]$groups$group)
  }
  expect_setequal(sampled_groups, list("g1", "g2"))

  set.seed(2024)
  sampled_groups = list(20)
  for (i in seq_len(20)) {
    sampled_groups[[i]] = unique(op$train(list(task_three))[[1]]$groups$group)
  }
  expect_setequal(sampled_groups, list("g1", "g2", "g3", c("g1", "g2"), c("g2", "g1"), c("g1", "g1_1"), c("g2", "g2_1")))

  # use_groups = TRUE and stratify = TRUE should throw an error
  op$param_set$set_values(stratify = TRUE, use_groups = TRUE)
  expect_error(op$train(list(task)))
})

test_that("PipeOpSubsample - use_groups - Modified row_roles$use", {
  op = PipeOpSubsample$new()
  op$param_set$set_values(replace = TRUE)

  test_df = data.frame(
    target = runif(300),
    x1 = runif(300),
    x2 = runif(300),
    grp = rep(paste0("g", 1:10), 30)
  )
  task = TaskRegr$new(id = "test", backend = test_df, target = "target")
  task$set_col_roles("grp", "group")

  # TESTCASE: Exclude some rows from row_roles$use
  task$row_roles$use = setdiff(task$row_roles$use, task$groups[group %in% c("g1", "g2", "g3", "g4")]$row_id)
  set.seed(1234)
  train_out = op$train(list(task))[[1]]
  # Grouped data are kept together
  grps_out = table(train_out$groups$group)
  dimnames(grps_out)[[1]] = sub("_\\d*", "", names(grps_out))
  expect_equal(grps_out, table(task$groups$group)[names(grps_out)])

  # TESTCASE: Set some rows to be included multiple times in row_roles$use
  #           Effectively, this just increases group sizes.
  #           However, task_filter_ex ignores duplicates in row_roles$use (i.e. only samples the rows that were given)
  grps = c("g5", "g6", "g7")
  task$row_roles$use = c(task$row_roles$use, task$groups[group %in% grps]$row_id)
  set.seed(2345)
  train_out = op$train(list(task))[[1]]

  # Grouped data are kept together
  grps_out = table(train_out$groups$group)
  dimnames(grps_out)[[1]] = sub("_\\d*", "", names(grps_out))
  expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
})

test_that("PipeOpSubsample - use_groups - Equal group sizes", {
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
  set.seed(2024)
  train_out = op$train(list(task))[[1]]
  # Grouped data are kept together
  grps_out = table(train_out$groups$group)
  expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
  # Frac is adhered to as best as possible
  expect_equal(train_out$nrow / task$nrow, op$param_set$values$frac, tolerance = 0.025)

  # TESTCASE: changed frac, replace = TRUE
  op$param_set$set_values(frac = 2, replace = TRUE)
  set.seed(2024)
  train_out = op$train(list(task))[[1]]
  # Grouped data are kept together
  grps_out = table(train_out$groups$group)
  # need to do this via dimnames since names(dimnames(grps_out)) would be changed by other methods resulting in failing all.equal()
  dimnames(grps_out)[[1]] = sub("_\\d*", "", names(grps_out))
  expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
  # Frac is adhered to as best as possible
  expect_equal(train_out$nrow / task$nrow, op$param_set$values$frac, tolerance = 0.025)
})

test_that("PipeOpSubsample - use_groups - Large variance in group sizes", {
  op = PipeOpSubsample$new()

  # Data with one very large group
  set.seed(2024)
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
  set.seed(2024)
  train_out = op$train(list(task))[[1]]
  # Grouped data are kept together
  grps_out = table(train_out$groups$group)
  expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
  # Frac is adhered to as best as possible
  expect_equal(train_out$nrow / task$nrow, op$param_set$values$frac, tolerance = 0.1)

  # TESTCASE: changed frac, replace = TRUE
  op$param_set$set_values(frac = 2, replace = TRUE)
  set.seed(2024)
  train_out = op$train(list(task))[[1]]
  # grouped data are kept together
  grps_out = table(train_out$groups$group)
  # need to do this via dimnames since names(dimnames(grps_out)) would be changed by other methods resulting in failing all.equal()
  dimnames(grps_out)[[1]] = sub("_\\d*", "", names(grps_out))
  expect_equal(grps_out, table(task$groups$group)[names(grps_out)])
  # Frac is adhered to as best as possible
  expect_equal(train_out$nrow / task$nrow, op$param_set$values$frac, tolerance = 0.1)
})
