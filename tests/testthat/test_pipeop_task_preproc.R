context("PipeOpTaskPreproc")

test_that("PipeOpTaskPreproc - basic properties", {
  expect_pipeop_class(PipeOpTaskPreproc, list(id = "potask"))

  po = PipeOpTaskPreproc$new("potask")

  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)
})

test_that("PipeOpTaskPreprocSimple - basic properties", {
  expect_pipeop_class(PipeOpTaskPreprocSimple, list(id = "posimple"))

  po = PipeOpTaskPreprocSimple$new(id = "posimple")
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)
})

test_that("Wrong affect_columns errors", {
  POPP = R6Class("POPP",
    inherit = PipeOpTaskPreproc,
    private = list(
      .train_dt = function(dt, levels, target) dt,
      .predict_dt = function(dt, levels) dt
    )
  )
  tsk = tsk("boston_housing_classic")
  po = POPP$new("foo", param_vals = list(affect_columns = is.factor))
  expect_pipeop(po)
  expect_error(po$train(list(tsk)), "affected_cols")

  po = POPP$new("foo", param_vals = list(affect_columns = function(x) x$target_names))
  expect_error(po$train(list(tsk)), "affected_cols")
})

test_that("PipeOpTaskPreproc - fix for #864 works", {
  # Fixes #864: A column that is a feature and something else does not loose the other role during training or
  # prediction of PipeOps with affect_columns set to non-NULL
  POPP = R6Class("POPP",
    inherit = PipeOpTaskPreproc,
    private = list(
      .train_dt = function(dt, levels, target) dt,
      .predict_dt = function(dt, levels) dt
    )
  )
  po = POPP$new("test", param_vals = list(affect_columns = selector_name("Petal.Length")))
  expect_pipeop(po)
  task = mlr_tasks$get("iris")
  task$col_roles$order = "Petal.Width"

  train_out = po$train(list(task$clone(deep = TRUE)))[[1L]]
  expect_equal(train_out$col_roles, task$col_roles)

  predict_out = po$predict(list(task$clone(deep = TRUE)))[[1L]]
  expect_equal(predict_out$col_roles, task$col_roles)
})

test_that("PipeOpTaskPreproc - affect_columns and selectors behavior", {
  # We test that the correct columns get written to state and train/predict is performed for the correct columns
  PipeOpDebugAffectCols = R6Class("PipeOpDebugAffectCols",
    inherit = PipeOpTaskPreproc,
    public = list(
      initialize = function(id = "debug_affectcols", param_vals = list()) {
        super$initialize(id = id, can_subset_cols = TRUE)
      }),
    private = list(
      .train_task = function(task) {
        # using *_task methods to get easy access to feature types
        catf("Training %s", self$id)

        cols = private$.select_cols(task)
        if (!length(cols)) return(task)
        dt = task$data(cols = cols)

        dt = as.data.table(set_names(pmap(list(dt, task$feature_types$type), function(col, type) {
          switch(type,
            character = rep("new", length(col)),
            numeric = rep(1, length(col)),
            logical = rep(TRUE, length(col)),
            factor = factor(rep("new", length(col)), levels = unique(c(levels(col), "new")))
          )
        }), task$feature_types$id))

        task$cbind(dt)
      },
      .predict_task = function(task) {
        catf("Predicting %s", self$id)

        cols = private$.select_cols(task)
        if (!length(cols)) return(task)
        dt = task$data(cols = cols)

        dt = as.data.table(set_names(pmap(list(dt, task$feature_types$type), function(col, type) {
          switch(type,
            character = rep("new", length(col)),
            numeric = rep(1, length(col)),
            logical = rep(TRUE, length(col)),
            factor = factor(rep("new", length(col)), levels = unique(c(levels(col), "new")))
          )
        }), task$feature_names))

        task$cbind(dt)
      }
    )
  )

  # Define columns for re-use
  chr_old = rep("old", 5)
  chr_new = rep("new", 5)
  num_old = c(NA, rep(0, 4))
  num_new = rep(1, 5)
  lgl_old = rep(FALSE, 5)
  lgl_new = rep(TRUE, 5)

  task = TaskUnsupervised$new(id = "test", backend = data.table(
    num1 = num_old, num2 = num_old, chr = chr_old, lgl = lgl_old))

  op = PipeOpDebugAffectCols$new()

  op$param_set$set_values(affect_columns = selector_all())
  dt = data.table(num1 = num_new, num2 = num_new, chr = chr_new, lgl = lgl_new)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)  # task$cbind does not return same column order
  expect_set_equal(op$state$affected_cols, c("num1", "num2", "chr", "lgl"))
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

  op$param_set$set_values(affect_columns = selector_none())
  dt = data.table(num1 = num_old, num2 = num_old, chr = chr_old, lgl = lgl_old)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)
  expect_set_equal(op$state$affected_cols, character(0))
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

  op$param_set$set_values(affect_columns = selector_type(c("numeric", "character")))
  dt = data.table(num1 = num_new, num2 = num_new, chr = chr_new, lgl = lgl_old)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)
  expect_set_equal(op$state$affected_cols, c("num1", "num2", "chr"))
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

  op$param_set$set_values(affect_columns = selector_missing())
  dt = data.table(num1 = num_new, num2 = num_new, chr = chr_old, lgl = lgl_old)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)
  expect_set_equal(op$state$affected_cols, c("num1", "num2"))
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

  op$param_set$set_values(affect_columns = selector_name("num1"))
  dt = data.table(num1 = num_new, num2 = num_old, chr = chr_old, lgl = lgl_old)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)
  expect_set_equal(op$state$affected_cols, "num1")
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

  op$param_set$set_values(affect_columns = selector_grep("num"))
  dt = data.table(num1 = num_new, num2 = num_new, chr = chr_old, lgl = lgl_old)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)
  expect_set_equal(op$state$affected_cols, c("num1", "num2"))
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

  op$param_set$set_values(affect_columns = selector_invert(selector_type(c("numeric"))))
  dt = data.table(num1 = num_old, num2 = num_old, chr = chr_new, lgl = lgl_new)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)
  expect_set_equal(op$state$affected_cols, c("lgl", "chr"))
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

  op$param_set$set_values(affect_columns = selector_union(selector_name("num1"), selector_type("logical")))
  dt = data.table(num1 = num_new, num2 = num_old, chr = chr_old, lgl = lgl_new)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)
  expect_set_equal(op$state$affected_cols, c("num1", "lgl"))
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

  op$param_set$set_values(affect_columns = selector_setdiff(selector_type("numeric"), selector_name("num1")))
  dt = data.table(num1 = num_old, num2 = num_new, chr = chr_old, lgl = lgl_old)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)
  expect_set_equal(op$state$affected_cols, "num2")
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

  op$param_set$set_values(affect_columns = selector_intersect(selector_type("numeric"), selector_name("num1")))
  dt = data.table(num1 = num_new, num2 = num_old, chr = chr_old, lgl = lgl_old)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)
  expect_set_equal(op$state$affected_cols, "num1")
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

  fct_onelvl_old = factor(rep("old", 5), levels = c("old", "new"))
  fct_onelvl_new = factor(rep("new", 5), levels = c("old", "new"))
  fct_twolvl_old = factor(c("old1", "old1", "old2", "old2", "old2"), levels = c("old1", "old2", "new"))
  fct_twolvl_new = factor(rep("new", 5), levels = c("old1", "old2", "new"))
  task = TaskUnsupervised$new(id = "test", backend = data.table(fct_onelvl = fct_onelvl_old, fct_twolvl = fct_twolvl_old))
  op$param_set$set_values(affect_columns = selector_cardinality_greater_than(1))
  dt = data.table(fct_onelvl = fct_onelvl_old, fct_twolvl = fct_twolvl_new)
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)
  expect_set_equal(op$state$affected_cols, "fct_twolvl")
  expect_equal(op$train(list(task))[[1]]$data(), dt, ignore.col.order = TRUE)

})

