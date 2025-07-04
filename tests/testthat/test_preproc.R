context("preproc")

test_that("preproc - basic sanity checks", {
  skip_if_not_installed("smotefamily")
  task = tsk("iris")
  op = po("scale")
  graph = as_graph(op)


  # Test indata
  expect_no_error(preproc(task, op))
  expect_no_error(preproc(task, op, predict = TRUE))
  op$state = NULL
  expect_data_table(preproc(task$data(), op))
  expect_data_table(preproc(task$data(), op, predict = TRUE))
  op$state = NULL
  expect_data_frame(preproc(as.data.frame(task$data()), op))
  expect_data_frame(preproc(as.data.frame(task$data()), op, predict = TRUE))
  op$state = NULL
  expect_error(preproc(NULL, op), "type 'data.frame'.*inherit from class 'Task'")
  expect_error(preproc(NULL, op, predict = TRUE), "type 'data.frame'.*inherit from class 'Task'")


  # Test processor
  # PipeOp processor already tested above
  expect_no_error(preproc(task, graph))
  expect_no_error(preproc(task, graph, predict = TRUE))
  expect_error(preproc(task, lrn("regr.rpart")), "no applicable method")

  # Test handling of multiple input channels
  gr = gunion(list(
    po("select_1", selector = selector_type("factor")) %>>% po("colapply", applicator = as.numeric),
    po("select_2", selector = selector_type("numeric")) %>>% po("pca")
  )) %>>% po("featureunion")
  expect_no_error(preproc(tsk("boston_housing"), gr))
  expect_no_error(preproc(tsk("boston_housing"), gr))

  # Test error for processor with more than one output channel
  expect_error(preproc(task, PipeOpDebugMulti$new(1, 2)), "must have exactly one output channel")
  expect_error(preproc(task, gunion(list(NULL, NULL))), "must have exactly one output channel")

  # Test error if processor cannot handle Tasks as input
  # Need a PipeOp that cannot handle Tasks as input to train
  PipeOpDebugNonTaskInput = R6Class("PipeOpDebugNonTaskInput",
    inherit = PipeOp,
    public = list(
      initialize = function(id = "non_task_input", param_set = ps()) {
        super$initialize(id = id, param_set = param_set,
          input = data.table(name = "input", train = "numeric", predict = "numeric"),
          output = data.table(name = "output", train = "character", predict = "character")
        )
      }),
    private = list(
      .train = function(inputs) {
        self$state = inputs
        list(as.character(inputs[[1L]]))
      },
      .predict = function(inputs) list(as.character(inputs[[1L]]))
    )
  )
  expect_error(preproc(task, PipeOpDebugNonTaskInput$new()), "Must inherit from class '.*', but has classes.*'Task'")
  expect_error(preproc(task, po("regravg"), state = list(), predict = TRUE), "Must inherit from class '.*', but has classes.*'Task'")

  # Test error for processors incapable of handling targetless tasks when indata is a data.frame
  expect_error(preproc(task$data(), po("smote")), "Must inherit from class 'TaskClassif'")
  # Need to have a trained PipeOp to test this for predict as well
  op2 = po("smote")
  op2$train(list(task))
  expect_error(preproc(task$data(), op2, predict = TRUE), "Must inherit from class 'TaskClassif'")

  # Test error for non-Task output of processor when indata is a data.frame
  # Construct a PipeOp that takes a Task as input but returns something else
  PipeOpDebugTaskToVec = R6Class("PipeOpDebugTaskToVec",
    inherit = PipeOp,
    public = list(
      initialize = function(id = "task_to_vec", param_set = ps()) {
        super$initialize(id = id, param_set = param_set,
          input = data.table(name = "input", train = "Task", predict = "Task"),
          output = data.table(name = "output", train = "character", predict = "character")
        )
      }),
    private = list(
      .train = function(inputs) {
        self$state = inputs
        list(inputs[[1L]]$feature_names)
      },
      .predict = function(inputs) list(inputs[[1L]]$feature_names)
    )
  )
  expect_error(preproc(task$data(), PipeOpDebugTaskToVec$new()), "Output channel of 'processor' does not return a Task")
  expect_error(preproc(task$data(), PipeOpDebugTaskToVec$new(), state = named_list("task_to_vec", task$data())), "Output channel of 'processor' does not return a Task")


  # Test state
  # Only basic tests, most checks are done by the graph
  expect_error(preproc(task, op, state = TRUE), "type 'list'.*or 'NULL'")

  # Test that there is no error if the state is correct
  op$train(list(task))
  expect_no_error(preproc(task, op, state = op$state))  # if processor is a PipeOp, i.e. internal conversion works
  expect_no_error(preproc(task, graph, state = list(scale = op$state)))  # if processor is a Graph

  # Test that there is an error if we pass a state for the Graph that does not have the correct names
  expect_error(preproc(task, graph, state = list(a = 1)), "Must be a subset of.*scale")
})

test_that("preproc - PipeOp processor", {
  task = tsk("iris")
  dt = task$data(cols = task$feature_names)
  processor = PipeOpScale$new()
  expected_train_out_task = processor$train(list(task))[[1L]]
  expected_predict_out_task = processor$predict(list(task))[[1L]]
  expected_train_out_dt = expected_train_out_task$data(cols = task$feature_names)
  expected_predict_out_dt = expected_train_out_task$data(cols = task$feature_names)
  # Extract state to pass to preproc in some of the tests
  state = processor$state
  processor$state = NULL


  # Untrained PipeOp, state NULL, predict FALSE -> train
  train_out = expect_no_error(preproc(task, processor, predict = FALSE))
  expect_equal(train_out, expected_train_out_task)
  expect_true(processor$is_trained)  # sufficient to test modification-in-place?
  processor$state = NULL

  # Untrained PipeOp, state NULL, predict TRUE -> error: Can't predict untrained PipeOp
  expect_error(preproc(task, processor, predict = TRUE), "Cannot predict.*not been trained yet")

  # Untrained PipeOp, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(task, processor, state = state, predict = FALSE), "Inconsistent function arguments")

  # Untrained PipeOp, state given, predict TRUE -> predict
  predict_out = expect_no_error(preproc(task, processor, state = state, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_task)

  # Trained PipeOp, state NULL, predict FALSE -> re-train
  train_out = expect_no_error(preproc(task, processor, predict = FALSE))
  expect_equal(train_out, expected_train_out_task)
  expect_true(processor$is_trained)

  # Trained PipeOp, state NULL, predict TRUE -> predict
  predict_out = expect_no_error(preproc(task, processor, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_task)

  # Trained PipeOp, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(task, processor, state = state, predict = FALSE), "Inconsistent function arguments")

  # Trained PipeOp, state given, predict TRUE -> predict
  predict_out = expect_no_error(preproc(task, processor, state = state, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_task)


  # Same tests for data.frame indata:
  processor$state = NULL

  # Untrained PipeOp, state NULL, predict FALSE -> train
  train_out = expect_no_error(preproc(dt, processor, predict = FALSE))
  expect_equal(train_out, expected_train_out_dt)
  expect_true(processor$is_trained)
  processor$state = NULL

  # Untrained PipeOp, state NULL, predict TRUE -> error: Can't predict untrained PipeOp
  expect_error(preproc(dt, processor, predict = TRUE), "Cannot predict.*not been trained yet")

  # Untrained PipeOp, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(dt, processor, state = state, predict = FALSE), "Inconsistent function arguments")

  # Untrained PipeOp, state given, predict TRUE -> predict
  predict_out = expect_no_error(preproc(dt, processor, state = state, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_dt)

  # Trained PipeOp, state NULL, predict FALSE -> re-train
  train_out = expect_no_error(preproc(dt, processor, predict = FALSE))
  expect_equal(train_out, expected_train_out_dt)
  expect_true(processor$is_trained)

  # Trained PipeOp, state NULL, predict TRUE -> predict
  predict_out = expect_no_error(preproc(dt, processor, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_dt)

  # Trained PipeOp, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(dt, processor, state = state, predict = FALSE), "Inconsistent function arguments")

  # Trained PipeOp, state given, predict TRUE -> predict
  predict_out = expect_no_error(preproc(dt, processor, state = state, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_dt)
})

test_that("preproc - Graph processor", {
  task = tsk("iris")
  dt = task$data(cols = task$feature_names)
  op = po("scale")
  processor = as_graph(op)
  expected_train_out_task = processor$train(task)[[1L]]
  expected_predict_out_task = processor$predict(task)[[1L]]
  expected_train_out_dt = expected_train_out_task$data(cols = task$feature_names)
  expected_predict_out_dt = expected_train_out_task$data(cols = task$feature_names)
  # Extract state to pass to preproc in some of the tests
  state = processor$state
  processor$state = NULL


  # Untrained Graph, state NULL, predict FALSE -> train
  train_out = expect_no_error(preproc(task, processor, predict = FALSE))
  expect_equal(train_out, expected_train_out_task)
  expect_true(processor$is_trained)  # sufficient to test modification-in-place?
  processor$state = NULL  # Reset PipeOp

  # Untrained Graph, state NULL, predict TRUE -> error: Can't predict untrained PipeOp
  expect_error(preproc(task, processor, predict = TRUE), "Cannot predict.*not been trained yet")

  # Untrained Graph, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(task, processor, state = state, predict = FALSE), "Inconsistent function arguments")

  # Untrained Graph, state given, predict TRUE -> predict
  predict_out = expect_no_error(preproc(task, processor, state = state, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_task)

  # Trained Graph, state NULL, predict FALSE -> re-train
  train_out = expect_no_error(preproc(task, processor, predict = FALSE))
  expect_equal(train_out, expected_train_out_task)
  expect_true(processor$is_trained)

  # Trained Graph, state NULL, predict TRUE -> predict
  predict_out = expect_no_error(preproc(task, processor, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_task)

  # Trained Graph, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(task, processor, state = state, predict = FALSE), "Inconsistent function arguments")

  # Trained Graph, state given, predict TRUE -> predict
  predict_out = expect_no_error(preproc(task, processor, state = state, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_task)


  # Same tests for data.frame indata:
  processor$state = NULL

  # Untrained PipeOp, state NULL, predict FALSE -> train
  train_out = expect_no_error(preproc(dt, processor, predict = FALSE))
  expect_equal(train_out, expected_train_out_dt)
  expect_true(processor$is_trained)
  processor$state = NULL

  # Untrained PipeOp, state NULL, predict TRUE -> error: Can't predict untrained PipeOp
  expect_error(preproc(dt, processor, predict = TRUE), "Cannot predict.*not been trained yet")

  # Untrained PipeOp, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(dt, processor, state = state, predict = FALSE), "Inconsistent function arguments")

  # Untrained PipeOp, state given, predict TRUE -> predict
  predict_out = expect_no_error(preproc(dt, processor, state = state, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_dt)

  # Trained PipeOp, state NULL, predict FALSE -> re-train
  train_out = expect_no_error(preproc(dt, processor, predict = FALSE))
  expect_equal(train_out, expected_train_out_dt)
  expect_true(processor$is_trained)

  # Trained PipeOp, state NULL, predict TRUE -> predict
  predict_out = expect_no_error(preproc(dt, processor, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_dt)

  # Trained PipeOp, state given, predict FALSE -> error: contradictory input
  expect_error(preproc(dt, processor, state = state, predict = FALSE), "Inconsistent function arguments")

  # Trained PipeOp, state given, predict TRUE -> predict
  predict_out = expect_no_error(preproc(dt, processor, state = state, predict = TRUE))
  expect_equal(predict_out, expected_predict_out_dt)
})
