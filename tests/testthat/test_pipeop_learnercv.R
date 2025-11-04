context("PipeOpLearnerCV")

test_that("PipeOpLearnerCV - basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerCV$new(lrn)
  expect_pipeop(po$clone(), check_ps_default_values = FALSE)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  task = mlr_tasks$get("iris")
  tsk = train_pipeop(po, list(task = task))[[1]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_equal(task$target_names, tsk$target_names)
  expect_equal(task$class_names, tsk$class_names)
  vals = factor(unique(tsk$data(cols = tsk$feature_names)$response))
  expect_character(setdiff(vals, task$class_names), len = 0)

  tsk = predict_pipeop(po, list(task = task))[[1]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_equal(task$target_names, tsk$target_names)
  expect_equal(task$class_names, tsk$class_names)
  vals = factor(unique(tsk$data(cols = tsk$feature_names)$response))
  expect_character(setdiff(vals, task$class_names), len = 0)

  lrn = mlr_learners$get("classif.featureless")
  iris_with_unambiguous_mode = mlr_tasks$get("iris")$filter(c(1:30, 70:150))  # want featureless learner without randomness
  expect_datapreproc_pipeop_class(PipeOpLearnerCV,
    list(lrn), iris_with_unambiguous_mode, predict_like_train = FALSE, deterministic_train = FALSE, check_ps_default_values = FALSE)
  # 'insample' PipeOpLearnerCV with deterministic Learner is deterministic in every regard!
  expect_datapreproc_pipeop_class(PipeOpLearnerCV,
    list(lrn, param_vals = list(resampling.method = "insample")), iris_with_unambiguous_mode, check_ps_default_values = FALSE)

  expect_error(PipeOpLearnerCV$new())

})

test_that("PipeOpLearnerCV - param values", {
  skip_if_not_installed("rpart")
  lrn = mlr_learners$get("classif.rpart")
  polrn = PipeOpLearnerCV$new(lrn)
  expect_true(all(c("minsplit", "resampling.method", "resampling.folds", "resampling.predict_method") %in% polrn$param_set$ids()))
  expect_false(any(c("resampling.se_aggr", "resampling.se_aggr_rho") %in% polrn$param_set$ids()))
  expect_equal(polrn$param_set$values, list(
    resampling.method = "cv",
    resampling.folds = 3,
    resampling.keep_response = FALSE,
    resampling.predict_method = "full",
    xval = 0
  ))
  polrn$param_set$values$minsplit = 2
  expect_equal(polrn$param_set$values, list(
    resampling.method = "cv",
    resampling.folds = 3,
    resampling.keep_response = FALSE,
    resampling.predict_method = "full",
    minsplit = 2,
    xval = 0
  ))
  polrn$param_set$values$resampling.folds = 4
  expect_equal(polrn$param_set$values, list(
    resampling.method = "cv",
    resampling.folds = 4,
    resampling.keep_response = FALSE,
    resampling.predict_method = "full",
    minsplit = 2,
    xval = 0
  ))
})

test_that("PipeOpLearnerCV se aggregation default matches learner predict_type", {
  learner_resp = LearnerRegrDebug$new()
  learner_resp$predict_type = "response"
  po_resp = PipeOpLearnerCV$new(learner_resp)
  expect_true("resampling.se_aggr" %in% po_resp$param_set$ids())
  expect_identical(po_resp$param_set$values$resampling.se_aggr, "none")

  learner_se = LearnerRegrDebug$new()
  learner_se$predict_type = "se"
  po_se = PipeOpLearnerCV$new(learner_se)
  expect_true(all(c("resampling.se_aggr", "resampling.se_aggr_rho") %in% po_se$param_set$ids()))
  expect_identical(po_se$param_set$values$resampling.se_aggr, "predictive")

  learner_no_se = lrn("regr.rpart")
  po_no_se = PipeOpLearnerCV$new(learner_no_se)
  expect_false(any(c("resampling.se_aggr", "resampling.se_aggr_rho") %in% po_no_se$param_set$ids()))
})

test_that("PipeOpLearnerCV - cv ensemble averages fold learners", {
  skip_if_not_installed("rpart")
  task = tsk("iris")
  learner = lrn("classif.rpart", predict_type = "prob")
  po = PipeOpLearnerCV$new(learner,
    param_vals = list(
      resampling.folds = 2,
      resampling.keep_response = TRUE,
      resampling.predict_method = "cv_ensemble"
    )
  )

  trained_task = po$train(list(task))[[1]]
  expect_setequal(trained_task$feature_names, c(
    sprintf("%s.response", po$id),
    paste0(po$id, ".prob.", task$class_names)
  ))
  expect_equal(po$state$predict_method, "cv_ensemble")
  expect_length(po$state$cv_model_states, 2)

  result_task = po$predict(list(task))[[1]]
  prob_feature_names = paste0(po$id, ".prob.", task$class_names)

  pred_probs = as.matrix(result_task$data(rows = task$row_ids, cols = prob_feature_names))
  manual_probs = mlr3misc::map(po$state$cv_model_states, function(state) {
    clone = learner$clone(deep = TRUE)
    clone$state = state
    dt = as.data.table(clone$predict(task))
    data.table::setorder(dt, row_ids)
    as.matrix(dt[, paste0("prob.", task$class_names), with = FALSE])
  })
  manual_prob = Reduce(`+`, manual_probs) / length(manual_probs)
  colnames(manual_prob) = prob_feature_names
  expect_equal(pred_probs, manual_prob)

  result_response = result_task$data(rows = task$row_ids, cols = sprintf("%s.response", po$id))[[1]]
  expect_equal(
    as.character(result_response),
    task$class_names[max.col(manual_prob)]
  )
})

test_that("PipeOpLearnerCV - cv ensemble drops response when requested", {
  skip_if_not_installed("rpart")
  task = tsk("iris")
  learner = lrn("classif.rpart", predict_type = "prob")
  po = PipeOpLearnerCV$new(learner,
    param_vals = list(
      resampling.predict_method = "cv_ensemble"
    )
  )
  po$train(list(task))
  result_task = po$predict(list(task))[[1]]
  expect_true(all(sprintf("%s.prob.%s", po$id, task$class_names) %in% result_task$feature_names))
  expect_false(any(sprintf("%s.response", po$id) %in% result_task$feature_names))
})

test_that("PipeOpLearnerCV - cv ensemble averages classif responses", {
  skip_if_not_installed("rpart")
  task = tsk("iris")
  learner = lrn("classif.rpart", predict_type = "response")
  po = PipeOpLearnerCV$new(learner,
    param_vals = list(resampling.predict_method = "cv_ensemble")
  )
  po$train(list(task))
  expect_equal(po$state$predict_method, "cv_ensemble")
  expect_true(length(po$state$cv_model_states) > 1)

  result_task = po$predict(list(task))[[1]]
  response_feature = sprintf("%s.response", po$id)
  expect_setequal(result_task$feature_names, response_feature)

  manual_responses = mlr3misc::map(po$state$cv_model_states, function(state) {
    clone = learner$clone(deep = TRUE)
    clone$state = state
    pred_dt = as.data.table(clone$predict(task))
    data.table::setorderv(pred_dt, "row_ids")
    as.character(pred_dt$response)
  })

  manual_matrix = as.matrix(do.call(cbind, manual_responses))
  n = nrow(manual_matrix)
  prob_matrix = vapply(task$class_names, function(cls) rowMeans(manual_matrix == cls), numeric(n))
  if (!is.matrix(prob_matrix)) {
    prob_matrix = matrix(prob_matrix, ncol = length(task$class_names))
  }
  colnames(prob_matrix) = task$class_names
  manual_response = task$class_names[max.col(prob_matrix, ties.method = "first")]
  manual_response = factor(manual_response, levels = task$class_names)

  observed_response = result_task$data(rows = task$row_ids, cols = response_feature)[[1]]
  expect_equal(as.character(observed_response), as.character(manual_response))

  learner_prediction = po$learner_model$predict(task)
  expect_equal(as.character(learner_prediction$response), as.character(manual_response))
  pred_dt = as.data.table(learner_prediction)
  data.table::setorderv(pred_dt, "row_ids")
  graph_prob = as.matrix(pred_dt[, paste0("prob.", task$class_names), with = FALSE])
  colnames(graph_prob) = task$class_names
  expect_equal(graph_prob, prob_matrix)
})

test_that("PipeOpLearnerCV - cv ensemble averages regression predictions", {
  skip_if_not_installed("rpart")
  task = TaskRegr$new("mtcars", backend = data.table::as.data.table(mtcars), target = "mpg")
  learner = lrn("regr.rpart")
  po = PipeOpLearnerCV$new(learner,
    param_vals = list(resampling.folds = 2, resampling.predict_method = "cv_ensemble")
  )
  po$train(list(task))
  result_task = po$predict(list(task))[[1]]
  feature_name = sprintf("%s.response", po$id)
  expect_true(feature_name %in% result_task$feature_names)

  manual_responses = mlr3misc::map(po$state$cv_model_states, function(state) {
    clone = learner$clone(deep = TRUE)
    clone$state = state
    pred = clone$predict(task)
    pred$response
  })
  manual_average = Reduce(`+`, manual_responses) / length(manual_responses)
  expect_equal(result_task$data(rows = task$row_ids, cols = feature_name)[[1]], manual_average)

  graph_pred = po$learner_model$predict(task)
  expect_equal(graph_pred$response, manual_average)
  expect_true(is.null(graph_pred$se) || all(is.na(graph_pred$se)))
})

test_that("PipeOpLearnerCV - cv ensemble handles multiplicity", {
  skip_if_not_installed("rpart")
  tasks = Multiplicity(tsk("iris"), tsk("sonar"))
  learner = lrn("classif.rpart", predict_type = "prob")
  po = po("learner_cv", learner,
    param_vals = list(resampling.predict_method = "cv_ensemble")
  )

  train_out = po$train(list(tasks))[[1]]
  expect_class(train_out, "Multiplicity")
  expect_equal(length(train_out), 2L)
  expect_true(all(mlr3misc::map_lgl(train_out, inherits, what = "Task")))

  expect_class(po$state, "Multiplicity")
  expect_true(all(mlr3misc::map_lgl(po$state, function(st) st$predict_method == "cv_ensemble")))
  expect_true(all(mlr3misc::map_lgl(po$state, function(st) length(st$cv_model_states) == po$param_set$values$resampling.folds)))

  predict_out = po$predict(list(tasks))[[1]]
  expect_class(predict_out, "Multiplicity")
  expect_equal(length(predict_out), 2L)
  expect_true(all(mlr3misc::map_lgl(predict_out, inherits, what = "Task")))

  orig_tasks = as.list(tasks)
  pred_tasks = as.list(predict_out)
  expect_true(all(unlist(Map(function(pred_task, orig_task) {
    all(pred_task$feature_names %in% paste0(po$id, ".prob.", orig_task$class_names))
  }, pred_tasks, orig_tasks))))
})

test_that("PipeOpLearnerCV - cv ensemble requires resampling method cv", {
  skip_if_not_installed("rpart")
  po = PipeOpLearnerCV$new(
    lrn("classif.rpart"),
    param_vals = list(resampling.method = "insample", resampling.predict_method = "cv_ensemble")
  )
  expect_error(po$train(list(tsk("iris"))), "cv_ensemble")
})

test_that("PipeOpLearnerCV - learner_model returns averaged ensemble", {
  skip_if_not_installed("rpart")
  task = tsk("iris")
  learner = lrn("classif.rpart", predict_type = "prob")
  po = PipeOpLearnerCV$new(learner,
    param_vals = list(resampling.predict_method = "cv_ensemble", resampling.keep_response = TRUE)
  )
  po$train(list(task))

  learner_model = po$learner_model
  expect_class(learner_model, "GraphLearner")

  task_prediction = po$predict(list(task))[[1]]
  dt_po = task_prediction$data(rows = task$row_ids, cols = task_prediction$feature_names)

  graph_prediction = learner_model$predict(task)
  expect_class(graph_prediction, "PredictionClassif")
  dt_graph = as.data.table(graph_prediction)
  data.table::setorder(dt_graph, row_ids)

  prob_cols = paste0(po$id, ".prob.", task$class_names)
  graph_prob_cols = paste0("prob.", task$class_names)
  graph_matrix = as.matrix(dt_graph[, graph_prob_cols, with = FALSE])
  colnames(graph_matrix) = prob_cols
  expect_equal(as.matrix(dt_po[, prob_cols, with = FALSE]), graph_matrix)

  expect_equal(
    as.character(dt_po[[sprintf("%s.response", po$id)]]),
    as.character(dt_graph$response)
  )
})

test_that("PipeOpLearnerCV - cv ensemble with predict_type = 'se'", {
  skip_if_not_installed("mlr3learners")
  task = tsk("mtcars")
  learner = lrn("regr.lm", predict_type = "se")
  po = PipeOpLearnerCV$new(learner,
    param_vals = list(resampling.predict_method = "cv_ensemble")
  )
  po$train(list(task))
  result_task = po$predict(list(task))[[1]]

  response_col = sprintf("%s.response", po$id)
  se_col = sprintf("%s.se", po$id)
  expect_true(all(c(response_col, se_col) %in% result_task$feature_names))

  manual_preds = mlr3misc::map(po$state$cv_model_states, function(state) {
    clone = learner$clone(deep = TRUE)
    clone$state = state
    clone$predict(task)
  })

  manual_dt = mlr3misc::map(manual_preds, function(pred) {
    dt = as.data.table(pred)
    data.table::setorderv(dt, "row_ids")
    list(response = dt$response, se = dt$se)
  })
  manual_response = Reduce(`+`, mlr3misc::map(manual_dt, "response")) / length(manual_dt)
  expect_equal(result_task$data(rows = task$row_ids, cols = response_col)[[1]], manual_response)

  weights = rep(1 / length(manual_dt), length(manual_dt))
  manual_se = mlr3pipelines:::aggregate_se_weighted(
    mlr3misc::map(manual_dt, "response"),
    mlr3misc::map(manual_dt, "se"),
    weights = weights,
    method = "predictive",
    rho = 0
  )
  expect_equal(result_task$data(rows = task$row_ids, cols = se_col)[[1]], manual_se)
})

test_that("PipeOpLearnerCV - within resampling", {
  skip_if_not_installed("rpart")
  lrn = mlr_learners$get("classif.rpart")
  gr = GraphLearner$new(PipeOpLearnerCV$new(lrn) %>>% po(id = "l2", lrn))
  rr = resample(tsk("iris"), gr, rsmp("holdout"))
  expect_class(rr, "ResampleResult")
})

test_that("PipeOpLearnerCV - insample resampling", {
  skip_if_not_installed("rpart")
  lrn = mlr_learners$get("classif.featureless")
  iris_with_unambiguous_mode = mlr_tasks$get("iris")$filter(c(1:30, 70:150))  # want featureless learner without randomness

  polrn = PipeOpLearnerCV$new(lrn, param_vals = list(resampling.method = "insample"))
  expect_equal(polrn$train(list(iris_with_unambiguous_mode))[[1]]$data(),
    cbind(iris_with_unambiguous_mode$data(cols = "Species"),
      classif.featureless.response = factor("virginica", levels = levels(iris[[5]]))))

  lrn = mlr_learners$get("classif.rpart")
  polrn = PipeOpLearnerCV$new(lrn, param_vals = list(resampling.method = "insample"))
  expect_equal(polrn$train(list(iris_with_unambiguous_mode))[[1]],
    polrn$predict(list(iris_with_unambiguous_mode))[[1]])
})

test_that("PipeOpLearnerCV - graph but no id", {
  skip_if_not_installed("rpart")
  g = PipeOpNOP$new() %>>% PipeOpLearner$new(LearnerClassifRpart$new())
  po = PipeOpLearnerCV$new(g)
  expect_string(po$id)
})

test_that("PipeOpLearnerCV - model active binding to state", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerCV$new(lrn)
  task = mlr_tasks$get("iris")

  # before training states are NULL
  expect_null(po$state)
  expect_equal(po$learner$state, po$state)
  expect_equal(po$learner_model$state, po$state)

  # after training learner_model's state and state are equal
  train_out = po$train(list(task))
  train_state = po$state
  expect_null(po$learner$state)
  expect_equal(po$learner_model$state, train_state)

  # after predicting states are unchanged
  predict_out = po$predict(list(task))
  expect_equal(po$state, train_state)
  expect_null(po$learner$state)
  expect_equal(po$learner_model$state, po$state)
})

test_that("predict_type", {
  skip_if_not_installed("rpart")
  expect_equal(po("learner_cv", lrn("classif.rpart", predict_type = "response"))$predict_type, "response")
  expect_equal(po("learner_cv", lrn("classif.rpart", predict_type = "prob"))$predict_type, "prob")

  lcv <- po("learner_cv", lrn("classif.rpart", predict_type = "prob"))

  lcv$predict_type = "response"
  expect_equal(lcv$predict_type, "response")
  expect_equal(lcv$learner$predict_type, "response")

  expect_equal(lcv$train(list(tsk("iris")))[[1]]$feature_names, "classif.rpart.response")

  lcv$predict_type = "prob"

  expect_equal(lcv$predict_type, "prob")
  expect_equal(lcv$learner$predict_type, "prob")

  expect_subset(c("classif.rpart.prob.virginica", "classif.rpart.prob.setosa", "classif.rpart.prob.versicolor"),
    lcv$train(list(tsk("iris")))[[1]]$feature_names)

})

test_that("marshal", {
  task = tsk("iris")
  po_lrn = as_pipeop(po("learner_cv", learner = lrn("classif.debug")))
  po_lrn$train(list(task))
  po_state = po_lrn$state
  expect_class(po_state, "pipeop_learner_cv_state")
  po_state_marshaled = marshal_model(po_state, inplace = FALSE)
  expect_class(po_state_marshaled, "pipeop_learner_cv_state_marshaled")
  expect_true(is_marshaled_model(po_state_marshaled))
  expect_equal(po_state, unmarshal_model(po_state_marshaled))
})

test_that("marshal multiplicity", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("bbotk")
  if (!"mlr3pipelines" %in% rownames(installed.packages())) {
    expect_man_exists <<- function(man) {
      checkmate::expect_string(man, na.ok = TRUE, fixed = "::")
      if (!is.na(man)) {
        parts = strsplit(man, "::", fixed = TRUE)[[1L]]
        if (parts[1L] %nin% rownames(installed.packages())) {
          return(invisible(NULL))
        }
        matches = help.search(parts[2L], package = parts[1L], ignore.case = FALSE)
        checkmate::expect_data_frame(matches$matches, min.rows = 1L, info = "man page lookup")
      }
    }
  }
  po = po("learner_cv", learner = lrn("classif.debug"))
  po$train(list(Multiplicity(tsk("iris"), tsk("sonar"))))
  s = po$state
  sm = marshal_model(po$state)
  expect_class(po$state, "Multiplicity")
  expect_true(is_marshaled_model(sm$marshaled[[1L]]))
  expect_true(is_marshaled_model(sm$marshaled[[2L]]))

  su = unmarshal_model(sm)
  expect_equal(su, s)

  # recursive
  po = po("learner_cv", learner = lrn("classif.debug"))
  po$train(list(Multiplicity(Multiplicity(tsk("iris")))))
  p1 = po$predict(list(Multiplicity(Multiplicity(tsk("iris")))))

  s = po$state
  sm = marshal_model(po$state)
  expect_class(po$state, "Multiplicity")
  expect_true(is_marshaled_model(sm$marshaled[[1L]][[1L]]))

  su = unmarshal_model(sm)
  expect_equal(su, s)

  po$state = su
  p2 = po$predict(list(Multiplicity(Multiplicity(tsk("iris")))))
  expect_equal(p1, p2)


  task = tsk("iris")
  learner = lrn("classif.debug")

  lrncv_po = po("learner_cv", learner)
  lrncv_po$learner$predict_type = "response"

  nop = mlr_pipeops$get("nop")

  graph = gunion(list(
    lrncv_po,
    nop
  )) %>>% po("featureunion") %>>% lrn("classif.rpart")

  glrn = as_learner(graph)
  expect_learner(glrn, task)

  p1 = glrn$train(task)$predict(task)
  p2 = glrn$marshal()$unmarshal()$predict(task)
  expect_equal(p1, p2)

})

test_that("marshal with cv ensemble", {
  skip_if_not_installed("rpart")
  task = tsk("iris")
  po = po("learner_cv", learner = lrn("classif.rpart", predict_type = "prob"),
    param_vals = list(resampling.predict_method = "cv_ensemble"))
  po$train(list(task))
  expect_equal(po$state$predict_method, "cv_ensemble")
  marshaled = marshal_model(po$state)
  expect_true(is_marshaled_model(marshaled) || inherits(marshaled, "pipeop_learner_cv_state"))
  unmarshaled = unmarshal_model(marshaled)
  expect_equal(names(unmarshaled), names(po$state))
  expect_equal(length(unmarshaled$cv_model_states), length(po$state$cv_model_states))
  po$state = unmarshaled
  expect_equal(
    po$predict(list(task)),
    po$predict(list(task))
  )
})

test_that("state class and multiplicity", {
  po = po("learner_cv", learner = lrn("classif.debug"))
  po$train(list(Multiplicity(tsk("iris"))))
  expect_class(po$state, "Multiplicity")
  expect_class(po$state[[1L]], "pipeop_learner_cv_state")

  # recursive
  po1 = po("learner_cv", learner = lrn("classif.debug"))
  po1$train(list(Multiplicity(Multiplicity(tsk("iris")))))
  expect_class(po1$state, "Multiplicity")
  expect_class(po1$state[[1L]], "Multiplicity")
  expect_class(po1$state[[1L]][[1L]], "pipeop_learner_cv_state")
})

test_that("PipeOpLearnerCV cv ensemble aggregates SE like PipeOpRegrAvg", {
  task_backend = data.table::data.table(
    x1 = c(1, 2, 3, 4),
    x2 = c(4, 3, 2, 1),
    y = c(2, 4, 5, 7)
  )
  task = TaskRegr$new("debug_se_task", backend = task_backend, target = "y")
  configs = list(
    list(se_aggr = "none", rho = NULL),
    list(se_aggr = "between", rho = NULL),
    list(se_aggr = "within", rho = NULL),
    list(se_aggr = "predictive", rho = NULL),
    list(se_aggr = "mean", rho = 0),
    list(se_aggr = "mean", rho = 1),
    list(se_aggr = "mean", rho = -0.5)
  )

  for (cfg in configs) {
    learner = LearnerRegrDebug$new()
    learner$predict_type = "se"
    param_vals = list(
      resampling.method = "cv",
      resampling.folds = 2,
      resampling.predict_method = "cv_ensemble",
      resampling.se_aggr = cfg$se_aggr
    )
    if (!is.null(cfg$rho)) {
      param_vals$resampling.se_aggr_rho = cfg$rho
    }
    po = PipeOpLearnerCV$new(learner, param_vals = param_vals)

    po$train(list(task))
    result_task = po$predict(list(task))[[1]]
    col_response = sprintf("%s.response", po$id)
    col_se = sprintf("%s.se", po$id)

    expect_true(col_response %in% result_task$feature_names)

    base_preds = mlr3misc::map(po$state$cv_model_states, function(st) {
      base = LearnerRegrDebug$new()
      base$predict_type = "se"
      base$state = st
      pred = base$predict(task)
      pred_dt = as.data.table(pred)
      data.table::setorder(pred_dt, row_ids)
      list(response = pred_dt$response, se = pred_dt$se)
    })

    k = length(base_preds)
    weights = rep(1 / k, k)
    response_list = mlr3misc::map(base_preds, "response")
    expected_response = Reduce(`+`, response_list) / k
    se_list = mlr3misc::map(base_preds, "se")
    expected_se = mlr3pipelines:::aggregate_se_weighted(
      response_list,
      se_list,
      weights = weights,
      method = cfg$se_aggr,
      rho = cfg$rho %??% 0
    )

    observed_response = result_task$data(rows = task$row_ids, cols = col_response)[[1]]
    expect_equal(observed_response, expected_response)

    if (is.null(expected_se)) {
      expect_false(col_se %in% result_task$feature_names)
    } else {
      expect_true(col_se %in% result_task$feature_names)
      observed_se = result_task$data(rows = task$row_ids, cols = col_se)[[1]]
      expect_equal(observed_se, expected_se)
    }

    learner_model = po$learner_model
    expect_class(learner_model, "GraphLearner")
    graph_pred = learner_model$predict(task)
    expect_equal(graph_pred$response, expected_response)
    if (is.null(expected_se)) {
      expect_true(is.null(graph_pred$se) || all(is.na(graph_pred$se)))
    } else {
      expect_equal(graph_pred$se, expected_se)
    }
  }
})
