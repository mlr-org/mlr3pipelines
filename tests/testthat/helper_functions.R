

# expect that 'one' is a deep clone of 'two'
expect_deep_clone = function(one, two) {
  # is equal
  expect_equal(one, two)
  visited = new.env()
  visited_b = new.env()
  expect_references_differ = function(a, b, path) {

    force(path)

    # don't go in circles
    addr_a = data.table::address(a)
    addr_b = data.table::address(b)
    if (!is.null(visited[[addr_a]])) {
      return(invisible(NULL))
    }
    visited[[addr_a]] = path
    visited_b[[addr_b]] = path

    # follow attributes, even for non-recursive objects
    if (utils::tail(path, 1) != "[attributes]" && !is.null(base::attributes(a))) {
      expect_references_differ(base::attributes(a), base::attributes(b), c(path, "[attributes]"))
    }

    # don't recurse if there is nowhere to go
    if (!base::is.recursive(a)) {
      return(invisible(NULL))
    }

    # check that environments differ
    if (base::is.environment(a)) {
      # some special environments
      if (identical(a, baseenv()) || identical(a, globalenv()) || identical(a, emptyenv())) {
        return(invisible(NULL))
      }
      if (length(path) > 1 && R6::is.R6(a) && "clone" %nin% names(a)) {
        return(invisible(NULL))  # don't check if smth is not cloneable
      }
      if (identical(utils::tail(path, 1), c("[element train_task] 'train_task'"))) {
        return(invisible(NULL))  # workaround for https://github.com/mlr-org/mlr3/issues/382
      }
      label = sprintf("Object addresses differ at path %s", paste0(path, collapse = "->"))
      expect_true(addr_a != addr_b, label = label)
      expect_null(visited_b[[addr_a]], label = label)
    }

    # recurse
    if (base::is.function(a)) {
      return(invisible(NULL))
      ## # maybe this is overdoing it
      ## expect_references_differ(base::formals(a), base::formals(b), c(path, "[function args]"))
      ## expect_references_differ(base::body(a), base::body(b), c(path, "[function body]"))
    }
    objnames = base::names(a)
    if (is.null(objnames) || anyDuplicated(objnames)) {
      index = seq_len(base::length(a))
    } else {
      index = objnames
      if (base::is.environment(a)) {
        index = Filter(function(x) !bindingIsActive(x, a), index)
      }
    }
    for (i in index) {
      if (utils::tail(path, 1) == "[attributes]" && i %in% c("srcref", "srcfile", ".Environment")) next
      expect_references_differ(base::`[[`(a, i), base::`[[`(b, i), c(path, sprintf("[element %s]%s", i,
        if (!is.null(objnames)) sprintf(" '%s'", if (is.character(index)) i else objnames[[i]]) else "")))
    }
  }
  expect_references_differ(one, two, "ROOT")
}

expect_shallow_clone = function(one, two) {
  expect_equal(one, two)
  if (base::is.environment(one)) {
    addr_a = data.table::address(one)
    addr_b = data.table::address(two)
    expect_true(addr_a != addr_b, label = "Objects are shallow clones")
  }
}


# check basic properties of a pipeop object
# - properties / methods as we need them
expect_pipeop = function(po) {

  label = sprintf("pipeop '%s'", po$id)
  expect_class(po, "PipeOp", label = label)
  expect_string(po$id, label = label)
  expect_class(po$param_set, "ParamSet", label = label)
  expect_list(po$param_set$values, names = "unique", label = label)
  expect_flag(po$is_trained, label = label)
  expect_output(print(po), "PipeOp:", label = label)
  expect_character(po$packages, any.missing = FALSE, unique = TRUE, label = label)
  expect_function(po$train, nargs = 1)
  expect_function(po$predict, nargs = 1)
  expect_function(po$train_internal, nargs = 1)
  expect_function(po$predict, nargs = 1)
  expect_function(po$predict_internal, nargs = 1)
  expect_data_table(po$input, any.missing = FALSE)
  expect_names(names(po$input), permutation.of = c("name", "train", "predict"))
  expect_data_table(po$output, any.missing = FALSE)
  expect_names(names(po$output), permutation.of = c("name", "train", "predict"))
  expect_int(po$innum, lower = 1)
  expect_int(po$outnum, lower = 1)
  # at least one of "train" or "predict" must be in every parameter's tag
  testthat::expect_true(every(po$param_set$tags, function(x) length(intersect(c("train", "predict"), x)) > 0))

}

# Thoroughly check that do.call(poclass$new, constargs) creates a pipeop
# - basic properties check (expect_pipeop)
# - deep clone works
# - *_internal checks for classes
# - *_internal handles NO_OP as it should
expect_pipeop_class = function(poclass, constargs = list()) {
  skip_on_cran()
  po = do.call(poclass$new, constargs)

  expect_pipeop(po)

  poclone = po$clone(deep = TRUE)
  expect_deep_clone(po, poclone)

  in_nop = rep(list(NO_OP), po$innum)
  in_nonnop = rep(list(NULL), po$innum)
  out_nop = rep(list(NO_OP), po$outnum)
  names(out_nop) = po$output$name

  expect_false(po$is_trained)
  expect_equal(po$train(in_nop), out_nop)
  expect_equal(po$predict(in_nop), out_nop)
  expect_true(is_noop(po$state))
  expect_true(po$is_trained)

  expect_error(po$predict(in_nonnop), "Pipeop .* got NO_OP during train")

  # check again with no_op-trained PO
  expect_pipeop(po)
  poclone = po$clone(deep = TRUE)
  expect_deep_clone(po, poclone)

}

# check that a do.call(poclass$new, constargs) behaves as a preprocessing pipeop should.
# This entails
#  - expect_pipeop_class
#  - input / output both have length 1, type "Task" or a subclass
#  - training on non-task gives error
#  - predicting on non-task gives error
#  - training twice gives the same result (if `deterministic_train`)
#  - predicting twice gives the same result (if `deterministic_predict`)
#  - train and predict with provided task gives the same prediction (if `predict_like_train` and `deterministic_predict`)
#  - prediction on row-subset is row-subset of prediction (if `deterministic_predict` and `predict_row_independent`)
#  - predicting on task that has different column layout than training gives an error
#  - training on task with no columns returns task with no columns
#  - training on task when affect_columns is FALSE does not change task
#  - predicting on task with no columns returns task with no columns (if training happened on the same)
#  - predicting on task with no rows returns task with no rows (if predict_rows_independent)
#  - predicting without target column works
#  - predicting without target column gives the same result as with target column (if `deterministic_predict`)
#  - training / prediction does not change input task
#  - is_trained is true
#  - deep cloning clones the state
#
# `task` must have at least two feature columns and at least two rows.
expect_datapreproc_pipeop_class = function(poclass, constargs = list(), task,
  predict_like_train = TRUE, predict_rows_independent = TRUE,
  deterministic_train = TRUE, deterministic_predict = TRUE,
  tolerance = sqrt(.Machine$double.eps)) {
  # NOTE
  # The 'tolerance' parameter is not used in many places yet; if tolerance becomes a problem, add the
  # 'tolerance = tolerance' argument to `expect_equal`.


  original_clone = task$clone(deep = TRUE)
  expect_shallow_clone(task, original_clone)

  expect_pipeop_class(poclass, constargs)

  po = do.call(poclass$new, constargs)
  po2 = do.call(poclass$new, constargs)

  expect_equal(po$innum, 1)
  expect_equal(po$outnum, 1)

  expect_true(are_types_compatible(po$input$train, "Task"))
  expect_true(are_types_compatible(po$input$predict, "Task"))
  expect_true(are_types_compatible(po$output$train, "Task"))
  expect_true(are_types_compatible(po$output$predict, "Task"))

  expect_error(po$train(list(NULL)), "class.*Task.*but has class")

  expect_false(po$is_trained)

  trained = po$train(list(task))

  trained2 = po$train(list(task))
  trained3 = po2$train(list(task))
  expect_list(trained, types = "Task", any.missing = FALSE, len = 1)
  expect_list(trained2, types = "Task", any.missing = FALSE, len = 1)
  expect_list(trained3, types = "Task", any.missing = FALSE, len = 1)
  trained = trained[[1]]
  trained2 = trained2[[1]]
  trained3 = trained3[[1]]
  expect_task(trained)
  expect_task(trained2)
  expect_task(trained3)
  if (deterministic_train) {
    expect_equal(trained$data(), trained2$data())
    expect_equal(trained$data(), trained3$data())
  }

  expect_true(po$is_trained)

  expect_deep_clone(po, po$clone(deep = TRUE))

  expect_error(po$predict(list(NULL)), "class.*Task.*but has class")

  predicted = po$predict(list(task))
  predicted2 = po$predict(list(task))
  predicted3 = po2$predict(list(task))
  expect_list(predicted, types = "Task", any.missing = FALSE, len = 1)
  expect_list(predicted2, types = "Task", any.missing = FALSE, len = 1)
  expect_list(predicted3, types = "Task", any.missing = FALSE, len = 1)

  predicted = predicted[[1]]
  predicted2 = predicted2[[1]]
  predicted3 = predicted3[[1]]
  expect_task(predicted)
  expect_task(predicted2)
  expect_task(predicted3)

  if (deterministic_predict) {
    expect_equal(predicted$data(), predicted2$data())
    if (deterministic_train) {
      # need to be deterministic_train here.
      # consider the case where train() creates a random `$state` but
      # predict() is deterministic given that state.
      expect_equal(predicted$data(), predicted3$data())
    }
    if (predict_like_train) {
      # if deterministic_train is FALSE then `trained` may be different from `predicted`!
      expect_equal(trained2$data(), predicted2$data(), ignore.col.order = TRUE, tolerance = tolerance)
      expect_equal(trained3$data(), predicted3$data(), ignore.col.order = TRUE, tolerance = tolerance)
    }
  }
  if (predict_rows_independent) {
    expect_equal(predicted$nrow, task$nrow)
  }

  task2 = task$clone(deep = TRUE)
  task2$select(task2$feature_names[-1])
  expect_error(po$predict(list(task2)), "Input task during prediction .* does not match input task during training")

  emptytask = task$clone(deep = TRUE)$select(character(0))

  expect_equal(length(po$train(list(emptytask))[[1]]$feature_names), 0)
  expect_equal(length(po$predict(list(emptytask))[[1]]$feature_names), 0)

  if ("affect_columns" %in% names(po$param_set$params)) {
    selector = function(data) data$feature_names[-1]
    po2$param_set$values$affect_columns = selector
    trained.subset = po$train(list(task2))[[1]]
    trained2.subset = po2$train(list(task))[[1]]
    if (deterministic_train) {
      expect_equal(trained.subset$data(cols = c(trained.subset$target_names, trained.subset$feature_names)),
        trained2.subset$data(cols = setdiff(c(trained2.subset$target_names, trained2.subset$feature_names), task$feature_names[1])))
    }
    predicted.subset = po$predict(list(task2))[[1]]
    predicted2.subset = po2$predict(list(task2))[[1]]
    if (deterministic_train && deterministic_predict) {
      expect_equal(predicted.subset$data(), predicted2.subset$data())
    }
    predicted2.subset2 = po2$predict(list(task))[[1]]
    if (deterministic_predict) {
      expect_equal(predicted2.subset$data(cols = c(predicted.subset$target_names, predicted.subset$feature_names)),
        predicted2.subset2$data(cols = setdiff(c(predicted2.subset2$target_names, predicted2.subset2$feature_names),
          task$feature_names[1])))
    }

    selector = function(data) character(0)
    po2$param_set$values$affect_columns = selector

    # NOTE: the following should ensure that data has not changed
    # but number of rows or row indices could change in theory, so the tests will need to be adapted if that is ever the case
    trained = po2$train(list(task))[[1]]
    expect_equal(trained$data(cols = trained$feature_names), task$data(cols = task$feature_names), ignore.col.order = TRUE, tolerance = tolerance)

    predicted = po2$predict(list(task))[[1]]
    expect_equal(predicted$data(cols = predicted$feature_names), task$data(cols = task$feature_names), ignore.col.order = TRUE, tolerance = tolerance)

    predicted2 = po2$predict(list(emptytask))[[1]]
    expect_equal(predicted2$feature_names, character(0))


    # test affect_columns
    po_orig = po$clone(deep = TRUE)

    # selecting no columns leaves task unchanged
    po$param_set$values$affect_columns = selector_none()
    expect_equal(task$data(), po$train(list(task))[[1]]$data())
    expect_equal(task$data(), po$predict(list(task))[[1]]$data())

    # names of every second task column
    halftasknames = task$feature_names[c(TRUE, FALSE)]

    # PO has affect_columns set to every other task column
    po$param_set$values$affect_columns = selector_name(halftasknames)

    halftrainres = po$train(list(task))[[1]]$data()
    halfpredres = po$predict(list(task))[[1]]$data()
    halfpredresL1 = po$predict(list(task$clone()$filter(task$row_ids[1])))[[1]]$data()
    halfpredresL0 = po$predict(list(task$clone()$filter(task$row_ids[0])))[[1]]$data()

    # for comparison, we run PO on a task with half the features missing and re-construct the result afterwards
    halftask = task$clone()$select(halftasknames)
    otherhalf = task$clone()$data(cols = setdiff(task$feature_names, halftasknames))

    explicittrainres = cbind(po_orig$train(list(halftask))[[1]]$data(), otherhalf)
    explicitpredres = cbind(po_orig$predict(list(halftask))[[1]]$data(), otherhalf)
    explicitpredresL1 = cbind(po_orig$predict(list(halftask$clone()$filter(task$row_ids[1])))[[1]]$data(), otherhalf[1, ])
    explicitpredresL0 = cbind(po_orig$predict(list(halftask$clone()$filter(task$row_ids[0])))[[1]]$data(), otherhalf[integer(0), ])

    if (deterministic_train) {
      expect_equal(halftrainres, explicittrainres, ignore.col.order = TRUE, tolerance = tolerance)
    }
    if (deterministic_predict) {
      if (deterministic_train) {
        expect_equal(halfpredres, explicitpredres, ignore.col.order = TRUE, tolerance = tolerance)
        expect_equal(halfpredresL1, explicitpredresL1, ignore.col.order = TRUE, tolerance = tolerance)
      }
      expect_equal(halfpredresL1, halfpredres[1, ], ignore.col.order = TRUE, tolerance = tolerance)
      if (predict_like_train) {
        expect_equal(halfpredres, halftrainres, ignore.col.order = TRUE, tolerance = tolerance)
        expect_equal(explicitpredres, explicittrainres, ignore.col.order = TRUE, tolerance = tolerance)
      }
    }
    expect_equal(halfpredresL0, explicitpredresL0, ignore.col.order = TRUE, tolerance = tolerance)
    expect_equal(halfpredresL0, halfpredres[integer(0), ], ignore.col.order = TRUE, tolerance = tolerance)

  }

  po$train(list(task))

  norowtask = task$clone(deep = TRUE)$filter(integer(0))
  whichrow = sample(task$nrow, 1)
  onerowtask = task$clone(deep = TRUE)$filter(whichrow)


  predicted = po$predict(list(norowtask))[[1]]
  if (predict_rows_independent) {
    expect_equal(predicted$nrow, 0)
  }

  predicted = po$predict(list(onerowtask))[[1]]
  if (predict_rows_independent) {
    expect_equal(predicted$nrow, 1)
    if (deterministic_predict) {
      expect_equal(predicted$data(), po$predict(list(task))[[1]]$filter(whichrow)$data(), ignore.col.order = TRUE, tolerance = tolerance)
    }
  }

  ## ## The following becomes relevant when mlr3 gets unsupervised tasks. We then want
  ## ## to test them automatically here by creating the unsupervised tasks.
  ##
  ## targetless = task$clone(deep = TRUE)$set_col_role(task$target_names, character(0))
  ##
  ## po$train(list(task))
  ## predicted = po$predict(list(task))[[1]]
  ## predicted.targetless = po$predict(list(targetless))[[1]]
  ##
  ## if (deterministic_predict) {
  ##   expect_equal(predicted$data(cols = predicted$feature_names),
  ##     predicted.targetless$data(cols = predicted.targetless$feature_names))
  ## }


  expect_shallow_clone(task, original_clone)  # test that task was not changed by all the training / prediction

}

train_pipeop = function(po, inputs) {

  label = sprintf("pipeop '%s'", po$id)
  expect_pipeop(po)
  expect_null(po$state, label = label)
  expect_false(po$is_trained, label = label)
  result = po$train(inputs)
  expect_list(result, len = po$outnum, label = label)
  expect_true(!is.null(po$state), label = label)
  expect_true(po$is_trained, label = label)
  expect_pipeop(po)
  return(result)
}

predict_pipeop = function(po, inputs) {
  expect_pipeop(po)
  expect_true(po$is_trained)
  result = po$predict(inputs)
  expect_list(result, len = po$outnum)
  expect_pipeop(po)
  return(result)
}

# check that features of task change according to
# compares po$train(list(traintask))[[1]] result with trainresult, and
# (if given), po$predict(list(predicttask))[[1]] with predictresult.
# -- This only checks feature columns --
expect_pipeop_result_features = function(po, traintask, trainresult,
  predicttask = NULL, predictresult = NULL) {
  result = train_pipeop(po, list(traintask))
  expect_equal(result$data(cols = result$feature_names), trainresult, ignore.col.order = TRUE, tolerance = tolerance)
  assert(is.null(predicttask) == is.null(predictresult))
  if (!is.null(predicttask)) {
    result = predict_pipeop(po, list(traintask))
    expect_equal(result$data(cols = result$feature_names), predicttask, ignore.col.order = TRUE, tolerance = tolerance)
  }
}

expect_graph = function(g, n_nodes = NULL, n_edges = NULL) {

  expect_class(g, "Graph")
  expect_list(g$pipeops, "PipeOp")
  if (!is.null(n_nodes)) {
    expect_length(g$pipeops, n_nodes)
  }
  expect_character(g$packages, any.missing = FALSE, unique = TRUE)

  expect_data_table(g$edges, any.missing = FALSE)
  if (!is.null(n_edges)) {
    expect_equal(nrow(g$edges), n_edges)
  }

  expect_character(g$hash)

  expect_class(g$param_set, "ParamSet")
  expect_list(g$param_set$values, names = "unique")

  expect_character(g$lhs, any.missing = FALSE)
  expect_character(g$rhs, any.missing = FALSE)

  expect_set_equal(g$ids(), names(g$pipeops))
  expect_set_equal(g$ids(sorted = TRUE), names(g$pipeops))

  expect_flag(g$is_trained)
}

make_prediction_obj_classif = function(n = 100, noise = TRUE, predict_types = "response",
  seed = 1444L, nclasses = 3L) {
  if (!noise) set.seed(seed)
  response = prob = NULL
  truth = sample(letters[seq_len(nclasses)], n, replace = TRUE)

  if ("prob" %in% predict_types) {
    prob = matrix(runif(n * nclasses), ncol = nclasses, nrow = n)
    prob = t(apply(prob, 1, function(x) x / sum(x)))
    colnames(prob) = unique(truth)
    response = colnames(prob)[max.col(prob, ties.method = "first")]
  } else if ("response" %in% predict_types) {
    response = sample(letters[seq_len(nclasses)], n, replace = TRUE)
  }

  PredictionClassif$new(row_ids = seq_len(n), truth = factor(truth, levels = letters[seq_len(nclasses)]),
    response = factor(response, levels = letters),
    prob = prob)
}

PipeOpLrnRP = PipeOpLearner$new(mlr_learners$get("classif.rpart"))
PipeOpLrnFL = PipeOpLearner$new(mlr_learners$get("classif.featureless"))

# turn data.table of character into comma-separated rows, so they can
# be checked easily with expect_equal and expect_set_equal
csvify = function(table) {
  apply(table, 1, paste, collapse = ",")
}

# canonicise param_set by calling the active binding
touch = function(x) {
  x$param_set
  x
}
