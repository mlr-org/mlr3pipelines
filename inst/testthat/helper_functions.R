# expect that 'one' is a deep clone of 'two'
expect_deep_clone = function(one, two) {
  # is equal
  expect_equal(one, two)
  visited = new.env()
  visited_b = new.env()
  expect_references_differ = function(a, b, path) {

    force(path)
    if (length(path) > 400) {
      stop("Recursion too deep in expect_deep_clone()")
    }

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
      if (identical(utils::tail(path, 1), c("[element fallback] 'fallback'"))) {
        return(invisible(NULL))  # workaround for https://github.com/mlr-org/mlr3/issues/511
      }
      label = sprintf("Object addresses differ at path %s", paste0(path, collapse = "->"))
      expect_true(addr_a != addr_b, label = label)
      expect_null(visited_b[[addr_a]], label = label)
    } else {
      a <- unclass(a)
      b <- unclass(b)
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
expect_pipeop = function(po, check_ps_default_values = TRUE) {

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
  expect_data_table(po$input, any.missing = FALSE)
  expect_names(names(po$input), permutation.of = c("name", "train", "predict"))
  expect_data_table(po$output, any.missing = FALSE)
  expect_names(names(po$output), permutation.of = c("name", "train", "predict"))
  expect_int(po$innum, lower = 1)
  expect_int(po$outnum, lower = 1)
  expect_valid_pipeop_param_set(po, check_ps_default_values = check_ps_default_values)
}

# autotest for the parmset of a pipeop
# - at least one of "train" or "predict" must be in every parameter's tag
# - custom_checks of ParamUty return string on failure
# - either default or values are set; if both, they differ (only if check_ps_default_values = TRUE)
expect_valid_pipeop_param_set = function(po, check_ps_default_values = TRUE) {
  ps = po$param_set
  expect_true(every(ps$tags, function(x) length(intersect(c("train", "predict"), x)) > 0L))

  if (mlr3pipelines:::paradox_info$is_old) {
    uties = ps$params[ps$ids("ParamUty")]
    if (length(uties)) {
      test_value = NO_DEF  # custom_checks should fail for NO_DEF
      results = map(uties, function(uty) {
        uty$custom_check(test_value)
      })
      expect_true(all(map_lgl(results, function(result) {
        length(result) == 1L && (is.character(result) || result == TRUE)  # result == TRUE is necessary because default is function(x) TRUE
      })), label = "custom_check returns string on failure")
    }
  } else {
    uties = ps$ids("ParamUty")
    if (length(uties)) {
      test_value = NO_DEF  # custom_checks should fail for NO_DEF
      results = map(uties, function(uty) {
        psn = ps$subset(uty, allow_dangling_dependencies = TRUE)
        psn$check(structure(list(test_value), names = uty))
      })
      expect_true(all(map_lgl(results, function(result) {
        length(result) == 1L && (is.character(result) || result == TRUE)  # result == TRUE is necessary because default is function(x) TRUE
      })), label = "custom_check returns string on failure")
    }
  }

  if (check_ps_default_values) {
    default = ps$default
    values = ps$values
    default_and_values = intersect(names(default), names(values))
    if (length(default_and_values)) {
      expect_true(all(pmap_lgl(list(default[default_and_values], values[default_and_values]), function(default, value) {
        !identical(default, value)
      })), label = "ParamSet default and values differ")
    }
  }
}

# Thoroughly check that do.call(poclass$new, constargs) creates a pipeop
# - basic properties check (expect_pipeop)
# - deep clone works
# - *_internal checks for classes
# - *_internal handles NO_OP as it should
expect_pipeop_class = function(poclass, constargs = list(), check_ps_default_values = TRUE) {
  skip_on_cran()
  po = do.call(poclass$new, constargs)

  expect_pipeop(po, check_ps_default_values = check_ps_default_values)

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
  expect_pipeop(po, check_ps_default_values = check_ps_default_values)
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
  affect_context_independent = TRUE,  # whether excluding a column does not change what happens to the other columns
  tolerance = sqrt(.Machine$double.eps),
  check_ps_default_values = TRUE) {
  # NOTE
  # The 'tolerance' parameter is not used in many places yet; if tolerance becomes a problem, add the
  # 'tolerance = tolerance' argument to `expect_equal`.


  original_clone = task$clone(deep = TRUE)
  expect_shallow_clone(task, original_clone)

  expect_pipeop_class(poclass, constargs, check_ps_default_values = check_ps_default_values)

  po = do.call(poclass$new, constargs)
  po2 = do.call(poclass$new, constargs)

  expect_equal(po$innum, 1)
  expect_equal(po$outnum, 1)

  expect_true(mlr3pipelines:::are_types_compatible(po$input$train, "Task"))
  expect_true(mlr3pipelines:::are_types_compatible(po$input$predict, "Task"))
  expect_true(mlr3pipelines:::are_types_compatible(po$output$train, "Task"))
  expect_true(mlr3pipelines:::are_types_compatible(po$output$predict, "Task"))

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

  expect_task(po$train(list(emptytask))[[1]])
  emptytaskfnames = po$train(list(emptytask))[[1]]$feature_names
  expect_task(po$predict(list(emptytask))[[1]])
  expect_equal(emptytaskfnames, po$predict(list(emptytask))[[1]]$feature_names)

  if ("affect_columns" %in% po$param_set$ids() && affect_context_independent) {
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
    # but at least one pipeop adds a new column even with 0 affect_cols, so we only check that original task's features have not changed.
    trained = po2$train(list(task))[[1]]
    expect_equal(trained$data(cols = task$feature_names), task$data(cols = task$feature_names), ignore.col.order = TRUE, tolerance = tolerance)

    predicted = po2$predict(list(task))[[1]]
    expect_equal(predicted$data(cols = task$feature_names), task$data(cols = task$feature_names), ignore.col.order = TRUE, tolerance = tolerance)

    predicted2 = po2$predict(list(emptytask))[[1]]
    expect_equal(sort(predicted2$feature_names), sort(emptytaskfnames))


    # test affect_columns
    po_orig = po$clone(deep = TRUE)

    # selecting no columns leaves task unchanged
    po$param_set$values$affect_columns = selector_none()
    expect_equal(task$data(), po$train(list(task))[[1]]$data(cols = colnames(task$data())))
    expect_equal(task$data(), po$predict(list(task))[[1]]$data(cols = colnames(task$data())))

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
  whichrow = task$row_ids[[sample.int(task$nrow, 1)]]
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

  if (packageVersion("mlr3") >= "0.1.3-9001") {
    y = task$data(cols = task$target_names, rows = shuffle(task$row_ids))
    permutated = task$clone(deep = TRUE)$cbind(y)

    po$train(list(task))
    predicted = po$predict(list(task))[[1]]
    predicted.permutated = po$predict(list(permutated))[[1]]

    if (deterministic_predict) {
      expect_equal(predicted$data(cols = predicted$feature_names),
        predicted.permutated$data(cols = predicted.permutated$feature_names))
    }
  }

  expect_shallow_clone(task, original_clone)  # test that task was not changed by all the training / prediction

  # test that predict on test rows during train is the same as predict on the rows

  po = do.call(poclass$new, constargs)

  tasktrain = original_clone$clone(deep = TRUE)
  n_use = length(tasktrain$row_roles$use)
  expect_true(n_use >= 4)
  expect_true(task$nrow >= 5)

  # overlap between use and test rows
  tasktrain$divide(ids = tasktrain$row_roles$use[seq(n_use - 2, n_use)], remove = FALSE)
  tasktrain$row_roles$use = tasktrain$row_roles$use[seq(1, n_use - 2)]

  taskpredict = tasktrain$clone(deep = TRUE)
  taskpredict$row_roles$use = taskpredict$internal_valid_task$row_roles$use
  taskpredict$internal_valid_task = NULL

  taskouttrain = po$train(list(tasktrain))[[1L]]
  taskoutpredict = po$predict(list(taskpredict))[[1L]]

  # other columns like weights are present during traing but not during predict
  cols = unname(unlist(taskouttrain$col_roles[c("feature", "target")]))
  dtrain = taskouttrain$internal_valid_task$data(cols = cols)
  dpredict = taskoutpredict$data(cols = cols)
  expect_permutation(colnames(dtrain), colnames(dpredict))
  expect_equal(nrow(dtrain), nrow(dpredict))
  dtrain = dtrain[, cols, with = FALSE]
  dpredict = dpredict[, cols, with = FALSE]

  if (deterministic_predict && deterministic_train) {
    expect_equal(dtrain, dpredict)
  }
}

train_pipeop = function(po, inputs) {

  label = sprintf("pipeop '%s'", po$id)
  expect_pipeop(po, check_ps_default_values = FALSE)
  expect_null(po$state, label = label)
  expect_false(po$is_trained, label = label)
  result = po$train(inputs)
  expect_list(result, len = po$outnum, label = label)
  expect_true(!is.null(po$state), label = label)
  expect_true(po$is_trained, label = label)
  expect_pipeop(po, check_ps_default_values = FALSE)
  return(result)
}

predict_pipeop = function(po, inputs) {
  expect_pipeop(po, check_ps_default_values = FALSE)
  expect_true(po$is_trained)
  result = po$predict(inputs)
  expect_list(result, len = po$outnum)
  expect_pipeop(po, check_ps_default_values = FALSE)
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
  lvls = letters[seq_len(nclasses)]
  truth = sample(lvls, n, replace = TRUE)

  if ("prob" %in% predict_types) {
    prob = matrix(runif(n * nclasses), ncol = nclasses, nrow = n)
    prob = t(apply(prob, 1, function(x) x / sum(x)))
    colnames(prob) = lvls
    response = colnames(prob)[max.col(prob, ties.method = "first")]
  } else if ("response" %in% predict_types) {
    response = sample(letters[seq_len(nclasses)], n, replace = TRUE)
  }

  PredictionClassif$new(row_ids = seq_len(n), truth = factor(truth, levels = lvls),
    response = factor(response, levels = lvls), prob = prob)
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

expect_multiplicity = function(x) {
  expect_true(is.Multiplicity(x))
}

# the following is a necessary workaround for the new R misbehaving when comparing R6 objects.
# See https://github.com/r-lib/R6/issues/208
# This is quite sloppy right now.
r6_to_list = function(x) {
  actives = c(".__enclos_env__", names(x[[".__enclos_env__"]][[".__active__"]]))
  ll = sapply(setdiff(names(x), actives), get, x, simplify = FALSE)
  ll[[".__enclos_env__"]] = list(`.__active__` = x[[".__enclos_env__"]][[".__active__"]], private = x[[".__enclos_env__"]][["private"]])
  if (!is.null(x[[".__enclos_env__"]][["super"]])) {
    ll[[".__enclos_env__"]][["super"]] = r6_to_list(x[[".__enclos_env__"]][["super"]])
  }
  ln = names(ll)
  attributes(ll) = attributes(x)
  names(ll) = ln
  ll[sort(names(ll))]
}

all.equal.R6 = function(target, current, ...) {
  if (!is.environment(target)) NextMethod()
  if (!is.environment(current)) NextMethod()
  if (!inherits(current, "R6")) return("'current' is not an R6 class")
  # avoid cycles
  r6_seen = dynGet("__r6_seen__", NULL)
  if (is.null(r6_seen)) {
    r6_seen = "__r6_seen__" = new.env(parent = emptyenv())
  }
  tca = sprintf("%s__%s", data.table::address(target), data.table::address(current))
  if (!is.null(r6_seen[[tca]])) return(TRUE)
  r6_seen[[tca]] = TRUE
  # call all.equal.list directly because objects still have R6 class
  base:::all.equal.list(r6_to_list(target), r6_to_list(current),  ...)
}

registerS3method("all.equal", "R6", all.equal.R6)
