context("PipeOpMutateTarget")

test_that("mutate", {

  # Simple derivation
  pom = po("mutate_target")
  expect_pipeop(pom)
  pom$param_set$values$mutation = list(
    y_tmp = ~ factor(Species == "virginica", levels = c(TRUE, FALSE))
  )
  newtsk = pom$train(list(tsk("iris")))[[1]]
  expect_task(newtsk)
  expect_true("y_tmp" %in% newtsk$feature_names)
  expect_true(all(newtsk$data()$y_tmp == (newtsk$data()$Species == "virginica")))
  expect_true(pom$is_trained)

  newtsk2 = pom$predict(list(tsk("iris")))[[1]]
  expect_task(newtsk2)
  expect_true("y_tmp" %in% newtsk2$feature_names)
  expect_true(all(is.na(newtsk2$data()$y_tmp)))
  expect_true(all(levels(newtsk2$data()$y_tmp) == c(TRUE, FALSE)))
  expect_true(all(levels(newtsk2$data()$y_tmp) == levels(newtsk$data()$y_tmp)))

  # Name clashes: Overwrites target if names clash.
  pom = po("mutate_target")
  expect_pipeop(pom)
  pom$param_set$values$mutation = list(
    Species = ~ factor(Species == "virginica", levels = c(TRUE, FALSE))
  )
  newtsk = pom$train(list(tsk("iris")))[[1]]
  expect_task(newtsk)
  expect_true("Species" %in% newtsk2$target_names)
  expect_true(
    all(newtsk$data()$Species == factor(mlr_tasks$get("iris")$data()$Species == "virginica", levels = c(TRUE, FALSE))))
  expect_true(pom$is_trained)

  newtsk2 = pom$predict(list(tsk("iris")))[[1]]
  expect_task(newtsk2)
  expect_true("Species" %in% newtsk2$target_names)
  expect_true(all(is.na(newtsk2$data()$Species)))
  expect_true(all(levels(newtsk2$data()$Species) == c(TRUE, FALSE)))
  expect_true(all(levels(newtsk2$data()$Species) == levels(newtsk$data()$Species)))
})

test_that("multi-to-binaryclass", {
  # Simple derivation
  pom = po("mutate_target", param_vals = list(mutation = list(
    species_virginica = ~ factor(Species == "virginica", levels = c(TRUE, FALSE)))))
  pontgt = po("new_target",
    param_vals = list(new_target = "species_virginica", new_task_type = "classif"))
  pipe = pom %>>% pontgt
  newtsk = pipe$train(tsk("iris"))[[1]]
  expect_task(newtsk)
  expect_true("species_virginica" %in% newtsk$target_names)
  expect_true(all(newtsk$data()$species_virginica == (newtsk$data()$Species == "virginica")))
  expect_true(newtsk$properties == "twoclass")

  newtsk2 = pipe$predict(tsk("iris"))[[1]]
  expect_task(newtsk2)
  expect_true("species_virginica" %in% newtsk2$target_names)
  expect_true(all(is.na(newtsk2$data()$species_virginica)))
  expect_true(all(levels(newtsk2$data()$species_virginica) == c(TRUE, FALSE)))
  expect_true(all(levels(newtsk2$data()$species_virginica) == levels(newtsk$data()$y_tmp)))
  expect_true(newtsk2$properties == "twoclass")
})

# test_that("one-vs-all", {
#   mutations = list(
#     virginica = ~ factor(Species == "virginica", levels = c(TRUE, FALSE)),
#     versicolor = ~ factor(Species == "versicolor", levels = c(TRUE, FALSE)),
#     setosa = ~ factor(Species == "setosa", levels = c(TRUE, FALSE))
#   )

#   tgt1 = po("mutate_target", id = "m1", param_vals = list(mutation = mutations[1])) %>>%
#     po("new_target", id = "n1", param_vals = list(new_target = "virginica", new_task_type = "classif")) %>>%
#     po("learner", lrn("classif.rpart", id = "l1", predict_type = "prob"))

#   tgt2 = po("mutate_target", id = "m2", param_vals = list(mutation = mutations[2])) %>>%
#     po("new_target", id = "n2", param_vals = list(new_target = "versicolor", new_task_type = "classif")) %>>%
#     po("learner", lrn("classif.rpart", id = "l2", predict_type = "prob"))

#   tgt3 = po("mutate_target", id = "m3", param_vals = list(mutation = mutations[3])) %>>%
#     po("new_target", id = "n3", param_vals = list(new_target = "setosa", new_task_type = "classif")) %>>%
#     po("learner", lrn("classif.rpart", id = "l3", predict_type = "prob"))
#   pipe = po("copy", 3) %>>% gunion(list(tgt1, tgt2, tgt3)) %>>% po("classifavg")
#   pipe$train(tsk("iris"))
#   pipe$predict(tsk("iris"))
# })
