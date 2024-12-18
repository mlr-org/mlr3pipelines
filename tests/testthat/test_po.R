context("mlr_pipeops")

test_that("mlr_pipeops access works", {
  skip_if_not_installed("rpart")

  expect_equal(po(), mlr_pipeops)

  expect_equal(
    po("scale"),
    mlr_pipeops$get("scale")
  )

  expect_equal(
    po("scale", center = FALSE),
    mlr_pipeops$get("scale", param_vals = list(center = FALSE))
  )

  expect_equal(
    po("scale", id = "sx", center = FALSE),
    PipeOpScale$new(id = "sx", param_vals = list(center = FALSE))
  )

  expect_equal(
    po("copy", 2),
    mlr_pipeops$get("copy", 2)
  )

  expect_equal(
    po("copy", outnum = 2),
    mlr_pipeops$get("copy", outnum = 2)
  )

  expect_equal(
    po("branch", options = 2),
    mlr_pipeops$get("branch", options = 2)
  )

  expect_equal(
    po("learner", mlr_learners$get("classif.rpart"), xval = 1),
    mlr_pipeops$get("learner", mlr_learners$get("classif.rpart"), param_vals = list(xval = 1))
  )

  expect_equal(
    po("learner", mlr_learners$get("classif.rpart"), xval = 1, param_vals = list(cp = 0.5)),
    mlr_pipeops$get("learner", mlr_learners$get("classif.rpart"), param_vals = list(xval = 1, cp = 0.5))
  )

  expect_equal(
    po("learner", mlr_learners$get("classif.rpart"), xval = 1, param_vals = list(cp = 0.5), id = "blabla"),
    mlr_pipeops$get("learner", mlr_learners$get("classif.rpart"), param_vals = list(xval = 1, cp = 0.5), id = "blabla")
  )

  expect_error(po("learnerx"), "'learner'")

  # check that we can set the 'key' value
  dblrn = R6Class("debuglearn", inherit = LearnerClassif,
    public = list(
      initialize = function() {
        super$initialize(id = "debuglearn", param_set = ps(key = p_dbl()))
      }
    )
  )

  expect_equal(
    po("learner", dblrn$new(), key = 99),
    mlr_pipeops$get("learner", dblrn$new(), param_vals = list(key = 99))
  )


  expect_equal(
    po("select", param_vals = list(selector = selector_all())),
    po(selector_all())
  )

  expect_equal(
    po(dblrn$new()),
    mlr_pipeops$get("learner", dblrn$new())
  )

  expect_equal(
    po(dblrn$new(), key = 99),
    mlr_pipeops$get("learner", dblrn$new(), param_vals = list(key = 99))
  )

  polrn = po(dblrn$new(), key = 99)

  expect_equal(po(polrn), po(dblrn$new(), key = 99))

  expect_equal(po(polrn, key = 100), po(dblrn$new(), key = 100))

  expect_equal(po(polrn, key = 100, id = "test"), po(dblrn$new(), key = 100, id = "test"))


})


test_that("mlr_pipeops multi-access works", {

  expect_equal(pos(), mlr_pipeops)

  expect_equal(
    unname(pos("scale")),
    list(mlr_pipeops$get("scale"))
  )

  expect_equal(
    unname(pos(c("scale", "nop"))),
    list(mlr_pipeops$get("scale"), mlr_pipeops$get("nop"))
  )

  expect_equal(
    {
      tmp = pos(c("scale", original = "nop"))
      names(tmp)[1] = ""
      tmp
    },
    list(mlr_pipeops$get("scale"), original = mlr_pipeops$get("nop", id = "original"))
  )

  expect_equal(
    unname(pos("scale", center = FALSE)),
    list(mlr_pipeops$get("scale", param_vals = list(center = FALSE)))
  )

  expect_error(
    pos(c("scale", "nop"), center = FALSE),
    "set argument.*center.*PipeOpNOP"
  )

  expect_equal(
    unname(pos(c("scale", "pca"), center = FALSE)),
    list(mlr_pipeops$get("scale", param_vals = list(center = FALSE)), mlr_pipeops$get("pca", param_vals = list(center = FALSE)))
  )


  expect_equal(
    unname(pos("scale", id = "sx", center = FALSE)),
    list(PipeOpScale$new(id = "sx", param_vals = list(center = FALSE)))
  )

  expect_equal(
    unname(pos("copy", 2)),
    list(mlr_pipeops$get("copy", 2))
  )

  expect_equal(
    unname(pos("copy", outnum = 2)),
    list(mlr_pipeops$get("copy", outnum = 2))
  )

  expect_equal(
    unname(pos("branch", options = 2)),
    list(mlr_pipeops$get("branch", options = 2))
  )


  expect_error(pos("learnerx"), "'learner'")

  # check that we can set the 'key' value
  dblrn = R6Class("debuglearn", inherit = LearnerClassif,
    public = list(
      initialize = function() {
        super$initialize(id = "debuglearn", param_set = ps(key = p_dbl()))
      }
    )
  )

  expect_equal(
    unname(pos("learner", dblrn$new(), key = 99)),
    list(mlr_pipeops$get("learner", dblrn$new(), param_vals = list(key = 99)))
  )


  expect_equal(
    po("select", param_vals = list(selector = selector_all())),
    po(selector_all())
  )

  expect_equal(
    unname(pos(list(dblrn$new(), dblrn$new()))),
    list(mlr_pipeops$get("learner", dblrn$new()), mlr_pipeops$get("learner", dblrn$new()))
  )

  expect_equal(
    unname(pos(list(dblrn$new(), dblrn$new()), key = 99)),
    list(mlr_pipeops$get("learner", dblrn$new(), param_vals = list(key = 99)), mlr_pipeops$get("learner", dblrn$new(), param_vals = list(key = 99)))
  )

  expect_equal(unname(pos(character(0))), list())
  expect_equal(pos(c(x = "nop")), list(x = mlr_pipeops$get("nop", id = "x")))
  expect_equal(unname(pos(list())), list())

  polrn = mlr_pipeops$get("learner", dblrn$new())
  polrn$id = "y"
  expect_equal(
    pos(c(x = po("nop"), y = dblrn$new())),
    list(x = po("nop", id = "x"), y = polrn)
  )

  expect_equal(
    pos(list(a = polrn, b = dblrn$new())),
    list(a = po(dblrn$new(), id = "a"), b = po(dblrn$new(), id = "b"))
  )

})

test_that("Incrementing ids works", {
  skip_if_not_installed("rpart")
  x = po("pca_123")
  expect_true(x$id == "pca_123")
  expect_r6(x, "PipeOpPCA")

  x = po("learner_1", lrn("regr.rpart"))
  expect_true(x$id == "regr.rpart_1")
  expect_r6(x, "PipeOpLearner")

  xs = pos(c("pca_1", "pca_2"))
  assert_true(all(names(xs) == c("pca_1", "pca_2")))
})

test_that("po - dictionary suggest works", {

  # test that correct dictionary is checked against
  expect_error(po("robustify"), "ppl\\(\\): 'robustify'")
  expect_error(pos("robustify"), "ppls\\(\\): 'robustify'")

})
