context("Selector")

test_that("Selectors work", {
  iris_task = mlr3::mlr_tasks$get("iris")
  bh_task = mlr3::mlr_tasks$get("boston_housing")
  pima_task = mlr3::mlr_tasks$get("pima")

  sela = selector_all()
  expect_set_equal(sela(iris_task), iris_task$feature_names)
  expect_set_equal(sela(bh_task), bh_task$feature_names)

  seln = selector_none()
  expect_set_equal(seln(iris_task), character(0))
  expect_set_equal(seln(bh_task), character(0))

  self = selector_type("factor")
  expect_set_equal(self(iris_task), character(0))
  expect_set_equal(self(bh_task), c("chas", "town"))

  selg = selector_grep("a.*i")
  expect_set_equal(selg(iris_task), c("Petal.Width", "Sepal.Width"))
  expect_set_equal(selg(bh_task), "ptratio")

  selgi = selector_invert(selg)
  expect_set_equal(selgi(iris_task), c("Petal.Length", "Sepal.Length"))
  expect_set_equal(selgi(bh_task), setdiff(bh_task$feature_names, "ptratio"))

  selgf = selector_union(selg, self)
  expect_set_equal(selgf(iris_task), c("Petal.Width", "Sepal.Width"))
  expect_set_equal(selgf(bh_task), c("chas", "town", "ptratio"))

  selnm = selector_name("Petal.Length")
  expect_set_equal(selnm(iris_task), "Petal.Length")
  expect_set_equal(selnm(bh_task), character(0))

  selnme = selector_name("Petal.Length", assert_present = TRUE)
  expect_set_equal(selnme(iris_task), "Petal.Length")
  expect_error(selnme(bh_task), "but is.*Petal.Length")

  selggi = selector_intersect(selg, selector_grep("^Petal\\."))
  expect_set_equal(selggi(iris_task), "Petal.Width")
  expect_set_equal(selggi(bh_task), character(0))

  selggd = selector_setdiff(selg, selector_grep("^Petal\\."))
  expect_set_equal(selggd(iris_task), "Sepal.Width")
  expect_set_equal(selggd(bh_task), "ptratio")

  selmiss = selector_missing()
  expect_set_equal(selmiss(iris_task), character(0))
  expect_set_equal(selmiss(pima_task), c("glucose", "insulin", "mass", "pressure", "triceps"))

  selcgt = selector_cardinality_greater_than(3)
  expect_set_equal(selcgt(bh_task), "town")
  expect_set_equal(selcgt(bh_task$filter(1:3)), character(0))
  expect_set_equal(selcgt(pima_task), character(0))
})
