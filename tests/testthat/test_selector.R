context("Selector")

test_that("Selectors work", {
  iris_task = mlr3::mlr_tasks$get("iris")
  bh_task = mlr3::mlr_tasks$get("boston_housing_classic")
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
  expect_error(selnme(bh_task), "subset.*Petal.Length")

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

test_that("numeric value selectors work", {
  task = mlr3::TaskUnsupervised$new("numeric_values", backend = data.table::data.table(
    positive = c(1, 2, 3),
    positive_integer = 1:3,
    negative = c(-1, -2, -3),
    negative_integer = -1:-3,
    zero = c(0, 0, 0),
    non_negative = c(0, 1, 2),
    non_positive = c(0, -1, -2),
    non_zero = c(-1, 1, 2),
    positive_missing = c(1, NA, 3),
    negative_missing = c(-1, NA, -3),
    non_negative_missing = c(0, NA, 2),
    non_positive_missing = c(0, NA, -2),
    non_zero_missing = c(-1, NA, 2),
    all_missing = rep(NA_real_, 3),
    factor = factor(c("-1", "0", "1"))
  ))

  selectors = list(
    selector_positive = selector_positive,
    selector_negative = selector_negative,
    selector_non_negative = selector_non_negative,
    selector_non_positive = selector_non_positive,
    selector_non_zero = selector_non_zero
  )
  expected = list(
    selector_positive = c("positive", "positive_integer"),
    selector_negative = c("negative", "negative_integer"),
    selector_non_negative = c("positive", "positive_integer", "zero", "non_negative"),
    selector_non_positive = c("negative", "negative_integer", "zero", "non_positive"),
    selector_non_zero = c("positive", "positive_integer", "negative", "negative_integer", "non_zero")
  )
  expected_keep_na = list(
    selector_positive = c(expected$selector_positive, "positive_missing", "all_missing"),
    selector_negative = c(expected$selector_negative, "negative_missing", "all_missing"),
    selector_non_negative = c(
      expected$selector_non_negative,
      "positive_missing", "non_negative_missing", "all_missing"
    ),
    selector_non_positive = c(
      expected$selector_non_positive,
      "negative_missing", "non_positive_missing", "all_missing"
    ),
    selector_non_zero = c(
      expected$selector_non_zero,
      "positive_missing", "negative_missing", "non_zero_missing", "all_missing"
    )
  )

  for (name in names(selectors)) {
    selector = selectors[[name]]
    expect_set_equal(selector()(task), expected[[name]], info = name)
    expect_set_equal(selector(keep_na = FALSE)(task), expected[[name]], info = name)
    expect_set_equal(selector(keep_na = TRUE)(task), expected_keep_na[[name]], info = name)
    expect_output(print(selector()), sprintf("%s\\(\\)", name), info = name)
    expect_output(
      print(selector(keep_na = TRUE)),
      sprintf("%s\\(keep_na = TRUE\\)", name),
      info = name
    )
    expect_error(selector(keep_na = NA), "Assertion on 'keep_na' failed", info = name)
  }
})

test_that("selector_non_missing works", {
  task = mlr3::TaskUnsupervised$new("non_missing", backend = data.table::data.table(
    numeric = c(1, 2, 3),
    numeric_missing = c(1, NA, 3),
    numeric_nan = c(1, NaN, 3),
    integer = 1:3,
    factor = factor(c("a", "b", "c")),
    factor_missing = factor(c("a", NA, "c")),
    character = c("a", "b", "c"),
    logical = c(TRUE, FALSE, TRUE)
  ))

  expect_set_equal(
    selector_non_missing()(task),
    c("numeric", "integer", "factor", "character", "logical")
  )
  expect_output(print(selector_non_missing()), "selector_non_missing\\(\\)")
})
