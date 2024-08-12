context("ppl - pipeline_convert_types")

test_that("general tests", {

  data_chr = data.table::data.table(
    x = factor(letters[1:3]),
    y = letters[1:3],
    z = letters[1:3]
  )

  data_fct = data.table::data.table(
    x = factor(letters[1:3]),
    y = factor(letters[1:3]),
    z = factor(letters[1:3])
  )

  data_ord = data.table::data.table(
    x = factor(letters[1:3]),
    y = ordered(letters[1:3]),
    z = ordered(letters[1:3])
  )

  task_chr = TaskClassif$new("task_chr", data_chr, "x")
  task_fct = TaskClassif$new("task_fct", data_fct, "x")
  task_ord = TaskClassif$new("task_ord", data_ord, "x")

  graph = ppl("convert_types", "character", "factor")
  expect_equal(graph$train(task_chr)[[1]]$data(), data_fct)

  graph = ppl("convert_types", c("ordered", "character"), "factor")
  expect_equal(graph$train(task_chr)[[1]]$data(), data_fct)
  expect_equal(graph$train(task_ord)[[1]]$data(), data_fct)

  graph_z = ppl("convert_types", "character", "factor",
    affect_columns = selector_name("z"))
  expect_equal(graph_z$train(task_chr)[[1]]$data()[, c("x", "z", "y")], cbind(data_fct[, c("x", "z")], data_chr[, "y"]))

  graph_z = ppl("convert_types", "numeric", "factor",
    affect_columns = selector_name("z"))
  expect_equal(graph_z$train(task_chr)[[1]]$data(), data_chr)

  graph_z = ppl("convert_types", "ordered", "factor",
    affect_columns = selector_name("z"))
  expect_equal(graph_z$train(task_ord)[[1]]$data()[, c("x", "z", "y")], cbind(data_fct[, c("x", "z")], data_ord[, "y"]))

  expect_equal(graph_z$train(task_fct)[[1]]$data()[, c("x", "z", "y")], data_fct[, c("x", "z", "y")])
  expect_equal(graph_z$train(task_chr)[[1]]$data()[, c("x", "z", "y")], data_chr[, c("x", "z", "y")])

})

test_that("more_args", {
  data_time = data.table::data.table(
    x = factor(letters[1:3]),
    y = paste0("2024-02-01 00:00:0", 1:3)
  )

  data_time_weird = data.table::data.table(
    x = factor(letters[1:3]),
    y = paste0("02/2024/01 00:00:0", 1:3)
  )

  task_time = TaskClassif$new("task_time", data_time, "x")
  task_time_weird = TaskClassif$new("task_time_weird", data_time_weird, "x")

  expect_equal(
    ppl("convert_types", "character", "POSIXct")$train(task_time)[[1]]$data()[, y],
    as.POSIXct(data_time$y)
  )

  expect_equal(
    ppl("convert_types", "character", "POSIXct",
      more_args = list(format = "%m/%Y/%d %H:%M:%OS")
    )$train(task_time_weird)[[1]]$data()[, y],
    as.POSIXct(data_time$y)
  )
})

test_that("fixfactors", {

  data_chr = data.table::data.table(
    x = factor(letters[1:3]),
    y = letters[1:3],
    z = letters[1:3]
  )

  data_fct = data.table::data.table(
    x = factor(letters[1:3]),
    y = factor(letters[1:3]),
    z = factor(letters[1:3])
  )
  task_chr = TaskClassif$new("task_chr", data_chr, "x")
  task_fct = TaskClassif$new("task_fct", data_fct, "x")

  nadata <- data.table(y = factor(c("a", "b", NA)), z = factor(c("a", "b", NA)))


  graph = ppl("convert_types", "character", "factor")

  graph$train(task_chr$clone(deep = TRUE)$filter(1:2))
  expect_equal(graph$predict(task_chr)[[1]]$data()[, c("y", "z")], nadata)

  graph$train(task_fct$clone(deep = TRUE)$filter(1:2))
  expect_equal(graph$predict(task_fct)[[1]]$data()[, c("y", "z")], nadata)

  graph = ppl("convert_types", "character", "numeric")

  graph$train(task_fct$clone(deep = TRUE)$filter(1:2))
  expect_equal(graph$predict(task_fct)[[1]]$data()[, c("y", "z")], data_fct[, c("y", "z")])

  graph = ppl("convert_types", "character", "numeric", fixfactors = TRUE)
  graph$train(task_fct$clone(deep = TRUE)$filter(1:2))
  expect_equal(graph$predict(task_fct)[[1]]$data()[, c("y", "z")], nadata)

  graph = ppl("convert_types", "character", "factor", fixfactors = FALSE)

  graph$train(task_chr$clone(deep = TRUE)$filter(1:2))
  expect_equal(graph$predict(task_chr)[[1]]$data()[, c("y", "z")], data_fct[, c("y", "z")])


  graph = ppl("convert_types", "character", "factor")
  graph$train(task_chr$clone(deep = TRUE)$filter(1:2))

  graph_z = ppl("convert_types", "character", "factor",
    affect_columns = selector_name("z"))
  graph_z$train(task_chr$clone(deep = TRUE)$filter(1:2))

  expect_equal(graph_z$predict(task_chr)[[1]]$data()[, c("y", "z")], cbind(data_chr[, "y"], nadata[, "z"]))

})


test_that("id", {

  expect_equal(
    ppl("convert_types", "character", "factor")$ids(),
    c("convert_chr_to_fct", "convert_chr_to_fct_ff")
  )

  expect_equal(
    ppl("convert_types", c("character", "POSIXct"), "factor")$ids(),
    c("convert_chrpxc_to_fct", "convert_chrpxc_to_fct_ff")
  )

  expect_equal(
    ppl("convert_types", c("character", "POSIXct"), "factor", fixfactors = FALSE)$ids(),
    "convert_chrpxc_to_fct"
  )

  expect_equal(
    ppl("convert_types", c("character", "POSIXct"), "factor", id = "abc")$ids(),
    c("abc", "abc_ff")
  )

  expect_equal(
    ppl("convert_types", c("character", "POSIXct"), "factor", id = "abc", fixfactors = FALSE)$ids(),
    "abc"
  )

})
