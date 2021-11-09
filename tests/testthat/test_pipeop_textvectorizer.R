context("PipeOpTextVectorizer")

test_that("PipeOpTextVectorizer - basic properties", {
  skip_if_not_installed("quanteda")
  suppressWarnings(attachNamespace("quanteda"))  # TODO: see https://github.com/quanteda/quanteda/issues/2116 , may not be an issue in the future

  task = mlr_tasks$get("iris")
  # create hacky text data:
  dt = data.table("txt" = apply(iris, 1, function(x) {
    if (x["Species"] == "setosa")
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[1:12], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
    else if (x["Species"] == "versicolor")
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[8:16], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
    else
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[12:24], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
  }))
  task$cbind(dt)

  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none"))
  expect_pipeop(op)
  result = op$train(list(task))[[1]]
  expect_task(result)
  expect_true(result$nrow == 150)
  expect_true(result$ncol > 6)
  expect_true(all(result$feature_types$type == "numeric"))
  expect_true(all(grepl("^Petal\\..*|^Sepal\\..*|^txt\\..*",result$feature_names)))

  # verify for first row
  strs = unlist(strsplit(dt[1, ][["txt"]], fixed = TRUE, split = " "))
  dt2 = result$data()
  expect_true(all(dt2[1, paste0("txt.", strs), with = FALSE] >= 1L))
  expect_datapreproc_pipeop_class(PipeOpTextVectorizer, task = task)

  prd = op$predict(list(task$filter(rows = integer(0))))[[1]]
  expect_task(prd)
  expect_true(prd$nrow == 0L)
})


test_that("PipeOpTextVectorizer - tfidf works", {
  skip_if_not_installed("quanteda")

  task = mlr_tasks$get("iris")
  # create some text data
  dt = data.table("txt" = apply(iris, 1, function(x) {
    if (x["Species"] == "setosa")
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[1:3], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
    else if (x["Species"] == "versicolor")
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[8:10], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
    else
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[12:14], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
  }))
  task$cbind(dt)

  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none", scheme_df = "inverse"))
  expect_pipeop(op)
  result = op$train(list(task))[[1]]
  expect_task(result)
  expect_true(result$nrow == 150)
  expect_true(result$ncol > 6)
  expect_true(all(result$feature_types$type == "numeric"))

  trueresult = as.data.table(quanteda::convert(quanteda::dfm_tfidf(quanteda::dfm(quanteda::tokens(dt$txt))), "matrix"))
  colnames(trueresult) = paste0("txt.", colnames(trueresult))

  popresult = result$data(cols = selector_grep("^txt\\.")(result))

  expect_equal(popresult, trueresult)




  # verify for first row
  strs = unlist(strsplit(dt[1, ][["txt"]], fixed = TRUE, split = " "))
  dt2 = result$data()
  expect_true(all(dt2[1, paste0("txt.", strs), with = FALSE] > 0))

  # verify for prediction
  result2 = op$predict(list(task))[[1]]
  expect_task(result2)
  expect_true(result2$nrow == 150)
  expect_true(result2$ncol > 6)
  expect_true(all(result2$feature_types$type == "numeric"))

  popresult2 = result$data(cols = selector_grep("^txt\\.")(result2))

  expect_equal(popresult2, trueresult)


  # verify same for first row
  dt3 = result2$data()
  expect_true(all(dt2[1, paste0("txt.", strs), with = FALSE] == dt3[1, paste0("txt.", strs), with = FALSE]))

  # out-of-bag tokens during prediction:
  df = task$data(rows = 1)
  df$txt = paste0(df$txt, " foobar")

  task$rbind(df)
  result3 = op$predict(list(task))[[1]]
  dt4 = result3$data()
  expect_true(all(dt4[1, paste0("txt.", strs), with = FALSE] == dt3[1, paste0("txt.", strs), with = FALSE]))
})

test_that("PipeOpTextVectorizer - bigrams", {
  skip_if_not_installed("quanteda")

  task = mlr_tasks$get("iris")
  # create hacky text data:
  dt = data.table("txt" = apply(iris, 1, function(x) {
    if (x["Species"] == "setosa")
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[1:12], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
    else if (x["Species"] == "versicolor")
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[8:16], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
    else
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[12:24], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
  }))
  task$cbind(dt)

  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none"))
  expect_pipeop(op)
  result = op$train(list(task))[[1]]
  expect_task(result)
  expect_true(result$nrow == 150)
  expect_true(result$ncol > 6)
  expect_true(all(result$feature_types$type == "numeric"))

  # verify for first row
  strs = unlist(strsplit(dt[1, ][["txt"]], fixed = TRUE, split = " "))
  dt2 = result$data()
  expect_true(all(dt2[1, paste0("txt.", strs), with = FALSE] >= 1L))
})


test_that("PipeOpTextVectorizer - integer sequence", {
  skip_if_not_installed("quanteda")

  task = mlr_tasks$get("iris")
  # create hacky text data:
  dt = data.table("txt" = apply(iris, 1, function(x) {
    if (x["Species"] == "setosa")
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[1:12], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
    else if (x["Species"] == "versicolor")
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[8:16], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
    else
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[12:24], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
  }))
  task$cbind(dt)

  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none", return_type = "integer_sequence"))
  expect_pipeop(op)
  result = op$train(list(task))[[1]]
  expect_task(result)
  expect_true(result$nrow == 150)
  expect_true(result$ncol > 6)
  expect_true(all(result$feature_types$type == "numeric"))
  expect_true(all(grepl("^Petal\\..*|^Sepal\\..*|^txt\\..*",result$feature_names)))

  # verify for first row
  strs = unlist(strsplit(dt[1, ][["txt"]], fixed = TRUE, split = " "))
  dt2 = result$data()
  expect_true(all(dt2[1, paste0("txt.V", seq_len(10)), with = FALSE] == 1:10))
  expect_datapreproc_pipeop_class(PipeOpTextVectorizer, task = task)

  prd = op$predict(list(task$clone()$filter(rows = 1L)))[[1]]
  expect_true(all(prd$data() == result$data(rows = 1L)))
  expect_true(prd$nrow == 1L)

  prd = op$predict(list(task$clone()$filter(rows = integer(0))))[[1]]
  expect_task(prd)
  expect_true(prd$nrow == 0L)

  # Pad pad0
  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none", return_type = "integer_sequence", sequence_length = 20L))
  result = op$train(list(task))[[1]]
  dt2 = result$data()
  expect_true(ncol(dt2) == 25L)
  prd = op$predict(list(task$clone()$filter(rows = 1L)))[[1]]
  expect_true(all(prd$data() == result$data(rows = 1L)))
  expect_true(prd$nrow == 1L)

  # Cut pad0
  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none", return_type = "integer_sequence", sequence_length = 4L))
  result = op$train(list(task))[[1]]
  dt2 = result$data()
  expect_true(ncol(dt2) == 9L)
  prd = op$predict(list(task$clone()$filter(rows = 1L)))[[1]]
  expect_true(all(prd$data() == result$data(rows = 1L)))
  expect_true(prd$nrow == 1L)

  # OOB Newdata
  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none", return_type = "integer_sequence", sequence_length = 4L))
  result = op$train(list(task$clone()$filter(2:3)))[[1]]
  prd = op$predict(list(task$clone()$filter(rows = 1L)))[[1]]
  expect_true(sum(prd$data()[, paste0("txt.V", 1:4)]) == 0)
  expect_true(prd$nrow == 1L)
})

test_that("PipeOpTextVectorizer - factor sequence", {
  skip_if_not_installed("quanteda")

  task = mlr_tasks$get("iris")
  # create hacky text data:
  dt = data.table("txt" = apply(iris, 1, function(x) {
    if (x["Species"] == "setosa")
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[1:12], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
    else if (x["Species"] == "versicolor")
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[8:16], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
    else
      paste(map_chr(1:10, function(x) {
        paste(sample(letters[12:24], 3, replace = TRUE), collapse = "")
      }), collapse = " ")
  }))
  task$cbind(dt)

  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none", return_type = "factor_sequence"))
  expect_pipeop(op)
  result = op$train(list(task))[[1]]
  expect_task(result)
  expect_true(result$nrow == 150)
  expect_true(result$ncol > 6)
  expect_true(all(grepl("^Petal\\..*|^Sepal\\..*|^txt\\..*", result$feature_names)))

  prd = op$predict(list(task$clone()$filter(rows = 1L)))[[1]]
  expect_true(all(prd$data() == result$data(rows = 1L)))
  expect_true(prd$nrow == 1L)

  prd = op$predict(list(task$clone()$filter(rows = integer(0))))[[1]]
  expect_task(prd)
  expect_true(prd$nrow == 0L)

  # Pad pad0
  nona = function(x) {x[!is.na(x)]}
  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none", return_type = "factor_sequence", sequence_length = 20L))
  result = op$train(list(task))[[1]]
  dt2 = result$data()
  expect_true(ncol(dt2) == 25L)
  prd = op$predict(list(task$clone()$filter(rows = 1L)))[[1]]
  expect_true(all(nona(prd$data() == result$data(rows = 1L))))
  expect_true(prd$nrow == 1L)

  # Cut pad0
  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none", return_type = "factor_sequence", sequence_length = 4L))
  result = op$train(list(task))[[1]]
  dt2 = result$data()
  expect_true(ncol(dt2) == 9L)
  prd = op$predict(list(task$clone()$filter(rows = 1L)))[[1]]
  expect_true(all(nona(prd$data() == result$data(rows = 1L))))
  expect_true(prd$nrow == 1L)

  # OOB Newdata
  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "none", return_type = "factor_sequence", sequence_length = 4L))
  result = op$train(list(task$clone()$filter(2:3)))[[1]]
  prd = op$predict(list(task$clone()$filter(rows = 1L)))[[1]]
  expect_true(all(is.na(prd$data()[, paste0("txt.V", 1:4)])), info = paste(capture.output(print(prd$data())), collapse = "\n"))
  expect_true(prd$nrow == 1L)
})
