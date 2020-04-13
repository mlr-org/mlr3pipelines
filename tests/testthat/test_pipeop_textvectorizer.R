context("PipeOpTextVectorizer")

test_that("PipeOpTextVectorizer - basic properties", {
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
  expect_true(all(dt2[1, paste0("txt.", strs), with = FALSE] == 1))

  expect_datapreproc_pipeop_class(PipeOpTextVectorizer, task = task)

  prd = op$predict(list(task$filter(rows = character(0))))[[1]]
  expect_task(prd)
  expect_true(prd$nrow == 0L)
})


test_that("PipeOpTextVectorizer - tfidf works", {
  task = mlr_tasks$get("iris")
  # create some text data
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

  op = PipeOpTextVectorizer$new(param_vals = list(stopwords_language = "en", scheme_df = "inverse"))
  expect_pipeop(op)
  result = op$train(list(task))[[1]]
  expect_task(result)
  expect_true(result$nrow == 150)
  expect_true(result$ncol > 6)
  expect_true(all(result$feature_types$type == "numeric"))

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
  expect_true(all(dt2[1, paste0("txt.", strs), with = FALSE] == 1))


})
