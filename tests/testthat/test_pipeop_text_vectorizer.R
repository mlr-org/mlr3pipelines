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

  op = PipeOpTextVectorizer$new(param_vals = list("language" = "en"))
  expect_pipeop(op)
  result = op$train(list(task))[[1]]
  expect_task(result)
  expect_true(result$nrow == 150)
  expect_true(result$ncol > 6)
  expect_true(all(result$feature_types$type == "numeric"))

  # verify for first row
  strs = unlist(strsplit(dt[1, ][["txt"]], fixed = TRUE, split = " "))
  dt2 = result$data()
  expect_true(all(dt2[1, strs, with = FALSE] == 1))

  # expect_datapreproc_pipeop_class(PipeOpTextVectorizer, task = task)
})
