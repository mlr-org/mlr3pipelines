context("Caching")

test_that("Bagging Pipeline", {
  library("mlr3learners")  
  library(profvis)
  library(ggplot2)

  tsk = tsk("iris")
  
  po$hash
  po$train(list(tsk))
  po$hash

  po = po("scale")
  print(po$hash)
  R.cache::clearCache(prompt = FALSE)
  # R.cache::findCache(list(map(list(tsk), "hash"), po_hash))
  po$train(list(tsk))
  po$train(list(tsk))
  # R.cache::findCache(list(map(list(tsk), "hash"), po_hash))

  po = po("nop")
  print(po$hash)
  R.cache::clearCache(prompt = FALSE)
  # R.cache::findCache(list(map(list(tsk), "hash"), po_hash))
  po$train(list(tsk))
  po$train(list(tsk))
  # R.cache::findCache(list(map(list(ts

  a = po("scale")
  a$hash
  a$param_set$values$center = FALSE
  a$hash

  for (kk in 1:5) {
    cat(sprintf("Iteration #%d:\n", kk))
    res <- evalWithMemoization({
    cat("Evaluating expression...")
    a <- 1
    b <- 2
    c <- 4
    Sys.sleep(1)
    cat("done\n")
    b
  })
  print(res)

  # Sanity checks
  stopifnot(a == 1 && b == 2 && c == 4)

  # Clean up
  rm(a, b, c)
  } # for (kk ...)

})

