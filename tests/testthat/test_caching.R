context("Caching")

test_that("Caching works for test hash pipeop", {

  PipeOpTestHash = R6Class("PipeOpTestHash",
    inherit = PipeOp,
    public = list(
      initialize = function(id = "test.hash", param_set = ParamSet$new()) {
        super$initialize(id = id, param_set = param_set,
          input = data.table(name = "input", train = "*", predict = "*"),
          output = data.table(name = "output", train = "*", predict = "*")
        )
      }),
    private = list(
      .train = function(inputs) {
        Sys.sleep(1)
        self$state = list()
        inputs
      },
      .predict = function(inputs) {
        Sys.sleep(1)
        inputs
      }
    )
  )

  # FIXME:
  # This could fail if load is very high, nonetheless I would keep it.

  tsk = tsk("iris")
  po = PipeOpTestHash$new()

  # Takes > 1 second
  R.cache::clearCache(prompt = FALSE)
  st = Sys.time()
  po$train(list(tsk))
  expect_true(st < Sys.time() - 1)

  # takes < 1 second
  st = Sys.time()
  po$train(list(tsk))
  expect_true(st > Sys.time() - 1)

  # Takes > 1 second
  st = Sys.time()
  po$predict(list(tsk))
  expect_true(st < Sys.time() - 1)

  # takes < 1 second
  st = Sys.time()
  po$predict(list(tsk))
  expect_true(st > Sys.time() - 1)

})


test_that("Caching works for scale", {
 
  old_hash = po$hash

  po$train(list(tsk))
  R.cache::saveCache(key = keya, "a")
  R.cache::loadCache(key = keya)
  R.cache::loadCache(key = keyb)

  R.cache::findCache(key = list(map(list(tsk), "hash"), po$hash))
  R.cache::findCache(key = list(map(list(tsk), "hash"), old_hash))

  po$train(list(tsk))
  # R.cache::findCache(list(map(list(tsk), "hash"), po_hash))
  
  po = po("scale")
  tsk = tsk("zoo")
  po$train(list(tsk))
  
  po = po("scale")
  po$state

  po = po("nop")
  print(po$hash)
  R.cache::clearCache(prompt = FALSE)
  # R.cache::findCache(list(map(list(tsk), "hash"), po_hash))
  po$train(list(tsk))
  po$train(list(tsk))


  a = po("scale")
  a$hash
  a$param_set$values$center = FALSE
  a$hash

})
