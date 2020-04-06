context("Caching")

# test_that("Caching works for test hash pipeop", {
#   skip_on_cran()

#   # cache to tempdir
#   old_tmpdir = R.cache::getCacheRootPath()
#   test_tmpdir = tempdir()
#   R.cache::setCacheRootPath(test_tmpdir)

#   PipeOpTestHash = R6Class("PipeOpTestHash",
#     inherit = PipeOp,
#     public = list(
#       initialize = function(id = "test.hash", param_set = ParamSet$new()) {
#         super$initialize(id = id, param_set = param_set,
#           input = data.table(name = "input", train = "*", predict = "*"),
#           output = data.table(name = "output", train = "*", predict = "*")
#         )
#       }),
#     active = list(
#       cache_state = function(val) {
#         if (missing(val))
#           return(private$.cache_state)
#         private$.cache_state = val
#       }
#     ),
#     private = list(
#       .train = function(inputs) {
#         Sys.sleep(1)
#         message("sleeping train")
#         self$state = list("train")
#         inputs
#       },
#       .predict = function(inputs) {
#         if (inputs[[1]] == "predict") {
#           Sys.sleep(1)
#           message("sleeping predict")
#         }
#         inputs
#       },
#       .cache = TRUE,
#       .cache_state = TRUE
#     )
#   )

#   # caching is only enabled for graphs
#   gr = as_graph(PipeOpTestHash$new())
#   gr$cache = TRUE

#   # takes > 1 second
#   R.cache::clearCache(prompt = FALSE)
#   st = Sys.time()
#   expect_message(gr$train("train"), "sleeping train")
#   expect_true(st < Sys.time() - 1)

#   # takes > 1 second
#   st = Sys.time()
#   expect_message(gr$predict("predict"), "sleeping predict")
#   expect_true(st < Sys.time() - 1)

#   # cached train takes < 1 second
#   st = Sys.time()
#   expect_silent(gr$train("train"))
#   expect_true(gr$train("train") == "train")
#   expect_true(st > (Sys.time() - 1))

#   # uncached (cach_state) predict takes > 1 second
#   st = Sys.time()
#   expect_message(gr$predict("predict"),  "sleeping predict")
#   expect_true(st < Sys.time() - 1)

#   # Obtain result from cache:
#   key = list(map_chr(list(input = "train"), get_hash), PipeOpTestHash$new()$hash)
#   # R.cache appends the expression to the key before storing
#   expr = substitute({
#     op[[fun]](input)
#     state = op$state
#   })
#   key = c(list(expr = expr), key)
#   expect_equal(R.cache::loadCache(key)$results[[1]], "train")


#   # PO stochastic is respected-----------------------
#   po = PipeOpTestHash$new()
#   po$stochastic = c("train", "predict")
#   po$cache_state = FALSE
#   gr = as_graph(po)
#   gr$cache = TRUE

#   # takes > 1 second
#   R.cache::clearCache(prompt = FALSE)
#   st = Sys.time()
#   expect_message(gr$train("train"), "sleeping train")
#   expect_true(st < Sys.time() - 1)

#   # takes > 1 second
#   st = Sys.time()
#   expect_message(gr$predict("predict"), "sleeping predict")
#   expect_true(st < Sys.time() - 1)
#   # Nothing was cached:
#   expect_true(all(list.files(R.cache::getCacheRootPath()) == "README.txt"))


#   # PO cache = FALSE is respected -----------------------
#   po = PipeOpTestHash$new()
#   po$cache = FALSE
#   gr = as_graph(po)
#   gr$cache = TRUE

#   # takes > 1 second
#   R.cache::clearCache(prompt = FALSE)
#   st = Sys.time()
#   expect_message(gr$train("train"), "sleeping train")
#   expect_true(st < Sys.time() - 1)
#   expect_true(all(list.files(R.cache::getCacheRootPath()) == "README.txt"))

  
#   # PO cache_state = FALSE is respected-------------------
#   po = PipeOpTestHash$new()
#   po$cache_state = FALSE
#   gr = as_graph(po)
#   gr$cache = TRUE

#   # takes > 1 second
#   R.cache::clearCache(prompt = FALSE)
#   st = Sys.time()
#   expect_message(gr$train("train"), "sleeping train")
#   expect_true(st < Sys.time() - 1)

#   # takes > 1 second
#   st = Sys.time()
#   expect_message(gr$predict("predict"), "sleeping predict")
#   expect_true(st < Sys.time() - 1)

#   # cached train takes < 1 second
#   st = Sys.time()
#   expect_silent(gr$train("train"))
#   expect_true(gr$train("train") == "train")
#   expect_true(st > (Sys.time() - 1))

#   # cached predict takes < 1 second
#   st = Sys.time()
#   expect_silent(gr$predict("predict"))
#   expect_true(st > Sys.time() - 1)
#   expect_true(length(list.files(R.cache::getCacheRootPath())) == 3)

#   # Reset old cachepath
#   R.cache::setCacheRootPath(old_tmpdir)
#   unlink(test_tmpdir, recursive = TRUE)
# })
