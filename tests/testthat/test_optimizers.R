context("Optimizer")

test_that("optimizer", {
  test_optimizer = function(opt) {
    expect_r6(opt)
    expect_function(opt$optimize, args = c("objfun", "init_weights"))
    # 3-d
    objfun = function(x) {(x[1] - .2)^2+(x[3] - .3)^2+(x[2] - .4)^2}
    out = opt$optimize(objfun, init_weights = rep(1, 3))
    expect_true(sum(abs(out - c(.2, .4, .3))) <= 0.5)

    # 1-d
    objfun = function(x) x^2
    suppressWarnings(out  <- opt$optimize(objfun, init_weights = 1))
    expect_true(out <= 0.1)
  }
  opt = OptimizerNloptr$new(param_vals = list(ub = 10))
  test_optimizer(opt)

  opt = OptimizerGenSA$new()
  test_optimizer(opt)
})


