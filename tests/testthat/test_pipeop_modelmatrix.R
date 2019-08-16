context("PipeOpModelMatrix")

test_that("PipeOpModelMatrix - basic properties", {

  task = mlr_tasks$get("iris")
  # General
  expect_datapreproc_pipeop_class(PipeOpModelMatrix,
    constargs = list(param_vals = list(formula = ~ . ^ 2)), task = task)

  # Intercept
  op = PipeOpModelMatrix$new(param_vals = list(formula = ~ . ^ 2))
  expect_pipeop(op)
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  expect_true("(Intercept)" %in% fn) # Intercept
  expect_equal(length(grep(":", fn)), 6) # 6 interaction terms
  expect_equal(length(fn), 6 + 4 + 1) # 6 interaction terms + 4 main + intercept

  multiplyCols <- function(df, nams){
    name1 = nams[[1]]
    name2 = nams[2]
    df[, name1, with = FALSE] * df[, name2, with = FALSE]
  }

  fn.inter = fn[grep(":", fn)]
  fn.sing = strsplit(fn.inter, ":")
  nt.dat = nt$data()
  sapply(1:3, function(i) {
    expect_true(all(nt.dat[, get(fn.inter[i])] ==
        multiplyCols(nt.dat, fn.sing[[i]])))
  })

  # Without intercept
  op = PipeOpModelMatrix$new(param_vals = list(formula = ~ 0 + Sepal.Length))
  expect_pipeop(op)
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  expect_true("(Intercept)" %nin% fn)
  expect_true("Sepal.Length" %in% fn)

  # other formula
  expect_datapreproc_pipeop_class(PipeOpModelMatrix,
    constargs = list(param_vals = list(formula = ~ 0 + Sepal.Length +
        log(Sepal.Length))), task = task)
  op = PipeOpModelMatrix$new(param_vals = list(formula = ~ 0 + Sepal.Length +
      log(Sepal.Length)))
  expect_pipeop(op)
  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names
  nt.dat = nt$data()
  expect_true(all(nt.dat[, "log(Sepal.Length)", with = TRUE] ==
      log(nt.dat[, Sepal.Length])))
}
)

