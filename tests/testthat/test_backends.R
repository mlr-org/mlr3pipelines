context("DataBackend")

test_that("PipeOp - simple pipe", {

  requireNamespace("Matrix")
  data = Matrix::Matrix(sample(0:1, 300, replace = TRUE), ncol = 5, sparse = TRUE)
  colnames(data) = c("x1", "x2", "x3", "x4", "target")
  rownames(data) = paste0("row_", seq_len(nrow(data)))
  b = as_data_backend(data)
  task = TaskRegr$new(id = "spmat", b, target = "target")

  op1 = PipeOpScaler$new()
  expect_class(op1, "PipeOpSparsePCA")
  expect_false(op1$is_learnt)

  n1 <- GraphNode$new(op1)
  expect_false(n1$can_fire)

  trainGraph(n1, task)
  expect_true(n1$can_fire)

  expect_class(op1, "PipeOpScaler")
  expect_true(op1$is_learnt)
  expect_class(op1$result, "Task")

})

