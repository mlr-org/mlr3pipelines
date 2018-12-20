# context("sparse Matrix Ops")

# test_that("PipeOp - SparsePCA", {

#   requireNamespace("Matrix")
#   data = Matrix::Matrix(sample(0:1, 400, replace = TRUE), ncol = 8, sparse = TRUE)
#   colnames(data) = c(paste0("x", seq_len(ncol(data) - 1)), "target")
#   rownames(data) = paste0("row_", seq_len(nrow(data)))
#   b = as_data_backend(data)
#   task = TaskRegr$new(id = "spmat", b, target = "target")

#   op1 = PipeOpSparsePCA$new()
#   expect_class(op1, "PipeOpSparsePCA")
#   expect_false(op1$is_trained)

#   graph = Graph$new()
#   graph$add_node(op1)

#   expect_class(graph$train(task), "Task")
#   expect_true(graph[["sparsePca"]]$pipeop$is_trained)
#   expect_class(op1, "PipeOpSparsePCA")

# })
