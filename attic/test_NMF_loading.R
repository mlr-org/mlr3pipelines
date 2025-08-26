devtools::load_all()
# debug(mlr3pipelines:::.__PipeOpTaskPreproc__.train_task)

# op = po("nmf")
# op$train(list(tsk("iris")))
# saveRDS(op$state, "attic/PipeOpNMF_state.RDS")

# ----------------------------------------------- #

orig_attached <- search()
op <- po("nmf")
op$train(list(tsk("iris")))
expect_equal(search(), orig_attached)

# ------------------------------------------- #

library(NMF)
orig_attached <- search()
op <- po("nmf")
op$train(list(tsk("iris")))
expect_equal(search(), orig_attached)

# ---------------------------------------------- #

library(BiocGenerics)
orig_attached <- search()
op <- po("nmf")
op$train(list(tsk("iris")))
expect_equal(search(), orig_attached)

# ------------------------------------------- #

orig_attached <- search()
op <- po("nmf")
op$state <- readRDS("attic/PipeOpNMF_state.RDS")
op$predict(list(tsk("iris")))
expect_equal(search(), orig_attached)

# ------------------------------------------- #

library(NMF)
orig_attached <- search()
op <- po("nmf")
op$state <- readRDS("attic/PipeOpNMF_state.RDS")
op$predict(list(tsk("iris")))
expect_equal(search(), orig_attached)

# ---------------------------------------------- #

library(BiocGenerics)
orig_attached <- search()
op <- po("nmf")
op$state <- readRDS("attic/PipeOpNMF_PipeOpNMF_state.RDS")
op$predict(list(tsk("iris")))
expect_equal(search(), orig_attached)

# different printer + accessing state somehow loads NMF (even already during field lookup ...)
