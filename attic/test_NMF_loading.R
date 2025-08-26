# Restart R between running each block. Here as an alternative to the tests in PipeOpNMF's tests since those only
# roughly emulate restarting of the R session by unloading packages.

# debug(mlr3pipelines:::.__PipeOpTaskPreproc__.train_task)

# ----------------------------------------------- #

# op = po("nmf")
# op$train(list(tsk("iris")))
# saveRDS(op$state, "attic/PipeOpNMF_state.RDS")

# ----------------------------------------------- #

devtools::load_all()
orig_attached <- search()
op <- po("nmf")
op$train(list(tsk("iris")))
expect_equal(search(), orig_attached)

# ------------------------------------------- #

devtools::load_all()
library(NMF)
orig_attached <- search()
op <- po("nmf")
op$train(list(tsk("iris")))
expect_equal(search(), orig_attached)

# ---------------------------------------------- #

devtools::load_all()
library(BiocGenerics)
orig_attached <- search()
op <- po("nmf")
op$train(list(tsk("iris")))
expect_equal(search(), orig_attached)

# ------------------------------------------- #

devtools::load_all()
orig_attached <- search()
op <- po("nmf")
op$state <- readRDS("attic/PipeOpNMF_state.RDS")
op$predict(list(tsk("iris")))
expect_equal(search(), orig_attached)

# ------------------------------------------- #

devtools::load_all()
library(NMF)
orig_attached <- search()
op <- po("nmf")
op$state <- readRDS("attic/PipeOpNMF_state.RDS")
op$predict(list(tsk("iris")))
expect_equal(search(), orig_attached)

# ---------------------------------------------- #

devtools::load_all()
library(BiocGenerics)
orig_attached <- search()
op <- po("nmf")
op$state <- readRDS("attic/PipeOpNMF_state.RDS")
op$predict(list(tsk("iris")))
expect_equal(search(), orig_attached)
