library(mlr3)
library(checkmate)
library(testthat)

lapply(list.files(system.file("testthat", package = "mlr3pipelines"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)