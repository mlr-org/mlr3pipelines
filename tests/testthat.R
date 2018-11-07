library(testthat)
library(mlr3)
library(paradox)
library(mlrPipelines)

load_github <- function(pkg, githubPath) {
  # FIXME: it should be removed when 
  # mlr3 and paradox will be available on CRAN
  # and replaced with `library` call
  if(!require(pkg, quietly = TRUE, character.only = TRUE)) {
    devtools::install_github(githubPath)
  }
}

load_github("paradox", "mlr-org/paradox")
load_github("mlr3", "mlr-org/mlr3")

library(paradox)
library(mlr3)

test_check("mlrPipelines")
