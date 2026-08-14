#' @title Synthetic Diabetes Classification Task
#'
#' @usage NULL
#' @name mlr_tasks_diabetes
#' @format [`R6Class`][R6::R6Class] object inheriting from [`TaskClassif`][mlr3::TaskClassif].
#'
#' @description
#' A synthetic binary classification task that mimics the structure of the former `pima` task.
#' It has the same eight numeric features and a `diabetes` target with the positive class set to `"pos"`.
#' Some feature columns contain missing values, which makes the task useful for preprocessing examples and tests.
#' The data is fully synthetic and contains no real patient data.
#'
#' @source
#' The data set is generated deterministically when the task is constructed.
#' 
#' @section Compatibility:
#' This help page documents the temporary compatibility copy supplied by `mlr3pipelines` when the installed version of
#' `mlr3` does not yet provide the `diabetes` task. Update to `mlr3` version 1.8.0 or newer to use the upstream task.
#' This compatibility copy will be removed in a later update of `mlr3pipelines`.
#' 
NULL

make_diabetes_fixture = function(n = 128L, seed = 20260724L) {
  stopifnot(
    length(n) == 1L,
    is.finite(n),
    n == as.integer(n),
    n >= 16L
  )
  n = as.integer(n)

  old_kind = RNGkind()
  had_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })

  # Pin the RNG kinds so direct generation remains identical to mlr3's fixture even if the caller changed RNGkind().
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion")
  set.seed(seed)

  pregnant = stats::rpois(n, lambda = 4)
  age = pmin(80L, 21L + stats::rpois(n, lambda = 13))

  glucose = round(pmax(40, stats::rnorm(n, 118 + pregnant, 25)))
  pressure = round(pmax(35, stats::rnorm(n, 71, 11)))
  triceps = round(pmax(5, stats::rnorm(n, 26, 8)))
  insulin = round(pmax(10, stats::rlnorm(n, log(85), 0.65)))
  mass = round(pmax(16, stats::rnorm(n, 32, 6)), 1)
  pedigree = round(stats::rgamma(n, shape = 2, rate = 4), 3)

  score = 0.035 * glucose +
    0.06 * mass +
    0.018 * age +
    0.35 * pedigree +
    stats::rnorm(n)

  diabetes = rep("neg", n)
  diabetes[
    order(score, decreasing = TRUE)[seq_len(max(1L, round(0.35 * n)))]
  ] = "pos"

  x = data.frame(
    pregnant = pregnant,
    glucose = glucose,
    pressure = pressure,
    triceps = triceps,
    insulin = insulin,
    mass = mass,
    pedigree = pedigree,
    age = age,
    diabetes = factor(diabetes, levels = c("neg", "pos"))
  )

  missing_columns = c(
    "glucose", "pressure", "triceps", "insulin", "mass"
  )
  for (i in seq_along(missing_columns)) {
    rows = seq.int(i, n, by = 23L + i)
    x[rows, missing_columns[[i]]] = NA
  }

  x
}

load_task_diabetes = function(id = "diabetes") {
  b = as_data_backend(make_diabetes_fixture())
  task = TaskClassif$new(id, b, target = "diabetes", positive = "pos", label = "Synthetic Diabetes")
  b$hash = "mlr3::mlr_tasks_diabetes"
  task$man = "mlr3pipelines::mlr_tasks_diabetes"
  task
}

supply_diabetes = function() {
  if (tsk()$has("diabetes")) return(invisible(NULL))
  tsk()$add("diabetes", load_task_diabetes)
}
