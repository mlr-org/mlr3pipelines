skip_if_no_mlr3_diabetes = function() {
  skip_if_not(
    exists("load_task_diabetes", envir = asNamespace("mlr3"), inherits = FALSE),
    "The installed mlr3 version does not provide the diabetes task"
  )
}

test_that("diabetes fallback matches the mlr3 task", {
  skip_if_no_mlr3_diabetes()

  reference = mlr3::tsk("diabetes")
  task = load_task_diabetes()

  checked_task = task$clone(deep = TRUE)
  checked_task$man = NA_character_
  expect_task(checked_task)
  expect_equal(task$data(), reference$data())
  expect_identical(task$backend$hash, reference$backend$hash)
  expect_identical(task$hash, reference$hash)
  expect_identical(task$label, reference$label)
  expect_identical(task$positive, reference$positive)
  expect_identical(task$man, "mlr3pipelines::mlr_tasks_diabetes")
})

test_that("generating the diabetes fallback preserves the RNG state", {
  skip_if_no_mlr3_diabetes()

  reference = mlr3::tsk("diabetes")
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

  RNGkind("L'Ecuyer-CMRG")
  set.seed(42)
  seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  kind = RNGkind()

  task = load_task_diabetes()

  expect_equal(task$data(), reference$data())
  expect_identical(RNGkind(), kind)
  expect_identical(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE), seed)

  rm(".Random.seed", envir = .GlobalEnv)
  load_task_diabetes()
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("diabetes fallback is only registered when missing", {
  skip_if_no_mlr3_diabetes()

  dictionary = mlr3::mlr_tasks
  original = dictionary$items[["diabetes"]]
  on.exit({
    if (dictionary$has("diabetes")) {
      dictionary$remove("diabetes")
    }
    do.call(
      dictionary$add,
      c(
        list(key = "diabetes", value = original$value),
        original$pars,
        list(.prototype_args = original$prototype_args)
      )
    )
  })

  supply_diabetes()
  expect_identical(dictionary$items[["diabetes"]]$value, original$value)

  dictionary$remove("diabetes")
  supply_diabetes()

  expect_true(dictionary$has("diabetes"))
  expect_identical(dictionary$get("diabetes")$man, "mlr3pipelines::mlr_tasks_diabetes")
})
