context("DataBackendMultiCbind")


test_that("DataBackendMultiCbind works as expected", {

  d1 = data.table(x = c(letters[1:3], NA), y = LETTERS[1:4], z = c(1, 2, 2, 1), id = (1:4) * 10L)
  d2 = data.table(a = c(paste0(letters[1:3], LETTERS[1:3]), NA), y = letters[20:23], id = -(2:5) * 10L, idx = (0:3) * 10L)
  d3 = data.table(x = as.character(1:4), z = 9:6, id = (3:6) * 10L)

  d1b <- DataBackendDataTable$new(d1, "id")
  d2b <- DataBackendDataTable$new(d2, "idx")
  d3b <- DataBackendDataTable$new(d3, "id")

  dbmc <- DataBackendMultiCbind$new(list(d1b, d2b, d3b))

  expect_backend(dbmc)

  expect_equal(
    dbmc$data((0:6) * 10L, dbmc$colnames),
    data.table(
      x = c(NA_character_, NA, NA, 1:4), y = c(d2$y, NA, NA, NA), z = c(NA, NA, NA, 9:6),
      id = c(NA, NA, NA, 3:6) * 10L, a = c(d2$a, NA, NA, NA), idx = c((0:3) * 10L, NA, NA, NA),
      ..row_id = (0:6) * 10L
    )
  )


  dbmc <- DataBackendMultiCbind$new(list(d1b, d3b))

  expect_backend(dbmc)

  expect_equal(
    dbmc$data((0:6) * 10L, dbmc$colnames),
    data.table(
      x = c(NA_character_, NA, 1:4), y = c(d1$y, NA, NA), z = c(NA, NA, 9:6),
      id = (1:6) * 10L
    )
  )

  d0b = DataBackendDataTable$new(data.table(id = c(10:20)), "id")

  dbmc <- DataBackendMultiCbind$new(list(d0b, d1b))

  expect_backend(dbmc)

  expect_set_equal(dbmc$rownames, c(10:20, 30L, 40L))

  expect_equal(dbmc$data(c(10:20, 30L, 40L), dbmc$colnames), data.table(
    id = c(10:20, 30L, 40L),
    x = c("a", rep(NA, 9), letters[2:3], NA),
    y = c("A", rep(NA, 9), LETTERS[2:4]),
    z = c(1, rep(NA, 9), 2, 2, 1)
  ))

  expect_identical(dbmc$data(11, dbmc$colnames), data.table(id = 11L, x = NA_character_, y = NA_character_, z = NA_real_))

  expect_identical(dbmc$data(11, "x"), data.table(x = NA_character_))



  dbmc <- DataBackendMultiCbind$new(list(d0b, d3b))

  expect_backend(dbmc)


  expect_set_equal(dbmc$rownames, c(10:20, (3:6)*10L))

  expect_equal(dbmc$data(c(10:20, (3:6) * 10L), dbmc$colnames), data.table(
    id = c(10:20, (3:6) * 10L),
    x = c(rep(NA_character_, 11), 1:4),
    z = c(rep(NA, 11), 9:6)
  ))


})


