context("DataBackendJoin")


test_that("DataBackendJoin works as expected", {

  d1 <- data.table(x = c(letters[-2], NA), y = LETTERS, z = rep(1:13, 2), id = (1:26) * 10L)
  d2 <- data.table(a = c(paste0(letters, LETTERS)[-2], NA), y = letters, idx = (27:2) * 10L)
  d3 <- data.table(a = c(paste0(letters, LETTERS)[-2], NA), y = letters, id = (27:2))

  d1b <- DataBackendDataTable$new(d1, "id")
  d2b <- DataBackendDataTable$new(d2, "idx")
  d3b <- DataBackendDataTable$new(d3, "id")



  dbj <- DataBackendJoin$new(d1b, d2b)

  expect_backend(dbj)

  expect_identical(dbj$data(1:3, c("x", "y", "id", "a", "idx")),
    data.table(x = letters[1:4][-2], y = c(NA, letters[26:25]), id = (1:3) * 10L, a = c(NA, NA, "zZ"), idx = c(NA, (2:3) * 10L))
  )

  expect_identical(dbj$data(1:3, c("x", "y", "id", "a", "idx", dbj$primary_key)),
    data.table(x = letters[1:4][-2], y = c(NA, letters[26:25]), id = (1:3) * 10L, a = c(NA, NA, "zZ"), idx = c(NA, (2:3) * 10L), ..row_id = 1:3)
  )

  expect_identical(dbj$missings(1:3, c("x", "y", "id", "a", "idx")), c(x = 0L, y = 1L, id = 0L, a = 2L, idx = 1L))
  expect_identical(dbj$missings(1:3, c("x", "y", "id", "a", "idx", dbj$primary_key)), c(x = 0L, y = 1L, id = 0L, a = 2L, idx = 1L, ..row_id = 0L))


  dbj <- DataBackendJoin$new(d1b, d3b, by_b1 = "z", b1_index_colname = "b1index", b2_index_colname = "b2index", type = "outer")

  expect_backend(dbj)

  expected = merge(d1[, c("x", "z", "id"), with = FALSE], rev(d3), by.x = "z", by.y = "id", all = TRUE, sort = FALSE)[, .(x, y, z = ifelse(z %inrange% c(1, 13), z, NA), id = z, a, b1index = id, b2index = z)]
  expected[, ..row_id := seq_len(nrow(expected))]
  expected[id == 1, id := NA]
  expected[b2index == 1, b2index := NA]

  expect_equal(dbj$data(dbj$rownames, dbj$colnames), expected, check.attributes = FALSE)


  dbj <- DataBackendJoin$new(d1b, d3b, by_b1 = "z", b1_index_colname = "b1index", b2_index_colname = "b2index", type = "inner")
  expect_backend(dbj)

  expected = merge(d1[, c("x", "z", "id"), with = FALSE], rev(d3), by.x = "z", by.y = "id", all = FALSE, sort = FALSE)[, .(x, y, z = ifelse(z %inrange% c(1, 13), z, NA), id = z, a, b1index = id, b2index = z)]
  expected[, ..row_id := seq_len(nrow(expected))]
  expect_equal(dbj$data(dbj$rownames, dbj$colnames), expected, check.attributes = FALSE)

  dbj <- DataBackendJoin$new(d1b, d3b, by_b1 = "z", b1_index_colname = "b1index", b2_index_colname = "b2index", type = "left")
  expect_backend(dbj)

  expected = merge(d1[, c("x", "z", "id"), with = FALSE], rev(d3), by.x = "z", by.y = "id", all.x = TRUE, all.y = FALSE, sort = FALSE)[, .(x, y, z = ifelse(z %inrange% c(1, 13), z, NA), id = z, a, b1index = id, b2index = z)]
  expected[, ..row_id := seq_len(nrow(expected))]
  expected[id == 1, id := NA]
  expected[b2index == 1, b2index := NA]

  expect_equal(dbj$data(dbj$rownames, dbj$colnames), expected, check.attributes = FALSE)

  dbj <- DataBackendJoin$new(d1b, d3b, by_b1 = "z", b1_index_colname = "b1index", b2_index_colname = "b2index", type = "right")
  expect_backend(dbj)

  expected = merge(d1[, c("x", "z", "id"), with = FALSE], rev(d3), by.x = "z", by.y = "id", all.x = FALSE, all.y = TRUE, sort = FALSE)[, .(x, y, z = ifelse(z %inrange% c(1, 13), z, NA), id = z, a, b1index = id, b2index = z)]
  expected[, ..row_id := seq_len(nrow(expected))]

  expect_equal(dbj$data(dbj$rownames, dbj$colnames), expected, check.attributes = FALSE)
})

test_that("DataBackendJoin edge cases", {
  d1 <- data.table(x = c(letters[-2], NA), y = LETTERS, z = rep(1:13, 2), ..row_id = (1:26) * 10L)
  d2 <- data.table(a = c(paste0(letters, LETTERS)[-2], NA), y = letters, ..row_id = (27:2) * 10L)

  d1b <- DataBackendDataTable$new(d1, "..row_id")
  d2b <- DataBackendDataTable$new(d2, "..row_id")

  dbj = DataBackendJoin$new(d1b, d2b, type = "inner", b1_index_colname = "b1", b2_index_colname = "b2")
  expect_backend(dbj)

  expect_set_equal(dbj$colnames, c(colnames(d1), colnames(d2), "..row_id.1", "b1", "b2"))

  expect_equal(
    dbj$data(dbj$rownames, dbj$colnames),
    d1[d2, .(x, y = i.y, z, ..row_id, a, b1 = ..row_id, b2 = ..row_id, ..row_id.1 = seq_len(25)), on = "..row_id", nomatch = NULL],
    check.attributes = FALSE
  )

  d1 = data.table(x = c(1, 2), y = c("a", "b"), ..row_id = 1:2)
  d2 = data.table(x = 1, y = "z", ..row_id = 3)
  d1b <- DataBackendDataTable$new(d1, "..row_id")
  d2b <- DataBackendDataTable$new(d2, "..row_id")

  dbj <- DataBackendJoin$new(d1b, d2b, type = "inner", by_b1 = "x", by_b2 = "x", b1_index_colname = "b1", b2_index_colname = "b2")
  expect_backend(dbj)

  expect_equal(dbj$data(dbj$rownames, dbj$colnames), data.table(x = 1, y = "z", ..row_id = 3, b1 = 1, b2 = 3, ..row_id.1 = 1))

  dbj <- DataBackendJoin$new(d1b, d2b, type = "outer", by_b1 = "x", by_b2 = "x", b1_index_colname = "b1", b2_index_colname = "b2")
  expect_backend(dbj)

  expect_equal(dbj$data(dbj$rownames, dbj$colnames), data.table(x = c(1, NA), y = c("z", NA), ..row_id = c(3, NA), b1 = 1:2, b2 = c(3, NA), ..row_id.1 = c(1, 2)))

  dbj <- DataBackendJoin$new(d1b, d2b, type = "inner", b1_index_colname = "b1", b2_index_colname = "b2")
  expect_backend(dbj)

  expect_equal(dbj$data(dbj$rownames, dbj$colnames), data.table(x = 1, y = "z", ..row_id = 3, b1 = 1, b2 = 3, ..row_id.1 = 1)[0])

  dbj <- DataBackendJoin$new(d1b, d2b, type = "outer", b1_index_colname = "b1", b2_index_colname = "b2")
  expect_backend(dbj)

  expect_equal(dbj$data(dbj$rownames, dbj$colnames), data.table(x = c(NA, NA, 1), y = c(NA, NA, "z"), ..row_id = c(NA, NA, 3), b1 = c(1, 2, NA), b2 = c(NA, NA, 3), ..row_id.1 = 1:3))

})

test_that("DataBackendJoin errors", {

  d1 <- data.table(x = c(letters[-2], NA), y = LETTERS, z = rep(1:13, 2), id = (1:26) * 10L)
  d2 <- data.table(a = c(paste0(letters, LETTERS)[-2], NA), y = letters, idx = (27:2) * 10L)

  d1b <- DataBackendDataTable$new(d1, "id")
  d2b <- DataBackendDataTable$new(d2, "idx")

  expect_error(DataBackendJoin$new(d1b, d2b, by_b1 = "n"), "by_b1.*of set.*but is 'n'")
  expect_error(DataBackendJoin$new(d1b, d2b, by_b2 = "n"), "by_b2.*of set.*but is 'n'")

  expect_error(DataBackendJoin$new(d1b, d2b, type = "inner", b1_index_colname = "x"), "already a non-primary-key")
  expect_error(DataBackendJoin$new(d1b, d2b, type = "inner", b1_index_colname = "a"), "already a non-primary-key")
  expect_error(DataBackendJoin$new(d1b, d2b, type = "inner", b2_index_colname = "x"), "already a non-primary-key")
  expect_error(DataBackendJoin$new(d1b, d2b, type = "inner", b2_index_colname = "a"), "already a non-primary-key")
  expect_error(DataBackendJoin$new(d1b, d2b, type = "inner", b1_index_colname = "n", b2_index_colname = "n"), "must be different")






})
