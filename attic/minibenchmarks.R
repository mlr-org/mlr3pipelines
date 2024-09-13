






library("ggplot2")
library("microbenchmark")
library("data.table")

bm_listaccess <- function() {
  env <- new.env()
  env$a <- 1
  env$b <- "x"
  env$c <- 1:10
  env$d <- list(1, 2, 3)
  env$e <- data.frame(x = 1:10, y = 11:20)
  env$f <- function(x) x + 1
  env$g <- FALSE


  lst <- as.list(env)

  indirect <- c(1, 3, 7)
  indirect_int <- c(1L, 3L, 7L)

  microbenchmark(times = 1000L,
    env_dollar = { env$a; env$c; env$g },
    env_dollar_set = { env$a <- 2; env$c <- 2:11; env$g <- TRUE; env$a <- 1; env$c <- 1:10; env$g <- FALSE },
    env_bracket = { env[["a"]]; env[["c"]]; env[["g"]] },
    env_bracket_set = { env[["a"]] <- 2; env[["c"]] <- 2:11; env[["g"]] <- TRUE; env[["a"]] <- 1; env[["c"]] <- 1:10; env[["g"]] <- FALSE },
    env_get = { get("a", env); get("c", env); get("g", env) },
    env_assign = { assign("a", 2, env); assign("c", 2:11, env); assign("g", TRUE, env); assign("a", 1, env); assign("c", 1:10, env); assign("g", FALSE, env) },
    lst_dollar = { lst$a; lst$c; lst$g },
    lst_dollar_set = { lst$a <- 2; lst$c <- 2:11; lst$g <- TRUE; lst$a <- 1; lst$c <- 1:10; lst$g <- FALSE },
    lst_bracket = { lst[["a"]]; lst[["c"]]; lst[["g"]] },
    lst_bracket_set = { lst[["a"]] <- 2; lst[["c"]] <- 2:11; lst[["g"]] <- TRUE; lst[["a"]] <- 1; lst[["c"]] <- 1:10; lst[["g"]] <- FALSE },
    lst_bracket_int = { lst[[1]]; lst[[3]]; lst[[7]] },
    lst_bracket_int_set = { lst[[1]] <- 2; lst[[3]] <- 2:11; lst[[7]] <- TRUE; lst[[1]] <- 1; lst[[3]] <- 1:10; lst[[7]] <- FALSE },
    lst_indirect = { lst[[indirect[1]]]; lst[[indirect[2]]]; lst[[indirect[3]]] },
    lst_indirect_set = { lst[[indirect[1]]] <- 2; lst[[indirect[2]]] <- 2:11; lst[[indirect[3]]] <- TRUE; lst[[indirect[1]]] <- 1; lst[[indirect[2]]] <- 1:10; lst[[indirect[3]]] <- FALSE },
    lst_indirect_int = { lst[[indirect_int[1]]]; lst[[indirect_int[2]]]; lst[[indirect_int[3]]] },
    lst_indirect_int_set = { lst[[indirect_int[1]]] <- 2; lst[[indirect_int[2]]] <- 2:11; lst[[indirect_int[3]]] <- TRUE; lst[[indirect_int[1]]] <- 1; lst[[indirect_int[2]]] <- 1:10; lst[[indirect_int[3]]] <- FALSE }
  )
}



bmr <- bm_listaccess()


autoplot(bmr)

bmrt <- as.data.table(bmr)[, .(mean = mean(time), median = median(time), mean_robust = mean(time, trim = 0.1)), by = expr][order(mean_robust)]

par(mar = c(8, 4, 4, 2) + 0.1)
matplot(bmrt[, -1], type = "b", pch = 1, xaxt = "n", ylab = "Values", xlab = "expr", main = "List access benchmarks", col = seq_along(bmrt[, -1]))
legend("bottomright", legend = colnames(bmrt)[-1], col = seq_along(bmrt[, -1]), pch = 1, lty = 1)
axis(1, at = seq_along(bmrt$expr), labels = bmrt$expr, las = 2)

round(sweep(as.matrix(bmrt[, 2:4], rownames = bmrt[[1]]), 2, as.numeric(bmrt[1, 2:4]), "/"), 2)

# GET: (lst_bracket_int, env_bracket) is faster than lst_bracket, which is faster than
# env_dollar, which is slightly faster than lst_indirect_(int), which is slightly faster than lst_dollar
#   HUGE difference to: env_get
# SET: env_bracket slightly faster than lst_bracket_int, much faster than env_dollar|lst_bracket, which is slightly faster than lst(indirect), which is faster than lst_dollar, which is much faster than env_assign

# -> envs are faster than named lists for setting; lists with numbers are the same (get) or almost the same (set) as envs
# -> don't use get() / assign()
# -> use brackets
# -> use integers for list access; list with names adds 25%
# -> indirect lst *set* makes barely any difference to named list



bm_subsettest <- function() {
  one <- c("elephant", "mouse", "tiger", "cat", "dog")
  two <- c("cat", "dog", "mouse", "tiger")
  three <- c("dog", "cat", "mouse", "tiger", "fish")

  microbenchmark(times = 1000L,
    allin_one_two = { x1 <- all(one %in% two); x2 <- all(two %in% one) },
    allin_one_three = { x1 <- all(one %in% three); x2 <- all(three %in% one) },
    allin_two_three = { x1 <- all(two %in% three); x2 <- all(three %in% two) },
    lengthcmp_one_two = { lu <- length(unique(c(one, two))) ; x1 <- lu == length(two) ; x2 <- lu == length(one) },
    lengthcmp_one_three = { lu <- length(unique(c(one, three))) ; x1 <- lu == length(three) ; x2 <- lu == length(one) },
    lengthcmp_two_three = { lu <- length(unique(c(two, three))) ; x1 <- lu == length(three) ; x2 <- lu == length(two) },
    lengthcmp_union_one_two = { lu <- length(union(one, two)) ; x1 <- lu == length(two) ; x2 <- lu == length(one) },
    lengthcmp_union_one_three = { lu <- length(union(one, three)) ; x1 <- lu == length(three) ; x2 <- lu == length(one) },
    lengthcmp_union_two_three = { lu <- length(union(two, three)) ; x1 <- lu == length(three) ; x2 <- lu == length(two) }
  )
}




bmr <- bm_subsettest()

autoplot(bmr)

bmrt <- as.data.table(bmr)[, .(mean = mean(time), median = median(time), mean_robust = mean(time, trim = 0.1)), by = expr][order(mean_robust)]

par(mar = c(8, 4, 4, 2) + 0.1)
matplot(bmrt[, -1], type = "b", pch = 1, xaxt = "n", ylab = "Values", xlab = "expr", main = "List access benchmarks", col = seq_along(bmrt[, -1]))
legend("bottomright", legend = colnames(bmrt)[-1], col = seq_along(bmrt[, -1]), pch = 1, lty = 1)
axis(1, at = seq_along(bmrt$expr), labels = bmrt$expr, las = 2)

round(sweep(as.matrix(bmrt[, 2:4], rownames = bmrt[[1]]), 2, as.numeric(bmrt[1, 2:4]), "/"), 2)

round(sweep(as.matrix(bmrt[, 2:4], rownames = bmrt[[1]]), 2, as.numeric(bmrt[5, 2:4]), "/"), 2)

# length cmp is 28% slower than all (x %in% y)
# union is 2x slower than allin; adds 50% to length cmp
#


bm_extend_list <- function(times = 1) {
  concatenating = list(
    c("elephant", "mouse", "tiger", "cat", "dog"),
    list(c("green", "blue", "purple"), c(1, 1, 1), 10:0),
    c(1, 2, 3, 4, 5),
    list(c(1, 2, 3), c("a", "b", "c"), c(1, 2, 3, 4, 5))
  )

  concatenating <- rep(concatenating, times)

  microbenchmark(
    expand = {
      entries = list()
      for (element in concatenating) {
        if (is.list(element)) {
          entries <- c(entries, element)
        } else {
          entries[[length(entries) + 1]] <- element
        }
      }
    },
    unlist = {
      entries = list()
      for (element in concatenating) {
        if (!is.list(element)) {
          element <- list(element)
        }
        entries[[length(entries) + 1]] <- element
      }
      entries <- unlist(entries, recursive = FALSE)
    }
  )
}

bm_extend_list()
# unlist() is clearly slower

bm_extend_list(10)

bm_extend_list(100)
# unlist() is clearly faster



bm_create_list <- function() {
  names <- c("elephant", "mouse", "tiger", "cat", "dog", "mouse")
  microbenchmark(times = 1000,
    list_plain = {
      result = list()
      for (n in names) {
        result[[n]] <- n
      }
    },
    list_from_env = {
      env <- new.env()
      for (n in names) {
        env[[n]] <- n
      }
      result <- as.list(env, all.names = TRUE)
    },
    list_from_precise_env = {
      env <- new.env(size = length(names))
      for (n in names) {
        env[[n]] <- n
      }
      result <- as.list(env, all.names = TRUE)
    },
    list_from_precise_x2_env = {
      env <- new.env(size = length(names), parent = emptyenv())
      for (n in names) {
        env[[n]] <- n
      }
      result <- as.list(env, all.names = TRUE)
    }
  )
}

bm_create_list() |> autoplot()
# plain list is faster than creating an env first; setting the size of the env and making parent = emptyenv() makes a small difference!


remove_class_info <- function() {
  sx <- structure(
    list(a = 1, b = 2, c = 3),
    class = "test",
    attr = list(a = 1, b = 2, c = 3)
  )

  microbenchmark(times = 1000,
    structure = structure(sx, class = NULL),
    unclass = unclass(sx),
    c = c(sx),
    assign.class = {
      class(sx) <- NULL
      sx
    },
    assign.attr = {
      attr(sx, "class") <- NULL
      sx
    }
  )
}

remove_class_info() |> autoplot()
# unclass() is MUCH faster than c(), which is better than the others



fill_list = function() {
  tofill = runif(10000)
  env = new.env(parent = emptyenv())

  microbenchmark(times = 100,
    prealloc = {
      v = numeric(length(tofill))
      for (i in seq_along(tofill)) {
        v[[i]] = tofill[[i]]
      }
    },
    extend = {
      v = numeric(0)
      for (i in seq_along(tofill)) {
        v[[i]] = tofill[[i]]
      }
    },
    extend_dynamic = {
      v = numeric(0)
      for (i in seq_along(tofill)) {
        v[[length(v) + 1]] = tofill[[i]]
      }
    },
    in_env_prealloc = {
      env[["v"]] = numeric(length(tofill))
      for (i in seq_along(tofill)) {
        env[["v"]][[i]] = tofill[[i]]
      }
    },
    in_env_extend = {
      env[["v"]] = numeric(0)
      for (i in seq_along(tofill)) {
        env[["v"]][[i]] = tofill[[i]]
      }
    },
    in_env_extend_dynamic = {
      env[["v"]] = numeric(0)
      for (i in seq_along(tofill)) {
        env[["v"]][[length(env[["v"]]) + 1]] = tofill[[i]]
      }
    }
  )
}

fill_list() |> autoplot()
# prealloc < extend (+8%) < extend_dynamic (+30%)
# (n_env_prealloc < in_env_extend ; approx 1.5x prealloc) << in_env_extend_dynamic (approx 2x prealloc)

bmr <- fill_list()

autoplot(bmr)

bmrt <- as.data.table(bmr)[, .(mean = mean(time), median = median(time), mean_robust = mean(time, trim = 0.1)), by = expr][order(mean_robust)]

par(mar = c(8, 4, 4, 2) + 0.1)
matplot(bmrt[, -1], type = "b", pch = 1, xaxt = "n", ylab = "Values", xlab = "expr", main = "List access benchmarks", col = seq_along(bmrt[, -1]))
legend("bottomright", legend = colnames(bmrt)[-1], col = seq_along(bmrt[, -1]), pch = 1, lty = 1)
axis(1, at = seq_along(bmrt$expr), labels = bmrt$expr, las = 2)

round(sweep(as.matrix(bmrt[, 2:4], rownames = bmrt[[1]]), 2, as.numeric(bmrt[1, 2:4]), "/"), 2)

local_globals = function() {
  env <- new.env(parent = emptyenv())
  env$a <- 100000000
  env$b <- 100000000
  i <- 100000000
  j <- 100000000

  funcall <- function() { }

  inc_i <- function() {
    i <<- i + 1
  }

  inc_ij <- function() {
    i <<- i + 1
    j <<- j + 1
  }

  inc_env <- function() {
    env[["a"]] <- env[["a"]] + 1
  }
  inc_env2 <- function() {
    env[["a"]] <- env[["a"]] + 1
    env[["b"]] <- env[["b"]] + 1
  }

  microbenchmark(times = 1000,
    funcall = { funcall() },
    inc_i = { inc_i() },
    inc_ij = { inc_ij() },
    inc_env = { inc_env() },
    inc_env2 = { inc_env2() },
    inc_i_direct = { i <- i + 1 },
    inc_ij_direct = { i <- i + 1; j <- j + 1 }
  )
}

local_globals() |> autoplot()

bmr <- local_globals()

autoplot(bmr)

bmrt <- as.data.table(bmr)[, .(mean = mean(time), median = median(time), mean_robust = mean(time, trim = 0.1)), by = expr][order(mean_robust)]

par(mar = c(8, 4, 4, 2) + 0.1)
matplot(bmrt[, -1], type = "b", pch = 1, xaxt = "n", ylab = "Values", xlab = "expr", main = "List access benchmarks", col = seq_along(bmrt[, -1]))
legend("bottomright", legend = colnames(bmrt)[-1], col = seq_along(bmrt[, -1]), pch = 1, lty = 1)
axis(1, at = seq_along(bmrt$expr), labels = bmrt$expr, las = 2)

round(sweep(as.matrix(bmrt[, 2:4], rownames = bmrt[[1]]), 2, as.numeric(bmrt[1, 2:4]), "/"), 2)
# accessing env is about 2x slower than direct access; <<- is not that bad actually, some part of it seems to be the function call itself


delete_vector_entry = function() {
  vct = runif(20)
  lst = as.list(vct)
  microbenchmark(times = 1000,
    set_null = { lst2 = lst ; lst2[[2]] <- NULL },
    neg_index_list = { lst2 = lst ; lst2 <- lst2[-2] },
    neg_index_vec = { vct2 = vct ; vct2 <- vct2[-2] }
  )
}

bmr <- delete_vector_entry()

bmrt <- as.data.table(bmr)[, .(mean = mean(time), median = median(time), mean_robust = mean(time, trim = 0.1)), by = expr][order(mean_robust)]

par(mar = c(8, 4, 4, 2) + 0.1)
matplot(bmrt[, -1], type = "b", pch = 1, xaxt = "n", ylab = "Values", xlab = "expr", main = "List access benchmarks", col = seq_along(bmrt[, -1]))
legend("bottomright", legend = colnames(bmrt)[-1], col = seq_along(bmrt[, -1]), pch = 1, lty = 1)
axis(1, at = seq_along(bmrt$expr), labels = bmrt$expr, las = 2)

round(sweep(as.matrix(bmrt[, 2:4], rownames = bmrt[[1]]), 2, as.numeric(bmrt[1, 2:4]), "/"), 2)


skip_loop = function() {
  vct = runif(20)
  skipped = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  microbenchmark(times = 1000,
    skip_entry = {
      entries = c(1L, 3L, 5L, 7L, 9L, 11L, 13L, 15L, 17L, 19L)
      result = 0
      for (i in entries) {
        if (skipped[[i]]) {
          next
        }
        result <- result + vct[[i]]
      }
    },
    delete_entry = {
      entries = c(1L, 3L, 5L, 7L, 9L, 11L, 13L, 15L, 17L, 19L)
      entries <- entries[-2]
      entries <- entries[-5]
      entries <- entries[-8]
      result = 0
      for (i in entries) {
        result <- result + vct[[i]]
      }
    },
    skip_entry_twice = {
      entries = c(1L, 3L, 5L, 7L, 9L, 11L, 13L, 15L, 17L, 19L)
      result = 0
      for (times in 1:2) {
        for (i in entries) {
          if (skipped[[i]]) {
            next
          }
          result <- result + vct[[i]]
        }
      }
    },
    delete_entry_twice = {
      entries = c(1L, 3L, 5L, 7L, 9L, 11L, 13L, 15L, 17L, 19L)
      entries <- entries[-2]
      entries <- entries[-5]
      entries <- entries[-8]
      result = 0
      for (times in 1:2) {
        for (i in entries) {
          result <- result + vct[[i]]
        }
      }
    }
  )
}

bmr <- skip_loop()

bmrt <- as.data.table(bmr)[, .(mean = mean(time), median = median(time), mean_robust = mean(time, trim = 0.1)), by = expr][order(mean_robust)]

par(mar = c(8, 4, 4, 2) + 0.1)
matplot(bmrt[, -1], type = "b", pch = 1, xaxt = "n", ylab = "Values", xlab = "expr", main = "List access benchmarks", col = seq_along(bmrt[, -1]))
legend("bottomright", legend = colnames(bmrt)[-1], col = seq_along(bmrt[, -1]), pch = 1, lty = 1)
axis(1, at = seq_along(bmrt$expr), labels = bmrt$expr, las = 2)

round(sweep(as.matrix(bmrt[, 2:4], rownames = bmrt[[1]]), 2, as.numeric(bmrt[1, 2:4]), "/"), 2)

# deleting is faster than skipping, apparently


sum_all_any = function() {
  vct1 = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  vct2 = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  vct3 = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  vct4 = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

  int1 = as.integer(vct1)
  int2 = as.integer(vct2)
  int3 = as.integer(vct3)
  int4 = as.integer(vct4)

  microbenchmark(times = 10000,
    all_any = {
      r1 = c(all(vct1), all(vct2), all(vct3), all(vct4))
      r2 = c(any(vct1), any(vct2), any(vct3), any(vct4))
    },
    sums = {
      vs1 = c(sum(vct1), sum(vct2), sum(vct3), sum(vct4))
      r1 = vs1 == 7L
      r2 = vs1 != 0L
    },
    sums_int = {
      vs1 = c(sum(int1), sum(int2), sum(int3), sum(int4))
      r = vs1 != 0L
    },
    sums_any = {
      vs1 = c(sum(vct1), sum(vct2), sum(vct3), sum(vct4))
      r = vs1 != 0L
    },
    any = {
      r = c(any(vct1), any(vct2), any(vct3), any(vct4))
    }
  )
}

bmr <- sum_all_any()

bmrt <- as.data.table(bmr)[, .(mean = mean(time), median = median(time), mean_robust = mean(time, trim = 0.1)), by = expr][order(mean_robust)]

par(mar = c(8, 4, 4, 2) + 0.1)
matplot(bmrt[, -1], type = "b", pch = 1, xaxt = "n", ylab = "Values", xlab = "expr", main = "List access benchmarks", col = seq_along(bmrt[, -1]))
legend("bottomright", legend = colnames(bmrt)[-1], col = seq_along(bmrt[, -1]), pch = 1, lty = 1)
axis(1, at = seq_along(bmrt$expr), labels = bmrt$expr, las = 2)

round(sweep(as.matrix(bmrt[, 2:4], rownames = bmrt[[1]]), 2, as.numeric(bmrt[1, 2:4]), "/"), 2)
# use all / any for when only requesting one, but when multiple are used use sums()
# sums don't make a difference between logical and integer vectors
