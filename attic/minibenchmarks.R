






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
