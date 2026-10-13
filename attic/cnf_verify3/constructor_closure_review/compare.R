review_dir = "attic/cnf_verify3/constructor_closure_review"
old = readRDS(file.path(review_dir, "payloads_r36.rds"))
current = readRDS(file.path(review_dir, "payloads_r46.rds"))
stopifnot(length(old) == 90L, identical(old, current))
cat("All 90 stored formula payload records agree exactly across R 3.6.3 and R 4.6.1.\n")
