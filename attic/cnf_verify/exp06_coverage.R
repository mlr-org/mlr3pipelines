# Coverage analysis: run the fuzzer workloads under covr's file_coverage and
# report which lines of CnfFormula_simplify.R (and the operator files) are
# never executed. Unexercised lines either need directed inputs (fuzzer blind
# spot -- a real bug there could have survived every campaign) or are
# unreachable (dead code).
#
# Parameters: CNF_TRIALS (default 3000; covr instrumentation is slow),
# CNF_SEED.
#
# Run: Rscript attic/cnf_verify/exp06_coverage.R

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
suppressMessages({library(checkmate); library(mlr3misc)})

n = n_trials(3000)
verify_dir = Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify")

test_code = sprintf('
  # local = TRUE: evaluate harness in the covr environment so its generator
  # closures can see the instrumented Cnf* definitions
  source(file.path("%s", "harness.R"), local = TRUE)
  set.seed(1)
  # standard fuzz trials
  for (i in seq_len(%d)) {
    trial = gen_standard_trial(i)
    clause_objs = lapply(trial$clauses, bare_to_clause, universe = trial$uinfo$universe)
    invisible(tryCatch(CnfFormula(clause_objs), error = function(e) NULL))
  }
  # directed unit-merge trials
  for (i in seq_len(%d)) {
    set.seed(7000003L + i)
    n_sym = sample(2:4, 1)
    dom_sizes = sample(3:5, n_sym, replace = TRUE)
    uinfo = gen_universe(n_sym, dom_sizes)
    cls = gen_unit_merge_hla_clauses(uinfo$domains)
    clause_objs = lapply(cls, bare_to_clause, universe = uinfo$universe)
    invisible(tryCatch(CnfFormula(clause_objs), error = function(e) NULL))
  }
  # operator workload
  for (i in seq_len(%d)) {
    set.seed(2000003L + i)
    uinfo = gen_universe(3, c(3, 3, 2))
    mk = function(n_cl) CnfFormula(lapply(gen_random_clauses(uinfo$domains, n_cl, 3L), bare_to_clause, universe = uinfo$universe))
    f = mk(sample(1:3, 1)); g = mk(sample(1:3, 1))
    invisible(tryCatch({
      `&.CnfFormula`(f, g); `|.CnfFormula`(f, g)
      if (!is.logical(unclass(f))) `!.CnfFormula`(f)
      a1 = CnfAtom(uinfo$syms[[1]], sample(uinfo$domains[[1]], 2))
      a2 = CnfAtom(uinfo$syms[[2]], sample(uinfo$domains[[2]], 1))
      cl = `|.CnfAtom`(a1, a2)
      `&.CnfAtom`(a1, a2); `!.CnfAtom`(a1); `!.CnfClause`(cl)
      as.list(CnfClause(list(a1, a2)))
      cl[1]; cl["V1"]; cl[c(TRUE, FALSE)]
      as.CnfFormula(a1); as.CnfClause(a1); as.logical(cl)
      all.equal(f, g); all.equal(a1, a2); all.equal(cl, cl)
      format(f); format(cl); format(a1)
      print(f); print(cl); print(a1)
    }, error = function(e) NULL))
  }
', verify_dir, n, max(200L, n %/% 5L), max(200L, n %/% 5L))

src_files = file.path(CNF_SRC_DIR, c("CnfUniverse.R", "CnfSymbol.R", "CnfAtom.R", "CnfClause.R", "CnfFormula.R", "CnfFormula_simplify.R"))

test_file = tempfile(fileext = ".R")
writeLines(test_code, test_file)

cat(sprintf("running covr::file_coverage with %d fuzz trials (this is slow under instrumentation)...\n", n))
sink(tempfile())  # suppress print() noise from the operator workload
cov = covr::file_coverage(source_files = src_files, test_files = test_file)
sink()

df = as.data.frame(cov)
# aggregate per line
agg = aggregate(value ~ filename + first_line, data = df, FUN = sum)
zero = agg[agg$value == 0, ]
zero = zero[order(zero$filename, zero$first_line), ]

cat("\n== per-file line coverage ==\n")
for (f in unique(agg$filename)) {
  sub = agg[agg$filename == f, ]
  cat(sprintf("%-28s %4d/%4d lines hit (%.1f%%)\n", basename(f), sum(sub$value > 0), nrow(sub), 100 * mean(sub$value > 0)))
}

cat("\n== unexecuted lines ==\n")
for (f in unique(zero$filename)) {
  lines = readLines(f)
  sub = zero[zero$filename == f, ]
  cat("--", basename(f), "--\n")
  for (ln in sub$first_line) {
    cat(sprintf("  %4d: %s\n", ln, trimws(lines[ln])))
  }
}
saveRDS(list(agg = agg, zero = zero), file.path(verify_dir, "results", "exp06_coverage.rds"))
