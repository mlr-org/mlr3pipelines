# Baseline fuzzer: random / redundancy-rich / unit-heavy / chain clause sets,
# constructed directly as bare clauses, fed to CnfFormula(), verified against
# a brute-force truth table plus structural invariants.
#
# Parameters (env vars):
#   CNF_TRIALS  (default 20000)  number of random formulas
#   CNF_CORES   (default 16)
#   CNF_SEED    (default 1)
#
# Run: Rscript attic/cnf_verify/exp02_fuzz_baseline.R

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

# configuration space, sampled per trial: number of symbols, domain sizes,
# generator flavor, clause counts. Kept small enough for exact truth tables.
# The generator itself lives in harness.R (gen_standard_trial), shared with
# the mutation-testing experiment exp03.
one_trial = function(i) {
  trial = gen_standard_trial(i)
  check_simplify(trial$clauses, trial$uinfo, extra_info = list(trial = i, gen = trial$gen))
}

n = n_trials(20000)
cat(sprintf("exp02 baseline fuzzer: %d trials, seed %d\n", n, base_seed()))
failures = run_trials(n, one_trial,
  results_file = file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "results", sprintf("exp02_failures_seed%d.rds", base_seed())))
if (length(failures)) quit(status = 1)
