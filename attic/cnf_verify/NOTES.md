# CNF verification campaign 2 (attic/cnf_verify)

Second verification campaign for `R/Cnf*.R`, independent from `attic/cnf/experiments`
(fresh oracle, fresh generators, new methods). Focus: algebraic soundness of
`simplify_cnf()` and the CnfFormula operators.

Environment note: this machine runs R 3.6.3. Mixed-class Ops dispatch
(`CnfAtom & CnfClause` etc.) does not work on R < 4.3 ("Incompatible methods"
warning, then error), because the `chooseOpsMethod` fix only exists on R >= 4.3.
DESCRIPTION declares R >= 3.3.0. Operator experiments here therefore call the
S3 methods directly (`` `&.CnfFormula`(a, b) ``), which is what R >= 4.3
dispatch resolves to.

## Files

- `harness.R` -- shared, independently written oracle (brute-force truth
  tables over all assignments), structural invariant checks on results,
  random/directed clause-set generators, parallel trial runner.
- `exp01_sanity.R` -- harness self-test (must detect fabricated wrong results),
  documented-example checks, status of the 4 known API bugs.
- `exp02_fuzz_baseline.R` -- baseline fuzzer, `gen_standard_trial` distribution.
- `exp03_mutation_kill.R` -- mutation testing: seed deliberate bugs into
  simplify_cnf, verify the exp02 fuzzer distribution catches them.
- `exp04_exhaustive.R` -- exhaustive enumeration of ALL k-clause formulas over a
  complete clause pool (parameterized universe).
- `results/` -- logs and failure RDS files.

All experiments are parameterized via env vars (CNF_TRIALS, CNF_CORES, CNF_SEED,
CNF_NVARS, CNF_DOMSIZE, CNF_MINK, CNF_MAXK, CNF_MUT_BUDGET, CNF_TIMEOUT) so they
can be scaled up on bigger machines.

## Findings so far

### Status of previously known bugs (attic/cnf/CLAUDE.md): all still present
1. `as.list()` on TRUE CnfClause errors (bad as.CnfAtom dispatch)
2. `u[["missing"]]` returns NULL silently
3. `CnfFormula(list(TRUE-clause-first, ...))` universe error
4. `CnfFormula(list(f1, f2, FALSE-formula))` crash in simplify_cnf

### Mutation testing (exp03): the verification approach has real teeth
17 mutants of simplify_cnf, fuzzer = exact exp02 distribution:
- All 11 soundness mutants (wrong unit merge, dropped contradiction check,
  wrong subsumption direction, SSE restriction target swapped, HLA complement
  wrong, HTE threshold eager in both HLA loops, 2nd-order disjointness check
  dropped, restriction by union, subsumption eliminating the wrong clause)
  are KILLED within 1-9 trials.
- Control mutants (HLA passes disabled, 2nd-order trigger disabled) SURVIVE
  30000 trials: elimination passes are redundancy-only, oracle has no false
  positives.
- M13 (unit-HLA donor count not adjusted): survives, analytically shown to be
  equivalent to disabling unit-HLA (count==1 becomes unreachable, the donor
  loop never starts).

### The unit-HLA phase relies on completed unit propagation (soundness-relevant!)
- M14 (unit propagation skipped for all matrix-built clauses): SEMANTIC failure
  at trial 1353. Isolation experiment: disabling the *unit-HLA phase* on top of
  M14 restores equivalence; disabling non-unit HLA does not. Mechanism: the
  unit-HLA donor count `lengths(entries[...]) - (clause %in%
  symbol_registry[[unitsymbol]])` silently assumes every registered clause's
  range on the unit symbol is inside the unit range. Missed propagation =>
  wrong donor eligibility => unsound hidden tautology/subsumption elimination.
- The real `use_inso` skip condition is exactly right: M18 (skip also when
  clause range == unit range, i.e. only subsumption elimination missed)
  survives 30k trials, consistent with analysis that subset-including-equality
  keeps unit-HLA sound; M19 (skip also on partial overlap, i.e. real
  restrictions missed) is KILLED (semantic, trial 5982).
- Invariant that keeps the real code sound: every clause in
  symbol_registry[[nu]] has clause[[nu]] contained in unit_domains[[nu]] at all
  times after registration; maintained at registration, at unit merges
  (restringent only shrinks, and old-matrix-based skips remain valid because
  earlier propagation already forced clause[[nu]] inside the previous unit
  range), and at clause creation (preprocessing propagates before registering).
- Kill latency for M19 (~6000 standard trials, ~3200 directed trials) shows
  this regime is rare under random generation => targeted invariant
  instrumentation (exp08) is the more sensitive tool here.

### exp04 exhaustive
- 3-var binary pool (26 clauses), all formulas with k <= 4 clauses: 0 failures
  (17,901 formulas). Larger runs pending.
