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

### exp04 exhaustive: complete small spaces are clean
- 3-var binary pool (26 clauses), ALL formulas with k <= 8 clauses:
  2,533,986 formulas, 0 failures.
- 2-var domain-3 pool (48 clauses), ALL k <= 4: 213,052 formulas, 0 failures.
- (further spaces: 2v-d3 k=5, 2v-d4 k<=3, 3v-d3 k<=3 -- see results/)

### exp08 invariant-instrumented fuzzing: internal bookkeeping is consistent
Hook calls injected at consistent phase boundaries (post_preprocess, pre_hla,
final) assert: I1 unit registry consistency, I2 symbol registry exactness,
I3 propagation completeness (every registered clause range strictly inside
unit ranges -- the unit-HLA load-bearing invariant), I4 clause shape,
I5 is_not_subset_of/not_subset_count consistency with entries (pre_hla).
Detector power validated on mutants: catches M19 at trial 123 (semantic
oracle needed 5982) and M14 at trial 6 (semantic: 1353) -- roughly 50-200x
more sensitive for propagation bugs. Result: 150,000 trials (standard +
directed unit-merge generators), 0 violations.

### exp05 operator fuzz: 30,000 trials clean
Random expression trees (&, |, ! via direct S3 method calls), logical
identities (De Morgan, double negation, absorption, distributivity,
complement), constructor round-trips (CnfClause(atoms), CnfClause(mixed),
CnfFormula(formulas), as.list round-trip, clause subsetting): 0 failures.

### exp06 coverage + exp09 reachability: HTE outcome is (almost certainly) dead code
covr line coverage of CnfFormula_simplify.R under the fuzz workloads: 98.3%.
The unexecuted lines are the *hidden tautology elimination* outcomes in BOTH
HLA loops (non-unit: lines 691-693; unit: 752-753) plus rare defensive
rechecks. Reachability probes over 200k fuzz trials + 83,681 exhaustive
formulas: HTE fired 0 times, while hidden *subsumption* elimination fired
thousands of times, and the defensive rechecks (P3/P5/P6/P7) did fire rarely
(2/17/9/5 times) -- so they are reachable and correct, not dead.

Analytic argument for HTE-unreachability: a donor is selected only while its
exceptional symbol m satisfies donor[m] not-subset-of C[m] (matrix rows are
kept accurate w.r.t. the virtually extended clause during the HLA loop, flips
are paired with count decrements, and a count that reaches 0 triggers hidden
subsumption elimination immediately). Then
range_new = C[m] u (dom \ (C[m] u donor[m])) misses donor[m] \ C[m], which is
nonempty -- so range_new can never equal the full domain. Even the documented
"hidden tautology elimination" example in ?CnfFormula is actually eliminated
through the HSE path (count reaching 0), not the HTE branch. The HTE branches
are harmless belt-and-suspenders, but they appear to be unreachable given
consistent bookkeeping. (Degenerate exception: duplicated values in a symbol
domain -- which CnfSymbol currently accepts -- could theoretically distort the
length comparison, another reason to reject duplicated domains at
construction.)

### exp07 scale testing via independent DPLL implication checker
Multivalued DPLL (integer-encoded, unit propagation + branching), validated
against truth tables on 800 small instances including deliberately perturbed
results. Verifies F |= G and G |= F clause-wise for formulas with 8-20
symbols, domains 2-8, 10-80 clauses -- far beyond truth-table reach.
Results: 5,000 trials clean; big-formula batch (up to 30 symbols, up to 150
clauses): 2,000 trials clean, no DPLL budget exhaustion.

### exp10 negation / OR-distribution at scale (DPLL-checked): clean
For formulas with 5-12 symbols: f & !f UNSAT, f | !f tautology, !!f
equivalent to f, and f | g equivalent to an independently implemented naive
cross-product distribution: 4,200 trials, 0 failures.

### exp11 misc directed checks: clean
Permutation invariance (semantic), large domains (10-30 values), OR operand
symmetry, duplicate-clause stress, all.equal soundness (TRUE implies equal
truth tables; reordered representations compare TRUE): 25,000 trials clean.
Constructor universe-inference asymmetry documented: CnfClause() errors when a
FALSE atom precedes real atoms, CnfFormula() errors when a TRUE clause
precedes real clauses (each constructor trips over its *neutral* element,
whose universe is NULL, while the absorbing element short-circuits safely) --
this is the known bug-#3/#4 family.

### exp12 mixed-class Ops dispatch on R >= 4.3: correct
This machine runs R 3.6, where mixed-class Ops (atom & clause etc.) error with
"Incompatible methods" -- the chooseOpsMethod fix only registers on R >= 4.3
(relevant given DESCRIPTION declares R >= 3.3.0, though the CNF tooling is
internal). Verified in an r-base 4.6.1 container (podman): all 67 ordered
mixed-type &, |, ! combinations produce correct truth tables with no warnings.

### testthat additions
tests/testthat/test_CnfFormula_simplify.R gained directed regression cases
(unit merge chains, use_inso shapes incl. the exact M14/M19 mutant-killer
formulas, unit-HLA donors, 2nd-order SSE, cascading contradiction) and a
150-trial seeded truth-table property test (~2s runtime).
