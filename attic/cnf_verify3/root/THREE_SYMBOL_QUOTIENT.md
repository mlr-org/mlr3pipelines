# Complete three-clause (3,3,2)-occurrence quotient

The root's next finite proof experiment covers three proper clauses over
three symbols, where two symbols occur in all three clauses and the third
occurs in exactly two. Every literal range is nonempty and proper in a fixed
nonempty finite domain. This is a new shape beyond campaign 3's complete
two-symbol / three-clause quotient.

After naming the symbols X,Y,Z, the clauses have shapes

```
C0 = X in A0 OR Y in B0
C1 = X in A1 OR Y in B1 OR Z in D1
C2 = X in A2 OR Y in B2 OR Z in D2.
```

Every assignment to a value of X determines its three membership bits in
A0,A1,A2. A domain is represented by the nonempty subset of the eight patterns
that actually occur. Nonempty proper ranges require that each coordinate
takes both values somewhere in that subset. Exactly 193 pattern subsets meet
that requirement. The same count applies to Y. For Z there are four two-bit
patterns and seven subsets in which both coordinates vary.

All operations in the simplifier preserve whole membership classes. The
reviewed refinement/renaming proof therefore lets each present pattern have
one representative value, irrespective of its actual nonzero multiplicity
or order in a concrete domain. The largest representative truth table has
8 x 8 x 4 = 256 assignments.

## Scheduling coverage

C0 is the unique shortest clause, so production's initial length sort puts it
first regardless of its original position. Either relative order of C1,C2 is
covered by the exhaustive membership-coordinate profiles. Rename X,Y, if
necessary, so that C0's symbol order is X,Y. Symbol renaming is harmless here:
the kernel uses names for exact lookup and membership, never their lexical
order; the enumeration of unit-domain names is only used for membership in a
clause-ordered intersection. Each remaining clause has all six symbol orders,
giving 36 combined orders. Fixing C0's order does not assume symmetry of a
particular output: exchanging X,Y also exchanges their exhaustively enumerated
profiles and the corresponding orders of C1,C2.

Thus the complete execution count is

```
193 * 193 * 7 * 6 * 6 = 9,386,748.
```

Full-domain literals would make a clause TRUE and reduce to the already
analyzed at-most-two-clause case; they are outside the explicit proper-clause
shape here. Missing additional literals produce other occurrence shapes,
which this experiment does not silently claim to enumerate. Repeated clauses
are allowed whenever their ranges match. This is a precise family, not a
universal three-symbol theorem inferred from sampling.

## Independent evaluator and calibration

`three_symbol_quotient.py` reuses only the production JSON bridge and the two
independent solvers for additional calibration. Its generator and primary
evaluator are new. Each symbol/value membership is encoded as a Python integer
whose bits identify the satisfying assignments. Disjunction and conjunction
use bitwise OR and AND. Every output is compared on every assignment; this is
an exact truth table, not a sample of valuations.

Before the full run, `three_symbol_truth_checks.py` compared this evaluator
against scalar direct evaluation on 2,000 independently generated formulas.
For each of 1,808 satisfiable cases it also removed exactly one known model by
adding its blocking clause and verified the resulting one-bit discrepancy.
SAT and MDD independently checked a subset of these deliberately unequal
pairs. During enumeration they calibrate a case every 4,093 executions per
worker, including an independent check of whether the input is contradictory.

The 2,016-execution pilot covered 60,480 assignments with zero discrepancies.
The full run uses 16 processes, preserving all input source hashes and saving
progress after each X profile. A truth-vector difference or reported R error
is recorded with exact inputs before the run stops. An unexpected Python
evaluator exception has no equivalent saved-fixture handler; none occurred.
Known residual unit-subsumption outputs are
counted separately and never mislabeled as truth-table discrepancies.

Commands from repository root:

```
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/root/three_symbol_truth_checks.py
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/root/three_symbol_quotient.py --workers 16
```

## Completed result and independent review

The full run completed at **2026-09-06 13:29:03 UTC**, after 2,386.43 seconds.
The result file contains all 193 X-profile indices exactly once, 9,386,748
executions, and **547,476,480 complete assignment comparisons**, with zero
R errors, Python exceptions, truth-vector differences or counted unit
subsumption leftovers. SAT and MDD additionally calibrated 2,316 source
executions. The expected valuation total independently factors as
`872 * 872 * 20 * 36 = 547,476,480`, using the sums of profile domain sizes.

The independent [quotient review](../quotient_review/QUOTIENT_REVIEW.md)
checks the source coupling and all ordering reductions. It also closes a
shared-assumption gap in the original scalar calibration: the review builds
102 assignment grids by independent mixed-radix enumeration, tests 9,769
one-point formulas covering every bit, and compares 1,020 complete model sets.
All 432 syntactic order arrangements of an asymmetric representative map to
enumerated cells, and 300 independently generated production refinement/
renaming pairs agree. These checks supplement the symbolic lifting argument.

Every proper input of this shape is satisfiable: choose X from C0's X range,
Y from C1's Y range, and Z from C2's Z range. These choices are independent
and satisfy all three clauses. Thus the complete experiment excludes model
changes in this shape but provides no new coverage of contradictory inputs.
Its zero unit-subsumption statistic is also not a general saturation check.
The completed result and exact timestamp are preserved in
`three_symbol_quotient_results.json` and `three_symbol_quotient.log`.
