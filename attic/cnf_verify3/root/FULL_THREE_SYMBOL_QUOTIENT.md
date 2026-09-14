# Complete quotient for three clauses containing all three symbols

Completed **2026-09-06 15:50:08 UTC** against the unchanged production kernel.
The run began at 15:07:32 UTC and took 2,555.46 seconds. Its independent
completion validator passed after the final result had been written.

## Exact scope and result

The input consists of exactly three ordered proper clauses. Each contains
all of X, Y, Z, in that within-clause order. Domains are arbitrary nonempty
finite ordinary character sets; each literal range is nonempty and proper.
There are no duplicate symbol occurrences or external universe changes.

Within this scope, the first simplification result preserves the truth table
and admits no further productive simplification pass. This is idempotence
**modulo clause order**, not necessarily exact ordered-object idempotence.
The reviewed nonproductive-call saturation theorem additionally makes its
stored clauses locally saturated under the rules listed there. A following
sort-only call can still change their order.

This result does not cover arbitrary independent within-clause symbol orders,
missing occurrences, extra clauses/symbols, or malformed selector results.
It is separate from the earlier `(3,3,2)`-occurrence quotient. Every proper
input here is satisfiable: assign X to satisfy clause one, Y clause two,
and Z clause three. This is therefore no test of contradiction recognition.

## Why a finite quotient covers arbitrary domains

A value at one symbol has three membership bits, one for each input clause.
Retain one representative for each realized bit pattern, including 000 when
present. Proper nonempty ranges require seeing both bits at each coordinate.
There are exactly 193 possible realized-pattern sets. Independent counts by
quotient domain size are `{2:4, 3:32, 4:64, 5:56, 6:28, 7:8, 8:1}`.
Thus the full product has `193^3 = 7,189,057` representatives.

The independently reviewed value-set simulation in `../set_symmetry/PROOF.md`
couples all actual and virtual ranges, decisions, callback schedules and
later passes under arbitrary positive unequal splitting of those fibers.
It also permits arbitrary value order inside the ranges and domains.
Productivity is preserved because every actual change removes a whole
initial fiber occurrence. The quotient therefore proves the same finite
pass classification for arbitrary concrete finite domain cardinalities,
within its fixed occurrence/order scope and ordinary-operation premises.

## Completed counts and independent calibration

| Quantity | Count |
| --- | ---: |
| Completed outer profiles | 193 of 193 |
| Input representatives | 7,189,057 |
| Inputs with zero productive calls | 4,646,328 |
| Inputs with one productive call | 2,542,729 |
| Inputs with two or more productive calls | 0 |
| Total production calls | 9,731,786 |
| Initial input valuation rows | 663,054,848 |
| Valuation rows across all checked calls | 849,165,820 |
| Independent SAT/MDD sample calibrations | 1,930 |
| Errors or semantic differences | 0 |

Each input is run repeatedly until a call preserves actual value-occurrence
weight. The program checks that equal weight also preserves the complete
clause multiset, retaining multiplicity. Each pass, including the last
nonproductive one, is compared with the original truth mask. Hence total
calls equal `zero_count + 2*one_count`. No subsequent call is silently
omitted from the semantic count.

The bitmask evaluator is distinct from the R code. Before the run, it was
calibrated against the older Cartesian truth-table evaluator for all 343
domain-size triples from two through eight. The independent reviewer also
generated assignment lists directly and checked 512 size triples from one
through eight, 6,912 literal masks, 10,240 formula masks and 1,531 decoded
witnesses. The SAT and decision-diagram checks sample both first and final
results throughout every outer profile; they are not claimed on every input.

`../symmetry_review/REVIEW.md` reviews the source simulation, profile space,
bit indexing, stopping rule and precise interpretation. Its separate final
validator checks all expected profile indices, input/execution/pass counts,
calibration totals, valuation bounds, source hashes and actual completion.
The completed audit is `../symmetry_review/full_report_audit.json`.

## A sort-only second call really occurs

Use X domain `{a,b}` and Y/Z domains `{a,b,c}`, with aligned X,Y,Z clauses:

```
C1 = (X=a OR Y=a OR Z=a)
C2 = (X=b OR Y in {a,b} OR Z in {a,b})
C3 = C2
```

The first result has widths `[3,2]`; the second has widths `[2,3]`; the third
equals the second exactly. Their weights are `13 -> 7 -> 7 -> 7`, so this
has one productive call. The independent current/old-R controls are in
`../symmetry_review/check_order_only_pass.R`. The result above is deliberately
not phrased as unconditional `identical(S(S(F)), S(F))`.

## Reproduction

```sh
attic/cnf_verify3/independent_solver/.venv/bin/python \
  attic/cnf_verify3/root/full_three_symbol_quotient.py --workers 16
python3 attic/cnf_verify3/symmetry_review/validate_full_report.py --require-complete
```

`full_three_symbol_results.json` is the full report and
`full_three_symbol.log` records every outer-profile completion. The separate
pilot files are small preliminary checks and are not added to the completed
counts. No production source or existing test-suite behavior was changed.
