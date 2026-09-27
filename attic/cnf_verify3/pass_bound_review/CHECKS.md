# Checks supporting the clause-only bound

The argument is in `PROOF.md`. These checks were performed 2026-09-06 with
the actual installed development package under R 4.6.1, version `0.11.0.9000`,
and the unchanged production source whose provenance is recorded there.
No production binding or file was changed.

## Exact small signature counts

`count_signatures.py` enumerates every reflexive binary relation on one
through four labeled elements, tests transitivity directly, and constructs
its proposed finite-set realization using principal lower sets plus one
unused domain value. It then recomputes every inclusion comparison from
those sets. All 389 resulting preorder realizations passed.

| Original clauses m | p(m) | Possible support/inclusion signatures S(m) | Sharper universal bound | Coarse universal bound |
| ---: | ---: | ---: | ---: | ---: |
| 1 | 1 | 1 | 3 | 5 |
| 2 | 4 | 6 | 38 | 130 |
| 3 | 29 | 44 | 801 | 12,291 |
| 4 | 355 | 499 | 25,708 | 4,194,308 |

These are upper bounds, not measured or attainable worst-case pass counts.
The sharper column uses
`m + sum_k choose(m,k) * p(k) * k * 2^k`.
Results are in `signature_counts.json` and `signature_counts.log`.

## Actual-write instrumentation

`check_frozen.R` uses a private source-function copy with a check inserted
after each of the four actual `entries` assignment sites: initial sorting,
old-unit intersection, nonempty range restriction, and whole-literal deletion.
The source contains the same assignment text for the latter two sites;
the script asserts the exact one/one/two match counts before adding checks.
It does not instrument virtual HLA storage as an actual value change.

Each original clause carries a private ghost-ID attribute through the
research copy. After every intercepted write the checker requires:

- Every symbol in an initial signature class of size at least three has
  exactly its original range in that ghost clause, including presence.
- Every actual current range is a subset of its original ghost range.
- Every original membership fiber is either wholly present or wholly absent.

Each ordinary installed-package call runs alongside the instrumented one.
After removing only the private ghost attributes, their stored outputs must
be identical, including clause, symbol, and value order. At every productive
pass the proposed `number of live clauses + small-class fiber occurrences`
potential must strictly decrease. The run continues to a nonproductive call.

There were 263 cases, 444 paired public/instrumented calls, 645 assignment-site
checks (including initial sorts), 181 productive passes, and 1,714 frozen
symbol instances across cases. Every required property held. For every case
whose complete domain product had at most 4,096 rows, a separate positional
OR/scalar-membership truth table checked each output, totaling 113,594 output
assignment-row comparisons. Larger products were checked for the frozen/fiber
invariants and identical installed output, not exhaustively truth-enumerated.

The cases include:

- Three disjoint-singleton clauses repeated over 2, 3, 4, 8, 32, and 100
  distinct symbols. Two symbols permit a productive refutation; at least
  three make the signature class frozen and the output unchanged.
- Three symbols with the same support/inclusion signature but different
  membership profiles and domain sizes. All three remain frozen. This
  checks that the signature criterion is weaker than full profile equality.
- The independently reconstructed seven-clause reverse scheduling family
  with 1, 2, 3, 4, 8, or 16 distinct guard symbols. Every case still takes
  exactly three productive passes. With at least three guard copies they
  are frozen, and the potential is always `24 -> 23 -> 22 -> 21 -> 21`,
  independent of their number.
- 250 seeded random formulas with 3–6 clauses. One or two chosen symbols are
  copied into at least three same-signature symbols, using nonuniform fresh
  value refinements; clause/symbol order is randomized. All complete runs
  satisfy the first-change exclusion while other symbols may simplify.

The ghost attributes are research metadata only. Their erasure is the sole
difference permitted in the paired source comparison. The checker reuses
this reviewer's independent scalar helpers from `structural_review`, without
running that earlier campaign or importing the author's oracle/predicates.
Records are in `frozen_checks.json`, `frozen_checks.rds`, and
`frozen_checks.log`.

## A proper nonempty shrink at the threshold of two

The root suggested the stronger threshold control with domain `{a,b,c,d}`
and ranges

```
A={a,b}, B={b,c}, C={b,d}.
```

Make three clauses, with the first selecting A on each of k symbols, the
second selecting B, and the third selecting C. Every symbol has the same
support/inclusion signature. `threshold_overlap.R` independently verifies:

- For `k=2`, the formula is equivalent to `s1=b OR s2=b`: it has 7 of 16
  models, and the installed source returns exactly that clause. There is
  a proper nonempty range shrink, followed by clause deletion; the potential
  decreases `15 -> 3 -> 3` in one productive pass.
- For `k=3`, all original ranges remain, there are 43 of 64 models, and no
  productive pass occurs.
- For `k=4`, all original ranges remain, there are 211 of 256 models, and
  no productive pass occurs.

These three cases add four paired calls, seven assignment-site checks, and
352 complete output truth-row comparisons. The threshold of three in the
first-change lemma cannot simply be replaced by two. The examples also show
why frozen symbol groups must not be replaced by two symbols as an alleged
behavior-preserving quotient.

Results are in `threshold_overlap.json`, `.rds`, and `.log`. Altogether the
two R studies have 266 cases, 448 paired calls, 652 assignment-site checks,
and 113,946 checked output truth rows.

Reproduce from the repository root:

```
python3 attic/cnf_verify3/pass_bound_review/count_signatures.py
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/pass_bound_review/check_frozen.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/pass_bound_review/threshold_overlap.R
```

These checks challenge the proof and calibrate its threshold; the finite
signature classification and first-change argument establish the arbitrary
clause/symbol/domain conclusion.
