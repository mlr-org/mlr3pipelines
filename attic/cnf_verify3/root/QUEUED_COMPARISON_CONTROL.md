# A necessary guard against counting a completed comparison twice

Checked 2026-09-06 against the unchanged production source. This is a positive
regression control for an existing correct guard, **not a newly discovered
production defect**. The deliberately changed function exists only in a
private evaluation environment.

## Why this guard matters

`apply_domain_restriction()` snapshots `rows_to_check` before visiting those
rows. Processing one row can recursively restrict the same source clause and
finish a later comparison from that snapshot. On returning to the older loop,
raw containment alone no longer establishes that its cached bit is still TRUE.
The guard at `R/CnfFormula_simplify.R:218` reads the bit again immediately before
clearing it and decrementing the associated count.

Removing that one guard makes the reduced control fail. At the relevant visit,
the sorted clause indices are donor 11, target 10, and symbol `X2`. Its cached
bit is already FALSE, while the row and stored count both correctly have two
TRUE entries. Production skips the repeated transition. The changed copy
clears the same FALSE bit again and reduces the count to one, leaving two TRUE
entries in the row. The first-order handler then receives a count-one pair
whose mask identifies two symbols.

The trace records nested calls through both second-order handlers and two
`apply_domain_restriction()` frames. It confirms a real reentrant completion,
rather than merely a branch with a theoretically possible stale snapshot.
There is no callback between the final bit check and its decrement; with this
guard intact, that particular count inconsistency is excluded.

## Reduction and independent expected result

The first saved witness for the guard branch (`random:169`) did **not** change
truth when the guard was omitted. This was a failed initial hypothesis about
that particular input, and is retained in the search log. Scanning the saved
first witnesses then found `random:1501`. Greedy removal of clauses, symbol
occurrences, literal values and domain values took 3,967 candidate attempts
and left 11 clauses, four symbols and 360 assignments. This is a local greedy
reduction, not a minimum-size proof.

The reduced input and both outputs are in `queued_comparison_control.json` and
the corresponding RDS. Its only models are:

| X3 | X1 | X4 | X2 |
| --- | --- | --- | --- |
| v3 | v5 | v2 | v4 |
| v6 | v6 | v4 | v4 |

An independent complete valuation calculation checks these models. Production
returns four equivalent clauses. Both the raw production function and its
observation-only copy preserve all 360 truth values and have identical output.

## Runtime and test results

| Runtime | Production and observed production | Guard omitted |
| --- | --- | --- |
| R 3.6.3 | Correct, two models | `subscript out of bounds`, after length-condition warnings |
| R 4.6.1 | Correct, two models | `the condition has length > 1` |

The observed changed copies record the same first incorrect count on both
runtimes. Different downstream error messages reflect R's treatment of a
non-scalar condition; they do not change the source cause.

The new test `simplification tolerates comparisons completed by nested
callbacks` in `tests/testthat/test_CnfFormula_simplify.R` constructs this input
through ordinary public objects and checks every assignment against the two
explicit models. `devtools::test(filter="^CnfFormula_simplify$",
stop_on_failure=TRUE)` completed in 122.2 seconds on R 4.6.1 with **1,525 passes,
zero failures, warnings or skips**. It supplements the earlier successful
3,815-expectation focused CNF suite; the rest of that suite was not rerun after
this isolated additional test.

The replay initially compared formula attributes from different freshly
created universes and correctly failed `identical()`. Reusing the same fixed
universe for the four variants fixed the harness comparison. This was not a
production discrepancy. Formula identity includes its universe environment.

## Reproduction

The full source-copy search and reduction is `queued_comparison_control.R`.
The short `queued_comparison_replay.R` reads the reduced RDS and reproduces
the observed count transition without repeating the search:

```sh
Rscript attic/cnf_verify3/root/queued_comparison_replay.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/root/queued_comparison_replay.R
```

The `_r36.json`, `_r46.json` and corresponding logs contain the exact output,
warnings and call-head trace. Production files and namespace bindings remain
unchanged.
