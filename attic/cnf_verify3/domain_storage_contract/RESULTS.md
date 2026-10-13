# Recorded observations and calibrated controls

Both complete campaigns passed, using unchanged kernel SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
The R versions were 3.6.3 and 4.6.1; both used real checkmate 2.3.4.

## Exact counts per R version

| Observation | R 3.6.3 | R 4.6.1 |
| --- | ---: | ---: |
| Input cases | 264 | 264 |
| Previous concrete branch witnesses | 57 | 57 |
| New directed cases | 7 | 7 |
| New generated cases, seed 860601 | 200 | 200 |
| Domain storage forms per case | 11 | 11 |
| Complete observed kernel runs | 2,904 | 2,904 |
| Exact paired full event-stream/payload comparisons | 2,640 | 2,640 |
| Recorded source events | 10,128,063 | 10,128,063 |
| Exact actual-range-write events | 30,635 | 30,635 |
| Fresh HLA-entry live pair-row/count checks | 56,958 | 56,958 |
| Fresh physical unit-containment checks | 3,861 | 3,861 |
| Nonunit HLA selected-donor/multiplicity checks | 15,059 | 15,059 |
| Unit HLA selected-donor/multiplicity checks | 2,640 | 2,640 |
| Extensions yielding repeated virtual values | 9,124 | 9,124 |
| Extensions yielding named virtual values | 8,045 | 8,045 |
| Paired virtual-support and selected-pivot comparisons | 16,090 | 16,090 |
| Independently evaluated input/output assignment rows | 172,320 | 172,320 |
| Feasible explicit-if outcomes observed | 212 | 212 |

No output mismatch, event-stream mismatch, selected-donor violation,
multiplicity violation, semantic mismatch, or constructor-payload
normalization failure occurred in these recorded runs.

The 296-site observer catalog includes 108 `if` sites, 23 `for` sequence
sites and their 23 iteration sites, two repeat sites, 13 nested helper
entries, four actual-entry-write sites, 34 pairs of scalar short-circuit
operand sites, 53 additional decision sites, and two HLA hooks. Parser
token counts independently agree for the conditions, loops, and
short-circuit operators. A helper-entry event is also emitted for the
small character set functions, not only the recursive clause handlers.

All 212 feasible explicit-if outcomes from the canonical branch review
were reproduced; the four unreachable TRUE outcomes remain absent.
This is outcome coverage, not every path or every recursion depth. The
local observer additionally checks actual source schedules pairwise,
including their order, rather than only comparing these outcome sets.

## Base-R storage checks

On **each** R version, exhaustive direct primitive checks passed:

| Check | Count |
| --- | ---: |
| Selected extensions under six storage shapes | 15,354 |
| Extensions with repeated new values | 11,088 |
| Extensions with named new values | 10,080 |
| Empty-atom classifications | 234 |
| Full-domain atom classifications | 234 |
| Proper constructor clause payloads | 216 |

These enumerate domains of one to three distinct values, every domain
multiplicity vector with entries one to three, every bounded old
submultiset, and every donor subset having a missing old value. Expected
new multiplicities come from integer count vectors; the actual result is
computed by the source's base-R filter/concatenation expression. The test
does not use the simplifier's caches or helper functions for its oracle.

The independent Python local lemma enumeration checked:

| Distinct domain values | Domain multiplicity vectors | Selected extensions |
| --- | ---: | ---: |
| 1 | 3 | 3 |
| 2 | 9 | 99 |
| 3 | 27 | 2,457 |
| 4 | 81 | 54,351 |
| 5 | 243 | 1,130,193 |
| **Total** | **363** | **1,187,103** |

The proof covers arbitrary finite positive multiplicities and arbitrary
finite support sizes under its execution contract. These finite counts
are independent observations of the lemma, not a finite-size justification
for the unbounded theorem.

## Controls

1. Adding a harmless explicit `if (FALSE)` to the actual `char_setdiff`
   source helper preserved the exact output and changed the event stream.
   The added branch itself was observed, ruling out a false pass caused
   solely by static site renumbering. This passed on both versions.
2. Changing the actual `char_setdiff` source helper from domain difference
   to domain intersection was rejected by the independent HLA check:
   `!any(missing %in% new) is not TRUE`. This passed on both versions.
3. Domain `c("a","a","b")`, old `"a"`, empty donor returns
   `c("a","b")` under the extension formula: full support, unequal
   lengths. The control refutes the unused strengthening without the
   donor-exception premise.
4. Domain `c("a","b")`, old `c("a","a")`, donor `"b"` retains equal
   lengths while missing `"b"`. The independent Python control refutes
   the unused strengthening without the multiplicity capacity bound.
5. The directed public two-clause example in `PROOF.md` yields named
   `c("b","a","a")` and `c("a","c","c","c","c")` virtual
   ranges. This falsifies virtual uniqueness and full-fiber occupancy
   while preserving exact actual output and scheduling.

## Cross-version check

`compare_versions.R` found exact `identical()` equality for all 264 saved
case/output/event-count/trace-digest records, all 14 first virtual-witness
records, input cases, if-outcome matrices, and run counts. Primitive
counts also agree. The complete within-version paired event streams were
compared directly with `identical()` during their runs; cross-version
trace comparison uses the saved SHA-256 digests.

Artifacts are linked from `README.md`. The failed development pilots are
documented there and are excluded from all recorded counts. No production
source was edited, and no commit was made by this audit.
