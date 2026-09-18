# Independent observations and calibrated controls

The final campaigns use unchanged production source, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
Seed: 904117. R versions: 3.6.3 and 4.6.1. The public constructor checks use
real checkmate 2.3.4 on both. No production or package test files were edited.

## Domain normalization, per R version

| Observation | Count |
| --- | ---: |
| Input cases | 135 |
| New directed / saved fixture / new generated cases | 7 / 8 / 120 |
| Domain storage forms per case | 7 |
| Observed kernel calls | 945 |
| Exact ordered payload pairs | 810 |
| Exact source decision/event stream pairs | 810 |
| Exact selected-donor/virtual-support event pairs | 2,910 |
| Recorded source events | 830,137 |
| Actual clause-payload writes | 4,081 |
| Fresh live pair-row/count checks at HLA entry | 10,836 |
| Fresh physical unit-containment checks | 462 |
| Nonunit HLA selected-donor/capacity checks | 3,052 |
| Unit HLA selected-donor/capacity checks | 343 |
| Extensions with repeated virtual values | 2,245 |
| Extensions with named virtual values | 970 |
| Independent input/output truth-table rows | 9,355 |

Forms are canonical, reversed unique, repeated, named repeated (including empty,
NA and repeated names), matrix, array, and inert metadata/class storage. The
same actual input ranges are used under every form. The eight saved inputs
include the nested deferred-unit fixture and seven scheduling-gap examples;
only their concrete inputs are imported.

The independent AST catalog contains all 108 explicit if sites, 23 for-sequence
sites, and both operands at all 34 scalar short-circuit sites (8 `&&`, 26 `||`).
Parser token counts independently confirm those totals. Dynamic events also
record all 13 nested helper entries and every existing-clause-payload write.
The HLA observer independently reconstructs physical donor exceptions and
integer value multiplicities at every selected extension. It never assumes
virtual uniqueness or full occupancy of a present value's domain copies.

## Component separability, per R version

| Observation | Count |
| --- | ---: |
| Combined input cases | 2,433 |
| Exhaustive small interleavings | 1,993 |
| Directed fixture insertion cases | 200 |
| Generated two-/three-group cases | 240 |
| Observed whole calls over repeated passes | 4,635 |
| Observed isolated calls over repeated passes | 9,475 |
| Exact ordered component-output comparisons | 9,274 |
| Exact local helper/write/HLA history comparisons | 9,274 |
| Local history prefix comparisons before global FALSE | 201 |
| Recognized FALSE cases | 88 |
| Non-FALSE terminal fixed points | 2,345 |
| Maximum productive-pass count identities | 2,345 |
| Non-FALSE cases with more than one productive pass | 99 |
| Independently checked local truth-table rows | 69,091 |
| Recorded events, whole and isolated calls | 3,902,765 |
| Actual clause-payload writes | 11,263 |
| Fresh HLA-entry live pair-row/count checks | 60,446 |
| Fresh cross-group HLA-entry row/width checks | 21,008 |
| Fresh physical unit-containment checks | 1,128 |
| Nonunit / unit HLA donor/capacity checks | 10,704 / 869 |
| Extra cross-group helper entries checked | 27,807 |
| Same-group helper entries with projected metadata bounds | 164,520 |

The exhaustive family consists of every ordered list of zero, one or two
full-width clauses on two Boolean symbols, with duplicates allowed. There are
21 possible components. Every pair is tested under all order-preserving
interleavings, including empty components. The directed family moves four
different challengers through every insertion boundary of the eight fixtures:
a unit, an inert pair of binary clauses, a unit-HLA example, and contradictory
units. The generated cases use independent symbol vocabularies with shared
value labels, variable clause widths, and both planted and unplanted formulas.

The local history observer translates clause indices to group/local ghost
identities and compares only helper invocations whose clause operands all
belong to the group, plus actual writes and selected HLA events. It projects
the current global initialization bound to the number of initialized local
indices, and includes that value in pre-HLA helper events. Cross-group callbacks
are counted separately and checked to involve a live nonunit source with an
entirely absent target vocabulary. Fresh HLA-entry cross rows are also checked
to contain exactly one TRUE per physically present source symbol.

No observed output, local scheduling, FALSE-recognition, repeated-pass, row,
containment, selected-donor or capacity mismatch occurred. The test deliberately
does not demand equality of the complete global callback streams. The FALSE
prefix check also allows a global early return to leave another group unfinished.

These are observed-call counts. Every observed call additionally had an
uninstrumented production replay, checked by `identical()` on its output.
Those replays, plain terminal fixed-point probes, controls and development
replays are excluded from the table counts. Truth-table checks cover every
normalization input and each non-FALSE component input/final output; FALSE
recognition is compared between whole and separate production executions.

## Independent local storage algebra, per R version

| Observation | Count |
| --- | ---: |
| Positive multiplicity vectors, support sizes 1-3 and entries 1-2 | 14 |
| Bounded old/donor states with a selected-donor exception | 526 |
| First-extension physical storage checks | 2,104 |
| First extensions with repeated / named results | 1,268 / 512 |
| Selected second-extension checks on the resulting virtual storage | 8,984 |

`local_algebra.R` predicts exact multiplicities from integer count vectors and
then evaluates the actual base-R filtering/concatenation expression. Plain and
named shapes retain the base multiplicity vector; matrix and array shapes use
a doubled domain sequence to guarantee a nontrivial dimension. Their expected
capacities are doubled accordingly. The enumeration is exhaustive over the
stated base old/donor states, not over every bounded old vector for those doubled
matrix capacities. Second steps consume the possibly repeated first result.

## Public boundaries and unused universe symbols, per R version

| Observation | Count |
| --- | ---: |
| Publicly constructed domain objects | 28 |
| Exact constructor clause-payload checks | 462 |
| Empty / full atom classifications | 28 / 28 |
| Exact payload comparisons with 40 added unused symbols | 80 |
| Exact event-stream comparisons with those unused symbols | 80 |

Constructor inputs include plain, named repeated, and matrix literal-value
storage. CnfClause construction is checked to produce the expected flat,
unnamed, unique actual range in the intended order. The check does not use the
known defective clause-selector paths. The unused-universe cases each run
both observed and plain production calls on both sides; these calls are
separate from the main tables.

## Controls

1. An in-memory `char_setdiff` mutation adds an output-preserving explicit branch
   whose condition detects repeated storage. Canonical/named outputs stay exact,
   while the paired source event streams differ. An output-only test would miss
   this deliberate failure of source-decision equivalence.
2. An in-memory complement-to-intersection mutation is rejected at the independent
   HLA check: `!any(missing %in% new) is not TRUE`. The oracle directly catches
   introduction of a supposedly missing selected-donor value.
3. An in-memory initial-sort mutation reverses equal-width ties only when unrelated
   clauses raise the total count above two. The exact projected-order comparator
   rejects it while all 81 independent whole-formula truth rows still agree.
   This calibrates the order-sensitive claim separately from logical equivalence.
4. The independent truth evaluator detects deletion of a necessary singleton
   clause. It is not an oracle that reports all formulas equivalent.
5. `S=(a,a,b), old=a, donor=empty` produces full support with unequal lengths.
   `S=(a,b), old=(a,a), donor=b` produces equal lengths while missing b. These
   refute respectively the unused full-occupancy strengthening and the missing-
   value argument without a multiplicity-capacity bound.

## Cross-version audit and development corrections

Final cross-version comparison passed and is recorded in `cross_version.json`.
Direct saved record comparison is used, including all 135 baseline normalization input/
payload/event/HLA records and all 2,433 component repeated output/local-history
records. Counters, controls, source hashes, source-site counts and the separate
boundary/algebra results are compared too.

A first attempt to compare binary-serialization SHA-256 digests across versions
was rejected even though the two-clause probe records were `identical()` after
loading into one R process. Serializing those identical probe objects under
native R 3.6.3 can produce different byte streams; their saved RDS files preserve
the reproduction. The final campaigns were replayed to save direct records, so
the final cross-version claim does not depend on canonical binary serialization.
Within each version, paired event streams were always compared directly.

Earlier development-only corrections were: stripping names from the outer
`do.call(c, ...)` argument list so component domain names are not prefixed by
group-list names; treating both NULL and list() as the empty observer stream for
an empty/TRUE component; and converting named counters/table objects for JSON
export. These were observer/reporting issues, not production counterexamples.
The zero-generated-input pilot passed its substantive checks before its final
JSON export encountered the table-class issue. Pilot results are excluded from
the final recorded counts.
