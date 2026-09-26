# Independent symmetry and full-three-symbol quotient review

Reviewed 2026-09-06 against `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
The production source and the active exhaustive run were not modified.

## Verdict

The canonical full-preimage simulation in `../set_symmetry/PROOF.md` is
sound under its stated representation, equality, immutability, and resource
contract. This includes nonuniform positive fiber sizes, injective value
renaming, arbitrary independent ordering of domain/range values, maximal
membership-cell collapse, and subsequent simplifier passes. The separate
positional duplicate-occurrence extension also survives the source audit;
it must retain its express disclaimer of semantic correctness.

The full-three-symbol enumerator has the stated scope and correct truth
masks. One important interpretation distinction is confirmed by a concrete
witness: **one productive pass does not imply exact ordered-object
idempotence after one call**. The following nonproductive call may reorder
clauses by their new widths. This does not invalidate the enumerator's
stopping rule or its contraction counts.

The exhaustive run was still active during this review. Successful partial
counts do not certify the entire profile space. A read-only completion
validator is supplied below.

## Complete value and control audit

For each symbol, a surjection from lifted values to original values gives
an injective Boolean-algebra embedding by full preimage. Positive fiber
weights preserve emptiness, inclusion, equality, and equality of
cardinalities of nested finite sets. The source never compares cardinalities
of unrelated value sets.

| Source line | Value-dependent cardinality decision | Audited containment |
| --- | --- | --- |
| 102 | Empty merged unit intersection | Intersection emptiness |
| 115 | Effective domain and incoming saved unit have equal lengths | The effective domain is the saved unit or its intersection with the old effective domain; no intervening callback |
| 153 | Restricted range and restringent have equal lengths | Restricted range is their intersection, hence a subset of the restringent |
| 151, 160 | Restricted range and old saved length agree | Restricted range is a subset of the old range |
| 161 | Empty restricted range | Intersection emptiness |
| 595 | Outer/inner lengths agree | Evaluated only after the left `&&` operand proves inner inclusion in outer |
| 694 | Nonunit virtual range covers its universe | Unique old range plus universe values outside that old range and the donor range |
| 756 | Unit virtual range covers its universe | The same unique, nested union construction |

The guarded equality at line 595 cannot change evaluation of its right
`||` operand under a lift: inclusion and equality are both preserved. The
guard at 115 does not need a correct or current subset cache; it concerns
two concrete nested sets before any callback. Whether either HLA tautology
branch is reachable is immaterial to the simulation.

All other `length`/`lengths` arguments count clauses, occurrence positions,
candidate symbol columns, registry indices, matrix dimensions, or environment
bindings. In particular, `length(unit_domains)` counts bindings. The sorts
at 48 and 657 therefore receive identical width vectors in corresponding
runs. Every explicit loop visits structural indices or symbol names; none
visits the individual values of a literal range. Registry-key enumeration
at 502 is only the membership table of a filter whose first argument is the
ordered clause name vector.

All remaining value tests are same-symbol membership tests, inclusion,
intersection, difference, union, or the nonemptiness of
`(oneend intersect twoends) minus target` at 463–464. The two operands of
that elementwise `&` share the same first range, so their lengths remain
aligned even after unequal expansion. The HLA complement is relative to the
actual original universe, not an effective unit domain. Repeated values in
the second argument of `char_setdiff` are harmless; the filtered first
argument remains unique.

Every actual/saved/virtual range consequently stays a full union of fibers.
Boolean caches can be stale or mathematically inaccurate while still being
identical in the paired executions. Structural state, queues, call stacks,
and delayed structural lookups then evolve together. This proves the
claimed source decisions and callback schedule without importing a
semantic-correctness or saturation invariant.

Exact ordered block substitution also follows: the helpers preserve their
first input's order and append unmatched second-input blocks. If only domain
order changes, actual returned literal vectors are unchanged, because the
universe-dependent expansions remain virtual. These claims concern ordinary
unique-value vectors; they do not cover duplicate value storage, custom
methods, altered bindings, encoding-dependent equality inconsistencies, or
resource exhaustion.

## Positional duplicates and recursive indexing

The arithmetic invariant needed here is weaker than correct mathematical
subset information: a nonmissing count equals the sum of its stored row at
each selection site.

Initialization at 608–610 explicitly sums the stored rows, including
duplicate columns. Pair construction at 587–604 has no callback between
its writes and those sums. Later scalar-column writes at 186–188,
221–222, and 270–272 are guarded toggles; repeated registry indices see the
already toggled bit. Whole-column clearing at 264–266 subtracts once for
each distinct changed row. Every such update finishes before calling a
handler. Thus a count-one first-order mask has one stored column, and a
count-two mask has two. The two-end handler selects those columns one at a
time. The one-end handler's explicit length guard at 396 rejects a filtered
mask unless it has exactly one position.

In nonunit HLA, the matrix row for the current virtual target is updated
together with `not_subset_count_current`. Rows for targets not yet processed
have not been changed by an earlier virtual target. The intentionally stale
global counts for already processed virtual targets are not reused as
current-target counts.

Unit HLA starts from `m - I`, where `m` is occurrence width and `I` indicates
registry membership for the physical unit symbol. Its materialized mask
starts with sum `m - k`, where `k` is the number of occurrences of that
symbol. Registry membership has no false positive, so `k >= I` and the
count exceeds the mask sum by `k - I >= 0`. Each bit clearing decreases both
quantities once. A selected count of one therefore permits only zero or one
TRUE positions. Zero positions fail at line 754's `clause[[character(0)]]`
before any literal value is read. That error is structural and preserved.

Physical unit registration has one stored occurrence, so its environment
keys are scalar too. Remaining `[[...]]` selectors are scalar structural
indices or guarded scalar symbol names. None can descend recursively into
a literal's individual values under this contract. Duplicate occurrences
can remain semantically wrong or become unregistered after deleting an
earlier copy; the simulation preserves those defects and does not repair
them.

## Exact finite enumeration scope and mask calibration

Each symbol occurs in all three ordered clauses, with nonempty proper
ranges. A value's membership vector is one of the eight Boolean triples.
There is one quotient value for every realized triple, including the
all-zero triple when present. A legal profile is a nonempty subset of the
eight triples that sees both bits at each coordinate.

Inclusion-exclusion gives

```
255 - 6 * 15 + 12 * 3 - 8 = 193.
```

An independent combination generator found profile counts by domain size
`{2: 4, 3: 32, 4: 64, 5: 56, 6: 28, 7: 8, 8: 1}`, exactly matching the
reviewed generator, including its bit-to-clause mapping. Thus the default
run covers `193^3 = 7,189,057` profile triples. The sum of domain sizes is
872, giving `872^3 = 663,054,848` initial valuation comparisons before any
repeated passes.

The three clauses are ordered and are always constructed with `X,Y,Z` in
that order. All assignments of membership profiles to ordered clause
positions are included. Independently permuted within-clause symbol orders,
missing occurrences, extra clauses, extra occurring symbols, and duplicate
occurrences are outside this enumeration. The value-symmetry theorem cannot
remove those structural restrictions.

For sizes `(nx, ny, nz)`, the valuation row is

```
row = (x_index * ny + y_index) * nz + z_index.
```

The X mask selects one contiguous `ny*nz` block; the Y mask selects one
contiguous `nz` block inside each X block; the Z mask selects one position
per consecutive `nz` block. Disjoining selected masks gives literal/clause
truth and intersecting clause masks gives formula truth. The witness method
decodes the least set bit in that same Cartesian order. Python integers
retain every row bit, with at most 512 rows for the enumerated profiles.

`check_masks.py` extracted the reviewed definitions through the Python AST
without importing the R worker or its solver dependencies. An independent
assignment-list builder and direct logical evaluator checked 512 size
triples from 1 to 8, shuffled domain values, 6,912 literal masks, 10,240
formula masks, 933,120 individual formula valuations, and 1,531 witness
selections. This included constants, empty clauses/conjunctions, absent
literals, singleton-string JSON ranges, and reordered clause dictionaries.
All checks passed.

The SAT/non-tautology assertion for initial full-shape inputs is justified:
choose an X value satisfying clause 1, a Y value satisfying clause 2, and a
Z value satisfying clause 3. Conversely, any one clause can be falsified by
choosing a value outside each of its proper ranges.

## Repeated-pass counts and their exact implication

The enumerator compares the truth mask of every pass against the original
formula. Its weight is the sum of retained range cardinalities. Actual
stored entries only lose clauses/occurrences or shrink ranges; virtual HLA
expansions are not returned. Every productive call therefore decreases the
weight. Equality of weight can leave only the initial width sort; the
returned sorted state is unchanged by a further call. The source facts,
together with the explicit normalized-equality assertion, justify stopping
after a weight-equal call. A count of `k` means `k` productive calls plus one
nonproductive verification call, so total executions must equal
`sum((k + 1) * count[k])`.

The fixed initial membership quotient couples every later pass because all
generated sets stay in the original membership algebra. Strict contractions
are preserved under any positive fiber weights. A completed count
distribution therefore proves the same productive-pass bound for every
finite ordinary domain with the enumerated aligned occurrence shape. It
does not itself prove a bound for other shapes, arbitrary symbol orders,
or all accepted API representations. The distribution counts membership
profiles, not a probability distribution over concrete finite-domain CNFs.

It also does not identify exact ordered-object idempotence. The following
aligned full-shape profile, masks `(66,193,193)`, demonstrates the distinction:

```
X = {a,b}; Y = Z = {a,b,c}
C1 = (X=a OR Y=a OR Z=a)
C2 = (X=b OR Y in {a,b} OR Z in {a,b})
C3 = C2
```

Its first result retains `C1` followed by `(Y in {a,b} OR Z in {a,b})`,
with widths `[3,2]`. The second result reverses those clauses to widths
`[2,3]` without contraction. The third is exactly the second. The weights
are `13,7,7,7`, and the enumerator correctly counts one productive pass.
`check_order_only_pass.R` confirms this on both reviewed R versions.

Consequently, a finished distribution supported only at 0 and 1 would prove
that the first result admits no subsequent contraction. Its second result
would be an exact ordered fixed point. Describing the first result as
idempotent needs the qualification "modulo clause order".

## Independent source execution evidence

`check_schedule.R` instruments a fresh in-memory source copy with an
independently written observer and explicit arithmetic/selector assertions.
Each observed result or error is first compared with the unmodified kernel
on the same universe environment. It records all 108 source `if` sites,
68 short-circuit operand sites, 23 loop sequences, 23 loop iterations,
two repeat sites, and 13 helper entries: 237 sites total. It does not claim
primitive-internal comparisons or exhaustive branch coverage.

Both R 3.6.3 and R 4.6.1 independently passed:

- 1,805 inputs, including 871 with positional duplicate occurrences;
- 9,025 transformed pairs: unequal splitting plus independent permutation,
  exact ordered block substitution, injective renaming, domain-order-only
  changes, and maximal membership quotienting;
- 3,469,794 observed events per version;
- 26,586 handler count checks, 16,464 two-end column checks, 354 one-end
  two-column checks, 4,854 nonunit-HLA count checks, and 2,094 unit-HLA checks;
- 54 original structural errors, each preserved across all five
  transformations; exactly 324 empty unit-HLA selectors account for their
  six executions apiece.

The random bank uses 1–4 symbols, 2–6 domain values, 1–9 clauses, canonical
widths up to 4 and positional widths up to 6. Duplicated positions can have
independent ranges, so this tests the stated kernel extension beyond the
specific public matrix-selector copying route. Full-fiber saturation is
checked before projection. All report objects except the R version are
exactly identical across versions. These are independent finite checks
supporting the source argument, not its unbounded proof.

## Reproduction and completion validation

From the repository root:

```sh
python attic/cnf_verify3/symmetry_review/check_masks.py
Rscript attic/cnf_verify3/symmetry_review/check_schedule.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/symmetry_review/check_schedule.R
Rscript attic/cnf_verify3/symmetry_review/check_order_only_pass.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/symmetry_review/check_order_only_pass.R
python attic/cnf_verify3/symmetry_review/validate_full_report.py --require-complete
```

The final command only reads the exhaustive report and writes its audit
under this directory. It checks default enumeration bounds, unique completed
X profiles, expected input counts, pass-count totals, execution totals,
calibration totals, valuation bounds, empty errors, source hashes, and an
actual completion marker. It deliberately fails the completion requirement
while the root run is active. Omitting `--require-complete` checks and records
a partial snapshot without claiming complete coverage. The saved
`full_report_audit.json` explicitly records whether its snapshot was complete.

No production edits, test-suite changes, commits, or interference with the
active exhaustive run were made by this review.
