# Independent review of the clause-count-only productive-pass bound

Reviewed 2026-09-06 against the unchanged production function in
`R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
The documents reviewed are `../pass_bound_review/PROOF.md` and
`../root/DOMAIN_INDEPENDENT_PASS_BOUND.md`, together with the implementation,
the prior repeated-pass argument, and the lifecycle/comparison invariants.
This reviewer wrote the new harness here independently; it does not import
the proposing reviewer's test helpers, instrumentation, or oracle.

## Verdict

I find no substantive gap in the proposed theorem. Under its stated
canonical finite-set and normal-completion assumptions, the first-change
lemma, the fiber potential, and the bound

```
productive_passes <= m + m * 2^(m^2 + m),  m >= 1,
```

are justified by the inspected source. The sharper bound using labeled
preorders is justified too. The large-group argument is a freezing theorem,
not an execution-preserving quotient that replaces a group by two symbols.

The proof needs only a restricted cache property. Raw comparison matrices
can be inaccurate during recursive propagation; that fact does not refute
this bound. Unchanged exceptional group columns remain TRUE, and ordinary
initialized counts equal their matrix-row sums at inference boundaries.
The independent controls deliberately encountered both raw FALSE and raw
TRUE inaccuracies while checking those narrower properties.

This is a reviewed source-level mathematical proof, not a mechanization of
R execution. The finite checks below support and challenge its implementation
premises. They are not the reason the theorem applies to unbounded finite
numbers of symbols or domain values.

## 1. Scope and the quantity being bounded

Here `m` counts original **stored clause occurrences**, including duplicate
clauses, at the beginning of the first kernel invocation. The universe is
fixed. Subsequent invocations receive the actual previous stored output.
There is no public reconstruction, external change, or reordering inserted
between calls.

For nonconstant inputs each clause is a nonempty list with unique valid
symbol names. Each present range is a nonempty finite set of unique values
in its fixed domain; canonical constructor storage additionally excludes
full-domain literals. Equality is the ordinary consistent character equality
used by the set operations. TRUE and FALSE are separate terminal forms.
The result does not cover accepted malformed selector results with duplicate
names or missing ranges, adversarial method overrides, or resource/indexing
failures. Each invocation under discussion must complete normally.

A productive call changes the multiset of actual clauses/ranges, ignoring
order but preserving multiplicity. It need not change semantics: these are
syntactic simplification passes. In particular:

* Deleting one of two identical stored clauses is productive.
* Only sorting surviving clauses is not productive.
* Returning FALSE after discovering a contradiction is productive from a
  nonconstant input, even if the last local contradiction test did not commit
  a new `entries` value.
* Applying the kernel to an existing TRUE/FALSE output is nonproductive.

These distinctions are necessary for the final-call accounting. They are
already made in the proposed proof or its cited repeated-pass result.

## 2. Exhaustive audit of actual storage changes

The actual clause storage is `entries`. The following are all ways it or
the choice of returned entries changes. Line numbers refer to the source hash
above.

| Source location | Effect on actual storage | Consequence for the proof |
| --- | --- | --- |
| 48 | Sort `entries` by clause length | Only permutes ghost identities. |
| 101–104 | Intersect the older unit with the registering unit, retain the older index, eliminate the candidate | No new clause identity or value is created. |
| 152–160 | Compute a local intersection and possibly eliminate the target | If the restringent is contained in the target, the clause is deleted; the local candidate intersection need not be committed. |
| 161–167 | Delete an empty literal or commit a nonempty strict intersection | A surviving actual range only shrinks. |
| 236–252 | Remove a symbol, write the shortened clause, possibly register a new unit | The same ghost identity persists unless subsequently eliminated or the whole call returns FALSE. |
| 472–478 | Flag a nonunit eliminated and remove its registry occurrences | Only removes a clause from the returned formula. |
| 698, 714 | Nonunit HLA eliminates its target | The virtual range is never written to `entries`. |
| 759, 777 | Unit HLA flags its target eliminated | Only clause deletion. |
| 34–42, 488–491, 508, 614, 621, 646, 788 | Return selected entries or a logical constant | Terminal constants need a separate zero-potential convention. |

The two HLA writes at 722 and 784 change the local `clause` value. Under
ordinary R list copy-on-modification behavior they do not modify the entry
from which it was read. Earlier HLA computations and `char_union()` can
enlarge virtual or restricting sets, but a committed actual restriction is
still the intersection with the target's old range.

There is no actual range-write path using arbitrary selection of one value,
adding a new symbol, creating a new clause, or writing a union over the old
target range. A candidate unit is an existing shortened clause, and a merge
retains an existing older clause. This is why persistent ghost identities
are legitimate even when duplicate clauses or duplicate-symbol units occur.

The proposed proof correctly lists the three value-write routes: old-unit
intersection, nonempty actual restriction, and literal deletion. The local
intersection which ends in whole-clause elimination is paid for as a clause
deletion instead of being mistaken for a surviving range change.

## 3. The first-change argument survives the implementation details

Fix an initial signature class G of at least three distinct symbols. The
signature records original support and every directed inclusion comparison
between distinct original clause positions. Until the first change at any
member of G, a retained original clause either has all of G, with its original
ranges, or has none of G. Whole-clause deletion cannot break this property.

It helps to phrase the induction at two levels. Assume it holds at the start
of a call, and consider the first candidate write inside that call. The case
analysis below excludes that write in a surviving clause. Return to a
subcollection of the original ghosts then establishes the same premise for
the next call. If all occurrences of the group disappear, there is no route
by which they could return. The original classification is never recomputed.

### Units

No initial unit contains a member of G. A clause containing any member has
at least three group symbols. Deleting other symbols cannot turn it into a
unit on a member of G without first changing the group itself.

The only sources of registered units are initial units and actual symbol
deletion from a nonunit at lines 245–251. HLA does not register a virtual
unit. Therefore no unit on a group symbol exists before the hypothetical
first change. Both initial/recursive unit propagation and old-unit merging
are excluded as its cause. Simultaneous cascades do not evade this argument:
R executes the individual writes sequentially, so one still has a first one.

### First-order restriction

At lines 298–322 the handler reads the ordinary count. Zero causes a whole
clause deletion; one selects the TRUE column as the pivot. A donor which is
exceptional at a group symbol is exceptional at every symbol of G, because
all those directed original inclusion bits agree and neither endpoint's
group ranges has changed. In particular the donor contains all of G.

The relevant matrix row therefore contains at least three TRUE group
columns. Its count cannot equal one. The zero-count branch does not create
a surviving range change and cannot be used to bypass this obstruction.

### Second-order restriction

Every path to the actual second-order restriction at line 467 comes from
one of two calls to `try_sse_2nd_order()`:

* The oneend handler checks its own one-exception comparison at 386,
  rechecks the twoend candidate's count at 390–392, and selects its other
  exceptional column at 394–396. This other column is the restriction pivot.
* The twoend handler checks the twoend comparison at 412, selects the two
  TRUE columns at 413–418, and rechecks both columns and count before each
  candidate at 427. One of those two columns is the restriction pivot.

Thus an actual second-order write always has a two-exception donor whose
exceptional columns include its restriction pivot. Snapshot candidate lists
do not weaken this requirement: the appropriate counts and columns are
rechecked before the trial. The other donor can have an absent intersection
range after a recursive change; that does not alter the twoend obstruction.

If the pivot belonged to G, the twoend donor's row would have at least three
TRUE group columns, contradicting its required count two. A general abstract
resolution schema can be broader than these production routes. In
particular, reasoning about two one-exception donors cannot manufacture a
new actual-write route absent from this call graph.

### HLA, deleted ghosts, and later calls

HLA can delete whole clauses, but writes no expanded actual range. Its
virtual comparison changes occur after all calls to actual range-changing
helpers. A following invocation builds its comparisons afresh from actual
storage. Those virtual changes therefore cannot seed a later first actual
change at a group member.

An eliminated ghost cannot return as a clause in a later call. The source's
callback lifecycle also prevents inactive donors from becoming an unchecked
new inference source: see the guards summarized above and the prior
`proof_state/LIFECYCLE_PROOF.md`. For extra empirical scrutiny, this harness
checks the original frozen ranges in **all materialized entry slots**, even
slots already marked eliminated, after every actual `entries` write. It did
not need to weaken that check to live output clauses.

## 4. Why transient cache inaccuracies do not supply a counterexample

The needed group-column claim is local to each unchanged symbol:

> If the original donor-to-target comparison is non-inclusion at a group
> member, its initialized bit remains TRUE before a first group change.

Each such bit starts TRUE. The initial common-symbol comparison loop clears
it only after an actual inclusion check, which cannot succeed in this case.
Every subsequent ordinary bit update addresses the symbol whose actual
range changed or was removed. An update at another symbol cannot clear a
group column. Unit-based contextual comparisons at a group member are ruled
out because there is no unit there. A donor containing the group has all its
group columns when its matrix is constructed.

The count equality must hold **when an inference consumes it**, not literally
between the separate assignment statements that flip a bit and adjust its
count. Before recursive callbacks, the source maintains it as follows:

| Source locations | Count-preservation reason |
| --- | --- |
| 608–610 | Newly initialized directed pairs receive the explicit row sums. |
| 185–188 | A guarded FALSE-to-TRUE change increments its count once. |
| 220–224 | A TRUE-to-FALSE change rechecks the bit after recursion, decrements once, then calls the handler. |
| 264–266 | Literal deletion snapshots exactly the TRUE initialized rows, clears the column, and subtracts once from those rows before any callback. |
| 270–285 | Reverse changes after literal deletion likewise increment only a previously FALSE bit before callbacks run. |

There is no callback between a bit assignment and its associated count
assignment. Conversion into a unit can leave old *raw* comparisons stale,
but does not disturb their internal row-sum/count equality. Ordinary future
pairs and the diagonal keep NA counts; neither can qualify as one or two in
a normally executing inference path. Future pair construction uses current
actual ranges and initializes both directed counts.

The initial pair loop cannot overwrite a previously consumed initialized
pair: it constructs each distinct pair once. Partial matrix rows for later
targets do not have usable counts until this construction. No hidden count
update was found outside these paths. The later HLA phases use local counts
for virtual targets; the unit-HLA phase even reuses the variable name for a
different vector. The equality is not asserted there.

The independent run observed 227 live FALSE bits lacking raw inclusion and
7,160 conservative TRUE bits whose raw ranges were already included, counting
observations at selected callback entries rather than distinct bit events.
All 1,980,682 checked initialized row sums still matched their counts. The
first-change proof avoids the false global-raw-exactness premise exposed by
those examples.

## 5. Fiber closure and terminal constants

For each symbol partition its original domain by membership in the original
ranges. Every original range is a union of these fibers. Union, intersection,
and relative complement preserve unions of whole fibers. This covers both
virtual HLA ranges and the actual intersection writes. It also covers the
nonempty or empty intersections during unit merging. Hence a strict actual
range reduction removes at least one whole original fiber occurrence.

For an occurring symbol with original support size `m_s`, the initial fiber
occurrence count is

```
sum_{realized patterns p} popcount(p) <= m_s * 2^(m_s - 1).
```

Each coordinate appears in exactly half of all bit patterns. Patterns may
have unequal positive numbers of concrete domain values; their sizes never
enter this count. The all-zero fiber contributes zero and cannot enter actual
storage, though it remains relevant to virtual complements.

Let A contain symbols whose original signature classes have size at most two.
The combined potential is

```
Phi(F) = live clause count + original fiber occurrences at symbols in A.
```

Actual ranges only shrink. A deleted clause lowers the first term even if
it consists entirely of an arbitrarily large frozen group. A changed surviving
range belongs to A by the first-change lemma and lowers the second term.
Duplicate-clause deletion is still counted once through its ghost occurrence.

Set both terminal potentials to zero. A nonconstant canonical input has at
least one live clause, so its potential is strictly positive before a terminal
transition. A FALSE exit may occur before the impossible empty range is
committed; it is the returned whole formula, not that uncommitted local
assignment, which receives zero potential. Constants return unchanged on
subsequent calls. This completes the descent case analysis without assuming
that every productive call contains a committed literal-range write.

## 6. Counting signatures and the sharper formula

There are at most `2^m` support masks and `2^(m*(m-1))` directed comparison
arrays, giving `2^(m^2)` initial signatures. There are at most two A symbols
per signature. Since `m_s <= m`, the potential satisfies

```
Phi(initial)
  <= m + sum_{s in A} m_s * 2^(m_s - 1)
  <= m + (2 * 2^(m^2)) * m * 2^(m - 1)
   = m + m * 2^(m^2 + m).
```

For support size k, pairwise inclusion is a preorder on those k labeled
clause positions. Conversely, for any such preorder assign position i its
principal lower set `{h: h <= i}`. Reflexivity makes it nonempty;
transitivity proves forward inclusion, and membership of i proves the reverse
implication from set inclusion to `i <= j`. Adding one unused domain value
makes every present range proper. Equal-range equivalence classes are
allowed, so preorders, not only partial orders, are required.

It follows that the exact signature count is

```
S(m) = sum_{k=1}^m choose(m,k) * p(k),
```

and that paying for two small-class symbols at support k gives

```
productive_passes <= m + sum_{k=1}^m choose(m,k) * p(k) * k * 2^k.
```

`count_profiles.py` independently checks the small counts by a different
enumeration: every subset of nonzero membership patterns, plus an unused
zero-pattern value. It does not enumerate or construct preorders. For
`m = 1,2,3,4` it examines `1,7,127,32767` pattern domains and recovers
`S(m) = 1,6,44,499` and `p(m) = 1,4,29,355`. The resulting sharper bounds
are `3,38,801,25708`, agreeing with the proposed arithmetic.

No tightness claim follows. In particular the small-clause bounds are much
larger than many actual pass counts.

## 7. Productive passes versus an exact storage fixed point

A nonproductive call cannot contain a hidden strict actual reduction:
actual ghost ranges never grow, and deleted clauses never return, so any
strict reduction would lower the total number of actual value occurrences.
The only possible output storage change in such a call is its initial stable
length sort. Symbol and value order in retained ranges are preserved by the
left-sided intersection/deletion operations.

After that sort, the remaining computation starts from the same sorted
actual state on the next call. The universe is fixed, local registries and
caches are new, and all scheduling decisions are deterministic under these
assumptions. The same computation therefore has no actual reduction again,
and its output is exactly equal to the already sorted input.

Thus productive calls form an initial consecutive segment. If B is either
reviewed productive-pass bound, at most `B + 1` applications produce an exact
storage fixed point. A loop that must observe equal consecutive stored
outputs may use `B + 2` calls. These are safe upper bounds; the final productive
output may already be fixed, and terminal outputs are immediately fixed.

The seven-clause calibration in the harness reproduces productive flags
`TRUE, TRUE, TRUE, FALSE, FALSE`, with the fourth call sorting only and the
fifth observing exact equality. Every generated case is continued through
one call after its first nonproductive result, requiring exact equality and
forbidding later productivity.

## 8. Independent executable evidence and its limits

Run from the repository root:

```
Rscript attic/cnf_verify3/pass_bound_check/check_bound.R \
  > attic/cnf_verify3/pass_bound_check/checks.log 2>&1
python3 attic/cnf_verify3/pass_bound_check/count_profiles.py \
  > attic/cnf_verify3/pass_bound_check/profile_counts.log
```

The R run used R 3.6.3 and only base R. It reads the exact production source
in a private environment and instruments a second private function copy.
Every pass compares that copy with an unmodified source invocation after
removing only private clause ghost attributes. Both sequences receive their
own actual preceding return objects directly. No public constructors are
used, so this is a kernel test rather than additional public-API coverage.

The 2,033 cases comprise:

* TRUE, FALSE, and contradictory units.
* Duplicate frozen clauses with 3, 8, or 64 group symbols; their potential
  falls `2 -> 1 -> 1 -> 1`, demonstrating that clause deletion is paid for
  independently of group size.
* 1,621 independently enumerated combinations spanning all 44 signatures on
  three clauses. Within each signature, every ordered pair of the 127 total
  membership profiles is used for x and y, and the first profile in that
  class supplies z. These are **not all 48,241 ordered triples** of profiles.
  Missing clause support is filled by independent unit symbols so that the
  kernel input always has three nonempty clauses.
* Threshold controls at 2, 3, and 5 same-signature symbols. At two, overlapping
  incomparable ranges permit a proper shrink and one productive pass; at
  three and five, the group is frozen and no productive pass occurs.
* 400 seeded random interference cases with 4–9 clauses. A three-symbol group
  shares an antichain inclusion relation on chosen support, but its domains,
  higher-order overlaps, value orders and positive fiber multiplicities are
  independently generated. Other symbols may shrink, become units, or cause
  clause deletion.
* Two separately identified historical transient-cache fixtures, each with
  an isolated frozen group appended. These are calibration replays, not
  independently discovered cases.
* The known three-productive-pass example used to check the final-sort split.

All checks passed:

| Check | Observed count |
| --- | ---: |
| Paired instrumented/unmodified-source calls | 4,686 |
| Productive calls | 620 |
| Sorting-only calls | 152 |
| Actual `entries` assignment hooks, including initial sort | 5,481 |
| Range/literal assignment hooks | 784 |
| Old-unit intersection assignment hooks | 71 |
| Helper-entry hooks | 23,050 |
| Initialized matrix-row/count equalities | 1,980,682 |
| Unchanged exceptional frozen-column checks | 920,577 |
| Frozen range checks across materialized ghosts | 74,885 |
| Whole-fiber checks of actual ranges | 79,716 |
| Actual second-order trial entries checked for a twoend pivot | 6,990 |
| Complete output truth-table assignment rows | 432,524 |

Truth tables use direct disjunction/conjunction and scalar membership, with
no simplifier inference predicates. They are complete only for the generated
cases within the recorded domain-product cap: ordinarily 2,048 valuations,
and 512 for random interference. Larger cases still receive every structural,
output-agreement, potential and stability check; their semantics are not
exhaustively enumerated here. The discrepancy counts in Section 4 are
observations during selected raw-comparison scans, not a tally of unique
production bugs.

`checks.rds` contains the summary and per-case pass/potential records;
`summary.R` is a readable summary; `profile_counts.json` records the independent
signature enumeration. The full input generators and hooks are reproducible
in `check_bound.R`. No production source or another agent's directory was
modified, and no commit was created by this review.

## 9. What is established and what remains outside this review

The unbounded conclusion is established by the source-case first-change
argument plus the finite signature and fiber counts. It rules out arbitrarily
many productive normally completing calls at a fixed initial clause count,
even when symbol and finite-domain cardinalities vary.

It does not establish an input-independent constant pass bound, a tight
clause-only bound, bounded execution time or stack depth, semantic
completeness of the simplifier, order independence, or a unique logical
normal form. The existing family with an increasing clause count is
consistent with the result. A future implementation that adds clauses,
writes expanded HLA ranges into actual storage, or uses donors with more
than two exceptional symbols would need a revised freezing argument.

No change to the theorem statement is required. When excerpting it, retain
the canonical-input and normal-completion scope, the distinction between
productive and sorting-only calls, and the fact that the finite experiments
support a source-level proof rather than constitute an exhaustive test of
all finite formulas.
