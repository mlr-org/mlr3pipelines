# Independent review of the three-clause (3,3,2) quotient

Reviewed 2026-09-06. The root's `THREE_SYMBOL_QUOTIENT.md`, generator,
bit-vector oracle, calibration script, persistent R bridge, and all relevant
production operations were inspected. This review makes no production edits
and does not repeat the 9,386,748-execution campaign.

## Verdict and a useful scope fact

The quotient and order reduction are valid for the stated shape, under the
canonical finite-set and ordinary-R assumptions below. The primary integer
bit-vector evaluator covers every assignment in each representative domain
product. I found no omitted clause/symbol order or incorrect finite-domain
lifting step.

**Every input in this proper shape is satisfiable.** Choose a value of X from
the short clause's X range, a value of Y from the first long clause's Y
range, and a value of Z from the second long clause's Z range. All three
ranges are nonempty and belong to different symbols, so the three choices
can be made independently. They satisfy the three clauses respectively.
The root's `expected == 0` calibration must therefore be false on every case
in this family. The enumeration can still detect an incorrect FALSE output,
lost or added models, or a production error; it is not a family of potentially
contradictory inputs. The previous two-symbol quotient has a different scope.

The full run was still active during this review. `root_progress_snapshot.json`
records the inspected progress, so the mathematical coverage assessment must
be kept separate from claiming that all scheduled executions have finished.
Full completion requires all 193 X indices exactly once, 9,386,748 executions,
no saved errors, and the matching full valuation count in that snapshot.

## Independent profile count and reconstruction

For k coordinates, regard each membership pattern as an explicit Boolean
tuple. A profile is a nonempty set of such tuples with no constant coordinate.
The independent script enumerates tuple subsets using combinations, rather
than the root's integer-mask loop, and obtains exactly the same domains and
ranges for all profiles.

For three coordinates, inclusion-exclusion gives

```
(2^8 - 1) - 3*2*(2^4 - 1) + 3*4*(2^2 - 1) - 8*(2^1 - 1)
= 255 - 90 + 36 - 8 = 193.
```

For two coordinates it gives `15 - 12 + 4 = 7`. Every required literal is
nonempty and proper exactly when its coordinate is nonconstant. Pattern 000
(or 00 for Z) is retained whenever present; values unused by every input
range are not discarded. Equal coordinates and repeated long clauses are
allowed rather than filtered out.

For any concrete input, map each X/Y value to its three input membership
bits and each Z value to its two bits in the long clauses. The image profile
is one of those enumerated. The original domain is a disjoint union of the
nonempty fibers of this map, and every original literal range is a complete
preimage of its quotient range.

## No scheduling order is missing

The only initial sort is `order(lengths(entries))`. A proper input has
lengths 2,3,3, so its unique short clause moves first and the two long clauses
retain their relative order. Moving the short clause among original positions
has no further effect on the constructed inputs. Public CnfClause construction
preserves the insertion order for these distinct-symbol atoms; the Python
dictionaries, JSON bridge, and R named lists preserve that order as used here.

Let the first and second symbols in the short clause be renamed X and Y.
The remaining symbol is Z. This maps either original short-clause orientation
to X,Y. It simultaneously swaps the X/Y profile coordinates and long-clause
names when needed, which is covered by the complete ordered Cartesian product
of X and Y profiles. It is not an assertion that swapping a fixed input has
an identical unnamed output without undoing the renaming.

Swapping the two long clauses swaps coordinates 1 and 2 of both three-bit
profiles and swaps the two Z coordinates. These maps preserve the property
that every coordinate varies and are bijections of the profile sets. The
independent check verifies closure under *all* coordinate permutations, which
is stronger than the particular long-clause swap needed here.

Each long clause retains all six orders of its three symbols. Thus there are
36 remaining schedules for every ordered profile triple. Fixing the short
orientation removes no case because its other orientation maps to another
exhaustively enumerated profile/schedule combination.

As an executable check, the reviewer generated all
`3! * 2! * 3! * 3! = 432` syntactic clause/symbol arrangements of a fixed
asymmetric profile triple, independently sorted/renamed/re-encoded each, and
checked membership in the root's enumeration. They map to 144 distinct
enumerated cells for that fixed triple (four transformed profile triples,
each with 36 long-clause orders). This finite mapping check supports the
general transformation argument; it is not a second exhaustive source run.

## Why the actual source respects the reduction

The source does not order symbol names lexically or branch on a value's
spelling. Dictionaries use symbols as exact keys. The only enumeration of
unit-domain names is the second operand of
`char_intersect(names(current_clause), names(unit_domains))` at line 502;
the result follows the current clause's symbol order. Renaming may change an
environment's hash/enumeration order, but not the propagated-symbol order.
Other positional indices refer to clauses, symbols in stored clause order,
or Boolean matrices, all of which the coupling preserves.

Every value-producing operation is membership, union, intersection, or
complement/difference within a symbol's own domain. There is no selection of
an arbitrary first value, cross-symbol domain-value comparison, or lexical
value sort in the simplifier. Surjective preimages commute with these set
operations. Consequently complete membership fibers remain complete in every
actual and virtual range throughout the coupled runs.

Checking operations alone would be insufficient without checking branches.
The cardinality-sensitive decisions reduce to the following set predicates:

| Source location | Predicate preserved by refinement |
| --- | --- |
| register_unit:102 | Empty intersection |
| register_unit:115 | Effective unit equals its birth range, since the effective range is contained in it |
| apply_domain_restriction:153 | Intersection equals restricting range |
| apply_domain_restriction:160–161 | No set change, or empty result |
| initial comparison:595 | Equality under an already established subset relation |
| non-unit HLA:694; unit HLA:756 | Virtual range equals its domain, since it is contained in the domain |

Positive but unequal fiber sizes preserve equality/inclusion and emptiness,
so none of these shortcuts requires uniform multiplicity. The other length
operations count clauses, present symbols, or indices, and are unchanged by
value refinement. Constructor coverage/emptiness tests and flat set unions
also commute on the canonical inputs used by the bridge.

The persistent bridge has diagnostic variants defined in the same process,
but `worker()` calls `bridge.simplify()` with default `audit=False`,
`direct=False`, and `variant=None`. `run_request()` explicitly restores
`plain_simplify` on that path before building the formula. The reviewed run
therefore invokes the unchanged original kernel, not an instrumented or
repaired variant. The manifest checks the six source hashes, including
simplifier SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.

The reviewer also generated 300 independent paired production inputs with
nonuniform fresh-label refinements, shuffled values and domain insertion
order, renamed symbols (including names whose lexical order changes), and
arbitrary clause/symbol order. An independent normalizer mapped each back to
an enumerated quotient cell. Every output matched its quotient under inverse
symbol/value mapping; clause and symbol order matched, and every concrete
range retained or removed entire membership fibers. All 600 calls used the
ordinary original production constructor path. This is empirical support for
the source induction, not a substitute for it.

## The primary oracle and the calibration's shared assumption

`BitTruth` enumerates `itertools.product(*domains.values())`, assigns each row
one distinct bit, and builds one bit mask per symbol/value. At each row,
exactly one value mask for each symbol contains its bit. Union of literal
masks is clause disjunction; intersection of clause masks is conjunction.
The all-row mask is `(1 << N) - 1`. Python integers are unbounded, so all
`N <= 8*8*4 = 256` rows fit without truncation. TRUE/FALSE and scalar string
ranges are handled explicitly. The witness operation extracts the least set
bit from a nonzero difference and returns that exact assignment.

The root's scalar calibration is useful but iterates `oracle.assignments`
itself. It therefore does not independently test that the assignment grid is
complete: an omitted row could be shared by both sides of that comparison.
The SAT/MDD unequal-pair checks still establish real differences, but their
existence assertions do not independently certify all bit-vector rows.

This review closes that particular shared-assumption gap with a separate
mixed-radix assignment generator, without calling a Cartesian-product API:

- 102 grids, including the maximum 8×8×4 grid, singleton domains, reordered
  symbols, and multi-character labels with substring relationships.
- Exact set and cardinality comparison between each independently generated
  grid and the root's rows.
- 9,769 singleton-assignment formulas, each required to map to exactly one
  bit, with the correct witness, no bit collision, and complete all-bit union.
- 1,020 unrelated formulas compared as full model **sets** against scalar
  evaluation on the independent grids. Constants, empty formulas, empty
  clauses/ranges, and string-versus-list range encodings are included.

All checks passed. The root's 2,000 scalar comparisons and 1,808 deliberate
one-model changes remain additional calibration evidence. Its one-model
blocking clause is mathematically correct: the disjunction of inequalities
to a point is false at exactly that point and true at every other assignment.

## Qualifications and cheap saturation check

The finite-to-arbitrary-finite lifting concerns canonical ranges and fixed,
finite, nonempty domains with ordinary total string equality. "Multiplicity"
here means multiple distinct concrete domain values in one membership fiber;
it should not silently stand for arbitrary duplicate storage or classed R
objects. Missing symbols/ranges, NA selectors, duplicate-name clauses,
byte-marked equality failures, universe mutation, and arbitrary constructor
constant combinations are outside this proof. Normal execution also requires
sufficient stack, memory, and supported R indexing capacity.

Completing this experiment proves semantic preservation for exactly this
occurrence shape, with those qualifications. It does not prove saturation,
idempotence, a unique normal form, or every three-symbol formula. Dropping a
full-domain literal can remove a TRUE clause, but other occurrence shapes
require their own coverage arguments rather than being silently included.

The root counts only one particular leftover condition: a unit that directly
subsumes a surviving nonunit. A zero value of that statistic is not a check
of all local rewrite rules. As a cheap independent screen, the 300 paired
quotient outputs were also checked by plain-set direct subsumption, SSE1,
and SSE2 premise enumeration; no opportunity was found in that sample. No
claim about all 9.4 million outputs' saturation follows from this result.

One robustness qualification concerns recording: reported R errors and
truth-vector differences preserve their exact inputs before stopping. An
unexpected malformed result that instead raises inside the Python evaluator
(for example an unknown symbol/value) is not caught by the worker's saved
error path. Such a failure would stop the run without the same explicit
counterexample record. This does not affect completed successful comparisons,
but the prose should not promise saved exact inputs for every possible Python
exception. No such exception was observed in this review.

`check_quotient.py`, `quotient_checks.log`, and `quotient_checks.json` contain
the independent checks. All new review artifacts are in this directory.
