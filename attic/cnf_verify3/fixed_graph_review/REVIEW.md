# Independent review: nonproductive calls and pure implication graphs

Reviewed 2026-09-06 against the unchanged `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`,
and the two proposals in `../root/FIXED_POINT_SATURATION.md` and
`../root/PURE_IMPLICATION_GRAPH.md`. This review makes no production edits,
edits no root scripts, and creates no commits.

## Verdict and precise scope

Both source claims are valid with the stated canonical finite-set and normal
completion assumptions. The fixed-point result is stronger than a descent
argument alone: a nonproductive call cannot have left a useful member of the
listed local rule family. The graph result is exact ordered greedy edge
deletion after retaining the first duplicate occurrence.

The representation premise needs its full meaning: unique symbol names per
clause; ordinary unique character sets; finite, fixed, nonempty domains;
nonempty proper literal ranges; and no external mutation or custom dispatch.
Successful public construction alone does not establish every one of these
properties. For example, `CnfSymbol` accepts repeated domain values. Accepted
selector objects with repeated names are outside this review.

Properness is necessary for the fixed-point deletion conclusion. The raw
kernel input `list(list(A = c("0", "1")))` over domain `A={0,1}` is an exact
fixed point by the all-unit early return, although the negation of its sole
tautological clause is immediately refutable. Ordinary atom/clause
construction converts this to scalar TRUE. This is a premise control, not a
counterexample to the proposal. Scalar TRUE and FALSE are terminal constants;
the empty clause list returns TRUE. No assertion of SAT completeness follows.

The cycle/star construction is intended for `n >= 2`, with different sizes
for `n >= 3`. The factorial count concerns distinct clause sets of Hamiltonian
cycles, all of size n; the star supplies a further size. It does not assert
factorially many distinct clause counts.

## 1. The fixed-point source argument

### Actual descent and the sorting distinction

The actual write sites are unit intersection/deletion in `register_unit`
(lines 87–105), restriction in `apply_domain_restriction` (146–175), literal
deletion in `eliminate_symbol_from_clause` (236–247), and whole-clause
elimination, including HLA. HLA's growing `clause` is a local R list, never
written back as an enlarged actual entry. No site adds an actual clause,
symbol, or value. The unit merge retains the old clause identity and removes
the new candidate; it does not replace the old unit with a larger range.

Consequently every actual change strictly decreases total stored value
occurrences, counting duplicate clauses separately. A call that preserves
the clause/value multiset cannot contain an intermediate actual change.
The logical early return is terminal; reaching a constant from a nonempty
canonical clause list is productive. On an empty list, conversion to TRUE is
the canonical representation of the same empty conjunction.

The only possible nonproductive storage change is the initial stable
clause-length sort at line 48. Starting the next call with its sorted output
reconstructs exactly the same local registries, matrices, and HLA work. Thus
the output of a sort-only call is already an exact fixed point. A caller
waiting for consecutive raw object equality may use a further observation
call. Neither semantic truth equivalence nor clause-set equality that drops
multiplicity is an adequate measure of productivity.

### All unit cases

The initial unit queue (485–491) visits every original unit while the symbol
registry is empty. Two units on the same symbol would merge and delete one,
so a nonproductive invocation has at most one unit per symbol. The all-unit
early return is saturated: each proper unit can be falsified while satisfying
all the others, which constrain different symbols.

Every original nonunit is then scanned against every applicable initial
unit (499–508). `is_not_subset_of` is still NULL. The later cache-based skip
is unavailable. A range outside its unit would shrink; a range containing
the unit would delete its clause. Therefore every remaining occurrence on a
unit symbol is a nonempty proper subset of that unit. This rules out both
propagation and unit subsumption, including the known equality remainder.
No unit is born later under the no-change hypothesis.

Useful direct subsumption or SSE1 involving units reduces to these facts.
A nonunit cannot subsume a one-symbol target. A nonunit donor with one
exception against a unit has an absent target pivot and cannot restrict it.
A unit donor either restricts at its own symbol or already subsumes the
target through an off-pivot symbol.

### Static pair and SSE2 coverage

The nested pair loop (551–625) initializes every distinct live nonunit pair
in both directions from actual ranges. Every count zero or one is dispatched
immediately. The early breaks and skips require a removed or newly unit
operand; these cannot occur in a nonproductive call. Count-zero subsumption
and every useful count-one SSE1 operation are therefore excluded.

The manual queue (628–646) contains every count-two ordered pair. Its operand
pool and all ranges remain fixed. Every queued pair is enabled and visited;
its handler tries both exceptional-column orientations (416–446), all
registered donors at the intersection symbol, and every matching one- or
two-exception donor. Inverse containment and intersection guards are exact.
No dynamic rescan premise is required here. The recursive change callbacks
at 180–229, 264–285, and 341–366 cannot create an additional scheduling case
because their productive entry routes have already been excluded.

For completeness beyond the explicit queue, let donors A,B and target T
satisfy the full SSE2 premises at distinct pivots s,t, with a value of T_t
outside A_t union B_t. Each donor's exceptional symbols against T lie within
{s,t}. An empty exceptional set gives direct subsumption; {t} gives stronger
SSE1. Two {s} donors give HLA deletion: adding the complement of A_s covers
every B_s value outside T_s by the intersection premise. Every remaining
case has a {s,t} donor, and its queued call covers the triple as described.
An excluded value of T_t proves both inverse guards are TRUE.

Unit donors on t contradict completed propagation; those on s reduce the
intersection premise to the other donor's containment at s; those elsewhere
give unit subsumption. A unit target must be on t, and its donors can only
have the s exception, giving the unit-HLA case. Repeating a donor reduces to
subsumption/SSE1; using the target as a donor makes a useful union restriction
impossible. Thus no omitted unit or repeated-participant case survives.

### Both HLA loops

At line 657 all relevant live nonunit comparisons are exact, and no distinct
live pair has count zero. For a target T, put P_s = domain_s minus T_s.
Donor D has one possible literal at s exactly when its comparison count
against virtual T is one. The source extension by the complement of D_s is
precisely the domain restriction P_s := P_s intersect D_s.

Each nonunit target's loop selects unused count-one donors until there are
none (681–726). A selected donor cannot properly restrict twice: its other
literals stay impossible and its selected possible domain is already inside
its allowed range. If it later becomes conflicting, the registry update
still checks it despite `was_used`; a newly zero count deletes the target.
Every change visits every live donor containing its symbol. Full-domain
virtual ranges detect empty possible domains. Thus single use does not make
this scan incomplete.

Virtual writes affect only the comparison rows for the current target.
A later target's initial rows and global counts remain unchanged. The unit
target loop (731–785) constructs the same comparisons lazily: on a donor's
first relevant registry visit, no earlier changed symbol of that donor can
have been missed, since that earlier visit would already have initialized
the row. Its initial count is exact from unit containment and donor width.

Actual units can be omitted as HLA donors. Intersecting an ignored unit U
into a possible-domain state preserves every nonunit donor's possible-literal
set because D_s is contained in U_s. The relation Q_s = P_s intersect U_s
is preserved by every donor restriction. Initially no such Q_s is empty,
since direct unit subsumption has been excluded. A donor restriction cannot
empty Q_s without a donor conflict. Therefore adding these unit donors
cannot create a missed refutation. For a unit target, all other units are on
different symbols, giving the same conclusion.

Finally, the no-change hypothesis means no target or donor is removed during
the sweep. Each target is tested with its entire final donor pool. This is
enough for the fixed-point claim without the more general final-survivor
argument needed for productive calls. It closes the oneend/oneend SSE2 cases.

Finite descent plus this no-change saturation theorem establishes eventual
local saturation when each call returns normally. It does not establish a
constant number of calls, a practical runtime bound, or complete entailment.

## 2. The graph source argument

### A direct prefix proof without relying on general semantic soundness

Consider the first hypothetical productive prefix operation other than
duplicate deletion. Before it, every surviving clause is its original pure
implication: two distinct symbols with singleton values 0 and 1. The live
pair comparisons are exact because only other whole clauses have disappeared.

Direct subsumption between two such clauses requires equality. Initialization
tries the earlier donor first, so a later duplicate is removed and the first
occurrence survives. No initial unit exists.

For distinct donor and target clauses, count one requires sharing an equal
literal. Their other literal has the same opposite polarity in both clauses.
The donor's other symbol therefore cannot be either target symbol without
being either a duplicate or a repeated endpoint. It is absent from the
target, so `apply_domain_restriction` returns at line 149. This is stronger
than classifying a potentially present exceptional literal as opposite.

For SSE2, let the target have t=v and r=1-v. The twoend donor contains t and
intersection symbol s. Its inverse guard forces t=1-v, so its other literal
is s=v. Since s is exceptional, v is outside T_s. The intersection guard
then forces the other donor to contain s=1-v; its other literal is q=v.
That q cannot equal t, by the other inverse guard. It cannot equal s, by
distinct endpoints. Outside {s,t}, containment in the target is impossible:
the only remaining target literal is r=1-v. These conditions contradict the
other donor's candidate premises. No successful SSE2 restriction call occurs.

This exhaustive source classification also excludes the restricting helper's
whole-clause-deletion shortcut. One cannot infer its absence merely from the
two uniform models, since those models protect literals but do not protect
redundant whole clauses. After the prefix, only first duplicate occurrences
remain, with their original ranges and exact live comparisons.

### HLA is alternative-path detection

Negating edge u -> v fixes u=1 and v=0. A directed path from u to v through
the other edges propagates ones to a contradiction. Without such a path,
setting exactly the vertices reachable from u to one satisfies every other
edge and falsifies u -> v. Hence domain propagation refutes exactly when
there is an alternative path; there is no appeal to general CNF entailment
completeness. Reverse propagation of zeros does not add a spurious case.

The exact HLA loop therefore deletes exactly the edges deletable by this
path predicate. Equal clause widths make both source sorts preserve input
order. Deleting an edge only removes donors. An edge retained at its visit
cannot acquire a new path later, so every final edge is indispensable.
The all-zero and all-one assignments witness the necessity of each literal.
The returned storage is an exact second-call fixed point.

For a DAG, every cover pair of the reachability order is a mandatory original
edge, and every non-cover edge has an alternative path through those covers.
This proves uniqueness of the output edge set, with storage order still
allowed to vary. For a complete directed graph, put all edges outside any
minimal strongly connected spanning subgraph H before H's edges. H deletes
all the earlier edges and then retains each of its own. Hamiltonian cycles
give (n-1)! sets of size n; bidirected stars give size 2(n-1). The resulting
size difference comes from distinct irredundant normal forms, not scheduling
incompleteness. The proposed five forms at n=3 are the two oriented triangles
and three bidirected two-edge trees.

## 3. Executable controls and the root harness review

The root harness has independent Warshall reachability, truth vectors,
prefix comparison, clause-deletion witnesses, and exact second-call checks.
Its original final size-vector assertion compared a named `vapply` result
against an unnamed expected vector, causing both runs to halt after the
per-case checks. This was reported immediately; the root corrected it with
`unname()` and reran. It was a control assertion issue, not a graph mismatch.

`control.R` here supplies a separately written breadth-first-search oracle,
all 1,555 ordered edge multisets of length at most four on three vertices,
and 200 deterministic duplicate-heavy cases on four through six vertices.
Literal storage order alternates. A private source prefix asserts that every
restriction-helper call has an absent target pivot, in addition to checking
the final duplicate-only prefix. A separate static enumeration checks every
edge pair/triple over four vertices against the direct polarity argument.

The fixed-point control repeatedly applies the unmodified raw kernel to all
18,473 clause sets of size at most three over two ternary symbols, and all
2,951 nonempty clause sets of size at most three over three Boolean symbols.
Its independently written rule audit enumerates the full SSE1/SSE2 premises
and uses repeated full domain scans under each target's negation. Every call
is checked against a complete input truth table; every productive call must
decrease actual value mass; the first nonproductive output must pass the
rule audit and be an exact next-call fixed point. Directed controls cover
proper unit containment, sort-only storage change, duplicate units/clauses,
scalar constants, and the excluded full-domain unit.

Both targeted runs completed successfully, on R 3.6.3 and R 4.6.1, with
identical counts:

| Check | Count per R version |
|---|---:|
| Graph inputs | 1,755 |
| Graph inputs containing duplicate edges | 1,201 |
| Static count-one edge pairs, all with absent pivots | 48 |
| Static SSE2 candidates, all rejected by intersection | 120 |
| Fixed-point input formulas | 21,429 |
| Productive calls | 20,601 |
| Nonproductive calls and exact next-call checks | 21,429 |
| Nonconstant final rule audits | 20,267 |
| Constant final audits | 1,162 |
| Nonproductive calls that changed only clause order | 2,369 |

The maximum productive-call count in this small-clause bank was one; these
executions do not extend the existing multi-pass lower-bound experiments.
The explicit full-domain raw-unit exclusion behaved as predicted.

Saved logs, machine-readable RDS records, and plain `dput` records are
`control_r36.{log,rds,txt}` and `control_r46.{log,rds,txt}`. Reproduce from
the repository root:

```sh
Rscript attic/cnf_verify3/fixed_graph_review/control.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/fixed_graph_review/control.R
```

These finite banks are falsification controls, separate from the unbounded
source arguments above. They do not prove canonical behavior for malformed
public inputs, resource-bounded success, or SAT completeness.
