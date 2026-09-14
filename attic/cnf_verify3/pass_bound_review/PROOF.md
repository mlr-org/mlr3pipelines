# Independent review and a clause-count-only productive-pass bound

Written 2026-09-06 for the unchanged `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
The input is canonical finite-set storage in a fixed universe. All invocations
considered return normally, and repeated invocations use the previous actual
stored formula without external mutation. Clause, symbol, and value order
may affect the schedule. These statements do not require order invariance.

## Results

The fiber-occurrence bound in `../root/DOMAIN_INDEPENDENT_PASS_BOUND.md` is
valid. Its proof can be combined with a different finite classification of
symbols to remove the dependence on the number of occurring symbols as well.

For `m >= 1` original clauses, a coarse explicit bound is

```
productive_passes <= m + m * 2^(m^2 + m).
```

Thus a fixed-clause family cannot have arbitrarily many productive passes
merely by increasing its symbols or domain cardinalities. The bound is
deliberately loose and is not a bound on execution time, recursion depth,
or allocation. At most one final sort-only application is needed to produce
an exact storage fixed point; a further call may be needed to observe its
equality with the preceding output.

The proof below uses a first-change argument about the actual source. It
does not claim a quotient in which many symbols can be replaced by two while
preserving behavior. Such a replacement can enable rules that were blocked
by the original multiplicity.

## 1. Review of the domain-independent fiber potential

Give the `m` input clauses persistent ghost identities, retained by any
surviving clause through sorting. A retained unit from a merge keeps its old
identity; the other clause is deleted. For a symbol `s` occurring in `m_s`
clauses, partition its domain by its membership bits in those original
clause ranges.

Each original range is a union of whole fibers. Unions, intersections, and
domain-relative differences preserve this property. These are all operations
that produce actual or virtual ranges. A unit merge intersects actual
ranges; HLA creates only virtual extensions and can delete an actual clause.
No actual surviving clause gains a symbol or value. Therefore every actual
range continues to contain each original fiber completely or not at all.

Count one occurrence for each fiber present in each actual clause range.
A productive pass either removes a clause or removes values in a surviving
clause. Every nonempty range contains at least one fiber, and every actual
value change removes a whole fiber. The count strictly decreases. Initially
it is

```
sum_s sum_{realized patterns p at s} popcount(p)
  <= sum_s m_s * 2^(m_s - 1).
```

The counting identity follows by summing over the coordinates: each one is
set in exactly half of all `2^m_s` patterns. Unequal positive fiber sizes do
not affect the potential. The all-zero pattern contributes zero but remains
available to virtual complements. The argument applies to ghost clause
occurrences, so duplicate stored clauses do not invalidate it.

This establishes the reviewed bound. It needs preservation of whole fibers,
not a claim that all concrete execution branches are invariant under symbol
or value quotienting.

## 2. A finite comparison signature for each symbol

For every originally occurring symbol `s`, record:

1. Its support `I_s`: which of the `m` ghost clauses contain it.
2. For each ordered pair of distinct original clauses `(i,j)`, the Boolean
   value of `C_i[s] subset C_j[s]`, treating absence as the empty range.

Call this its initial signature. Two symbols can have the same signature
despite different domain sizes, different value labels, different actual
membership fibers, and different intersections of three ranges. Only support
and pairwise inclusion are recorded.

There are at most `2^m` supports and `2^(m(m-1))` ordered-pair bit arrays,
so at most `2^(m^2)` signatures. Some arrays are unrealizable; this upper
bound need not count them exactly.

Partition the originally occurring symbols into their signature classes.
Call a symbol **small-class** if its initial class has size at most two.
There are at most `2 * 2^(m^2)` small-class symbols. This classification
remains the initial one throughout the argument; classes are not recomputed
after deleting clauses.

## 3. First-change lemma: a class of at least three symbols is frozen

Fix an initial signature class `G` with at least three distinct symbols.
Until the first actual range change at any member of `G`, each surviving
ghost clause has either all those symbols or none, according to its original
support. When present, their ranges are their original ranges. Whole-clause
deletions do not disturb this fact.

Suppose for contradiction that some operation makes the first actual range
change at `t in G`, in a clause that is retained at that operation. Inspect
all production routes to such a change.

### 3.1 Unit propagation or unit merging cannot be the first change

No initial unit can contain `t`: every clause containing it also contains
all of `G`, hence has at least three symbols. Before the hypothesized first
change, deleting other symbols from such a clause still leaves all of `G`.
It cannot become a unit on `t`. HLA only deletes whole actual clauses.
Consequently no unit at `t` can exist to restrict another literal or merge
with another unit. This excludes both unit-based actual write routes.

### 3.2 A first-order restriction cannot be the first change

The actual first-order handler selects its pivot from the one exceptional
column of a donor-to-target comparison. Thus a value-changing first-order
call at `t` requires a donor whose range at `t` is not contained in the
target's range there. A zero-exception donor instead triggers whole-clause
subsumption and supplies no counterexample to the lemma.

Because the two clauses' ranges at every member of `G` are still their
initial ranges, the signature says that this same donor is exceptional
against the target at every member of `G`. It therefore has at least three
exceptional symbols, incompatible with the count-one handler.

### 3.3 A second-order restriction cannot be the first change

Every actual second-order call in this source has a two-exception donor.
Both the oneend and twoend handlers choose the restriction pivot from this
donor's exceptional columns; the other chosen column is the intersection
pivot. Therefore a value-changing call at `t` includes a donor exceptional
at `t`. Its initial signature again makes it exceptional at all of `G`,
contradicting the required count two.

This distinguishes actual source calls from every abstract form of the
two-donor rule. In particular, an abstract pair of one-exception donors
whose only exception is the intersection pivot is covered by HLA deletion.
The implementation does not use that arrangement to write an arbitrary
restriction pivot. A whole-clause deletion remains permitted and is counted
separately below.

### 3.4 HLA and logical exits do not escape the argument

HLA may enlarge local virtual ranges on members of `G`, but it never writes
them to an actual retained clause. It only deletes whole clauses. Its virtual
matrix changes occur after the range-changing phases, and a later invocation
rebuilds all matrices from actual storage. A TRUE/FALSE return is terminal
and can be charged as deletion of the remaining actual formula.

These cases exhaust actual range writes: a nonempty restriction in
`apply_domain_restriction`, a literal deletion in
`eliminate_symbol_from_clause`, and intersection of the old unit in
`register_unit`. None can make the hypothesized first change. All members of
`G` are therefore frozen in every surviving ghost clause through every pass.

## 4. Why transient caches do not invalidate that lemma

The lemma requires less than global raw-exactness of all transient caches.
Consider a donor and target that still contain members of the frozen group.
If their original comparison is exceptional, each corresponding initialized
matrix bit starts TRUE and stays TRUE until a range at that same symbol
changes. The initial common-symbol loop clears a bit only after testing
actual inclusion. Updates at other symbols touch other columns. There is no
unit at a group member that could introduce a contextual comparison there.
Uninitialized rows start TRUE; their ordinary counts remain NA until the
pair is constructed.

Before recursive callback consumption, ordinary initialized counts are the
sums of the corresponding matrix rows. Direct inspection of the write sites
establishes this independently of whether the bits at *other* symbols are
temporarily conservative:

- Initial pair construction explicitly sums the rows.
- Reverse FALSE-to-TRUE changes increment once, guarded by the old FALSE bit.
- Forward TRUE-to-FALSE changes decrement once, rechecking that the bit is
  still TRUE immediately before the mutation after recursive calls.
- Whole-literal deletion snapshots the TRUE initialized rows, clears that
  column, and decrements exactly those counts before invoking callbacks.

Thus at least three unchanged TRUE group columns imply count at least three
at every relevant inference guard. An uninitialized NA count cannot qualify
as one or two on a normally executing path. No global assumption of complete
revisiting, pending-callback exhaustion, or HLA saturation is needed.

## 5. The clause-only potential

Let `A` be the initial set of small-class symbols. For a current actual
formula define

```
Phi(F) = number of live clauses
       + number of original fiber occurrences at symbols in A.
```

Set `Phi(TRUE)=Phi(FALSE)=0`. Actual ranges and clause identities only
disappear or shrink, so this potential never increases. A productive pass
deletes a whole clause, decreasing the first term, or changes a surviving
range. The first-change lemma forces the latter change to occur at a symbol
in `A`, where it removes at least one whole fiber and decreases the second
term. Therefore every productive pass decreases `Phi` by at least one.

The initial potential has the useful input-specific bound

```
Phi(F_initial)
  = m + sum_{s in A} sum_{realized patterns p at s} popcount(p)
  <= m + sum_{s in A} m_s * 2^(m_s - 1).
```

Using `|A| <= 2 * 2^(m^2)` and `m_s <= m` gives

```
Phi(F_initial) <= m + m * 2^(m^2 + m).
```

This proves the stated clause-count-only bound, independently of how many
symbols belong to the frozen classes. It does not count the potentially
unbounded number of frozen values removed by a whole-clause deletion; that
operation is paid for by the clause-count term instead.

## 6. Optional sharper signature count

For support size `k`, the inclusion relation of its `k` nonempty ranges is
a preorder. Let `p(k)` be the number of preorders on `k` labeled elements.
Every preorder is realizable by nonempty proper finite ranges: set the range
at `i` to `{h : h <= i}` in the preorder, and add one unused domain value to
make all ranges proper. Inclusion of these principal lower sets recovers
exactly the preorder, including equal-range equivalence classes.

Thus the exact number of possible signatures on `m` ghost clauses is

```
S(m) = sum_{k=1}^m choose(m,k) * p(k).
```

Paying for at most two small-class symbols of each signature and using their
own support sizes yields the sharper universal bound

```
productive_passes <= m + sum_{k=1}^m choose(m,k) * p(k) * k * 2^k.
```

The coarse explicit bound avoids needing any table or asymptotic information
about `p(k)`. Neither bound is claimed tight.

## 7. Stability and scope

The earlier actual-storage descent proof shows that productive passes form
an initial consecutive segment. A call with no actual mutation can only
perform the stable initial length sort; its output restarts the same
nonmutating computation with fresh local state and is already an exact fixed
point. Adding one sort-only application and one equality-verification call
therefore gives the same distinctions as in the repeated-pass review.

The result is specific to the inspected implementation's actual mutation
routes. An extension that writes virtual HLA ranges into actual clauses, adds
new clauses, or permits higher-exception inferences would require a new
argument. It also excludes duplicate-name/missing-range accepted R objects,
external mutation, abnormal resource/index behavior, and nonterminating or
failing invocations. The existing unbounded-pass family increases its clause
count and is fully consistent with this bound.

Executable checks and calibration results are recorded separately in this
directory. They support the first-change and counting arguments; the finite
class argument above supplies the unbounded conclusion.
