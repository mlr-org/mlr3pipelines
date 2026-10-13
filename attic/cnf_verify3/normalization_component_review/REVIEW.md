# Independent review of domain normalization and component separability

Reviewed 2026-09-06 against unchanged `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
The reviewed proposals are `../domain_storage_contract/PROOF.md`, its
`RESULTS.md`, and `../root/COMPONENT_SEPARABILITY.md`. This directory imports
production source and saved concrete input fixtures; it does not import either
proposal's observer, oracle, generator, bridge, or comparison implementation.

## Verdict

I found no separating example or undischarged source obligation under the
contracts below. Both source simulations are supported. These are reviewed
source arguments, not mechanizations of R evaluation, memory allocation, or
the complete interpreter. The finite experiments described in `RESULTS.md`
support the review; their bounds do not establish universal claims.

Domain normalization preserves the exact ordered actual payload and source
choices with the outer universe association interpreted separately. It need
not preserve raw virtual vectors. Component separability preserves the exact
ordered projection, actual FALSE recognition, and repeated-pass behavior.
It need not preserve the full global stream of callback invocations.

## Representation and execution premises

* Domains have fixed finite nonempty character support and no missing values.
  Domain normalization additionally permits repeated values and ordinary
  names/dim/dimnames/inert attributes with the base-operation behavior below.
* Each actual clause has distinct, nonmissing registered binding names. Each
  actual range is flat, unnamed, nonempty, unique, and a proper subset of its
  symbol domain. Actual input clause, symbol, and value order is kept fixed.
  Scalar TRUE/FALSE and the empty conjunction use their constant paths.
* Values obey one ordinary total equality respected by membership, matching,
  filtering, and deduplication. Empty string values and nonsyntactic names are
  unobjectionable; conflicting encoding/byte equality models are not assumed.
* Domain objects obey ordinary one-index vector access and concatenation.
  The proposal's allowance for an inert class means absence of applicable
  behavior-changing dispatch, not every arbitrary class accepted by a public
  argument assertion.
* Relevant primitive operations, environment bindings, integer/index/length
  arithmetic, recursion and allocation succeed normally. No external mutation,
  active bindings, observation-changing callbacks or custom dispatch is used.
  This is not a claim that a combined instance fits whenever its components fit.
* Component groups partition input symbol names. Equal value labels at different
  symbols do not connect groups. Every group's induced clause order is retained;
  global interleaving may be arbitrary. No clause belongs partly to two groups.

Malformed selector output with repeated/NA symbol names or NULL ranges remains
excluded. Neither proposal repairs the known selector defect, scheduling gaps,
or lack of first-pass saturation. Exact payload comparison drops only the outer
formula class/universe attributes and preserves all actual list and vector order.

## Domain normalization: discharged obligations

### Domain reads and actual writes

The computational reads of original stored domains occur only in the HLA
extensions and length comparisons at lines 693-694 and 755-756. The remaining
use attaches the supplied universe to the returned object. Prior unit merging,
restriction, symbol deletion, comparison initialization and second-order work
therefore execute identically on identical actual input ranges. In particular,
stale snapshots and missed callbacks are identical too; normalization need not
assume away scheduling omissions.

The only writes modifying existing clause payloads are at lines 103, 167 and
244 (line 103 also writes the effective unit domain). They merge/intersect old actual ranges or remove a
symbol. They preserve actual value order and uniqueness. HLA writes `clause`
and its local ranges only: it does not assign expanded virtual ranges to
`entries`, nor does unit-HLA write them to `unit_domains`.

### Multiplicity algebra

For a stored domain S, capacity `mu(v)` counts copies of v; `m(v)` counts copies
in the old virtual range. Directly from

```
c(old, S[!S %in% c(old, donor)])
```

the next multiplicity is m(v) when m(v)>0, zero when m(v)=0 and the donor
contains v, and mu(v) otherwise. Initial actual values have one copy and
previously absent symbols have zero. Thus `0 <= m(v) <= mu(v)` is inductive.
One-index matrix/array filtering flattens the values; names may survive but
neither names nor dimensions become extra values. The support follows the
same complement/union equation as for normalized domains.

This is a bounded-submultiset invariant. It is not virtual uniqueness and it
is not full occupancy of every present value's domain copies. Full support
alone does not imply `length(virtual) == length(S)`.

### The physically missing selected-donor value

The length predicates require more than the multiplicity bound. At selection,
the sole exceptional donor symbol s has a value v missing from the current
virtual target. The extension cannot introduce v because the donor contains it.
Consequently the stored result omits at least mu(v)>0 possible copies, giving
`length(new) <= length(S)-mu(v) < length(S)`. Its normalized counterpart also
omits v and has smaller length than the normalized domain. Both branch outcomes
are therefore FALSE when selected, rather than merely logically equivalent
tests on arbitrary virtual storage.

The missing-value premise is established from the implementation as follows.

1. A unit candidate's index is born only once. Initial unit registration happens
   before the nonunit registry is populated. Later birth removes both relevant
   registry memberships before recursive propagation; the candidate is marked
   unit/eliminated before its callbacks. A stale propagation index that now
   refers to a different-symbol unit exits on the missing-symbol guard. An old
   representative can shrink through a merge without becoming a fresh birth.
2. FALSE matrix certificates can temporarily be only contextual under live
   units. Source restriction preserves them. Nonunit target restriction/deletion
   repairs reverse comparisons before callbacks. Unit target restriction can
   defer physical changes because the new constraint restricts both sides.
3. At a unit's birth, incoming retained-symbol FALSE bits have a frozen
   certificate under earlier births' constraints. Later source changes cannot
   circularly justify those bits using this new unit: inactive unit targets are
   absent from the nonunit registry; clearing a removed source column merely
   certifies an empty source range. Induction over birth order therefore proves
   final physical containment in every unit after propagation returns.
   Containment is nonstrict. The known equality skip is compatible with it.
4. A physically wrong TRUE bit for a live pair can arise only after its source
   shrinks. That shrink owns a pending source-row repair using current entries.
   A later nested shrink owns its own repair, while a new reverse TRUE is correct
   when introduced. Symbol deletion clears its source column before callbacks.
   Inactive pairs lose the obligation; future pairs are initialized afresh.
   With no pending frame at HLA entry, all live rows are raw-exact. Guarded bit
   flips and initialization also establish count/row-sum agreement.
5. During nonunit HLA, expanding one target symbol can only clear donor bits.
   The complete symbol registry visits every donor that could newly become a
   subset at that symbol. Writes concern only this target's row in each donor
   matrix. A later target has a different row and unchanged original counters.
6. Unit-HLA starts with exact counts `width - contains_unit_symbol` by physical
   unit containment. Its lazily created row is exact: if any other symbol had
   expanded earlier, that donor would already have been materialized when the
   expansion traversed the complete symbol registry. The same monotone updates
   then preserve its row/count meanings.

These facts supply the physical exception for every selected donor in the
canonical execution. The coupled storage execution has identical actual data,
bits/counters/flags and virtual support, so donor choice and every subsequent
decision remain coupled. Different virtual names, multiplicities or order do
not escape through an actual write.

## Component separability: discharged obligations

### Stable orders and the changing global bound

Stable `order(lengths(entries))` induces the same local order as sorting each
group alone. Fixed clause and available indices embed increasingly into global
indices. Registries for a symbol contain only its owning group's clauses, with
the same induced order. Unit representative choice, old-range order in merges,
and recursive propagation are therefore local.

The important fact for lines 118, 133, 176, 261 and 277 is not equality of numeric
indices. During pair initialization, a productive root callback can only come
from a same-group pair involving the current outer clause. Its recursive tree
remains in that group. The set of this group's initialized available indices
is exactly its corresponding local prefix; comparison with `meta_idx_outer`
therefore has the same answer under the increasing embedding. Unrelated outer
iterations cannot start productive work in another group. During second-order
work, both bounds have reached their respective final available index.

The observer independently compares component-local helper histories carrying
this projected initialized-prefix count, not the numerically unequal global
indices. This detects an accidental assumption that index values themselves
remain unchanged.

### Contextual FALSE bits and cross-group work

For a live nonunit source D and a target outside its group, every physically
present source symbol has a nonempty range against an absent target range.
Its matrix bit stays TRUE. A removed source-symbol column can be FALSE, which
correctly describes the now-empty range. The resulting count is exactly D's
current width and at least two.

The contextual FALSE argument cannot create an extra cross-group inclusion:
the source update code visits targets registered at its symbol, which excludes
every outside target; initialization directly compares absent versus present
ranges; reverse repairs use the same symbol registry. Source-symbol deletion
clears the absent column, and unit conversion immediately removes the source
from live nonunit inference. Hence an empty contextual interpretation does not
turn a still-present cross-group source symbol into a stored FALSE bit.

A cross-group count-two callback can occur. At lines 416-420, both orientations'
restriction symbols are absent from the target, so both skip before selecting
another donor, entering `try_sse_2nd_order`, or restricting anything. This is
why these extra helper entries are harmless and must not be equated with
absence of all cross-group work. Same-group second-order candidate searches use
only the relevant symbol registry, so every recruited donor stays in the group.
Stale candidate snapshots and absent intersection ranges do not change that
vocabulary restriction.

### Initialization and the manual queue

Local pair initialization order is the restriction of global nested-loop order.
Unrelated pair visits neither change local flags/counts nor fire useful local
callbacks. Symbol-deletion row repairs can include outside rows, but their
callbacks retain the inert width argument. They cannot suppress, enable or
reorder a same-group callback through shared counters because each matrix cell
belongs to a particular pair and there is no cross-pair aggregate threshold.

The second-order flag changes only after the complete first-order sweep.
`which(!second_order_enabled_matrix, arr.ind = TRUE)` is column-major. An
increasing embedding preserves the relative order of all pairs within a group.
Cross-group queued pairs are inert. Their per-pair flags are distinct from
same-group flags; recursively repairing a local pair cannot consult an outside
pair as a donor because the symbol registries prevent it. Thus the manual queue
preserves local useful scheduling, including omissions preserved across passes.

### HLA and unit-HLA

At HLA entry, cross-group live donor counts equal their widths. They are at least
two, so an outside donor cannot be selected for an initially local virtual target.
Every selected donor adds only a pivot in the same group's vocabulary. Its symbol
registry updates only same-group donors, preserving the induction. The first
eligible local donor is unchanged by inserting any number of ineligible outside
donors into the remaining list.

For a unit target, an outside nonunit lacks the unit symbol and starts at its
full width, at least two. The unit's virtual expansion never reaches an outside
symbol, so outside counts cannot decrease and such donors are never selected.
Unit/nonunit target splitting uses the number of retained unit bindings solely
to split the length-sorted surviving list. There is one retained singleton per
binding and every live nonunit has width at least two, so extra units do not
move a target into the wrong category. Decreasing-width HLA order restricts to
the same local order, and each deletion changes only its own group registry.

### FALSE, ordering, and repeated histories

Until a FALSE signal, the useful local execution is the same. Its triggering
merge/restriction belongs wholly to one group. A separate FALSE-producing local
step must be reached globally unless another group has already returned FALSE;
finite inert work cannot starve it. Recognition therefore agrees even for
contradictory formulas that this heuristic does not recognize.

Surviving actual clauses never acquire new symbols, so group membership survives.
Their global return order projects to the exact local return order. Reapply the
simulation on each returned input to couple repeated calls. If a call has no
productive deletion or value restriction, its possible initial stable sort has
already put it in the exact same starting order for its next call. Therefore
productive behavior cannot resume after a nonproductive call. With no FALSE,
the number of productive global passes is the maximum of the local counts.
This is compatible with additional sort-only final calls and known two-pass
scheduling witnesses. Unused universe symbols never enter any work enumeration.

## Scope of artifacts and review limitations

All work is under this directory. No production source, existing tests, proposal
documents or other review streams were modified, and no commit was made.
The observer's semantics checks enumerate assignments directly and do not rely
on production subset caches. Normalization event comparisons cover all explicit
if sites, for sequences, scalar short-circuit operands, helper entries and actual
writes, plus freshly checked HLA events; they are not a claim of exhaustive paths.
Component comparisons use only same-group helper/write/HLA subsequences; outside
pair work is separately counted and constrained. A FALSE call's local events
are compared as prefixes because the global call may terminate before another
group finishes. Primitive operation traces and resource costs are excluded.
