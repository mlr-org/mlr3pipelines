# Diagnostic unit-propagation queue and the remaining recursive path

The in-memory queue variant removes the reversed unit-chain stack limitation:
16,384-symbol chains pass on R 3.6.3 and R 4.6.1. It does not make the whole
simplifier iterative. A separate guarded implication-chain family contains
no units at all and still overflows through nonunit subsumption callbacks.

All production source remains unchanged. `variant.R` builds three functions
by reading and evaluating source strings in private environments:

* `production`: the inspected original source;
* `no_skip`: synchronous propagation with its optional subset-matrix skip
  disabled, isolating the effect of removing that optimization;
* `queued`: immediate unit registration, followed by a non-reentrant FIFO
  drain for physical unit propagation; the matrix skip is omitted.

This is a reviewable diagnostic experiment and a conditional source-level
argument. It is not an installed fix or a claim that production should accept
the implementation without review.

The inspected production file's SHA-256 is
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.

## 1. Exact source of the unit-chain stack growth

Use Boolean symbols X1,...,Xn, seed `X1=1`, and implication clauses

    Ei = (Xi=0 or X(i+1)=1), i=1,...,n-1.

The conjunction is exactly the conjunction of all units `Xi=1`, by induction
from the seed. This supplies an analytic expected result even when truth-table
enumeration is infeasible.

In forward clause order, preprocessing visits Ei after its predecessor unit
is already registered. Its successor link is not yet in the symbol registry.
Each Ei becomes a unit, registers, finds no next registered link, and returns
before preprocessing advances. Chain length does not accumulate unit frames.

In reverse order, E(n-1),...,E2 have already entered the symbol registry when
preprocessing reaches E1. Restricting E1 by X1=1 deletes X1 and creates the
unit X2=1. Registration finds E2 already registered. Its restriction creates
X3=1, and so on, before any ancestor registration can return:

    register_unit(Xi)
      apply_domain_restriction(Ei, Xi, {1}, TRUE)
        eliminate_symbol_from_clause(Ei, Xi)
          register_unit(X(i+1))

The relevant source calls are at production lines 135, 163 and 251. Each
additional already-registered link adds exactly three named helper frames.
Thus the unit recursion depth satisfies `d(k+1)=d(k)+3`, with a fixed wrapper
offset. The preprocessing phase has no subset matrices yet, so neither HLA
nor the unit matrix-skip optimization causes this failure.

With the diagnostic observer/wrapper, the measured maximum `sys.nframe()`
was `3n+3` for n=8,16,32,64 on both R versions. R 3.6's native C stack usage
grew correspondingly, to about 2.15 MB at n=64; its 256-symbol run failed with
the C-stack-limit error. R 4.6's measured native C stack stayed roughly flat
while its R frame/evaluation depth grew; at n=1024 it reported **node stack
overflow**. These are different interpreter resource failures arising from
the same unbounded helper-call recurrence. Native C stack measurements alone
would not reveal the R 4.6 limitation.

## 2. Queue design and the scheduling change

The diagnostic preserves the first half of `register_unit()` unchanged:

1. A new symbol's unit is entered into `unit_registry` and `unit_domains`,
   and its clause is marked as a unit.
2. A new unit on an already constrained symbol is immediately intersected
   with the old unit. Empty intersection immediately returns contradiction;
   otherwise the old representative is updated and the new candidate is
   marked eliminated.
3. Only after this logical registration is complete, append the symbol name
   to `unit_work`.

If a drain is already active, registration returns immediately. Otherwise
it starts the drain:

    while unread queue entries remain:
      s = next symbol; advance the queue head
      for C in the current symbol_registry[s] snapshot:
        if C is eliminated: continue
        apply_domain_restriction(C, s, current unit_domains[s], TRUE)
        if contradiction: return contradiction
    clear the queue and reset its head

The existing domain restriction and all SSE/HLA helpers remain unchanged.
The current effective unit range is read separately for every target call.
Only symbol strings are stored in the queue; it does not retain delayed calls
or promises whose captured indices might later change.

Repeated queued symbols are intentional. If a nested registration strengthens
U_s after some targets were already processed, the new queue entry revisits
those earlier targets. Later targets in the current snapshot already receive
the latest U_s. No deduplication argument is assumed, and the optional matrix
skip is not reused without a new proof for this scheduling discipline.

An outermost registration still returns only after all its queued physical
work has drained. A registration inside an active drain returns after logical
registration and enqueuing, before its own physical work. This is the precise
behavioral change to the internal helper contract.

At most one drain is active. During it an inner `register_unit()` cannot
re-enter the drain. Consequently there are at most two simultaneously active
registration helpers: the outer registration that owns the drain and the
current short inner registration. On the simple reversed chain, observed
maximum frame depth was 13 for every n=8,16,32,64, and maximum pending queue
length was one. This does not bound recursive nonunit callbacks inside a
physical restriction; Section 6 supplies an independent counterexample.

## 3. Normal-return assumptions and touched invariants

The following argument concerns the exact inspected source and diagnostic
builder, ordinary R list/vector copy semantics, and canonical finite kernel
inputs. Domains are finite nonempty unique character sets; clauses have unique
valid symbol names; each present literal is a nonempty proper subset of its
domain; logical constants are well formed. Malformed class-tagged lists,
duplicate/NA names, NULL ranges, mutable external changes to the universe,
unsupported coercions and arithmetic/index overflow are excluded.

“Normally returned” means the function completes through its specified return
paths without interpreter stack overflow, memory exhaustion, interruption or
other runtime error. A returned FALSE may legitimately abandon pending work:
once a contradiction has been certified, that work is unnecessary. Statements
about an empty queue, physical containment and HLA saturation below concern
the nonconstant return path, or the explicitly indicated completed drain.

### Immediate logical unit context

At every callback boundary all registered units already appear in the actual
active formula, either as fresh unit clauses or as intersections stored in
old representatives. Enqueuing changes no formula. Delaying physical unit
propagation therefore does **not** delay the availability of a unit as a
logical assumption. This was the load-bearing requirement in the task.

Every physical restriction remains sound: under the asserted unit U_s, a
literal C_s can be replaced by C_s intersect U_s. If U_s is contained in C_s,
the entire clause is implied and may be removed. An empty resulting literal
may be deleted; a resulting singleton is registered immediately. An empty
unit intersection is a direct contradiction, independent of any undrained
work. These statements do not require the raw ranges of other clauses to
have been physically restricted yet.

### Contextual soundness of consumed FALSE comparisons

Raw subset comparisons may temporarily be inaccurate while physical work is
pending. The invariant needed by intermediate SSE rewrites is instead

    FALSE(A -> B, s) implies
      A_s intersect U_s is contained in B_s intersect U_s,

where U is the conjunction of the currently registered units.

Immediate unit registration can only strengthen U. Intersecting both sides
with a common smaller set preserves every existing FALSE certificate.
Source shrinking preserves certificates; nonunit target shrinking refreshes
reverse FALSE bits before callbacks; unit target shrinking preserves the
certificate under the already asserted unit; deleting a source symbol clears
its column and repairs reverse comparisons before callbacks. Those code paths
are unchanged by the queue. Pair construction still creates direct raw
certificates. Thus delaying physical work does not by itself invalidate a
logical rewrite premise consumed from a FALSE bit.

The explicit second-order intersection test uses raw sets, which is sufficient
for its contextual form: intersecting its raw inclusion with U preserves
the inclusion. All actual rewrites therefore retain the same contextual
logical-schema justification as production. A failed raw guard can defer a
useful simplification, but cannot authorize an unsound one.

### Lifecycle and registry coverage

New units are marked inactive as nonunits before registration returns, even
when physical work is queued. Symbol elimination removes both the deleted
symbol's and the retained singleton symbol's registry entries before calling
registration. A same-symbol stale snapshot finds a merged candidate eliminated;
a different-symbol stale snapshot finds its old symbol absent and returns
without another restriction. The existing SSE caller checks still reject
eliminated/unit operands before new rewrites and after recursive callbacks.

Thus a clause cannot be registered repeatedly as a fresh unit, and no stale
registry snapshot reintroduces a unit as a live nonunit. The queue has no
operation that grows ranges, adds symbols, reactivates a clause or modifies
the registry independently of the existing helpers.

### Finite queue work and physical **proper** containment

Every queue insertion comes from one initial unit or one clause's first
conversion to a unit. Each such clause is then permanently a unit or
eliminated. The total number of insertions is bounded by the input clause
count; repeated symbol names do not violate this bound. Each dequeue handles
a finite registry snapshot. Normal completion of the unchanged nonunit
recursive calls plus this finite bound establishes termination of the drain.

Each registering constraint queues its symbol. A target still registered
when that entry is consumed is explicitly restricted using a range contained
in the queued birth constraint. A later tighter unit has its own queued visit,
so a previously processed target is not missed. A target removed or losing
that symbol needs no more work. During initial preprocessing, not-yet-registered
clauses receive all current units when their own iteration starts; a new
unit cannot be produced midway through processing a still-nonunit clause
while subset matrices are NULL.

Consequently every live registered range is physically contained in every
applicable unit after a completed drain. Before HLA every live nonunit is
registered and all drains have completed. Since the diagnostic omits the
matrix skip, any target range containing a propagated unit is eliminated by
the domain restriction's equality branch. Hence surviving ranges are
**proper** subsets of all applicable final units. This is stronger than the
production invariant, which permits the known equality gap.

This reasoning does not reuse the production birth-order certificate to
justify skipped work: there is no skipped physical restriction in the
diagnostic. That simplification is one reason to keep the no-skip control.

### Exact comparisons, counts and consumed zero pairs before HLA

The queue never writes pair matrices or counters directly. Their update
sites remain the original ones. An incorrect TRUE bit for a live initialized
pair must result from a source shrink and is covered by that shrink's pending
finite update frame. Those frames either finish their updates or make the
source inactive. Delayed unit work introduces later source changes with their
own update frames, not unowned debts. FALSE certificates become raw-correct
once physical unit containment holds. The original guarded one-at-a-time
increments/decrements preserve count sums.

Every decrease to count zero still reaches the ordinary deletion handler,
possibly on a finite pending callback list. Such a list can be abandoned only
when its source becomes inactive or contradiction ends the computation.
The queue does not change this handling. At the start of HLA no unit drain
or pair-update callback remains outstanding, so all live pair bits/counts
are raw-exact and every distinct live nonunit pair has positive count.

The HLA loops are unchanged. The source-level result from
`../review_hla/REVIEW.md` then applies: the only possible residual propagation
refutations would be targets directly subsumed by final units. Proper final
containment excludes those too. **Conditional on these invariant arguments
and normal canonical execution, the no-skip queue output is fully saturated
for domain-refutation clause deletion.** This says nothing about general
entailment or fixed-point saturation of SSE1/SSE2 literal restrictions.

## 4. Saved-example and semantic checks

`semantic_checks.py` exercises 16 saved canonical examples, three finite
fan-out families and 1,000 seeded formulas. Each of the three variants is
checked against the original input using two independent complete-valuation
oracles: ordinary Boolean SAT with one-hot value encoding, and a reduced
multivalued decision diagram. Production output is a comparison baseline,
not the semantic oracle. Each variant's instrumented output is additionally
checked identical to its uninstrumented output.

All 1,019 formulas passed both semantic oracles for all three variants.
The queue matched the synchronous no-skip control's canonical result in
every case. Both controls differed from production on exactly the known
`minimized_subsumption.json` unit-equality example, removing its redundant
nonunit. This isolates that improvement to disabling the optional skip;
it is not evidence of a new completeness benefit from FIFO order itself.

The queue audit checked 94,759 consumed contextual FALSE comparisons,
38,362 exact live pairs before HLA, and 3,968 unit-containment pairs at
completed-drain/HLA boundaries. It reached 895 HLA boundaries and 1,072
completed drains. All lifecycle, queue-empty, raw-exactness, count-positive
and proper-unit-containment assertions passed. There were no final
domain-refutation redundancies in either no-skip variant; production retained
the one directed unit-equality redundancy.

The same two SSE1 and five SSE2 residual operations survived in all three
variants. They include `minimized_first_order_phase_sse1.json`,
`minimized_sse1.json`, `minimized_sse2.json`, and the directed oneend examples.
The queue changes neither the missing nonunit event triggers nor their static
candidate logic, so it does not establish full literal-reduction saturation.

All 19 saved/fan-out cases were then repeated on R 4.6.1 with both semantic
oracles and the same source-level assertions. They passed with the same
structural comparison and residual-operation results. Its queue audit checked
1,356 contextual FALSE comparisons, 142 quiescent pairs and 46 physical
containment pairs. These cross-version results are saved separately in
`semantic_results_r46.json`.

Structural equality in this test corpus is not a general theorem. Queue order
changes intermediate registrations and callback counts, as the results show;
the source's reduction scheduling is already nonconfluent/incomplete in other
respects. The proved scope is semantic preservation and the stated normal-return
invariants, not identical clause order, unit representatives, traces, cost or
intermediate reduction opportunities on arbitrary inputs.

## 5. Resource results

The probes use raw canonical kernel inputs and the single production
simplifier, so constructor overhead is outside their core elapsed times.
No R stack limit, expression limit or evaluator option was increased.

| Version | Family and variant | Size | Result | Core seconds |
|---|---|---:|---|---:|
| R 3.6.3 | Original unit chain, forward | 4,096 | Exact all-unit result | 0.899 |
| R 3.6.3 | Original unit chain, reverse | 256 | C-stack-limit error | 0.149 |
| R 3.6.3 | Queued unit chain, reverse | 4,096 | Exact all-unit result | 0.409 |
| R 3.6.3 | Queued unit chain, reverse | 16,384 | Exact all-unit result | 1.397 |
| R 4.6.1 | Original unit chain, forward | 4,096 | Exact all-unit result | 1.494 |
| R 4.6.1 | Original unit chain, reverse | 1,024 | Node-stack overflow | 0.189 |
| R 4.6.1 | Queued unit chain, reverse | 4,096 | Exact all-unit result | 0.461 |
| R 4.6.1 | Queued unit chain, reverse | 16,384 | Exact all-unit result | 1.531 |

These are measurements of this environment, not new universal thresholds or
a general performance claim. `resource_results.json` records 44 process-isolated
measurements, including the frame-growth series and the guarded failures below.
Queue storage is finite, bounded by the number of unit births, but this
experiment does not prove linear total time or eliminate the simplifier's
quadratic matrices and other memory costs.

## 6. Independent nonunit recursion family

Introduce one additional Boolean guard G and replace the seed and every link
by

    B = (G=1 or X1=1)
    Ei = (G=1 or Xi=0 or X(i+1)=1).

Again store the links in reverse order. The exact formula is

    conjunction over i of (G=1 or Xi=1).

For G=1 both forms are tautological; for G=0 the previous unit-chain induction
applies. Every resulting clause has two symbols, so no unit propagation is
needed. The shared guard never becomes exceptional relative to another
clause because all guard ranges are identical.

The binary seed is compared before the ternary links. When the last-visited
E1 meets B, SSE1 deletes its false X1 literal, leaving `G=1 or X2=1`.
Its relation to E2 then deletes E2's X2 literal, leaving `G=1 or X3=1`.
The earlier links' matrices are already initialized, so successive symbol
deletions trigger recursive count-decrease callbacks:

    eliminate_symbol_from_clause(Ei, Xi)
      on_updated_subset_relations(Ei, E(i+1), FALSE)
        apply_domain_restriction(E(i+1), X(i+1), {1}, FALSE)
          eliminate_symbol_from_clause(E(i+1), X(i+1)).

The source calls are at lines 285, 322 and 163. The clause remains nonunit
at every level; `register_unit()` is never entered. Each additional recursive
link again adds three helper frames. A short initial pair-construction step
has a different fixed offset because the final outer clause's future pairs
are not all initialized yet. With the probe wrapper the measured maximum
was `3n+2` for n=8,16,32,64, in both original and queued variants, and the
registration observer recorded zero calls throughout.

At n=256 on R 3.6 both variants fail by native C-stack exhaustion; at n=1024
on R 4.6 both fail by node-stack overflow. The R 4.6 guarded probes spend
about 30–31 seconds building/processing their nonunit pair structure before
failure. The queue cannot change this family because it has no work to do.

Other recursive edges remain too: nonempty range updates can call the subset
handler at 224, second-order handlers can invoke `apply_domain_restriction()`
at 467, and their resulting updates can cascade. The guarded family already
proves that a global recursion-free guarantee is false, without needing to
speculate that every remaining recursive edge supports the same exact family.
Removing the whole limitation would require a separate worklist/continuation
design for nonunit callbacks, with new invalidation and scheduling proofs.

## 7. Reproduction and remaining scope

From the repository root:

    python3 attic/cnf_verify3/unit_queue/resource_sweep.py
    attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/unit_queue/semantic_checks.py
    attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/unit_queue/semantic_checks.py --r46 --saved-only

The resource and cross-version commands reuse the already provisioned
`cnf-review-r46` container; the host semantic command reuses the existing
independent solver environment containing Z3. No packages or environments
were installed or modified by this follow-up. A plain `python3` semantic
attempt failed immediately because the system Python has no Z3; the existing
solver virtual environment is the intended interpreter for those oracle checks.

`chain_probe.R` can also be run directly with `CNF_CHAIN_N`,
`CNF_QUEUE_VARIANT=production|no_skip|queued`,
`CNF_CHAIN_FAMILY=unit|guarded`, `CNF_CHAIN_ORDER=forward|reverse`, and
`CNF_CHAIN_MEASURE=1` for small frame traces. Base R suffices for this probe.

The proof obligations touched by replacing recursive physical propagation
have been separated above: immediate logical context, contextual cache
soundness, permanent inactivity, registry completeness, finite queue work,
strict physical containment after draining, exact quiescent matrices/counts,
zero-count scheduling, and unchanged HLA completeness. The finite audits
exercise those obligations but are not a mechanized proof of arbitrary R
execution. Runtime resource errors, accepted malformed public objects,
unrelated constructor/operator API defects and general nonunit recursion
remain outside the successful scope.
