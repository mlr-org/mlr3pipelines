# Finding ledger and evidence boundaries

Live synthesis for the unchanged `09770eaa` CNF sources. This ledger separates
changed truth values, incomplete application of implemented rules, public
representation failures, and runtime limits. Older dated reports remain as
the historical record; the links here point to the strongest current evidence.
No production repair has been applied.

## 1. Two accepted public paths can change a formula's truth function

### Dimensional clause subsetting

**Status: independently reproduced on R 3.6.3 and R 4.6.1, including normal
installed-package execution.** The four-clause input below is contradictory,
but construction returns `X=c AND Y=a`:

```r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("a", "b", "c"))
c1 = as.CnfClause(X %among% c("b", "c"))
c2 = X %among% "b" | Y %among% "c"
c3 = X %among% "c" | Y %among% "b"
c4 = X %among% "a" | Y %among% "a"
CnfFormula(list(c1[matrix(c(1L, 1L), nrow = 1L)], c2, c3, c4))
```

Repeating the same disjunct cannot alter `c1`. Together `c1` and `c4` force
`Y=a`; then `c2` requires `X=b`, while `c3` requires `X=c`. The output's
accepted assignment `X=c,Y=a` therefore separates its meaning from the input.
Flattening the selector to `c(1L,1L)` yields the correct FALSE result.

At `R/CnfClause.R:212`, `unclass(i)` retains a matrix's dimensions. The
following `unique()` removes duplicate rows instead of repeated individual
indices. The accepted selector creates duplicate symbol names, and later
kernel updates assume unique names and touch only the first occurrence.
The example's final wrong output itself has unique names, so its interpretation
does not depend on choosing a convention for malformed output storage.

Matrix/array selectors are accepted public inputs, although the documentation
does not explicitly promise support for those shapes. This is a public
normalization failure; it does not contradict the canonical-input theorem.
Rejecting dimensions or flattening before uniqueness would address this path.
The investigation has not chosen or installed either repair.

Evidence: [independent normalization study](r_values/NOTES.md),
[root truth-table replay](root/reproduce_selector_semantics.R),
[24-mode package/runtime comparison](execution_modes/NOTES.md).

The [small-selector follow-up](small_selectors/NOTES.md) also gives a
**two-clause runtime error**: duplicate a unary `X=a` clause with the matrix
selector and conjoin the ordinary unit `X in {a,b}` on a ternary domain.
Unit HLA computes count one but an all-FALSE mask, then tries an empty symbol
lookup. A different two-clause input leaves a stale duplicate range whose
positional disjunction is wrong even though first-name lookup hides it.
Both reproduce on R 3.6.3 and 4.6.1. One selected proper clause is unchanged,
so two clauses are minimal for these two categories in that selector class.
The four-clause canonical failure is not claimed globally minimal.

The [event-level source trace](selector_semantic_trace/NOTES.md) identifies
the first wrong operation precisely. SSE2 first narrows only c1's first X
copy, leaving heterogeneous ranges. A later SSE1 callback selects the cached
trailing occurrence but reads the first range by name and deletes c2; this
first admits `X=c,Y=a` positionally. Removing c1's first occurrence later
exposes the stale range and also breaks the first-name projection. Neither
failure originates in HLA. The two R versions have identical lossless traces,
and 103 focused expectations check exact states and current live contexts.

### Unicode symbol names in a C character locale

**Status: independently reproduced on R 3.6.3 and R 4.6.1, including actual
installed, uncompiled, and development package namespaces.** This later
discovery has canonical clause shapes and needs no selector or mixed encoding.
Under `LC_CTYPE=C`, use a marked UTF-8 name for X, ordinary name Y, and ASCII
domain values:

```r
u = CnfUniverse()
X = CnfSymbol(u, "\u00e9", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("a", "b"))
CnfFormula(list(
  as.CnfClause(X %among% "a"),
  X %among% "b" | Y %among% "a",
  X %among% "c" | Y %among% "b"
))
```

The input is contradictory. The result drops the unit X=a and admits exactly
X=c,Y=a and X=b,Y=b. In `LC_CTYPE=C.UTF-8`, the same construction returns FALSE.
The [standalone reproduction](character_identity_review/reproduce_public.R)
sets and restores the locale and independently checks all six assignments.

R's native environment key for that name enumerates as literal ASCII
`"<U+00E9>"`, while direct access by the original marked name still works.
At source line 502, intersecting clause names with `names(unit_domains)` misses
the registered unit. At unit HLA, the lazy counts then assume containment
which propagation never established; line 777 first changes truth by deleting
the unit. Both UTF-8 and Latin-1 names reproduce. Current R emits translation
warnings and still returns the wrong result; R 3.6.3 is silent.

Root's [independent finite boundary and package replay](root/CTYPE_UNIT_REVIEW.md)
checks 33,684 public calls per runtime: every ordered list of at most three
clauses over a ternary X/Boolean Y palette, crossed with ASCII/UTF-8 X names
and C/C.UTF-8 character locales. Only the C/UTF-8 triples fail: 612 of 8,000,
including 36 contradictory inputs becoming satisfiable and 576 satisfiable
inputs gaining assignments. A private direct-binding lookup at line 502
passes the same complete bank, but is not a complete public-name repair.

The [character-identity study](character_identity_review/FINDING.md) separately
shows accepted UTF-8/Latin-1 aliases can create duplicate ordinary names under
C, and literal ASCII escape names can collide with Unicode native keys.
The correctness proofs now explicitly require faithful symbol identity across
membership, list access, environment access, and environment enumeration.
Syntactic canonicality alone does not imply that contract. No production fix
has been applied to either semantic failure.

## 2. Four independent conditions fail to schedule useful simplification

**Status: reduced canonical examples, source traces, independent truth-table,
SAT and decision-diagram checks.** Each first output preserves all models but
still admits a specific rule implemented by this simplifier. This is stronger
evidence than observing two different equivalent normal forms.

| Omitted condition | Small saved witness | Consequence |
| --- | --- | --- |
| An SSE1 donor range shrinks while its exceptional comparison stays TRUE | Four clauses, two symbols; `minimized_first_order_phase_sse1.json` | An existing first-order restriction is not repeated, already during the first-order phase. |
| Target shrink raises a reverse donor count from one to two after the manual queue is built | `minimized_sse2.json` | A newly eligible second-order pair is not enqueued. |
| An already-contained donor range shrinks, reducing the union used by SSE2, without a comparison flip | `directed_oneend_shrink_min.json`, `minimized_oneend_symbol_removal.json` | Both partial range shrinkage and complete literal removal can leave a second-order restriction pending. |
| Nested unit registration reads a stale outgoing TRUE while its incoming FALSE only establishes containment | Three clauses, two occurring symbols; `minimized_subsumption.json` | Equality is mistaken for strict containment, leaving a clause directly subsumed by a retained unit. |

All named JSON files are under `independent_solver/`. The last condition
survives the previous campaign's unit-merge fix. The incoming FALSE remains
sound in context, so this is a strictness/scheduling failure, not an invalid
logical inference. The [root replay](root/replay_findings.R) checks all saved
small formulas through public constructors and direct valuation enumeration.

The [diagnostic source copies](independent_solver/CANDIDATE_REPAIRS.md)
exercise focused scheduling changes without altering production. They are
evidence about causes and possible repairs, not a claim that a final efficient
implementation has been selected.

The [independent scheduling review](scheduler_review/REVIEW.md) now completes
the diagnostic saturation arguments. It proves the necessary pending-visit
invariant for count-zero pairs, handles zero-to-one transitions, and establishes
final comparison stabilization for a surviving SSE2 witness. With the stated
extra notifications and strict unit-length guard, all listed local rules are
saturated on normal return under the canonical-input prerequisites. This is
a correctness result for those exact source copies, not a performance claim.

## 3. No fixed number of repeated passes repairs scheduling in general

**Status: explicit family, source-level induction, completed independent
review.** For every positive integer `n`, the family in
[the repeated-pass proof](repeated_passes/PROOF.md) needs `n` productive
passes. Each pass removes one value occurrence. The same family with its
target clauses in forward order needs one pass. All its clauses are
indispensable, with explicit separating valuations, so HLA cannot shortcut
the construction by deleting a clause.

The experiment confirms `n=1,2,3,4,5,8,12,16,24,32`. A reduced seven-clause
example has only 64 valuations and needs three useful passes, followed by one
pass that only changes storage order. Thus a blanket “run it twice” change
would leave unbounded cases unfinished.

The [independent review](quotient_review/REPEATED_PASS_REVIEW.md) checks the
unique-useful-rule classification, source queue order and callback omissions.
Its separate installed-package reconstruction confirms 27 frontier states,
251 clause witnesses, 876 protected-value witnesses and 51,168 truth rows.

Actual stored values only disappear. Counting original membership-cell
occurrences gives the sharper reviewed bound
`sum_s m_s * 2^(m_s - 1)`, where `m_s` counts input clauses mentioning symbol
`s`. Thus fixed clause and symbol counts exclude unbounded productive passes
even as domain sizes increase. See the [fiber potential](root/DOMAIN_INDEPENDENT_PASS_BOUND.md).
A [clause-count-only proof](pass_bound_review/PROOF.md) further removes
symbol-count dependence; its [independent review](pass_bound_check/REVIEW.md)
confirms the coarse bound `m + m * 2^(m^2 + m)` and a sharper preorder-count
bound. Groups of at least three symbols with the same original support and
directed inclusion signature are frozen against actual range changes. A final
sort-only pass may be needed for exact storage stability. These results do
not claim a unique or logically minimal output.

## 4. Two separate recursive paths exhaust the runtime stack

**Status: ordinary finite-domain formula families on two R versions.** A
reversed implication chain forces linear-depth nested unit registration;
the same edges in forward order do not. R 3.6.3 fails around 256 symbols with
C-stack exhaustion; R 4.6.1 fails at 1,024 with node-stack overflow in the
recorded environment. Forward order succeeds at 4,096.

A second family adds a common guard literal to every clause, keeping every
clause non-unit. Its depth comes from
`eliminate_symbol_from_clause -> on_updated_subset_relations ->
apply_domain_restriction`. It fails at the same measured scales and never
calls unit registration. A temporary physical unit queue therefore cannot
resolve this second cause.

The [unit-queue study](unit_queue/) separates immediate logical registration
from queued physical propagation in an in-memory source copy. It handles
16,384-symbol reversed unit chains with constant measured helper depth, while
the guarded non-unit family still overflows. Resource thresholds depend on
the R environment; the linear recurrence is the general result. Finite
mathematical descent does not guarantee successful bounded-stack execution.

The new [recursive-progress proof](root/RECURSION_PROGRESS_BOUND.md), with
[independent source review](pass_bound_check/RECURSION_REVIEW.md), excludes
unpaid cycles of the kernel's local helper closures. Each active restriction
ancestor commits a strict live fiber decrease before another restriction can
be entered. There are at most `W0+1` active restriction frames and
`6*W0+11` local helper frames, including the lazy union-evaluation edge.
The alternative signature-class potential gives clause-count-only dependence.
These are mathematical bounds for the inspected local graph, not sufficient
machine-stack sizes or bounds on arbitrary caller computation.

## 5. Other public-boundary failures

| Behavior | Evidence and scope |
| --- | --- |
| `TRUE | proper_CnfClause` returns bare logical TRUE | `|.CnfClause` returns its raw left operand on one short circuit. Truth is correct; subsequent typed constructor composition fails. Reproduced across all 24 runtime configurations. |
| Missing clause selectors are accepted | Missing `any.missing=FALSE` allows logical NA, and all-missing numeric or character selectors through the logical validator, to create an NA symbol name and NULL range. The resulting object has no well-defined ordinary finite-domain interpretation; a clause round trip can become TRUE. |
| Byte-marked character mixtures are not uniformly operable | Accepted strings can fail membership or printing in base R. This is a representation/runtime boundary of total character equality, separate from ordinary finite-set semantics. |
| Equivalent UTF-8/Latin-1 representations can make identical formulas compare unequal | A two-clause public example satisfies `identical(f,g)` but fails `all.equal(f,g)` on both R versions, even in locale C. Serialized digest ordering distinguishes encoding marks, reverses clause alignment and causes false differences. |
| Locale collation ties defeat comparison normalization | Reversing a proper range containing distinct precomposed/decomposed Unicode values produces Atom/Clause/Formula comparison false negatives in C.UTF-8 and en_US.UTF-8. Distinct-symbol-name ordering also fails, including delegated comparison of equal universe binding maps built in opposite orders. Formula truth is unchanged. |

See [representation findings](representation/NOTES.md) and the independent
[R-value review](r_values/NOTES.md). Existing issues in
[`../cnf/CLAUDE.md`](../cnf/CLAUDE.md) and its companion review are retained as
prior findings: TRUE-clause `as.list`, documented list names, universe lookup,
neutral-constant universe selection, and FALSE after nested-formula flattening.
They are not credited as discoveries of this campaign.

The new [comparison reproductions](root/COMPARISON_NORMALIZATION.md) distinguish
these ordinary Unicode normalization failures from byte-marked strings or
different logical normal forms. All 24 pairs per R version preserve their
independently specified meanings; 13 comparison false negatives occur. A
private UTF-8/radix normalization candidate passes these examples. The completed
[independent review](comparison_review/REVIEW.md) checks 39 reduced pairs and
15,552 permutation/encoding comparisons per runtime, including 3,888 unequal
controls. The latter bank has 5,256 production false negatives and no candidate
disagreements. Private copies retaining the production guards preserve 36
additional scope outcomes. Equal universe binding maps can still compare
unequal through base environment sorting; payload-only normalization leaves
that demonstrated residual case. No digest collision is needed for either
confirmed cause, and no production correction has been installed.

A concrete [private selector correction](root/SELECTOR_CANDIDATE.md) flattens
atomic selectors before deduplication and rejects missing logical selections.
It passed 111,695 calls on each R version: every accepted result was canonical,
preserved the independently evaluated selected disjunction, and matched the
existing flat nonmissing selector behavior. It also resolves the saved
four-clause semantic failure and two-clause unit-HLA error. This is a tested
boundary proposal; no production repair or blanket public-API claim is made.

## 6. What can be excluded, and under which assumptions

Unless a row explicitly studies a broken representation, the semantic and
saturation theorems below use the ordinary finite-set contract, including
faithful symbol identity across every list/registry operation. The C-locale
counterexample makes that qualification necessary even for canonical shapes.

| Scope | Evidence level and result |
| --- | --- |
| Every local unit, subsumption, SSE1, SSE2, and HLA inference | Universal pointwise set-algebra proofs, with exhaustive checks of the Boolean schemas. |
| Normally returning kernel on canonical finite sets with faithful symbol-map identity | Independently reviewed [source-level preservation proof](independent_solver/SEMANTIC_PRESERVATION_MAP.md). Contextual FALSE bits, chronological unit-birth certificates, quiescent exactness and lazy HLA rows supply the mutable-state premises. Registry name enumeration must agree with clause identity; the new C-locale example violates that substantive premise. This is not a mechanization of R. |
| Canonical kernel with faithful symbol-map identity, ordinary total primitives, representable arithmetic/indices and sufficient resources | Reviewed [total-correctness composition](root/TOTAL_CORRECTNESS_COMPOSITION.md): a prefix proof excludes consumed-index/shape/condition failures, finite progress bounds exclude infinite execution, and the semantic theorem then applies. The name-identity and resource premises are necessary because the Unicode and stack failures are real. |
| At most two clauses, arbitrary symbols and finite domain sizes | Semantic preservation, first-order saturation, and idempotence modulo order proved in [TWO_CLAUSE_PROOF.md](independent_solver/TWO_CLAUSE_PROOF.md). |
| At most three clauses and two occurring symbols, arbitrary finite domain sizes | Complete 520,200-execution membership quotient, independently reviewed lifting argument, and two distinct exact oracles on every result. No semantic failures; 32 known unit-equality leftovers. |
| Three clauses of the `(3,3,2)` occurrence shape, arbitrary finite domain sizes | Complete 9,386,748-execution quotient and 547,476,480 valuation checks, with independently reviewed ordering, lifting and assignment grids. No semantic differences. Every proper input in this shape is satisfiable. |
| Three clauses each containing all three symbols in aligned order, arbitrary finite domain sizes | Complete 7,189,057-input quotient, 9,731,786 production calls and 849,165,820 valuation checks. Truth preserved; at most one productive pass, hence first-result local saturation modulo clause order. A sort-only second call is possible. Arbitrary independent symbol orders are outside this enumeration. |
| Three clauses each containing the same three distinct symbols, with arbitrary clause and internal symbol orders | Separate [source proof](three_full_clauses/THREE_FULL_CLAUSES.md) and [independent scheduling review](three_full_review/REVIEW.md) establish first-call local saturation. Value projection excludes productive unit restrictions; incidence and exact initialization/deferred-tail arguments close the remaining SSE1/SSE2 obligations. This result does not depend on extrapolating the aligned-order enumeration. |
| Hidden domain-refutation opportunities among final survivors | [Independent HLA review](review_hla/REVIEW.md): every residual refutable clause is directly subsumed by a different surviving unit, and conversely. This excludes other residual HLA/domain-propagation cases, not arbitrary logical redundancy. |
| Static second-order candidate pruning | Complete relative to saturated earlier rules; the demonstrated omissions are dynamic scheduling conditions. |
| Ordinary repeated, named, dimensional and inertly attributed domain storage, with canonical actual clauses | [Domain normalization proof](domain_storage_contract/PROOF.md) and [independent review](normalization_component_review/REVIEW.md) establish exact actual output vectors and source decision schedules under flat-unique normalization. Virtual ranges may contain duplicates and partial multiplicities; bounded capacity plus a physically missing donor value keeps both HTE length predicates FALSE. |
| Public constructors and Boolean operators over ordinary named/repeated/dimensional domain and atom storage, with faithful symbol identity | [Constructor closure proof](constructor_domain_closure/PROOF.md) and [independent review](constructor_closure_review/REVIEW.md) show the specified successful grammar establishes canonical actual ranges before each kernel entry. `unique.matrix` preserves scalar first-occurrence order; clause accumulation and both R versions' complement paths flatten/deduplicate. Known constant, selector and dispatch exceptions remain explicit. |
| Equivalent valid string encodings and collation changes with faithful native symbol identity | [Source-prefix simulation](character_identity_review/PROOF.md) preserves exact ordered payloads and all inspected source decisions. Distinct precomposed/decomposed strings stay distinct. 2,080 public formulas per R version calibrate the proof; ordinary equality preservation alone is insufficient under C CTYPE. |
| Disjoint input symbol components under arbitrary global clause interleaving | [Component simulation](root/COMPONENT_SEPARABILITY.md) and [independent review](normalization_component_review/REVIEW.md) establish exact ordered output projections, equivalence of actual FALSE recognition, and repeated-pass coupling. Productive global pass count is the maximum of component counts when none returns FALSE. Unused symbols have no effect. Runtime and complete global event streams are not claimed equal. |
| Initial unit-propagation prefix on canonical unary-range disjunctions | [Source correspondence](root/SEMILATTICE_COMPLETENESS.md) and [independent relational review](semilattice_review/REVIEW.md) establish the greatest generalized arc-consistent domain box, or failure. The proof preserves all common GAC sub-boxes, not merely full models. Later cached-unit scheduling gaps are outside this prefix. |
| Each literal range meet closed, with at most one non-downset range per clause in chosen finite semilattices | The same [reviewed theorem](semilattice_review/REVIEW.md) proves initial propagation decides satisfiability and permits external extraction of the least model. Ordered multivalued Horn is a special case; nonchain examples strictly extend it. Whole-formula meet closure alone does not suffice. |
| Source versus package/bytecode execution | Exact agreement for the saved fixtures across 24 measured modes; not a universal compiler theorem. |
| Formula AND/OR/negation on canonical operands | [Operator proof](operator_proof/README.md) establishes semantic and proper-representation closure on normal return, subject to the kernel theorem. Public constructor failure paths and Clause result-class loss remain explicit exceptions. |
| Negation of a contradictory canonical formula | Complete distribution and individual-clause tautology removal force `!F` to be literal TRUE on normal return, even with an identity simplifier. This does not make the ordinary simplifier a complete SAT procedure. |

The three-symbol shape quotient completed at 13:29:03 UTC. Its exact scope,
counts and independent calibration are in
[THREE_SYMBOL_QUOTIENT.md](root/THREE_SYMBOL_QUOTIENT.md).
The separate full-occurrence quotient completed at 15:50:08 UTC; its reviewed
scope and final validated counts are in
[FULL_THREE_SYMBOL_QUOTIENT.md](root/FULL_THREE_SYMBOL_QUOTIENT.md).

The [independent structural review](structural_review/REVIEW.md) establishes
incidence-forest saturation, incidence-pseudoforest and Boolean renamable-Horn
contradiction recognition, and local saturation for binary clauses selecting
equal-or-disjoint range blocks. Forests additionally have explicit witnesses
for every surviving clause and literal value. The pseudoforest and Horn
results do not imply complete forced-value extraction.

The exact [Boolean 2-CNF graph criterion](structural_classes/BOOLEAN_2CNF_GRAPH.md)
also passed its [independent source review](boolean_graph_review/REVIEW.md).
For a proper unit/binary input list, let `K` be implication-graph closure of
initial units and every literal reachable from its complement in at most
three input edges. FALSE is returned exactly when `K` has both polarities;
otherwise the final unit set is exactly `K`. Scalar FALSE is handled
separately. Opposed four-edge cycles give an eight-clause contradiction with
empty `K` that remains unchanged, separating this exact characterization
from complete SAT recognition. The review checked 3,088 independent cases
on each R version in addition to the author's 8,238 checks.

The [minimum-boundary proof](binary_minimal_boundary/README.md) now sharpens
this example: eight proper binary clauses and four used variables are both
necessary and attainable for an unrecognized Boolean unit/binary contradiction.
An inconsistent four-variable parity cycle attains both minima and is
unchanged by production. The clause lower bound reduces to a seed-free
minimal core and completely enumerates 95,743 finite signings; all 3,442
seed-free candidates with at most seven clauses are satisfiable. Root's
[independent review and second formulation](root/BINARY_MINIMUM_REVIEW.md)
checks every reduction and reaches the same bound using separate finite
clause-selection constraints plus a degree-two-cycle argument. Recorded
solver timeouts are not counted as exclusions.

The conditional-site study has observed 212 of 216 outcomes across all 108
kernel `if` sites, with exact output comparison against the original function
in 30,034 cases. Four unobserved outcomes have independently reviewed source exclusions:
emptying a singleton through the nonunit deletion helper, deleting a unit
through the ordinary deletion helper, and the two immediate HLA full-domain
branches. See [branch obligations](root/BRANCH_OBLIGATIONS.md) and the
[independent catalog/source review](branch_review/REVIEW.md). This covers
feasible individual `if` outcomes, not all paths or short-circuit combinations.

The [nested-comparison control](root/QUEUED_COMPARISON_CONTROL.md) proves why an
existing reentrancy guard is necessary. Removing it in a private source copy
decrements a count twice for the same bit and fails on an 11-clause input.
Production preserves its two models; the new focused regression passes.
This is evidence supporting an existing correct guard, not a production bug.

The new [indexing review](index_contract_review/REVIEW.md) independently checked
19,787 canonical inputs per R version and 7,176,512 row/count boundaries,
including stale unit snapshots, optional matrix allocation, delayed inverses,
and the matrix-to-vector phase transition. It also checks finite write/repair
intervals separately from dispatch boundaries. Some intermediate `[` results
contain intentional NA values; the source filters them before a consuming
index operation. Eight changed copies calibrate these observations. The
associated [composition review](index_contract_review/COMPOSITION_REVIEW.md)
supplies a conservative finite bound on every local-helper activation, not
only maximum active depth.

Further independent rounds establish the following scoped results:

* [Value-set symmetry](set_symmetry/PROOF.md), with its
  [independent review](symmetry_review/REVIEW.md), preserves every inspected
  source decision and clause/symbol callback schedule under value renaming,
  independent value ordering, and arbitrary unequal positive splitting of
  membership cells. It also couples repeated passes. The positional
  duplicate-occurrence extension preserves structural errors and makes no
  semantic-correctness claim. Primitive cost, memory, and concrete vector
  cardinalities need not agree.
* [Boolean repeated occurrences](boolean_occurrences/PROOF_ATTEMPT.md), with
  [independent review](occurrence_review/REVIEW.md), preserve positional truth
  when every copied initial name has the same singleton range and all clauses
  share one unchanged ordinary Boolean universe. Trailing copies can become
  unregistered, and old unit snapshots can skip them; the proof handles both.
  During HLA the needed cache containment refers to the current virtual
  target, not all stored targets. The subsequent
  [totality extension](boolean_occurrence_totality/PROOF.md) and
  [independent dependency audit](proof_dependency_review/REVIEW.md) also exclude
  consumed-index/shape failures and infinite source execution under explicit
  primitive, representability and resource premises. A hypothetical unit-HLA
  row can have the wrong count, but the unit phase provably cannot select its
  first donor. Physical occurrence count supplies finite progress; canonical
  helper-cost bounds depending on clause count alone do not extend.
* A [nonproductive call](root/FIXED_POINT_SATURATION.md) leaves no useful
  implemented local rule under proper canonical finite-set premises. Thus
  finite repeated calls reach full local saturation. The
  [independent review](fixed_graph_review/REVIEW.md) checks this separately
  from semantic preservation; local saturation still does not imply SAT
  completeness or a minimum-size representation.
* [Pure Boolean implications](root/PURE_IMPLICATION_GRAPH.md) have exact
  ordered greedy alternative-path deletion semantics. Every final clause
  and literal is indispensable. Directed acyclic inputs yield the unique
  cover-edge set; cyclic inputs can have many fully irredundant fixed points.
  Complete graphs can produce `(n-1)!` Hamiltonian-cycle outputs or a larger
  bidirected star, purely from input ordering. These differences are genuine
  alternative normal forms rather than the recorded scheduling omissions.

The theorem does not cover malformed accepted selectors, arbitrary custom
classes, changed universe bindings, non-total character operations, arithmetic
overflow, or failed execution. The user explicitly requested continued rounds;
the campaign remains active beyond this integration point.
