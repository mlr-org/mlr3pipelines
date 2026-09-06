# Finding ledger and evidence boundaries

Live synthesis for the unchanged `09770eaa` CNF sources. This ledger separates
changed truth values, incomplete application of implemented rules, public
representation failures, and runtime limits. Older dated reports remain as
the historical record; the links here point to the strongest current evidence.
No production repair has been applied.

## 1. Accepted clause subsetting can change a formula's truth function

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

Actual stored values only disappear. Their initial total count gives a
finite upper bound on productive passes; a final sort-only pass may be needed
for exact storage stability. This establishes finite convergence under normal
completion, without claiming a unique or logically minimal result.

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

## 5. Other public-boundary failures

| Behavior | Evidence and scope |
| --- | --- |
| `TRUE | proper_CnfClause` returns bare logical TRUE | `|.CnfClause` returns its raw left operand on one short circuit. Truth is correct; subsequent typed constructor composition fails. Reproduced across all 24 runtime configurations. |
| Logical NA clause selectors are accepted | Missing `any.missing=FALSE` allows an NA symbol name and NULL range to enter CnfFormula. The resulting object has no well-defined ordinary finite-domain interpretation; a clause round trip can become TRUE. |
| Byte-marked character mixtures are not uniformly operable | Accepted strings can fail membership or printing in base R. This is a representation/runtime boundary of total character equality, separate from ordinary finite-set semantics. |

See [representation findings](representation/NOTES.md) and the independent
[R-value review](r_values/NOTES.md). Existing issues in
[`../cnf/CLAUDE.md`](../cnf/CLAUDE.md) and its companion review are retained as
prior findings: TRUE-clause `as.list`, documented list names, universe lookup,
neutral-constant universe selection, and FALSE after nested-formula flattening.
They are not credited as discoveries of this campaign.

## 6. What can be excluded, and under which assumptions

| Scope | Evidence level and result |
| --- | --- |
| Every local unit, subsumption, SSE1, SSE2, and HLA inference | Universal pointwise set-algebra proofs, with exhaustive checks of the Boolean schemas. |
| Normally returning kernel on canonical finite character sets | Independently reviewed [source-level preservation proof](independent_solver/SEMANTIC_PRESERVATION_MAP.md). Contextual FALSE bits, chronological unit-birth certificates, quiescent exactness and lazy HLA rows supply the mutable-state premises. This is not a mechanization of R. |
| At most two clauses, arbitrary symbols and finite domain sizes | Semantic preservation, first-order saturation, and idempotence modulo order proved in [TWO_CLAUSE_PROOF.md](independent_solver/TWO_CLAUSE_PROOF.md). |
| At most three clauses and two occurring symbols, arbitrary finite domain sizes | Complete 520,200-execution membership quotient, independently reviewed lifting argument, and two distinct exact oracles on every result. No semantic failures; 32 known unit-equality leftovers. |
| Three clauses of the `(3,3,2)` occurrence shape, arbitrary finite domain sizes | Complete 9,386,748-execution quotient and 547,476,480 valuation checks, with independently reviewed ordering, lifting and assignment grids. No semantic differences. Every proper input in this shape is satisfiable. |
| Hidden domain-refutation opportunities among final survivors | [Independent HLA review](review_hla/REVIEW.md): every residual refutable clause is directly subsumed by a different surviving unit, and conversely. This excludes other residual HLA/domain-propagation cases, not arbitrary logical redundancy. |
| Static second-order candidate pruning | Complete relative to saturated earlier rules; the demonstrated omissions are dynamic scheduling conditions. |
| Duplicate domain labels | Submultiset argument excludes false-positive HTE coverage; exact live-donor bookkeeping makes the HTE full-domain branch unreachable and closes the remaining control-flow concern. |
| Source versus package/bytecode execution | Exact agreement for the saved fixtures across 24 measured modes; not a universal compiler theorem. |
| Formula AND/OR/negation on canonical operands | [Operator proof](operator_proof/README.md) establishes semantic and proper-representation closure on normal return, subject to the kernel theorem. Public constructor failure paths and Clause result-class loss remain explicit exceptions. |
| Negation of a contradictory canonical formula | Complete distribution and individual-clause tautology removal force `!F` to be literal TRUE on normal return, even with an identity simplifier. This does not make the ordinary simplifier a complete SAT procedure. |

The three-symbol shape quotient completed at 13:29:03 UTC. Its exact scope,
counts and independent calibration are in
[THREE_SYMBOL_QUOTIENT.md](root/THREE_SYMBOL_QUOTIENT.md).

Further structural source proofs are now undergoing independent review:
incidence-forest saturation, incidence-pseudoforest and Boolean renamable-Horn
contradiction recognition, and local saturation for binary clauses selecting
equal-or-disjoint range blocks. An exact Boolean 2-CNF graph criterion predicts
the final units from implication paths of at most three edges followed by
unbounded unit propagation. The current source checks agree in 8,238 cases;
the proof's independent review remains a separate obligation. See
[structural classes](structural_classes/PROOF.md) and
[Boolean graph criterion](structural_classes/BOOLEAN_2CNF_GRAPH.md).

The conditional-site study has observed 212 of 216 outcomes across all 108
kernel `if` sites, with exact output comparison against the original function
in 30,034 cases. Four unobserved outcomes have proposed source exclusions:
emptying a singleton through the nonunit deletion helper, deleting a unit
through the ordinary deletion helper, and the two immediate HLA full-domain
branches. These are being independently reviewed; see
[branch obligations](root/BRANCH_OBLIGATIONS.md).

The theorem does not cover malformed accepted selectors, arbitrary custom
classes, changed universe bindings, non-total character operations, arithmetic
overflow, or failed execution. The user explicitly requested continued rounds;
the campaign remains active beyond this integration point.
