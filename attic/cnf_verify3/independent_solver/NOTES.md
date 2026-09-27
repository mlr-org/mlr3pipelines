# Independent solver and algebraic audit, campaign 3

Started 2026-09-06 08:18 UTC. Scope: `simplify_cnf()` on valid finite-domain
clauses, with independent Boolean SAT and decision-diagram semantics, plus
local rule proofs. This directory is owned by the independent-solver agent.
Production sources are read, never changed here.

## Prior campaign assumptions worth challenging

The existing brute-force evaluator is independent of the implementation but
shares R's `%in%` coercion semantics and the same bare-clause representation.
The larger DPLL oracle follows the same multivalued domain-restriction model
as production. Its semantic result is independently implemented, but this is
less formulation diversity than using ordinary Boolean SAT with explicit
exactly-one domain constraints. Its 800-instance calibration cannot establish
the absence of rare oracle bugs. This campaign uses Python-generated formulas,
JSON transport into R, and two independent Python interpretations; previous
generators and test runners are not sourced.

The earlier structural oracle only excludes direct pairwise subsumption in
outputs. It does not imply that all advertised heuristic rules are at a fixed
point, and semantic equivalence alone cannot detect incomplete simplification.

## Work plan

1. Prove the algebraic transformation schemas by a pointwise Boolean
   abstraction. Make the abstraction's completeness argument explicit.
2. Cross-check a one-hot Boolean SAT encoding against an independent reduced
   ordered multivalued decision diagram and directly evaluated truth tables.
3. Generate structured clause systems: finite-state implication cycles,
   resolution ladders, split-value refinements, and known-consequence redundant
   clause families. Compare production outputs and audit actual local events.
4. Challenge all promising findings using witness extraction, reduction, and
   independent replay. Record finite bounds separately from general proofs.

## Environment

Host R is 3.6.3. `jsonlite`, `checkmate`, and `mlr3misc` are installed;
`devtools` and `testthat` are not. The independent R bridge directly sources
the six CNF source files. No package API is reimplemented.

Python is 3.8. Attempting to install current `z3-solver` 5.1.0.0 chose a source
build that requires C++20; GCC 9 does not accept the selected `-std=c++20` flag.
Use the prebuilt wheel `z3-solver==4.12.2.0` in the ignored local `.venv`.
This avoids changing the global Python or R environment.

## General algebraic observations, to be machine-checked

Let a clause be a finite map from symbol `s` to set `C_s`, with absent entries
interpreted as the empty set. A valuation satisfies `C` iff some `x_s in C_s`.

For second-order SSE, let donor clauses be `A,B`, target `T`, intersection
symbol `s`, and restriction symbol `t`. Sufficient conditions are:

* `A_v` and `B_v` are subsets of `T_v` for every `v` outside `{s,t}`;
* `A_s intersect B_s` is a subset of `T_s`.

Then, in a formula containing both donors, replacing `T_t` by
`T_t intersect (A_t union B_t)` is equivalent. A valuation lost by this
restriction would satisfy `T_t` but neither donor's `t` literal, and would
falsify every other literal of `T`. The donors' remaining non-`s` literals
would consequently be false, forcing both `s` literals true. Their intersection
is inside `T_s`, a contradiction. The code's additional matrix test on `t`
only prunes no-op opportunities; it is not a soundness premise.

This is a proof of the rule under its stated premises. It is not yet a proof
that every scheduling path in the implementation always meets those premises.

## 08:29 UTC: independent oracle calibration passed

`calibration.py` checks direct truth tables, one-hot Boolean SAT, and reduced
ordered multivalued decision diagrams. The last two use structurally different
algorithms. It enumerates every conjunction of the six proper clauses over one
three-valued symbol and every conjunction of the eight proper clauses over two
Boolean symbols, with systematic pair comparisons and 200 additional
three-symbol cases. Results:

* 728 formulas and 10,024 equivalence comparisons checked;
* 5,448 deliberately unequal formula pairs rejected by both oracles;
* 522 actual production outputs matched direct truth tables;
* 1,698 instrumented local rewrite witnesses satisfied their independent
  sufficient premises.

The calibration includes TRUE, FALSE, empty conjunction, and empty disjunction
at the semantic-oracle level. Invalid bare clauses are not fed to production.
The R bridge uses real `CnfAtom`, `CnfClause`, and `CnfFormula` constructors.
Auditing instruments a source copy in the R process; production files remain
unchanged.

`rule_proofs.py` exhausted pointwise Boolean abstractions of subsumption (4
patterns), unit restriction (8), first-order SSE (16), HLA (16), and
second-order SSE (512). Every pattern meeting the stated premises preserves
truth: respectively 3, 8, 12, 12, and 280 admitted patterns. This is an
unbounded-domain proof of the algebraic schemas: each actual valuation maps
to one enumerated Boolean pattern. For the aggregate off-target literals,
per-symbol inclusion ensures the required Boolean implication.

## 08:33-08:40 UTC: residual advertised-rule opportunities found

The old experiment 210 was not merely observing different clause orderings:
its supplied seed and trial reproduce a result that still admits first-order
SSE, and a second call removes a literal. Calling this "non-confluence" does
not explain the residual rule. A confluent implementation is not promised;
this finding concerns saturation of a specific advertised rule already used
by the implementation.

The independent `fixed_point.py` checker found 10 residual opportunities in
7,813 new dense multivalued trials (91 seconds): 4 first-order SSE cases,
4 second-order SSE cases, and 2 direct unit-subsumption cases. Each second
simplification pass changed the first result. Both results were checked
semantically equivalent with Boolean SAT. Reduction produced:

* `minimized_sse1.json`: 4 clauses, 3 symbols with domains 3/2/3;
* `minimized_sse2.json`: 6 clauses, 3 symbols with domains 4/3/4;
* `minimized_subsumption.json`: 3 clauses, 2 occurring symbols with domains
  4/3 (an irrelevant one-valued symbol remains in the saved universe).

All three reduced instances were validated by both SAT and MDD. No semantic
failure was found. Their first/second-pass event logs are saved as well.
The subsumption reproducer is especially small: on X domain {1,2,4,5} and
Y domain {2,3,5}, the clauses are

```
(X in {2,4} | Y=3)
(X in {2,1} | Y=5)
(X in {2,5} | Y=2)
```

The output retains `(X=2 | Y=2)` beside `X=2`; a second call returns `X=2`.
This resembles the earlier fixed unit-merge equality gap, but occurs on the
current implementation. Root-cause tracing is in progress.

## Local audit refinement: unit context is essential during recursion

The first larger structured run stopped at a local-premise audit failure on
trial 95, a permutation-cycle instance, *after* SAT and MDD had both confirmed
the final result. A second-order donor's raw stored range was not a subset of
the target on an off-pivot symbol. Replaying with live unit-domain snapshots
showed that intersecting the ranges with the current units restores every
premise. This is expected during nested unit propagation, which has not yet
visited every affected clause. `contextual_audit_replay.json` contains the
complete live-clause snapshots.

Consequently an event-level soundness proof must use the current unit domain
as its valuation context, even when final/pre-HLA matrix invariants are stated
for raw ranges. The audit now checks raw premises first, then the equivalent
unit-restricted premises; it separately counts contextual-only cases. The
first replay contained 38 SSE1 events, 9 SSE2 events, and one contextual-only
SSE2 event. This corrects an over-strong audit premise; it does not conceal a
production counterexample.

## 08:40-08:53 UTC: four scheduling defects distinguished

The call-tree traces in `trace_*.json` preserve every nested helper's entry and
exit state without forcing its argument promises early. All traces reproduce
the same final outputs as uninstrumented production.

1. **A range shrinks but its first-order non-subset bit stays TRUE.**
   `apply_domain_restriction()` dispatches first-order work only when a bit
   changes TRUE to FALSE. If donor A's range was `{5,3}`, target B's range was
   `{5}`, and A later shrinks to `{3}`, their non-subset bit is still TRUE and
   their count is still one. The old intersection was a no-op; the new one
   removes B's literal. `on_update_range()` invokes only second-order handlers,
   so the first-order opportunity is missed. `minimized_sse1.json` demonstrates
   this with four clauses.

2. **A reverse count increases from one to two after the manual queue was
   built.** Shrinking target T can make donor B become a twoend instead of a
   oneend. `apply_domain_restriction()` increments the reverse count but does
   not dispatch it. `on_update_range(T, ...)` inspects T as donor and cannot
   handle the newly eligible B-to-T pair. That pair was absent from the
   initial manual twoend queue. `minimized_sse2.json` demonstrates this with six
   clauses. Before T shrank, its two oneend donors could have HLA-eliminated
   it; after the shrink, the needed operation is a second-order restriction.

3. **A oneend donor shrinks a range which was already a subset of the target.**
   This is a distinct trigger omission, constructed algebraically after the
   initial random findings. Because the changed-symbol bit was already FALSE,
   `on_update_range()` excludes that target entirely. Yet the union with the
   other donor's range has shrunk, creating a second-order restriction.
   `directed_oneend_shrink_min.json` is a five-clause example with domains
   2/4/2/2, only 32 valuations. The result retains
   `(T in {0,1,2} | C=0)` where another pass produces
   `(T in {0,2} | C=0)`. SAT and MDD both confirm equivalence. This case was
   synthesized from the event semantics, not discovered by another random
   batch.

4. **A unit skip reads transiently asymmetric comparison rows.** In the
   three-clause subsumption example, an outer restriction shrinks clause A
   and updates its comparison against B. The resulting recursive propagation
   shrinks C, which turns A into a unit before A's comparison against C was
   refreshed. `use_inso` sees stale `A not-subset C = TRUE` beside fresh
   `C subset A = TRUE` and skips C as supposedly a strict subset. In fact the
   ranges are equal. The earlier effective-intersection guard does not block
   this because the new unit's own range already equals the effective range.
   `minimized_subsumption.json` and `trace_subsumption.json` show the complete
   sequence. Disabling the skip should remove this completeness issue; that
   does not by itself establish whether the optimization can ever miss an
   actual restriction.

All four cases preserve semantics; they falsify saturation/idempotence claims
and a claim that all output clauses are free of direct unit subsumption.

## New exhaustive route: membership-cell selector families

`membership_cells.py` enumerates each nonempty subset of the eight possible
X-membership vectors for three input ranges. For each, it constructs
`(X in R1 | Y=a) & (X in R2 | Y=b) & (X in R3 | Y=c)` and checks all six
clause orders and eight within-clause symbol orders. There are 12,240
instances. By the value-refinement theorem, multiplicities inside membership
classes cannot add new behaviors for this family. This is a new exhaustive
parameterization by logical structure rather than a fixed domain cardinality.
It records every `use_inso` skip as a proper-subset, equality, or outside-domain
case and independently checks final semantics with SAT and MDD.

`PROOFS.md` now records general algebraic exclusions: rule soundness under
current units, single-use HLA donors, HLA loop termination, conditional HTE
unreachability, isolation of virtual matrix updates between target clauses,
and reduction of larger domains to membership-pattern classes. The known
scheduling defects are explicitly outside those claims.

## 08:54 UTC: structured and selector rounds completed

The corrected contextual event audit checked 1,600 structured formulas:

* 400 non-unit HLA chains, 400 unit HLA chains, 400 second-order bundles, and
  400 finite-state permutation cycles;
* all 1,600 passed both Boolean SAT and MDD equivalence;
* 958 additional exact generator promises checked;
* 39,140 SSE1, 107,691 SSE2, 509,592 non-unit HLA, and 12,807 unit HLA events;
* seven SSE2 events needed the live-unit context; all other event premises
  held directly on stored ranges;
* 493 unequal-value-refinement comparisons preserved the exact output sets;
* largest instances: 92 symbols, 120 clauses, about `10^83` valuations;
* no MDD or solver budget exhaustion and no semantic failures.

The complete three-clause selector family checked all 12,240 executions:
7,440 `use_inso` skips, of which 7,392 were strict subsets and 48 were equal
ranges. Exactly 48 outputs retained a directly subsumed clause. No skipped
clause was outside its unit's range anywhere in this family; every output was
SAT- and MDD-equivalent. Results are in `membership_cells_results.json`.

## 09:01 UTC: complete two-symbol/three-clause space launched

The selector family generalizes to all at-most-three-clause formulas over at
most two symbols. Each symbol's domain is any nonempty subset of the eight
three-bit membership patterns. There are `255 * 255` canonical domain pairs.
For each pair, test all eight within-clause symbol orders: **520,200 total**.
All clause orders are already represented by permutations of the three bit
coordinates, which the complete set of domain masks contains. Empty/full
ranges and duplicate clauses are included and normalized by the public
constructors. Fewer than three clauses are represented by tautological
padding clauses. Larger cardinalities inside membership classes are covered
by the value-refinement theorem; arbitrary value labels/orders require the
same-symbol set-operation inspection premise stated in `PROOFS.md`.

`exhaustive_two_symbol.py` runs eight worker processes, each with its own R
bridge and SAT/MDD instances. Every output is checked by both oracles. This
experiment is intended to yield a precise exclusion for a bounded number of
clauses/symbols with **unbounded domain cardinality**, rather than another
fixed-small-domain trial count. At 09:03 UTC, 130,560 executions had completed
with no semantic errors and 16 known equality-subsumption outputs.

## 09:10 UTC: complete two-symbol space passed

All **520,200** executions completed in 528 seconds with eight workers. Every
output passed both SAT and MDD; there were no production errors. The 107,968
unit skips comprised 107,936 strict subsets and 32 equality skips. Exactly 32
outputs retained a subsumed clause; no first- or second-order SSE opportunity
remained in this complete space. No skip was outside its unit domain.

The count differs from the selector experiment because this enumeration
already factors clause permutations through bit-pattern relabeling, whereas
the earlier selector run explicitly duplicated all six permutations. Both
runs expose the same equality-skipping mechanism.

`ROUND1_REPORT.md` summarizes the first 54 minutes. `SOURCE.sha256` was checked
again after the full run: all six CNF files match. The birth-time unit
containment argument is written in `UNIT_CONTAINMENT_ARGUMENT.md`, with
outstanding lifecycle proof obligations explicitly named.

## 09:17 UTC: lifecycle obligations probed directly

`lifecycle_probe.py` instruments a source copy to reject registration of a
clause already marked as a unit/eliminated once matrices exist, and to record
whether both donors and the target are still active at each SSE event. Its
inputs combine dense multivalued interactions with second-order bundles and
permutation cycles. It checks contextual rule premises and both semantic
oracles on every case. This targets the lifecycle assumptions in the proof;
it is not simply another final-truth-table batch.

## 09:35 UTC: nested propagation legitimately defers physical restrictions

The 5,000-case lifecycle round completed in 527 seconds. Both independent
semantic oracles passed every result; the active-donor/target and no-repeat
unit-registration probes found no violation. Counts: 74,998 SSE1 events,
109,814 SSE2 events, 259,399 non-unit HLA events, 6,069 unit HLA events, and
4,531 subsumption events. Seven SSE1 and eighteen SSE2 events needed the
current-unit contextual proof. Every unit-HLA event satisfied its stronger
**raw** off-pivot inclusion premise, avoiding circular use of the target unit.

There were 5,229 unit skips: 5,136 proper, 5 equal, 57 physically outside the
current unit domain, and 31 whose symbol had already disappeared from the
target clause. The absent-symbol cases come from a legitimate symbol-registry
iteration snapshot after a recursive child removed the symbol; the existing
restriction helper returns without action if called on the absent symbol.
They are not evidence of re-registration or bad semantics.

Outside-domain skips are a real intermediate state, including on satisfiable
formulas. The birth-order proof must therefore reason about deferred ancestor
work; a raw-inclusion invariant at every nested registration is false.
`outside_skip_satisfiable_dense.json` preserves dense trial 4294, and
`outside_skip_satisfiable_cycle.json` preserves a new fixed-zero permutation
cycle construction, found in its third case. The generic earlier filename
`outside_skip_satisfiable_example.json` equals the dense case; separate names
prevent concurrent experiment output from hiding either construction.

`minimize_deferred_skip.py` reduces the dense case to five clauses with two
four-valued symbols, using only seven SSE1 events. The exact case is in
`minimized_deferred_skip.json`. The first X1 registration excludes {0,3}; a
nested X0 unit creates a stronger X1=6 unit while the outer X1 propagation has
not yet visited a clause containing X1=3. The inner registration skips that
clause using an older contextual certificate; the ancestor subsequently
removes X1=3. Final output is X0=5 and X1=6, equivalent by SAT and MDD.

The proof-state agent independently replayed the unreduced case with its own
entry/exit hooks and confirmed all outermost-unit containment postconditions.
It independently derived the same well-founded idea by choosing the earliest
unit registration to exclude a given value. This is useful positive evidence
for the proof mechanism, and is explicitly **not** a newly found semantic bug.

## 09:52 UTC: a stronger small-clause proof and a failed broader conjecture

`TWO_CLAUSE_PROOF.md` argues that normalized formulas with at most two clauses
are sound and saturated for direct subsumption/first-order SSE, for arbitrary
domain sizes and arbitrary numbers of symbols. After the sole possible useful
intersection, its own pivot restriction is a no-op, and any newly enabled
reverse restriction concerns another symbol already contained in its donor.
Unit cases cannot involve a third clause; HLA with only one other donor cannot
find a hidden subsumer. Thus these inputs are idempotent modulo ordering.

`two_clause_probe.py` tried to falsify this argument on every nonempty
two-clause membership-pattern subset for one through four symbols, with both
clause orders and varied symbol orders: 108,480 executions, 397 seconds, no
semantic, first-order saturation, or idempotence failures. Every output passed
SAT and MDD. The unbounded-symbol statement rests on the proof, not merely this
finite probe. The previous 520,200-case quotient proof received an independent
review in `../representation/PROOF_REVIEW.md`, including 3,000 unequal value
refinements and 852,862 matching branch decisions. The unit birth-order proof
received an independent caller audit and 86 pointwise certificate obligations
in `../proof_state/LIFECYCLE_PROOF.md` and `certificate_obligations.py`.

I then challenged a broader conjecture: perhaps first-order scheduling itself
is saturated and only second-order integration creates the observed SSE1 gap.
This conjecture is **false**. A diagnostic source copy returns immediately
before `second_order_enabled = TRUE`, exposing the exact first-order phase.
`first_order_phase_probe.py` found three equality-subsumption leftovers and a
useful SSE1 leftover within 20,370 cases. `minimized_first_order_phase_sse1.json`
reduces the latter to four clauses on two four-valued symbols:

```
domain(X) = domain(Y) = {0, 1, 4, 5}
A: Y in {0,5} | X in {0,5}
U: X = 1      | Y in {0,1,4}
T: X = 0      | Y in {4,0,5}
D: Y = 0      | X in {4,5}
```

Exact clause and symbol ordering is as displayed. The first-order phase
leaves a pending restriction of T's X=0 literal by A's X=5 literal. The full
production simplifier also leaves it pending, after unit-HLA removes U:

```
first pass:  (Y=0 | X=5) & (X=0 | Y in {0,4})
second pass: (Y=0 | X=5) & (Y in {0,4})
```

The causal chain explains why an apparent transitive repair argument fails.
D first restricts A's X range to {5}; A's old non-subset bit toward T stays
TRUE, so no SSE1 callback runs. D could itself repair T, but its pair with T
has not been initialized yet. Before that pair is reached, D eliminates U's
X literal, creating a Y unit. That unit restricts A's Y range to {0}, which
makes A subsume D. D disappears before it can repair T; A's Y-to-T comparison
was already FALSE and gives no new callback. Later HLA does not repair T.
This is a distinct **pure first-order/unit-propagation route** into the
already-confirmed unchanged-bit SSE1 scheduling mechanism, and needs only
two symbols. SAT and MDD validate both full-production outputs in
`first_order_sse1_production_replay.json`. No second-order rewrite is needed.

## 10:10 UTC: an independent HLA saturation oracle and static SSE2 coverage

The first-order example reproduces under **R 4.6.1 with checkmate 2.3.4**.
`reproduce_first_order_sse1.R` uses real constructors and checks every one of
the sixteen valuations; its R 3.6 and R 4.6 logs record matching outputs.

Public review of the cited original paper supplied a different formulation
for HLA/ALA: assume the target clause false, then propagate value domains in
the other clauses. `domain_refutation.py` implements this without virtual
clauses or comparison counts and produces elementary donor-indexed
restriction/conflict certificates. Its separate certificate replayer,
Boolean SAT, and direct valuations calibrated 1,836 successful refutations
among 7,800 three-clause queries; 100 long chains added 1,384 propagation steps.
The primary source and the independently derived finite-domain correspondence
are recorded in `HLA_DOMAIN_REFUTATION.md`.

The main probe checked **62,322 surviving target clauses across 10,000
formulas** in 118 seconds. It found precisely three cases of the already
known unit-equality subsumption gap, and no additional domain-refutation
redundancy. This is a different completeness oracle, not another final-value
equivalence batch. A monotonicity argument shows why one complete HLA target
sweep can suffice for clause deletion: removing donors cannot create new
domain-refutation proofs. This does not extend to range-restricting phases.

The proof-state agent has also supplied `../proof_state/QUIESCENT_MATRIX_PROOF.md`,
which I independently reviewed: each raw-inaccurate TRUE comparison is owned
by a pending source-range update. Target shrink cannot create such an error;
each later source change creates its own obligation. Normal unwinding
therefore leaves all live comparisons exact before HLA. This connects the
earlier algebraic HLA proofs to the implementation's actual selection state.

`SSE2_COVERAGE_PROOF.md` excludes another class of proposed mistakes: a
statically useful second-order rule cannot be lost merely through the
oneend/twoend pattern filters or pivot orientation. Its donors' exceptional
sets are subsets of {s,t}. Zero exceptions give direct subsumption; a sole
t exception gives stronger SSE1; two sole s exceptions give HLA deletion;
every remaining pattern contains a twoend donor and passes the actual static
handler filters. Physical unit containment reduces unit cases to the earlier
rules. `sse2_static_coverage.py` checks all 65,025 distinguished-domain patterns:
16,256 useful rules split into 3,536 direct subsumptions, 8,624 stronger SSE1s,
512 HLA deletions, and 3,584 statically enumerated twoend cases.

This is deliberately a static theorem. The observed SSE2 failures concern
changes after the queue or a callback's candidate list was formed; the theorem
does not excuse those missed revisits or claim dynamic fixed-point completion.

## 10:43 UTC: complete literal removal, diagnostic repairs, and recursion

Trying to repair the already-subset oneend range-change case exposed another
necessary call site: **whole literal deletion**. `eliminate_symbol_from_clause`
also misses a second-order revisit when the removed symbol's comparison to
the target was already FALSE. `minimized_oneend_symbol_removal.json` is a
directed five-clause example with only 24 valuations:

```
S,U,R binary; T has values {1,2,3}
(T in {1,2} | R=0)
(S=0 | T=1 | R=0)
(S=1 | T in {2,3})
(U=1 | T=2)
(S=0 | U=0 | R=0)
```

The last two clauses delete T=1 from the second clause, leaving S=0 | R=0.
Together with the third clause, this permits restricting the first clause's
T range to {2}, but the first production pass retains {1,2}. A second pass
does the restriction. SAT and MDD confirm both results. Symmetric earlier
attempts repaired themselves through first-order propagation; giving the
remaining R literal to only one of the two resolution donors prevents that
repair. Both failed constructions are saved alongside the successful one,
so their repair paths are not lost as negative evidence.

`CANDIDATE_REPAIRS.md` records source-copy diagnostics. Adding a current strict-
length test to the unit-skip guard has a general last-registration proof of
final proper containment, in addition to the selector tests. Rechecking
oneend targets after a donor range changes fixes both first-order witnesses.
On 5,000 dense/structured formulas, every candidate output passed SAT and MDD
and none retained an SSE1 operation. Three independent SSE2 leftovers remained;
the unit-equality fixture also remained, correctly distinguishing the repairs.

A broader source-copy rescan then handles outgoing oneend/twoend pairs after
all range changes, incoming twoend pairs after target changes, and complete
symbol removal. Combined with the unit guard, it fixes all seven saved
mechanism fixtures. `combined_rescan_probe.py` checked 5,000 further inputs:
all passed SAT and MDD, direct SSE/subsumption checks, domain-refutation HLA
checks, and exact-set second-pass stability. Fourteen outputs differed from
baseline. Summed measured simplification time was 105.45 seconds for baseline
and 99.11 for the candidate; these are paired diagnostic timings, not a general
performance benchmark. **Production source is unchanged.**

I also followed the root agent's reverse-implication-chain design to fill in
reproducible resource measurements. `chain_resource_probe.R` uses real
constructors and verifies the exact expected conjunction of all n units.
Host R 3.6.3 fails reverse chains at n=256 and n=512 with a C-stack-limit error;
forward orders pass. Current R 4.6.1 passes both orders through n=512 but fails
reverse n=1,024, 2,048, and 4,096 with **node stack overflow**. All corresponding
forward orders pass, including n=4,096 in 1.7 seconds. Both environments use
checkmate 2.3.4 and report about 7.97 MB nominal C-stack limit; current R's
observed failure is its node stack, not the host C-stack error. Logs and full
process output are in `chain_resource*_results.json`.

`SEMANTIC_PRESERVATION_MAP.md` now composes the local logical schemas and
independent lifecycle/cache lemmas into a proposed complete source-level
normal-return theorem for canonical kernel inputs. It explicitly retains
runtime/representation assumptions and awaits independent final challenge;
the accepted-API selector problems and recursion failures are not hidden by
that theorem's restricted scope.

## 11:00 UTC: birth certificates and a stronger HLA exclusion

`unit_birth_certificate_probe.py` now records every unit immediately before
registration. It reconstructs the effective unit domains from the complete
chronological history of earlier births, independently of the R registry,
and checks that each observed registry snapshot agrees. SAT and MDD separately
check that every newly born unit follows from the original input formula.
For each optimized propagation skip, the checker uses only unit constraints
born **before that registering index**, never the new unit itself, to prove
the skipped clause's current range lies in the registering unit's birth range.

The completed 3,000-formula run checked **5,244 unit births and 4,798 skipped
restrictions**. All passed. Exactly **246** skipped ranges required an older
unit constraint and were also temporarily outside the current unit domain.
The generator includes known-satisfiable permutation cycles whose transitions
all fix zero, so this evidence is not limited to ultimately contradictory
formulas. The first such satisfiable certificate is saved in
`unit_birth_satisfiable_deferred_certificate.json`; full totals and timing are
in `unit_birth_certificate_results.json` (536 seconds).

An earlier version of this same run was deliberately interrupted after at
least 750 cases to strengthen the checker with independent chronological
registry reconstruction and a saved satisfiable witness. Its KeyboardInterrupt
and R broken-pipe log are harness interruption artifacts, not simplifier
failures. The stronger v2 run completed normally from the beginning.

`HLA_SATURATION_PROOF.md` proposes a stronger, useful completeness theorem:
every domain-refutation-redundant clause left in a normally returned canonical
formula is directly subsumed by a surviving unit. The known equality gap is
therefore the only permitted residual under this predicate. The proof does
not assume SSE1/SSE2 saturation. It combines exact HLA selection, the fact
that all surviving nonunit ranges lie inside units, and monotonicity of
domain refutations when donor clauses are added. This last property explains
why a single deletion-only target sweep can be complete even though the
earlier range-changing phase misses revisits. The new source obligation is
that no distinct live nonunit count-zero pair survives to HLA entry; all
count-construction/decrement call sites dispatch zero before quiescence.
Both peer agents received the theorem for independent challenge.
