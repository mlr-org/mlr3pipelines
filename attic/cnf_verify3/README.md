# CNF correctness investigation, campaign 3

Started **2026-09-06 08:16:41 UTC**, on branch `cnf-verify2`, source commit
`09770eaa`. The user requested at least ten hours of active investigation,
diverse independent agents, repeated synthesis and redirection, and detailed
records of both discoveries and exclusions. The earliest eligible completion
time is **2026-09-06 18:16:41 UTC**; this timestamp is a minimum, not a deadline.

The session exposes four concurrent agent slots, including the root, despite
the user's allowance of 64. We therefore rotate three independent agent
streams alongside root work. Production source remains unchanged during the
investigation so every finding has a stable baseline. Root integrates and
commits research artifacts at coherent milestones.

## Evidence standards

- A semantic defect needs an accepted input, independently evaluated expected
  semantics, actual output, and an assignment separating them.
- A crash needs an ordinary constructor path and enough environment details
  to distinguish an implementation limitation from malformed internal input.
- A simplification gap must identify an advertised or explicitly implemented
  rule that remains applicable. Arbitrary nonminimal output is not a bug in a
  heuristic simplifier.
- An exclusion identifies its exact scope: a proved rule, a proved program
  invariant, a finite exhaustive space, or an empirical result. These are not
  interchangeable. A clean random campaign is not a correctness proof.
- Prior conclusions and oracles are evidence to challenge, not assumptions.

## Round 1 portfolio

| Stream | Formulation and questions | Artifacts |
| --- | --- | --- |
| Root | Termination measure; recursion and resource scaling; residual local-rule completeness; experiment blind spots | `root/` |
| `proof_state` | Mutable state transition system; callback reentrancy; cached subset matrices and registries; inductive invariants | `proof_state/` |
| `representation` | Accepted R values and names; normalization; constant constructors; character representation and universe semantics | `representation/` |
| `independent_solver` | One-hot Boolean SAT and recursive decision diagrams; exact local-rule enumeration; structured finite-domain constraints | `independent_solver/` |

## Baseline and earlier evidence

Read `../cnf_verify/NOTES.md` and `../cnf/CLAUDE.md`. Campaign 2 reports about
18.1 million exhaustive formulas and 1.5 million randomized trials, one fixed
unit-merge subsumption completeness defect, and no semantic violation. Four
constructor/accessor defects remain documented. Its coverage argument leaves
hidden-tautology branches unexecuted, with a mathematical unreachability
argument. The unit-HLA phase depends on completed unit propagation.

The existing environment has R 3.6.3, `checkmate` and `mlr3misc`; it lacks
`devtools` and `testthat` in that R library. `podman` and an `r-base` image are
available for current-R verification. Earlier notes about an initially empty
R library are historical, not the present package inventory.

## Progress journal

### 2026-09-06, initial synthesis

Large counts from earlier campaigns mainly test the semantic preservation of
small, canonical set-valued formulas. New questions include whether the
accepted R input space establishes those canonical assumptions, whether
intermediate stale data is safe rather than merely eventually consistent,
whether implementation recursion fails on simple long formulas, and whether
the final result still admits one of the implementation's own local rules.

The root is constructing implication chains in both clause orders. A reversed
chain is registered before its initial unit reaches it and should force a
linear-depth recursive `register_unit` / `apply_domain_restriction` /
`eliminate_symbol_from_clause` call chain. This separates mathematical
termination from practical stack safety. It has not yet been measured.

### 2026-09-06 12:30 UTC, first integration and new independent round

Automated content classification repeatedly interrupted root and two agent
streams, although the work is local finite-domain logic. The independent
solver stream continued, preserved its results, and restored useful peer
reviews. The root resumed integration at 11:51 UTC. This operational history
explains the long interval without a root commit; it is not evidence about the
R implementation. New agents now receive narrowly specified mathematical or
R-contract tasks, with minimal inherited context.

Confirmed core findings are **four distinct missed scheduling conditions**,
all on canonical inputs and all preserving the truth table:

1. A first-order donor range shrinks while its exceptional comparison bit
   stays TRUE; no new first-order restriction is dispatched. A four-clause,
   two-symbol example shows this already happens in the first-order phase.
2. Shrinking a target raises a reverse donor count from one to two, creating
   a second-order candidate after the manual queue was built; it is omitted.
3. A donor range that is already contained in its target shrinks, reducing
   the union used by second-order resolution, without changing a comparison
   bit. Both nonempty shrinkage and complete literal removal can be missed.
4. Nested unit registration reads an outgoing TRUE comparison whose update
   is still pending in an ancestor. The opposite FALSE comparison is safe for
   containment but cannot justify *strict* containment; equality elimination
   is skipped. This survives the previous campaign's effective-range fix.

`independent_solver/ROUND1_REPORT.md`, `NOTES.md`, and the minimized JSON files
give exact inputs, outputs, independent SAT/MDD checks, and scheduler traces.
No production repairs have been made. Diagnostic repaired copies are clearly
separated in that stream's experiments and `CANDIDATE_REPAIRS.md`.

The root's reverse implication-chain example has now been measured on both
R versions. R 3.6.3 fails at 256 symbols with C-stack exhaustion, while the
forward order succeeds. R 4.6.1 succeeds at 512 reversed symbols but fails at
1,024 with node-stack overflow; 4,096 forward symbols succeed. The program has
a finite descent measure, but its recursive implementation has a separate
resource limitation. Exact measurements are in
`independent_solver/chain_resource_*` with attribution to the root design.

The representation stream found class loss for `TRUE | CnfClause`, missing
logical-selector validation, and duplicate symbols from matrix selectors.
These are public composition/normalization issues, separate from the
canonical-input kernel proof. Byte-marked character mixtures also expose an
explicit limit of the abstract character-set assumptions. See
`representation/NOTES.md`; a fresh `r_values` stream is independently checking
the scope and documented selector contracts.

Proof progress now includes:

- A complete 520,200-execution membership-class enumeration for every formula
  with at most two occurring symbols and three clauses. SAT and MDD both
  checked every output. The independent representation review proves how
  this finite quotient covers arbitrary nonempty finite domain sizes and
  multiplicities under ordinary character-set semantics. It does not prove
  saturation; 32 equality leftovers were observed.
- Universal pointwise proofs of unit propagation, subsumption, first- and
  second-order SSE, and HLA; finite-descent and single-use-donor arguments.
- A proposed source-level normal-return semantic-preservation theorem,
  independently reviewed in `review_semantics/REVIEW.md`. The review supplies
  a simpler frozen unit-birth certificate and an explicit lemma for lazy
  unit-HLA comparison initialization. Its fresh instrumentation passed 5,002
  cases and 41,694 individual semantic mutations. This is a human-readable
  source proof, not a machine formalization of R or an unconditional claim
  about every object carrying a CNF class.
- Static second-order candidate completeness and a proposed HLA saturation
  classification are receiving a separate new review in `review_hla/`.

The current round deliberately changes the mode of work: independent proof
challenge, Horn/domain-refutation completeness, R input normalization, and
root reproduction/integration. Additional random counts alone would not
resolve the remaining proof and contract questions.

This is a live investigation journal. The ten-hour minimum has not elapsed.

### 2026-09-06 13:06 UTC, second integration and structural follow-ups

The strongest current results are collected in [FINDINGS.md](FINDINGS.md).
This includes the newly reduced **truth-value error through an accepted
matrix clause selector**: a contradictory four-clause input returns the
canonical satisfiable formula `X=c AND Y=a`. Three independent replays check
the original selected disjunction and every assignment. The normal installed
package reproduces it; flattening the same indices to a vector returns FALSE.
The kernel's unique-symbol assumption is broken at public subsetting.

The fresh execution-mode study compares 24 configurations: source,
explicit compiler optimization levels, development namespace, installed
bytecode and installation without bytecode, each across JIT levels 0–3.
All 192 saved fixture executions, 1,752 constructor/operator paths and 384
serialization cases agree. The result qualifies the source-harness evidence
with actual package behavior. Serialization preserves shared universe identity
inside a jointly serialized bundle but creates a new identity relative to
the original environment, as expected from the documented combination rule.

Independent source reviews have now discharged the proposed kernel proof's
remaining presentation obligations: chronological frozen unit births,
phase-bounded ambient unit assumptions, and lazy unit-HLA row initialization.
The semantic composition document incorporates these reviews. The separate
HLA review proves that residual domain-refutation opportunities among final
survivors are exactly the known unit-subsumption leftovers. Static SSE2
pruning is complete relative to saturated earlier rules; dynamic notification
is the source of the recorded omissions.

The repeated-pass stream constructs a family needing exactly `n` productive
passes for every positive integer `n`, with measurements through `n=32` and
explicit witnesses excluding HLA shortcuts. Reversing the target order gives
one pass. A reduced seven-clause member has 64 assignments and three useful
passes, then a sort-only pass. An independent reviewer is checking the
unbounded induction while the author begins a new study of incidence forests,
binary constraints and exact structural completeness classes.

The unit-queue diagnostic handles reversed unit chains with 16,384 symbols
and constant measured helper depth. A separately constructed common-guard
family contains no units and still overflows through recursive nonunit SSE.
Thus queueing only unit propagation does not resolve general stack depth.
The source copy passes 1,019 independent SAT/MDD comparisons and its contextual
matrix/physical-containment checks. No diagnostic implementation is installed.
Its author now reviews operator and constructor semantic closure, including
the R mutation/copy behavior in OR distribution.

Root has started a complete new `(3,3,2)`-occurrence three-symbol quotient:
9,386,748 source executions, with at most 256 valuations per representative.
At this checkpoint 3,890,880 executions and 203,420,160 valuations have passed.
Its independent reviewer observed that every proper input of this shape is
satisfiable, by a direct three-clause matching assignment. This is therefore
additional model-preservation coverage, not new contradiction coverage.
The full run and its independent assignment-grid calibration remain active.

Four focused semantic regression tests were added earlier and passed with
the actual current-R package dependencies: `devtools::test(filter="Cnf")`
reported **3,815 passes, zero failures, warnings or skips**. The tests preserve
known semantic expectations without requiring unfixed scheduling behavior.

### 2026-09-06 14:06 UTC, completed quotient and another independent round

The full three-symbol quotient finished at **13:29:03 UTC** with every one of
193 outer profiles completed: **9,386,748 executions, 547,476,480 valuation
checks, zero semantic differences or runtime errors**. Independent grid,
renaming, refinement and schedule checks are in `quotient_review/`. The
repeated-pass family's independent source and installed-package review also
passed, including the distinction between a useful pass and a sort-only pass.

New source proofs identify positive structural classes. Incidence forests
become fully irreducible after initial unit propagation, with explicit models
showing every surviving clause and literal value is necessary. A matching
argument extends contradiction recognition to incidence pseudoforests.
Equal-or-disjoint range blocks in clauses with at most two symbols exclude
every proper nonempty range change; their surviving nonunits are immutable,
which gives first-pass local saturation. Boolean renamable Horn formulas
have complete contradiction recognition. The separate Boolean graph theorem
characterizes exactly which unit seeds the first- and second-order rules can
derive. These new theorems have distinct independent reviewers now.

The operator study proves normal-return semantic/proper-representation closure
for Formula AND, OR and negation, while explicitly preserving known constructor
and Clause-class exceptions. It checks the local binding behavior in the OR
distribution callback and a deliberately changed binding control. Negation
detects contradictions even with an identity simplifier: complete distribution
and tautology removal are sufficient. The old documented “unrecognized”
three-clause contradiction is actually recognized now. A new eight-clause
Boolean control remains unchanged by simplification, while its negation is
TRUE and double negation FALSE.

Root completed a new conditional-outcome catalog: **108 `if` sites, 62,634,404
evaluations, 212 of 216 outcomes observed**, with identical original/observed
outputs for all 30,034 inputs. All four remaining outcomes have source-level
unreachability arguments; another reviewer is checking those arguments and
the catalog independently. This measures individual condition outcomes,
not all paths or short-circuit combinations.

Another automated interruption affected the root while agents and experiments
continued. All records were recovered and the next round launched at 13:57 UTC.
The original ten-hour minimum and the obligation to resolve material open
avenues remain in force; this is an integration checkpoint, not a conclusion.

### 2026-09-06 14:43 UTC, independent reviews and boundary reductions

The structural, Boolean graph, conditional-outcome and diagnostic-scheduler
reviews have completed. Each now includes an independent source argument,
explicit exclusions, and fresh executable controls. In particular, the
scheduler review closes the count-zero/zero-to-one pending-visit obligation;
cache exactness alone would not establish the claimed saturation. The
Boolean theorem keeps scalar FALSE separate and concerns initialized live
binary comparisons, not every allocated matrix entry.

The new small-selector stream checked 455,492 formulas and separately reran
42,536 membership-profile cases on current R. It reduced unit-HLA failure to
two selected clauses and retained a two-clause stale duplicate output. Its
finite-to-arbitrary-domain exclusions keep their exact occurrence, clause and
symbol bounds. No three-clause canonical error was found; there is no global
minimality or all-size Boolean safety claim. A new independent stream now
investigates the Boolean repeated-occurrence case directly at source level.

Root removed an existing comparison guard in a private source copy and found
a concrete count inconsistency caused by nested callbacks completing a queued
comparison. A reduced 11-clause, four-symbol input distinguishes the changed
copy on both R versions. Production has exactly two models among 360 valuations.
The added public regression passed with the full simplification test file:
1,525 expectations, no failures, warnings or skips. This validates an existing
correct guard and is not counted as a production defect.

The repeated-pass potential now counts original membership fibers instead of
concrete labels, excluding domain-size-only unbounded families. Its reviewer
found a stronger clause-count-only bound by freezing signature classes with
at least three symbols. A fresh independent reviewer is challenging that
first-change proof. Another new stream examines full source-level equivalence
under value renaming, reordering and nonuniform membership-cell splitting,
including precisely what a decision-schedule claim can mean in R.

These new proof and representation tasks continue alongside root integration.
The ten-hour minimum remains 18:16:41 UTC.

### 2026-09-06 15:47 UTC, source bounds and independent closure results

Fresh reviews completed the clause-count-only pass bound, the local-helper
recursion bound, value-set/control-schedule symmetry, Boolean repeated-name
truth preservation, fixed-point local saturation, and pure-implication graph
characterization. The master ledger links the full statements and reviewers.
The reviews narrowed important claims: live-target progress is needed for
the recursion charge; HLA containment concerns the current virtual target;
and sort-only calls must be distinguished from productive simplification.

The Boolean repeated-occurrence result is stronger than the earlier bounded
search: source reasoning covers arbitrarily many homogeneous singleton
copies in one unchanged Boolean universe. It does not extend to the ternary
selector failure or claim all R executions are free of errors. Separately,
the graph result proves real nonconfluence among saturated irredundant
outputs and excludes that phenomenon as evidence of missed scheduling.

Root tested a two-expression private selector correction on 111,695 calls
per R version. Flattening before deduplication and rejecting missing logical
selections restore the canonical subsetting invariant and resolve both saved
semantic/runtime examples. Real checkmate also accepts all-missing numeric
selectors through the permissive logical alternative; the record and oracle
now account for this additional boundary case. Production remains unchanged.

A new full-three-symbol membership quotient remains active: every clause
contains all three symbols in aligned order, with 7,189,057 representatives.
Each result is repeatedly simplified until a nonproductive call, and every
intermediate result has an independent exact truth-mask comparison. The
independent review already checked profile completeness, bit masks, scope,
and the final report validator. Partial counts do not certify the full space.

Three new streams are still active: exact event-level semantic divergence in
the selector counterexample; R index/scalar/initialization obligations under
canonical finite storage; and a source proof for three full clauses with
arbitrary within-clause symbol order. The last uses matching and projection
properties, not just enlargement of the exhaustive bank. The ten-hour minimum
is still 18:16:41 UTC, and the investigation continues.

### 2026-09-06 15:51 UTC, full-occurrence quotient completed

The full three-clause XYZ quotient completed at 15:50:08 UTC. Independent
completion validation passed: all 7,189,057 inputs, 9,731,786 production
calls, 849,165,820 all-pass valuation checks, and 1,930 SAT/MDD calibrations.
Exactly 4,646,328 representatives need no productive call and 2,542,729 need
one; none needs two. The reviewed simulation lifts this to arbitrary finite
domains for the aligned symbol order. An explicit sort-only second-call
witness prevents overstating this as exact ordered-object idempotence.
See [the full result](root/FULL_THREE_SYMBOL_QUOTIENT.md).

The selector trace also completed: the first positional semantic error is
clause deletion at source line 474; a later literal removal at line 244
exposes a stale range under first-name projection. The two R versions give
identical full traces, with 103 focused expectations. Character NA selectors
share the newly clarified all-missing numeric acceptance path. A new stream
now examines whether all ordinary accepted domain storage can be normalized
without changing source decisions, including HLA's duplicated virtual values.

### 2026-09-06 16:13 UTC, argument validity and new simulation reviews

The indexing author completed a source-prefix proof covering 340 indexing
sites, 108 `if` sites and 34 short-circuit sites. Both R versions passed the
same 3,596 canonical inputs, with 1,769,855 reads, 221,459 final writes and
1,192,136 scalar checks per version. Seven changed-source controls and eight
direct negative shape controls calibrated the observer. A fresh reviewer is
now checking the proof independently, especially the boundary invariants
which must hold before a possibly failing operation. A new public regression
for a queued clause becoming a different-symbol unit passed within the
1,542-expectation focused simplification test file.

The three-full-clause author proposed an all-symbol-order saturation proof
using individual-value incidence and a three-clause relay argument. Its 87
SMT implications concern local set algebra, not the complete scheduler. The
fresh reviewer is examining the exact initialization and deferred-callback
cases; the completed aligned-order quotient is not used as a substitute.

Another completed author study extends domain normalization to repeated,
named, dimensional and inertly attributed domain storage while retaining
canonical actual ranges. Both R versions agree over 10,128,063 source events
and 264 inputs across 11 storage forms. Its bounded virtual-multiplicity
argument explicitly refutes virtual uniqueness and full-fiber occupancy as
unnecessary strengthenings. A fresh reviewer checks it and root's independent-
component simulation. The latter passed 2,400 globally interleaved inputs on
both R versions, including components with known multi-pass scheduling gaps.

Root separately composes indexing validity, finite source execution, and
normal-return semantic preservation, and proposes a conservative total helper-
invocation bound depending only on initial clause count. Its child/root-count
observer passed 1,023 cases on both R versions. These new composition/count
claims are explicitly marked for review, separate from the already reviewed
active-depth bound. Three independent review streams remain active, and the
ten-hour minimum remains 18:16:41 UTC.

### 2026-09-06 16:40 UTC, completed totality and simulation reviews

The independent index reviewer completed 19,787 inputs on both R versions,
including 7,176,512 row/count boundaries per version. Its prefix induction
supports the no-first-consuming-error argument, and a separate source recount
supports the finite-tree composition and helper invocation bound. The new
[total-correctness composition](root/TOTAL_CORRECTNESS_COMPOSITION.md) is now
reviewed under explicit ordinary-operation, representability and resource
premises. This does not erase the demonstrated stack overflows or accepted
malformed selector failures.

The three-full-clause review closes arbitrary internal symbol ordering through
a strengthened source argument. It distinguishes a pending reverse initialization
visit from an uninitialized pair and assigns the final conservative comparison
debt to its precise remaining source tail. Local SMT and the aligned quotient
remain supporting evidence rather than substitutes for these scheduling cases.

Independent domain/component reviews also completed. Per R version, there are
810 exact normalization payload/decision comparisons and 9,274 exact projected
component output/local-history comparisons. Complete saved records compare
identically across R versions; binary serialization hashes themselves were
not portable enough for that comparison, a documented observer limitation.

A new root proof relates initial unit propagation to generalized arc
consistency, then constructs a least model for a finite semilattice clause
class that includes multivalued ordered Horn formulas. Its 12,000-input bank
passed on both R versions, and a fresh reviewer is using explicit relation
tuples and additional nonchain semilattices. Another fresh stream is extending
the Boolean repeated-occurrence proof to all consumed-index/termination
obligations; it has already isolated an all-FALSE lazy row which is safely
unreachable because the unit-HLA donor count is two. A third new stream is
proving the smallest Boolean binary contradiction missed by the graph criterion.
Root's finite clause-selection encoding currently establishes the four-symbol
boundary of eight clauses; a larger-symbol bound and its lifting are ongoing.

### 2026-09-06 17:01 UTC, new comparison findings and completeness boundaries

Two new ordinary-string comparison failures reproduce in both R versions.
A pair of two-clause formulas is `identical()` under UTF-8/Latin-1 string
equivalence, but `all.equal()` reports four mismatches because serialized
digest sorting reverses clause alignment. Separately, equal locale collation
weights leave reversed orders of distinct Unicode values or names unnormalized.
The first persists in locale C; the second appears in C.UTF-8 and en_US.UTF-8.
An independent comparison review is checking both causes and the private
UTF-8/radix candidate against unequal-set and symbol-association controls.

The semilattice source correspondence passed independent relational review.
The new review enumerates every labeled semilattice through size four and
explicit small GAC boxes, supplies a support-tuple proof of least-model
extraction, and makes the source unit-birth certificates preserve every GAC
sub-box. This strengthens the earlier root explanation of greatestness.

The exact minimum unrecognized Boolean unit/binary contradiction is now
eight proper binary clauses and four used variables. A separate graph/sign
enumeration closes every <=7-clause core; root independently reviews its
symmetries and gives a second finite selection route, explicitly retaining
the unhelpful larger-encoding timeouts. The four-variable parity witness is
checked in 1,030 unchanged-source/public calls on each R version.

The Boolean repeated-occurrence totality author also completed. Its lazy unit
rows need not satisfy the canonical count equality, but no starting unit-HLA
donor can exist. A physical-occurrence potential bounds finite helper depth.
Its two-/three-clause examples show why the canonical clause-only helper cost
bounds cannot transfer. A fresh reviewer is auditing proof dependencies and
finite-prefix obligations, while another new stream checks public constructor
closure with dimensional atom/domain storage and both R versions' set operations.
The ten-hour minimum remains 18:16:41 UTC; active rounds continue.

### 2026-09-06 17:22 UTC, proof dependency audit and comparison review

The independent dependency audit found no circular reliance on whole-call
return or semantic preservation in either the canonical totality composition
or its homogeneous Boolean repeated-occurrence extension. It clarified the
canonical domain-vector premise and supplied a simpler fallback termination
potential: count all stored literal values, including eliminated slots.
Strict restriction ancestry decreases this count, independently of the sharper
membership-fiber and signature-class bounds. The Boolean unit-HLA no-start
proof is structural and does not depend on the semantic orphan lemma.

The comparison reviewer independently reproduced both ordinary Unicode
failures and checked 15,552 permutation/encoding comparisons per runtime,
including unequal-set and association controls. UTF-8/radix payload copies
preserving the public guards pass that bank, but a new reduced case shows
that delegated base environment comparison also misorders tied symbol names
across identically populated universes. This is an explicit remaining limit
of a payload-only correction; the root prototype is not a complete method
replacement. Production methods remain unchanged.

The constructor-storage author now closes the public grammar for ordinary
named/repeated/dimensional domains and atom values, keeping the known constant,
selector and dispatch exceptions explicit. The proof checks both R versions'
different `setdiff` bodies and the direct complement-clause construction in
formula negation. A fresh reviewer is challenging these normalization lemmas.

Two other active routes extend beyond those completed results. Root checked
20,000 three-clause inputs with initial widths at least three on each runtime,
with 40,000 exact MDD comparisons per version and no productive second call.
A separate source proof is examining arbitrary differing supports and callback
tails. Another stream tests kernel character-identity invariance without using
the now-known defective comparison methods, including native-encoding limits.
The required minimum remains 18:16:41 UTC.

### 2026-09-06 17:43 UTC, a second independent truth-preservation failure

The character-identity stream found a new semantic failure with **canonical
clause shapes**. Under `LC_CTYPE=C`, a valid UTF-8 symbol name enumerates from
an environment as literal ASCII escape text. Initial propagation at source
line 502 misses its unit. Unit HLA later assumes the missing containment and
deletes that unit at line 777, changing a contradictory three-clause formula
to one with two models. All domain values are ASCII; no selector, malformed
clause, mixed encoding, or universe mutation is involved. Latin-1 names also
reproduce. The stream traced the precise R native-key versus string-equality
distinction against both official runtime source versions.

Root independently enumerated all 8,421 ordered lists of up to three clauses
over a 20-clause ternary/Boolean palette in four name/locale configurations.
Each R version completed 33,684 public calls. Only the C/UTF-8 triples change
truth: 612, including 36 unsatisfiable inputs becoming satisfiable and 576
satisfiable inputs gaining assignments. The new witness also reproduces in
108 calls across installed, uncompiled, and development package namespaces;
all 56 CNF definitions match the current unchanged source.

A private direct-binding lookup replacing the single enumerated-name
intersection passes the same complete 33,684-call bank per runtime. The
accepted alias-name construction problem is broader, so this is a diagnostic
correction, not a complete public-name repair. The source proofs now explicitly
require faithful identity across membership, list access, environment lookup,
and registry name enumeration. The new counterexample refutes an unconditional
all-locale reading of canonical-input correctness. A separate source reviewer
is checking the first wrong transition, candidate scope, and two-clause lower
boundary.

The constructor-shape review completed positively with that explicit name
premise: 4,570 independent order/shape cases, 1,645 crossed domain/value cases,
246,650 positional valuation rows, and 58 grammar controls per runtime. This
supports the scalar normalization lemmas while keeping the new name failure
outside their semantic corollary.

The wider three-clause saturation author also completed; its reviewer supports
the pending initialization and arbitrary outside-coordinate arguments and is
finishing fresh runtime controls. Another bounded route now asks whether four
clauses can retain a scheduling gap at arbitrarily large initial widths by
adding identical fresh literals. These distinct rounds continue until all
material review obligations and the ten-hour minimum are satisfied.

### 2026-09-06 18:01 UTC, sharp structural bounds and comparison follow-through

The independent wide-three review supports first-call local saturation for
any three proper clauses whose initial widths are all at least three, with
arbitrary supports and symbol orders. Its new width-four-through-nine controls
exercise deferred comparisons and reject a known four-clause leftover. The
review clarified that an SSE1-shaped bound can arrive through a repeated-donor
SSE2 call; the proof now says so explicitly instead of implying one call site.

The CTYPE source reviewer independently observed the exact missing propagation,
false raw-containment counts, and first truth-changing deletion. It also proves
at most two actual clauses preserve truth under stable injective native keys,
even if enumeration changes their spelling. Thus the three-clause example is
minimal in that scope. The direct-lookup candidate has a source simulation to
an ASCII-named execution, with its alias and resource limits explicit.

Crossing the constructor and comparator reviews found a further accepted-shape
comparison failure. A one-row matrix containing two copies of `a` produces a
proper atom selecting `{a}`, but compares unequal to the ordinary singleton
atom. Matrix uniqueness removes rows and comparator sorting retains scalar
multiplicity. Both R versions show 116 false negatives among 248 small pairs;
an independent review confirms larger matrices/arrays, unequal controls, and
the guard-preserving flatten/unique/UTF-8/radix correction. Conversion through
Clause or Formula normalizes this storage and remains correct.

The four-clause width-boundary author now gives a fixed family with an omitted
SSE1 restriction at every initial width >=3, padding every clause by the same
fresh proper literal block. Its proof couples one pad to any positive number;
zero padding has different unit callbacks and is not silently equated. A fresh
reviewer is checking the initialization and HLA details. A separate short
source review is checking whether default comparison can ever return TRUE for
different proper structural content under the stated common-universe contract.
The mandatory minimum remains 18:16:41 UTC.
