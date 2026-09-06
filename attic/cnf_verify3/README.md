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
