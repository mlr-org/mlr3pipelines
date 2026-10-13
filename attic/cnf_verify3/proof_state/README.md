# State-transition audit of `simplify_cnf`

Started 2026-09-06 at approximately 08:10 UTC. This directory belongs to the
`proof_state` agent in verification campaign 3. Production sources are not
modified by any experiment here.

## Scope and approach

The preceding campaign checked bookkeeping at quiescent phase boundaries.
This audit studies the recursive helpers as a transition system and checks
decisions at the point they are made. The distinction matters: during a
unit-propagation cascade, a subset matrix may temporarily disagree with the
stored, not-yet-propagated clause ranges. A raw-state invariant is therefore
too strong; the candidate soundness invariant is that each matrix `FALSE`
still certifies a subset after intersecting both ranges with every currently
known unit domain.

The target is exact equivalence of formulas, not merely equisatisfiability.
Assertions about the implementation will be separated from mathematical
proofs of its individual inference rules.

## Open proof obligations

1. Does every positive subset certificate remain true after projection to
   the current unit domains at every recursive inference boundary?
2. Can stale snapshots allow an eliminated/unit source or target to reach
   a rule that assumes a live non-unit clause?
3. Can a nested merge change the meaning of an outer `use_inso` skip so
   that a restriction, rather than just a redundant-clause deletion, is missed?
4. Do all three-valued return paths (`TRUE`, `FALSE`, `NULL`) preserve the
   caller's assumptions about its source and target?
5. Does the recursive scheduler fail on a long but structurally simple
   propagation chain? Is the limitation semantic, completeness-only, or
   resource-related?

## Current source references

References below are to `R/CnfFormula_simplify.R` as read at campaign start.
The production file contains the previous campaign's `register_unit` merge
fix at lines 108–115.

- `register_unit`: lines 87–139.
- `apply_domain_restriction`: lines 146–232.
- `eliminate_symbol_from_clause`: lines 236–291.
- `on_updated_subset_relations`: lines 295–335.
- `on_update_range`: lines 341–367.
- Second-order handlers: lines 372–468.
- Pair construction and scheduling: lines 551–647.
- HLA over non-units and units: lines 650–788.

## Initial observations

- `entries`, `eliminated`, and `is_unit` only move toward smaller clauses or
  fewer live clauses during the recursive phases; no helper revives a clause.
- Matrices intentionally retain columns of deleted symbols. A deleted source
  symbol's column is made `FALSE` in every row; indexing by column name in
  range updates avoids confusing the current clause position with the old
  matrix position (lines 194–198).
- HLA mutates matrices relative to a local virtual extension while leaving
  the stored clause unchanged. Those target columns are not consumed again
  for pairwise inference because HLA is the final phase. This is a deliberate
  change of matrix interpretation, not itself evidence of corruption.

## Round 1 results and proof exclusions

`LIFECYCLE_PROOF.md` contains the detailed source-level review, including an
independent challenge of the other agent's birth-order containment proof.
Under valid finite-set inputs and normal completion of the relevant phase,
the reviewed caller guards exclude registration of the same clause index
twice, inactive operands at actual SSE execution, and self-subsumption using
a clause's own matrix diagonal. Consumed FALSE comparisons remain sound under
the current unit context. A stronger birth-order argument establishes physical
unit containment before unit-HLA, including when individual inner frames skip
still-unrestricted raw ranges.

These claims do **not** establish strict containment. The other agent's
three-clause equality-skip counterexample falsifies that stronger property,
and this audit's outermost-unit postcondition detects it immediately. Running
the same case with `CNF_AUDIT_PROPERNESS=false` verifies that containment and
semantics remain intact. This is independent confirmation of the mechanism,
not a separately discovered fifth defect.

The attempted five-clause derivation of the same bug simplified correctly:
an additional donor-induced propagation repaired its pending work. That
failed candidate was not retained as a supposed counterexample.

### Decision-boundary experiment

`exp02_decision_invariants.R` injects entry/exit hooks into a source copy of all
nine recursive helpers. It checks the complete truth table of every live
intermediate formula against the input, rejects false contradiction signals,
checks actual consumed matrix certificates under units, and checks completed
outermost registrations. It never modifies production code.

Run:

```
CNF_TRIALS=20000 CNF_SEED=27 Rscript attic/cnf_verify3/proof_state/exp02_decision_invariants.R
```

The completed run took 138.8 seconds and verified:

- 20,000 inputs from the preceding campaign's standard/unit-merge generators;
- 435,330 helper entry/exit equivalence checks;
- 70,286 pair-count checks and 59,272 projected subset certificates;
- 59,250 outermost-unit postconditions;
- 23,374 `on_updated_subset_relations` and 9,855 `try_sse_2nd_order` entries
  with active operands;
- 444 matrix-based skips and 81 nested same-symbol registration exits.

No production violation occurred in that run. The generator's transient-state
coverage was limited: it did not expose an out-of-domain raw skip. The
independent solver's satisfiable dense example did, and was replayed through
these hooks: one deferred raw skip, one raw-stale retained-symbol certificate,
all 188 intermediate equivalence checks and three outermost-unit postconditions
passed. This establishes a concrete case where raw-per-skip reasoning fails
and deferred-ancestor reasoning succeeds.

Replays:

```
CNF_AUDIT_REPLAY=attic/cnf_verify3/independent_solver/outside_skip_satisfiable_dense.json CNF_SEED=801 Rscript attic/cnf_verify3/proof_state/exp02_decision_invariants.R
CNF_AUDIT_REPLAY=attic/cnf_verify3/independent_solver/minimized_subsumption.json CNF_SEED=802 Rscript attic/cnf_verify3/proof_state/exp02_decision_invariants.R
CNF_AUDIT_REPLAY=attic/cnf_verify3/independent_solver/minimized_subsumption.json CNF_SEED=803 CNF_AUDIT_PROPERNESS=false Rscript attic/cnf_verify3/proof_state/exp02_decision_invariants.R
```

The middle command intentionally fails on the confirmed equality defect.
The last separates containment from strictness. Replays are capped at 100,000
assignments because this experiment uses a complete truth table.

### Detector correction

The first audit version incorrectly checked deleted-symbol columns of a newly
inactive unit matrix. Trial 1747 failed that over-strong assertion. The code
intentionally never repairs these columns, and registration only reads the
remaining unit symbol. The corrected audit checks exactly that symbol. The
old failure RDS is retained with this explicit classification so it cannot be
mistaken for a production defect.

### Small executable proof obligations

`certificate_obligations.py` exhausts the pointwise Boolean states for source
restriction, non-unit target restriction with reverse repair, unit target
restriction without reverse repair, symbol deletion, raw-comparison creation,
and the birth-induction discharge step. Since inclusion is pointwise, these
local implications cover arbitrary finite domain sizes. This checks the
algebraic transition obligations, not every R scheduling path.

The root agent independently owns the reverse implication-chain stack-overflow
finding. A duplicate unrun resource script was removed from this directory;
see the root campaign's chain-depth experiment instead.
