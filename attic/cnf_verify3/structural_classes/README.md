# Structural simplification classes

`PROOF.md` distinguishes first-pass saturation of UP/SSE1/SSE2/HLA from actual
contradiction recognition. It proves:

* arbitrary finite-domain incidence forests finish all productive work during
  initial unit propagation, leaving every surviving clause/value essential;
* incidence pseudoforests also have complete initial contradiction recognition;
* two-symbol clauses with pairwise disjoint/equal ranges per symbol, including
  Boolean 2-CNF, reach local saturation in one productive pass;
* Boolean renamable Horn clauses have complete contradiction recognition.

`BOOLEAN_2CNF_GRAPH.md` gives an exact additional characterization. Form the
input implication graph, seed all initial units and every literal reached
from its complement in at most three edges, and take ordinary reachability
closure. This is the final unit set, unless both polarities occur, exactly
the condition for the simplifier to return FALSE.

`opposed_cycles.json` records the small negative boundary: two opposing
four-edge implication cycles sharing one Boolean symbol are unsatisfiable,
clause-minimal, locally saturated, and returned unchanged. Repeated calls
cannot solve this inference-strength limitation.

Reproduce the focused experiments from the repository root with:

```
attic/cnf_verify3/independent_solver/.venv/bin/python \
  attic/cnf_verify3/structural_classes/probe.py
attic/cnf_verify3/independent_solver/.venv/bin/python \
  attic/cnf_verify3/structural_classes/controls.py
attic/cnf_verify3/independent_solver/.venv/bin/python \
  attic/cnf_verify3/structural_classes/implication_graph.py
```

The prepared `cnf-review-r46` container must be running; see
`../review_semantics/R46_ENVIRONMENT.md`. Saved logs and JSON results record
the executed checks. All scripts invoke the unchanged production kernel;
truth tables, SAT/MDD, local set premises, domain propagation, and graph
reachability are separate independent checks as described in the proofs.
