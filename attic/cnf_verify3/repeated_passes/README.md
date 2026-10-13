# Repeated application investigation

`PROOF.md` gives the unbounded family, its scheduling proof, and the general
finite bound on productive passes. The source snapshot is identified there.

* `reproduce_three.R` is a self-contained 64-valuation reproduction of three
  useful passes, followed by one ordering-only call and an equality check.
  Its launch command is in the proof.
* `repeated.py` defines `chain_family(n)` and calls the original kernel through
  the persistent current-R bridge. It independently checks every intermediate
  result with both the one-hot SAT and MDD oracles.
* `scale_family.py` compares reverse and forward target ordering. Saved results
  in `family_scale.json` reach `n=32`: 32 versus one productive pass, with the
  same final normalized clause/value multiset.
* `check_family_rules.py` exhausts local SSE1/SSE2 premises at every prefix for
  `n <= 10` and checks explicit clause-essentiality witnesses. Results are in
  `family_rule_results.json`.
* `minimize_three.py` records the directed deletion minimization of the third
  family member; the complete result is `minimized_three_pass.json`.

For the Python experiments, use the existing isolated interpreter:

```
attic/cnf_verify3/independent_solver/.venv/bin/python \
  attic/cnf_verify3/repeated_passes/check_family_rules.py
```

The container `cnf-review-r46` must be running; the environment guide and
launcher are in `../review_semantics/`. These scripts depend on research
helpers in `../independent_solver/`, never change production sources, and
make no claim that a stored fixed point is a complete logical normal form.

An independent source/proof review and separate public-package truth-table
replay are recorded in `../quotient_review/REPEATED_PASS_REVIEW.md` and
`../quotient_review/check_repeated.R`. The reviewer found no substantive gap.
