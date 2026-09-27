# Independent conditional-coverage review

`REVIEW.md` audits the root's catalog and proves the four missing TRUE
outcomes unreachable under the canonical source-level contract. It explains
the exact scope of “all feasible if outcomes” and the legitimate stale-unit
restriction entry that must be excluded from a stronger blanket assertion.

Run the independent parser, saved-witness, and local-contract checks from the
repository root:

```
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/branch_review/review.R
python3 attic/cnf_verify3/branch_review/check_hla_complements.py
```

The review reads the completed mixed/dense root artifacts without changing
them. `independent_catalog.tsv` adds source locations to the combined counts.
`review.log` and `review_results.rds` preserve 57 exact-input replays, 212
covered outcomes, the local contract counts, and illustrative states.
`hla_complement_results.json` records 77,540 independent finite-set checks.
