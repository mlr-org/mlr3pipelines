# Normalization and component-separability review

Independent source review: [REVIEW.md](REVIEW.md).
Exact experiment counts and controls: [RESULTS.md](RESULTS.md).

Run from the repository root:

```sh
Rscript attic/cnf_verify3/normalization_component_review/checks.R
Rscript attic/cnf_verify3/normalization_component_review/local_algebra.R
Rscript attic/cnf_verify3/normalization_component_review/boundary_checks.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/normalization_component_review/checks.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/normalization_component_review/local_algebra.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/normalization_component_review/boundary_checks.R
Rscript attic/cnf_verify3/normalization_component_review/compare_versions.R
```

`observer.R` instruments a fresh in-memory copy of production source and provides
an independent assignment evaluator and HLA multiplicity/exception checks.
`checks.R` compares seven storage forms and whole/separate component executions
through repeated passes. `local_algebra.R` enumerates bounded virtual multiset
states and two-step base-R extensions. `boundary_checks.R` uses real public
constructors and checks added unused universe bindings. Main RDS artifacts retain
direct input/output/trace records; JSON and logs contain the named counts and
controls. `compare_versions.R` compares loaded records directly, excluding
noncanonical binary-serialization hashes.

The default seed and 120 normalization/240 component generated cases are fixed.
`REVIEW_RANDOM=0` runs the directed/exhaustive pilot without the generated cases.
Only this directory was changed. Production source, proposal documents and other
streams are unmodified, and no commit was made.
