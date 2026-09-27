# Arbitrarily wide four-clause first-order scheduling gap

The known four-clause gap survives appending the same proper fresh literals
to every clause. For every initial width `w >= 3`, there is a canonical
four-clause input with a common support of size `w` whose first result still
has a productive SSE1 restriction. This sharpens the clause-count boundary
of the separately reviewed three-clause width-at-least-three theorem.

[`PROOF.md`](PROOF.md) gives the precise family, exact ordered outputs,
truth algebra, finite one-padding trajectory, and a source simulation from
one padding literal to any positive number. The unpadded execution takes
different unit-related callbacks and is not silently treated as the same
source execution.

The [independent review](../wide_four_review/REVIEW.md) supports the full
source coupling and adds five fresh configurations with 65,840 complete
assignment rows per runtime. It explicitly observes default TRUE columns
before pair initialization and the distinct zero-padding unit paths.

Run the standalone public reproduction from the repository root:

```sh
Rscript attic/cnf_verify3/wide_four_boundary/reproduce.R 1
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/wide_four_boundary/reproduce.R 256
```

The first argument is the number of fresh common padding literals. These
commands reproduce input widths 3 and 258. Both return two clauses; the
second public construction removes `X=0` from the second clause. There is
one unchanged universe, real checkmate, ordinary ASCII domains/names, and
no fabricated CNF representation or comparison-helper oracle.

## Checks and results

`checks.R` tests padding counts `0,1,2,3,7,31,256` on native R 3.6.3 and the
existing `cnf-review-r46` R 4.6.1 container, both with checkmate 2.3.4.
Every configuration passes exact ordered first/second output assertions,
retains every pad verbatim, and preserves positional assignment truth.

Per runtime:

* Seven input widths: `2,3,4,5,9,33,258`.
* 6,944 concrete assignment rows, each evaluated on input and both outputs.
  Padding counts through seven use complete truth tables; the two larger
  controls test all-false, all-true, and each sole-true-padding assignment.
* Complete sixteen-row core truth tables plus common-padding factorization
  establish truth on all `16 * 2^k` assignments, including at large width.
* Independent ordered donor/target scanning finds exactly the leftover
  first-pass SSE1 restriction and none after the second pass.
* Every positive-padding run gives exactly 58 first-pass and 16 second-pass
  core/HLA records, identical across padding counts. Records include core
  entries, callback arguments, matrices without padding columns, row counts,
  eliminated flags, core registries and the shared padding registry.
* Observational invariants check that pads remain unchanged, initialized
  pair padding columns are FALSE, live cores never become empty, and no
  unit is registered with positive padding. The observer's actual output
  must always be identical to the unchanged public production output.

All saved records and traces are also exactly equal across the two R
versions (`cross_version.json`). This finite evidence supports the proof;
the arbitrary-width conclusion follows from the source simulation and
explicit family trajectory, not from testing width 258 alone.

```sh
Rscript attic/cnf_verify3/wide_four_boundary/checks.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/wide_four_boundary/checks.R
Rscript attic/cnf_verify3/wide_four_boundary/compare_versions.R
Rscript attic/cnf_verify3/wide_four_boundary/summarize_trace.R
```

`checks_*.rds` contains the complete traces; `results_*.json` records all
outputs, rules, widths and counts; `one_padding_state_changes.json` is a
compact source-trajectory aid. The native-R pilot in `pilot.R` additionally
verified that the alternate saved `minimized_sse1.json` gap survives the same padding idea;
the theorem is stated only for the equal-width, two-core-symbol family.

`source_hashes.json` records unchanged production source and the input
fixture hash. No production/other-stream files were edited, no commit was
made, and no package-wide test suite was run.
