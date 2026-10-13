# Domain storage contract audit

`PROOF.md` proves that flattening/deduplicating ordinary inert domains
accepted by `CnfSymbol` preserves exact actual-range payloads and the source
decision/scheduling sequence, provided clauses already have distinct symbol
names and flat unique proper ranges. HLA virtual ranges may contain names
and duplicates. A selected missing donor value together with a bound on
each value's multiplicity proves the two full-domain guards false.

No production files are changed. The proof explicitly excludes malformed
selectors, active bindings, custom semantic dispatch, changed universes,
encoding-inconsistent equality, and resource/index failures.

Both R 3.6.3 and R 4.6.1 complete campaigns passed. `RESULTS.md` gives exact
counts, controls, and the cross-version comparison. Each run checked 2,904
observed executions and 2,640 exact paired full event streams over 264
inputs; the 10,128,063 event counts agree across versions.

## Reproduction

Run from the repository root:

```sh
Rscript -e 'source("attic/cnf_verify3/domain_storage_contract/checks.R")'
Rscript attic/cnf_verify3/domain_storage_contract/primitive_checks.R
python3 attic/cnf_verify3/domain_storage_contract/local_lemma.py

bash attic/cnf_verify3/review_semantics/run_r46.sh -e \
  'source("attic/cnf_verify3/domain_storage_contract/checks.R")'
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/domain_storage_contract/primitive_checks.R
Rscript attic/cnf_verify3/domain_storage_contract/compare_versions.R
```

The source harness uses real `checkmate` 2.3.4 and unchanged `R/Cnf*.R`.
The only non-checkmate compatibility helpers are formatting wrappers for
`stopf` and `map_chr`; no assertion or simplifier is shimmed. The checks
use `digest` and `jsonlite` for artifacts. `DOMAIN_RANDOM_CASES` can change
the default 200 generated cases; the recorded campaign uses seed 860601.

## Experiments and independence

* `harness.R`: an independently written AST observer. It records every
  explicit condition, evaluated scalar short-circuit operand, structural
  loop sequence/iteration, helper entry, scalar/structural comparison,
  and actual range write. Entire event streams and returned payloads are
  compared with `identical()`. An independent parser token count checks
  the condition, loop, and short-circuit catalogs.
* `checks.R`: 57 exact saved inputs from the two earlier branch-coverage
  runs, seven directed inputs, and 200 independently generated inputs.
  The saved inputs are reused solely as concrete cases. Previous
  observers, decisions, and output traces are not reused. Each case is
  built through the public constructors and tested under 11 domain forms:
  plain, reordered, repeated, named, repeated/named, matrix, array,
  repeated/named matrix, repeated/named array, ordinary metadata, and
  an unused inert class marker. Same-universe public formula results and
  unmodified-kernel results are checked against the instrumented run.
* At HLA entry, fresh positional comparisons verify all live pair rows and
  counts plus physical unit containment. At every HLA extension, an
  independent recomputation verifies the donor mask, its unique exceptional
  symbol, the still-missing donor values, domain containment, support
  equation, multiplicity bounds, and strict length inequality. Paired
  runs also compare each selected target/donor/pivot and virtual support.
* An independent truth-assignment evaluator uses scalar value equality,
  no simplifier rules or source set helpers. It checks each baseline
  output against the original clauses; exact output equality then covers
  every storage form. It iterates raw lists/positions, avoiding CNF
  `as.list` dispatch.
* `primitive_checks.R`: direct concrete base-R checks of all selected
  extension cases on one to three distinct values, domain multiplicities
  one to three, every bounded old submultiset, every donor subset, and
  six domain storage shapes. It additionally checks empty/full atom
  classification and constructor canonicality.
* `local_lemma.py`: independent integer-count enumeration through five
  distinct values and multiplicities one to three. It checks 1,187,103
  selected extensions, including old submultisets broader than the actual
  reachable forms. This supports the stated unbounded mathematical lemma;
  finite enumeration is not its proof.

`sites.tsv` is the common source catalog. `results_r36.json` and
`results_r46.json` contain exact campaign counts and source hashes.
`checks_r36.rds` and `checks_r46.rds` preserve every exact input, returned
payload, trace digest, branch coverage, and first repeated/named virtual
witness. `primitives_r*.json` and `local_lemma.json` record the local checks.

## Calibration and limitations

The source schedule control adds a harmless `if (FALSE)` inside the actual
`char_setdiff` helper. It requires equal output, a changed event stream,
and an observation of that new branch; static site renumbering alone
cannot satisfy the control. A second source mutation changes domain
difference into intersection and must fail the independent missing-donor
check. Both mutations exist only in in-memory function bodies.

Directed algebra controls refute length-equality/full-support equivalence
without a selected donor exception, and refute the missing-value argument
without its capacity bound. They also demonstrate why the earlier general
preimage theorem cannot automatically handle unchanged single-copy actual
ranges together with repeated stored domains.

Instrumentation forces each original observed expression once and returns
that original value. The new read-only HLA hooks run only after source
values are established. Under the inert ordinary-value contract, added
frames cannot trigger user callbacks or mutate the kernel state. Exact
unmodified-kernel equality is checked for every observed run. This is
not a transformation theorem for arbitrary R programs, and resource
behavior is explicitly outside scope.

During test development, one pilot was stopped to replace a slow growing
list trace with an environment-backed trace. A second pilot was invalidated
by editing its `Rscript --file` input while R was reading it, producing a
malformed token near the control section. Neither is counted as evidence.
Recorded full runs use `Rscript -e 'source(...)'`, which parses the script
before evaluating it. These were harness issues; no production change or
counterexample resulted.
