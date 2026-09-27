# Installed CNF namespace compatibility check

This is an **isolated CNF subsystem package** named `cnfdispatchcheck`. It
contains the six production `R/Cnf*.R` files unchanged, the relevant generated
`NAMESPACE` directives copied verbatim, and real `checkmate`, `mlr3misc`, and
`digest` dependencies. Its scope is the CNF subsystem; the complete
`mlr3pipelines` installation on R 4.6.1 is checked separately in the
[parent validation record](../README.md#validation).

The baseline is reconstructed from commit
`da252ba5212fd9774e6a57f6dbc606bc35385c78`. The repaired package was copied from
the working tree after roxygen2 generated the six `cnf_and`/`cnf_or`
registrations; these files match implementation commit `9fb17923`. Exact
source and namespace hashes are in
`results/{baseline,current}_source_manifest.tsv`; the selected namespace
directives are in `results/{baseline,current}_NAMESPACE`.

`prepare.R` creates only the package scaffolding and extracts the applicable
generated registrations, exports, and dependency imports. It does not modify
the copied production R files or rewrite registration directives. `run.R`
executes `R CMD INSTALL --byte-compile`, loads the resulting installed
namespace in a fresh process, and runs `verify.R`. The verification does not
source production functions into the global environment or register methods
at runtime. It checks the actual resolved S3 methods, their owning namespace,
and their byte-compilation through `compiler::disassemble()`.

The repaired namespace resolves all three CNF classes directly to the same
compiled `cnf_and` closure for `&` and the same compiled `cnf_or` closure for
`|`. Both baseline namespaces resolve distinct methods. The loaded method
registration tables are saved in `results/*_registrations.txt`.

The binary matrix has 17 operands: one proper Atom, Clause, and Formula;
TRUE/FALSE wrappers in each class with and without a universe; and raw
TRUE/FALSE. Every ordered pair is evaluated with both operators, giving
578 cases per installation. An independent evaluator reads stored payloads
by position and checks all 12 assignments over domains of sizes 3, 2, and 2.
Each successful result must have the expected CNF class as well as the exact
truth vector. CNF `all.equal()` is not used as an oracle.

| Installed source | Runtime | Binary cases | Warning cases | Operator errors | Wrong class among returned values | Wrong truth among returned values |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| Baseline | R 3.6.3 | 578 | 300 | 108 | 192 | 0 |
| Baseline | R 4.6.1 | 578 | 0 | 0 | 0 | 0 |
| Shared handlers | R 3.6.3 | 578 | 0 | 0 | 0 | 0 |
| Shared handlers | R 4.6.1 | 578 | 0 | 0 | 0 | 0 |

For example, the byte-compiled baseline on R 3.6.3 warns about incompatible
`&.CnfAtom` and `&.CnfClause` methods for a proper `atom & clause`, then errors
because the fallback operator cannot handle the list payloads. The repaired
package returns a CnfFormula with the expected truth vector on both runtimes.

Each repaired installation additionally passes all 36 ordered cross-universe
error controls without dispatch warnings, all 34 unary/double-negation
controls, and all six composed-negation controls. On the older-R baseline,
24 cross-universe controls encounter the dispatch failure before reaching
universe validation; the other 12 reject incompatible universes normally.
Five composed-negation cases fail through mixed dispatch. The current-R
baseline passes all these additional controls.

The runs used R 3.6.3 (2020-02-29) and R 4.6.1 (2026-06-24), both with
checkmate 2.3.4 and digest 0.6.39. The installed mlr3misc versions were 0.22.0
on R 3.6.3 and 0.23.0 on R 4.6.1. All four package installations completed
successfully and explicitly performed byte-compilation and lazy loading.
The isolated package intentionally has no manual pages; its install logs
record that package-scaffolding limitation.

Run these commands from the repository root. The R 4.6 wrapper uses the
existing environment documented in
`attic/cnf_verify3/review_semantics/R46_ENVIRONMENT.md`.

```sh
cnf_audit_dir=attic/cnf_fixes/ops_dispatch/installed_check
Rscript "$cnf_audit_dir/prepare.R" baseline
Rscript "$cnf_audit_dir/run.R" baseline > "$cnf_audit_dir/results/baseline_r36_console.log" 2>&1
bash attic/cnf_verify3/review_semantics/run_r46.sh "$cnf_audit_dir/run.R" baseline > "$cnf_audit_dir/results/baseline_r46_console.log" 2>&1
Rscript "$cnf_audit_dir/prepare.R" current
Rscript "$cnf_audit_dir/run.R" current > "$cnf_audit_dir/results/current_r36_console.log" 2>&1
bash attic/cnf_verify3/review_semantics/run_r46.sh "$cnf_audit_dir/run.R" current > "$cnf_audit_dir/results/current_r46_console.log" 2>&1
```

Every run writes an install log, console log, registration table, complete
binary-case TSV, and summary under `results/`. The copied baseline snapshot,
package build trees, and installed libraries are ignored by this directory's
`.gitignore`; only scripts, documentation, small manifests, and recorded
results are intended for version control.
