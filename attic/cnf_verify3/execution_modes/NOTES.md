# Source harness versus normal package execution

Completed 2026-09-06 using the existing `cnf-review-r46` local container.
This distinct follow-up asks whether the source-based CNF evidence faithfully
represents installed/development package behavior. No production file,
namespace binding, or source test was modified, and no commit was made by
this stream. No package test suite was rerun.

## Result

All **24 runtime configurations** agreed exactly on **192 fixture executions,
1,752 constructor/operator paths, and 384 serialization cases**. This
includes the seven reduced canonical scheduling fixtures and the four-clause
matrix-selector semantic example. Every canonical fixture preserved its
independently evaluated input truth function in every mode. Every list
constructor replay of the selector example returned the same incorrect
canonical formula `X=c AND Y=a`, with the separating assignment `X=c,Y=a`.

The source-based evidence is therefore faithful to ordinary package use for
these measured inputs and paths. This finite runtime comparison does not
prove compiler correctness for all R programs or expand the canonical-input
mathematical proof to malformed representations.

## Runtime and source provenance

- R 4.6.1 (2026-06-24), `x86_64-pc-linux-gnu`, in the pinned local container
  described in `../review_semantics/R46_ENVIRONMENT.md`.
- The working-tree Git revision at installation was
  `30f29856d5afb3e36dd70853d3d87ed13c0e28a2`. The six production CNF files are
  unchanged from the campaign's `09770eaa` baseline. Later root commits had
  added research artifacts and focused tests only.
- Package DESCRIPTION version `0.11.0-9000`, reported by R's package-version
  parser as `0.11.0.9000`.
- Actual dependencies: checkmate 2.3.4, mlr3misc 0.23.0, mlr3 1.8.0,
  paradox 1.0.1, pkgload 1.5.3. These are real imported/attached packages;
  this follow-up uses no formatting or assertion helper replacements.
- The normal installation is in the ignored local `library/`; a second
  installation with `--no-byte-compile` is in `library_no_bytecode/`. Both
  were installed directly from this working tree using `R CMD INSTALL`.
- Development loading uses `pkgload::load_all(".", export_all=FALSE,
  helpers=FALSE)`. Package modes invoke the actual namespace's functions and
  registered S3 methods. Sourced modes invoke definitions from the same six
  files in the global environment, as the research harness does.

The six SHA-256 values are preserved in every per-runtime RDS result:

| File | SHA-256 |
| --- | --- |
| CnfUniverse.R | `34ceb2fb392db49ba47adf2a970a824222dfa792b93a895147f0a437b5bdd13d` |
| CnfSymbol.R | `19a159daa63d2bafb8f5238cfd91d9940a81c08f366bbc5ecb81eca3a3807341` |
| CnfAtom.R | `d03da14a7bce4989477efa09303e099a3d5b7a8abb3be3526ee850161a59dce6` |
| CnfClause.R | `40d021025c290a5ad6522fadcda166ff55d83bdd1d27eb0f4fd1fbc6222f12f8` |
| CnfFormula.R | `e94aabfb277cf3bc2c951a09571658fd7dba0b14bce484f8c7d642e8d7e41f60` |
| CnfFormula_simplify.R | `7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc` |

All 56 top-level CNF function definitions have identical formals and syntax
after source-reference metadata is removed from temporary comparison copies.
The functions actually executed are unchanged. Installed function closures
identify `namespace:mlr3pipelines`; source closures identify the global
environment.

One methodological surprise was resolved before drawing runtime conclusions:
pkgload retains `srcref`, `srcfile`, and `wholeSrcref` attributes on 52 bodies,
so raw `identical(body(source),body(development))` is false even though the
code is identical. `utils::removeSource()` on temporary function copies
removes precisely this difference. The result files preserve both raw and
normalized comparisons, and `runtime_provenance.tsv` reports the counts.
This was a comparison-harness issue, not a production behavior discrepancy.

## Compiler matrix

Each mode was run in a fresh process at JIT levels 0, 1, 2, and 3:

| Mode | Construction/loading | Initial bytecode |
| --- | --- | --- |
| source | Six original R files, real dependency packages | Absent |
| cmpfun0 | Each source CNF function passed through `compiler::cmpfun`, optimize=0 | Present |
| cmpfun3 | Each source CNF function passed through `compiler::cmpfun`, optimize=3 | Present |
| development | Actual pkgload namespace | Absent |
| installed | Normal byte-compiled package installation | Present |
| installed_no_bytecode | Actual package installed with `--no-byte-compile` | Absent |

Bytecode presence was measured with `compiler::disassemble()` on
CnfFormula, CnfClause, and simplify_cnf before and after the cases. With JIT
0, uncompiled modes remained uncompiled. With JIT 1–3, these initially
uncompiled functions became compiled during use. Explicitly compiled and
normally installed functions were compiled from the start. Thus the matrix
actually exercised different execution modes; it did not merely relabel a
single always-compiled run.

## Constructor and ordinary-operator comparisons

The oracle is independent of CNF constructors and rewrite code. For each
plain fixture, it enumerates all finite-domain assignments and evaluates
every atom occurrence by equality, combines occurrences with `any`, and
combines clauses with `all`. Expected truth functions are computed before
construction. The matrix selector duplicates one original occurrence, and
the oracle explicitly checks that this does not change its input truth.
Every saved result contains the raw input, assignments, expected vector,
actual vector, and indices of discrepancies.

Each runtime compares the list constructor, a second constructor pass,
public `%among%`/`|` atom and clause construction, a left-associated public
`&` chain, identity coercion, and the TRUE/FALSE identity-operator paths.
It additionally checks negation and Boolean contradiction/tautology
identities from each actual operand's truth function. The selector fixture's
ordinary duplicate-vector selector is checked separately and returns FALSE.

The table reports stored value occurrences; smaller counts alone are not a
proof of a particular simplification rule. The original scheduler diagnoses
remain in the independent-solver reports.

| Fixture | Worlds / input models | Input → first → second | Sequential `&` result weight |
| --- | ---: | --- | ---: |
| minimized_subsumption | 12 / 3 | 9 → 3 → 1 | 3 |
| minimized_sse1 | 18 / 5 | 11 → 7 → 6 | 6 |
| minimized_first_order_phase_sse1 | 16 / 5 | 15 → 5 → 4 | 5 |
| minimized_sse2 | 48 / 9 | 23 → 18 → 17 | 17 |
| directed_oneend_shrink_min | 32 / 9 | 17 → 12 → 11 | 12 |
| minimized_oneend_symbol_removal | 24 / 9 | 14 → 10 → 9 | 10 |
| minimized_deferred_skip | 16 / 1 | 15 → 2 → 2 | 2 |
| selector | 9 / 0 | 8 → 2 → 2, wrong satisfiable output | 0, correctly FALSE |

All table entries, actual raw output lists, and error outcomes agree across
the 24 configurations. Public operator grouping can nevertheless change
the scheduler's reduction path: it processes intermediate prefixes, whereas
the list constructor receives all clauses together. Two canonical
fixtures have lighter outputs through the sequential chain. This is a
grouping/scheduling distinction present in every runtime, not a source versus
namespace or compiler difference.

The selector case makes this especially visible. The list constructor
returns the wrong `X=c AND Y=a`; a left-associated `&` chain returns FALSE.
Therefore one operator spelling succeeding does not invalidate the public
constructor counterexample. Public `%among%` and `|` followed by the same
list constructor reproduce it exactly.

The already observed `TRUE | CnfClause` class loss and accepted logical-NA
selector were also reproduced through each runtime's actual dispatch. The
study did not rerun the entire package test suite or infer other package
behavior from these CNF-only cases.

## Serialization and universe identity

Each of the eight proper first-result formulas was serialized and
deserialized under R serialization versions 2 and 3, in every runtime:

- Raw clauses and truth functions stayed exactly equal.
- Re-running CnfFormula on the restored formula's clauses produced exactly
  the same second-pass result as before serialization.
- The restored universe contains the same domain values but is a **new
  environment**, so it is not `identical()` to the original universe.
- Consequently `original & restored` raises the documented
  `Both formulas must be in the same universe.` error. That is an identity
  mismatch, not a changed Boolean meaning.
- Serializing a bundle containing the formula, its clauses, symbols and
  universe preserves all shared references *within the restored bundle*.
  Reconstructing its formula and combining two jointly serialized related
  formulas work and preserve the expected truth functions.

This distinction applies to the proper formulas tested here; a logical
constant converted without a universe has no environment identity to copy.

`cross_runtime.R` additionally writes eight real source-created bundles into
`source_transfer.rds` and reads them in two independent fresh sessions, one
using the development namespace and one using the installed package.
All eight bundles preserve class dispatch, internal universe sharing,
constructor replay, and independent truth functions in both readers. The
canonical scheduling results and the selector's wrong model survive this
transfer unchanged. No source closures are stored in the CNF objects.

## Reproduction and artifacts

Use the existing current-R launcher from the repository root:

```sh
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/execution_modes/run_matrix.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/execution_modes/cross_runtime.R write_source
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/execution_modes/cross_runtime.R read_installed
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/execution_modes/cross_runtime.R read_development
```

The local installation commands (run inside that container at `/work`) were:

```sh
R CMD INSTALL --library=/work/attic/cnf_verify3/execution_modes/library \
  --no-multiarch --no-html --no-docs --no-demo .
R CMD INSTALL --library=/work/attic/cnf_verify3/execution_modes/library_no_bytecode \
  --no-byte-compile --no-multiarch --no-html --no-docs --no-demo .
```

`runtime.R` performs each fresh-process comparison;
`run_matrix.R` launches the 24 cells and requires exact equality of complete
result records against source/JIT-0. Per-cell `.log`/`.rds` files retain
observations and provenance. `runtime_matrix.log` and
`runtime_matrix_counts.rds` record aggregate agreement.
`runtime_provenance.tsv` makes the metadata/bytecode differences readable.
The two installation logs and `cross_*.log` complete the execution record.
