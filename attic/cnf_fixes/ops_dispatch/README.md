# Shared operator dispatch repair for bug 19

Bug 19 is fixed in **`9fb17923`**. The package retains its **R >= 3.3.0**
declaration. One shared function implements `&` and one implements `|`, each
registered for all three CNF classes.

## Roxygen implementation

The internal `cnf_and` and `cnf_or` functions in
[R/CnfFormula.R](../../../R/CnfFormula.R) each have three `@rawNamespace` tags
and `@noRd`. Roxygen2 generates these directives:

```r
S3method("&", CnfAtom, cnf_and)
S3method("&", CnfClause, cnf_and)
S3method("&", CnfFormula, cnf_and)
S3method("|", CnfAtom, cnf_or)
S3method("|", CnfClause, cnf_or)
S3method("|", CnfFormula, cnf_or)
```

The common `&` handler retains the previous Formula implementation unchanged.
The common `|` handler retains formula distribution when either operand is a
formula, and the previous clause disjunction otherwise. Result classes and
constant behavior are preserved. The class-specific binary wrappers and all
`chooseOpsMethod()` hooks are removed; unary `!` methods are unchanged.

Roxygen2 documents `@rawNamespace` for
[specialized namespace directives](https://roxygen2.r-lib.org/articles/namespace.html#manual-exports).
Its `@exportS3Method` tag generates two-argument registrations and cannot
express the shared implementation as the third argument. With roxygen2 8.1.0,
repeating that tag in one block also raises a duplicate-tag diagnostic.

## Why direct shared registrations matter

R examines both operand classes for group-generic operators. When both resolve
to the same method, no `chooseOpsMethod()` arbitration is needed. See the
[official group-generic documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/groupGeneric.html).
The three-argument `S3method` directive loads correctly on R 3.6, including
from the installed, byte-compiled package tested below.

Assigning one closure to several method names works while sharing survives,
but separately byte-compiling those bindings restores the incompatible-method
failure on R 3.6. Defining several wrappers that delegate to one function also
leaves distinct methods. The generated registrations therefore reference one
compiled function directly for each operator.

The historical [prototype.R](prototype.R) preserves those failed alternatives.
It now reads the pre-fix source from commit `da252ba5`, so it remains runnable
after removal of the old internal method names. Run it with
`Rscript attic/cnf_fixes/ops_dispatch/prototype.R` from the repository root in
a fresh process. The original proposal passed 242 shared compiled operator
cases on each of R 3.6.3 and R 4.6.1. Its updated baseline-loading step was
rechecked on R 3.6.3 after the production repair.

## Validation

The new [test_CnfOperators.R](../../../tests/testthat/test_CnfOperators.R)
checks shared method registration, both orders of mixed proper classes,
distribution over a two-clause formula, bare and wrapped constants, result
classes, unary and nested negation, and incompatible universes. Its truth
oracle evaluates stored payloads independently of CNF comparison methods.
Before implementation on R 4.6, it produced 133 passing expectations and
three failures identifying nonshared registrations. After implementation:

| Check | Runtime | Result | Record |
| --- | --- | --- | --- |
| Focused operator tests with `devtools::test()` | R 4.6.1 | 136 expectations pass; no failures, warnings, or skips | [focused_r46.log](focused_r46.log) |
| All CNF tests with `devtools::test(filter = "^Cnf")` | R 4.6.1 | 4,391 expectations pass; no failures, warnings, or skips | [all_cnf_r46.log](all_cnf_r46.log) |
| Operator tests against the installed, compiled full `mlr3pipelines` package | R 4.6.1 | 136 expectations pass; no failures, warnings, or skips | [Install](full_install_r46.log), [tests](full_operators_r46.log) |
| Installed, compiled isolated CNF subsystem | R 3.6.3 and R 4.6.1 | Each passes 578 binary cases, 36 universe-error controls, 34 negation controls, and six composition controls | [Detailed records and reproduction](installed_check/README.md) |

The isolated package copies all six production CNF files unchanged and the
applicable generated namespace directives verbatim, using real dependencies.
It checks actual namespace ownership and byte-compilation, then checks every
ordered pair of 17 proper/constant operands with both operators against truth
tables and expected classes. The baseline installed on R 3.6 produces 300
warning cases, 108 operator errors, and 192 wrong-class results among the 578
binary cases; the repair eliminates all of these. The R 4.6 baseline already
passes the behavioral matrix, providing a compatibility control.

The older-R installation validates the isolated CNF subsystem; the full
`mlr3pipelines` package was installed and tested on R 4.6.1. R 3.3 was not
available for execution. The simplifier kernel is unchanged and the eight
other [open bugs](../README.md#remaining-open-bugs) remain open.

## Reproduction and generation record

Run from the repository root. The R 4.6 wrapper and container are described in
[R46_ENVIRONMENT.md](../../cnf_verify3/review_semantics/R46_ENVIRONMENT.md).

```sh
bash attic/cnf_verify3/review_semantics/run_r46.sh -e 'devtools::document(roclets = "namespace")'
bash attic/cnf_verify3/review_semantics/run_r46.sh -e 'devtools::test(filter = "^CnfOperators$", stop_on_failure = TRUE)'
bash attic/cnf_verify3/review_semantics/run_r46.sh -e 'devtools::test(filter = "^Cnf", stop_on_failure = TRUE)'
podman exec cnf-review-r46 mkdir -p /tmp/cnf_ops_full_library
podman exec cnf-review-r46 R CMD INSTALL --library=/tmp/cnf_ops_full_library /work
bash attic/cnf_verify3/review_semantics/run_r46.sh -e '
  library(mlr3pipelines, lib.loc = "/tmp/cnf_ops_full_library")
  library(testthat)
  ns = asNamespace("mlr3pipelines")
  stopifnot(normalizePath(getNamespaceInfo(ns, "path")) ==
    "/tmp/cnf_ops_full_library/mlr3pipelines")
  for (handler in c("cnf_and", "cnf_or")) {
    invisible(capture.output(compiler::disassemble(get(handler, envir = ns))))
  }
  cat(R.version.string, "\nLoaded compiled full package from:",
    getNamespaceInfo(ns, "path"), "\n")
  testthat::test_file("tests/testthat/test_CnfOperators.R",
    reporter = "summary", stop_on_failure = TRUE)
'
```

The namespace-only documentation run used roxygen2 8.1.0. Initial loading
against the stale namespace warned about the removed methods; regeneration
replaced those registrations, and the installed package subsequently loaded
without method warnings. Documentation also reported existing missing-Suggests
cross-reference diagnostics unrelated to this change. Generator-only version
metadata changes in DESCRIPTION and unrelated import formatting were discarded;
the six CNF registrations are exactly those generated by roxygen2. No manual
pages changed in this repair. Recorded console output and generated summary
files have trailing whitespace removed; devtools logs also omit transient
spinner lines.
