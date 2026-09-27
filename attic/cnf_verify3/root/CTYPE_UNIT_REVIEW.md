# Independent native-name reproduction, complete small boundary, and candidate

2026-09-06. The character-identity stream discovered this semantic failure;
root independently reconstructed it through public constructors and a separate
positional valuation oracle. The source remains unchanged. The author's
[finding and source trace](../character_identity_review/FINDING.md) identify
the primitive mismatch and exact source transition.
The subsequent [independent source review](../ctype_semantics_review/REVIEW.md)
confirms the cause, proves the two-clause lower boundary, and supports the
specific candidate under its narrower stable/injective-key contract.

## The failure has canonical clauses

Use a single unchanged universe. X has the ordinary marked UTF-8 symbol name
`"\u00e9"` and domain `c("a", "b", "c")`; Y has name `"Y"` and domain
`c("a", "b")`. The public formula is

```
X=a AND (X=b OR Y=a) AND (X=c OR Y=b).
```

Every clause has unique nonmissing names and plain nonempty proper character
ranges. All domain and literal values are ASCII. The first unit forces both
Y=a and Y=b, so the input has no models. Under `LC_CTYPE=C`, normal construction
instead returns just the two binary clauses. They accept X=c,Y=a and X=b,Y=b.
Under `LC_CTYPE=C.UTF-8`, the same public construction returns FALSE.

This is a different cause from the dimensional selector failure. Here the
representation is syntactically canonical. A registry name obtained through
`names(environment)` is an ASCII escape such as `"<U+00E9>"`, which does not
equal the original marked string. Direct access using that original marked
string still retrieves the binding. Source line 502 therefore omits the
existing unit from both initial propagation sequences. At unit HLA the source
assumes the missing propagation has established containment and finally
deletes the unit at line 777. This deletion is the first actual truth change.
Merely missing the earlier propagation had not yet changed the stored formula.

The mathematical symbol-map premises of the preservation and totality proofs
now explicitly cover identity across list access, environment access, and
environment enumeration. Passing checkmate assertions or a canonical shape
check alone does not establish that premise. This counterexample rules out
the previously tempting interpretation of "ordinary R character values" as
an unconditional all-locale symbol-name guarantee.

## A complete independently specified finite bank

[`ctype_unit_boundary.R`](ctype_unit_boundary.R) does not source the author's
harness or use CNF `all.equal`. Its 20 public proper-clause types consist of
six X units, two Y units, and twelve X/Y binary clauses. It executes every
ordered list of zero through three clauses, allowing repetitions: 1 + 20 +
400 + 8,000 = 8,421 inputs. Each input's six valuation rows are evaluated
positionally from the original public clause payloads before constructing the
formula. Output names are matched to the two explicitly supplied symbols;
an unrecognized name is an oracle error, not silently discarded.

Four configurations independently cross ASCII versus marked UTF-8 X names
with C versus C.UTF-8 character locales. Both R 3.6.3 and R 4.6.1 complete
all **33,684 public calls** with exactly the same counts:

| Cohort | Truth differences |
| --- | ---: |
| Every zero-, one-, or two-clause list in all four configurations | 0 |
| Three clauses with ASCII X, either character locale | 0 |
| Three clauses with marked UTF-8 X, C.UTF-8 character locale | 0 |
| Three clauses with marked UTF-8 X, C character locale | 612 of 8,000 |

Of those 612 wrong triples, 36 change an unsatisfiable input to a satisfiable
output and 576 widen an already satisfiable input. Across all wrong triples,
972 valuation occurrences are newly admitted and none is removed. There are
no caught runtime errors. R 4.6.1 emits native-translation warnings but returns
the incorrect formulas; R 3.6.3 also reproduces the truth difference.

The first stored satisfiable counterexample is

```
(X=b OR Y=b) AND (X=a OR Y=a) AND X=a.
```

The input's sole model is X=a,Y=b. Its result additionally admits X=b,Y=a.
The first unsatisfiable example is the stated primary witness with its two
binary clauses reversed. All cohort summaries and reduced examples are in
`ctype_unit_boundary_r36.json` and `_r46.json`; RDS retains exact encoded names
and native enumeration strings. Finite absence through two clauses in this
palette is not by itself a global minimality proof.

The separate source review now supplies a clause-count proof under consistently
encoded unique representatives and stable injective native binding keys,
allowing enumeration to change their spelling. With at most two actual
clauses, all-units inputs return before enumeration; no-initial-unit inputs
only enumerate an empty registry; and one missed unit leaves at most one
nonunit donor. That donor's complement cannot contain its own nonempty range
or fill a domain, so unit HLA cannot commit an incorrect deletion. The stated
three-clause example is therefore minimal within that exact scope. This is
not a minimum claim for encoding aliases, malformed storage, domain sizes,
or the number of outer list members before nested formulas are flattened.

## Actual installed-package execution

[`ctype_package_modes.R`](ctype_package_modes.R) uses each actual R 4.6.1
namespace: the normal package installation, the installation made with
`--no-byte-compile`, and `pkgload::load_all` development loading. It independently
compares the formals and source bodies of all 56 CNF function definitions
with the current six source files, removing only source-reference metadata
from temporary comparison copies. Every comparison passes.

Each namespace runs all six clause orders with UTF-8, Latin-1, and ASCII X
names under both C and C.UTF-8 character locales: 36 calls per mode, 108 total.
All 12 C/non-ASCII calls per mode have the same two wrong models; all 24
controls return FALSE. The kernel closure in each execution belongs to the
package namespace. Captured warning text, witnesses, exact inputs and runtime
provenance are saved in the `ctype_package_*.json`, `.rds`, and `.log` files.
This checks the new fixture in real package use; it does not claim an R
compiler theorem or repeat the earlier 24-mode bank's counts.

## A narrow, private correction and its limits

The `--candidate` mode replaces exactly one expression in a private in-memory
copy of the simplifier:

```r
# Original source line 502 right-hand side:
char_intersect(names(entries[[clause_idx]]), names(unit_domains))

# Private diagnostic right-hand side:
Filter(function(symbol) !is.null(unit_domains[[symbol]]),
  names(entries[[clause_idx]]))
```

The candidate preserves clause-symbol order and tests the actual registry
binding directly. Under the ordinary faithful-name contract, a registered unit
range is non-NULL and the two expressions select exactly the same symbols in
the same order. Under the specific failed-enumeration contract here, repeated
direct access by the consistently marked symbol still works, so the candidate
restores the missing initial propagation. It changes no inference rule,
registry write, or comparator.

The independent review also proves a source simulation for the candidate:
give every consistently represented variable a distinct ASCII name and pair
registries through their stable injective native keys. The replacement makes
initial unit selection agree; every other registry operation already uses
direct keys. The remaining source decisions correspond. This transfers the
canonical semantic theorem without requiring faithful name enumeration.
Its 140 fresh controls per R version cover UTF-8, Latin-1, Greek, and two
Unicode variables, and confirm that ambiguous aliases remain unrepaired.

The same complete **33,684-call bank per R version** passes with zero truth
differences and zero errors in candidate mode. Production failures are still
reproduced in the separately saved original-mode runs. No production source
or installed namespace is changed by this experiment.

This is not a complete public character-name repair. The author also shows
that C-locale UTF-8/Latin-1 aliases can register separately yet compare equal
as ordinary strings, and that an actual ASCII escape spelling can collide
with a Unicode name's native key. Those accepted paths can violate clause-name
uniqueness or the symbol-to-domain map before the proposed expression runs.
Whether to normalize names, encode internal keys, or reject names whose native
identity cannot be maintained is a separate API decision. The candidate makes
the demonstrated single-name failure concrete without claiming to resolve
those other cases.

Reproduce from the repository root, redirecting logs as desired:

```sh
Rscript attic/cnf_verify3/root/ctype_unit_boundary.R
Rscript attic/cnf_verify3/root/ctype_unit_boundary.R --candidate
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/root/ctype_unit_boundary.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/root/ctype_unit_boundary.R --candidate
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/root/ctype_package_modes.R installed
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/root/ctype_package_modes.R installed_no_bytecode
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/root/ctype_package_modes.R development
```

All scripts restore the caller process's original character locale after their
experiments. Each command launches its own R process. Production hashes remain
those of `09770eaa`; the task-local installed package libraries are ignored by git.
