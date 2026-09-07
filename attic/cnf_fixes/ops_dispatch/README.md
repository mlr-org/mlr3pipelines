# Backward-compatible operator dispatch proposal for bug 19

Bug 19 remains open in production. A temporary prototype confirms a repair
can retain the current R >= 3.3 declaration: register one shared function for
`&` and one shared function for `|`, each for all three CNF classes. For example:

```r
S3method("&", CnfAtom, cnf_and)
S3method("&", CnfClause, cnf_and)
S3method("&", CnfFormula, cnf_and)
S3method("|", CnfAtom, cnf_or)
S3method("|", CnfClause, cnf_or)
S3method("|", CnfFormula, cnf_or)
```

The common `&` handler can use the current Formula implementation. The common
`|` handler chooses formula distribution when either operand is a formula,
and clause disjunction otherwise. Result classes and constant behavior are
preserved. Distinct per-class wrappers must be removed from method dispatch;
the registrations must point to the shared implementation itself.

R examines both operand classes for group-generic operators. When both resolve
to the same method, no `chooseOpsMethod()` arbitration is needed. See the
[official group-generic documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/groupGeneric.html).
The local R 3.6 `parseNamespaceFile()` accepts the three-argument S3method
directive shown above.

Run `Rscript attic/cnf_fixes/ops_dispatch/prototype.R` from the repository root
in a fresh process. The prototype sources the production CNF functions and
changes only that process's methods. On R 3.6.3 and R 4.6.1, shared compiled
registrations pass **242 operator cases per runtime**, checking every ordered
combination of proper/TRUE/FALSE Atom, Clause and Formula operands plus bare
logical constants, with independent truth tables and result-class checks.

An important failed alternative is preserved in the script: assigning one
closure to several method names works while sharing survives, but separately
byte-compiling those bindings restores the incompatible-method failure on
R 3.6. The namespace registrations should reference one compiled function
directly. Defining several wrappers that delegate to one function likewise
leaves distinct methods. This is a dispatch prototype, not a complete package
implementation or a full regression run; an implementation should also test
actual namespace loading and retain all existing CNF regressions.
