# Public constructor and domain-storage closure review

The [source proof](PROOF.md) closes the canonical-actual-clause premise for
ordinary repeated, named and dimensional domains/atom values through atom
negation, clause construction, formula negation and mixed Boolean methods.
No new public path violating that premise was found in this scope.

A proper `CnfAtom` can retain matrix dimensions and repeated scalar values.
`CnfClause` normalizes with `unique(c(...))`. Formula negation bypasses that
constructor, but the inspected `setdiff` bodies produce plain unique
complements in both R 3.6.3 and R 4.6.1. Current R's conditional set-operation
coercion explicitly flattens matrices/higher arrays, and its class-stability
guard also flattens one-dimensional arrays. This is why the runtime change
does not open an ordinary dimensional complement path into the kernel.

The result assumes unchanged universes, ordinary total character operations,
canonical operands established by the reviewed grammar, supported dispatch,
and adequate execution resources. It does not include known selector-damaged
objects or repair existing constant-constructor, TRUE-class, `as.list(TRUE)`
or comparison-normalization issues. The source proof states exact constructor
success conditions and distinguishes these prior findings.

## Minimal ordinary example

After loading the package/source:

```r
u = CnfUniverse()
X = CnfSymbol(u, "X", matrix(c("a", "b", "b", "c"), 2L))
a = CnfAtom(X, matrix(c("b", "b"), 1L))

a$values                    # a 1-by-2 matrix containing b, b
(!a)$values                 # plain c("a", "c")
c(as.CnfClause(a))          # list(X = "b")
c(!as.CnfFormula(a))        # list(list(X = c("a", "c")))
```

Both runtime logs in `boundary_cases_r*.log` preserve this exact sequence and
the separate known-exception controls. Double atom negation can change range
order to domain order; the claim is exact membership, not atom-object identity.

## Results

Real checkmate 2.3.4 and all six unchanged production CNF source files are used
on both runtimes. Primary runs do not replace a constructor, operator or
simplifier. The separate gate run adds only a pre-call representation check
and then executes the original simplifier with unmodified inputs.

| Check | R 3.6.3 | R 4.6.1 |
| --- | ---: | ---: |
| Stored domain objects in the exhaustive primitive grid | 1,500 | 1,500 |
| Proper atoms | 113,064 | 113,064 |
| Atoms retaining dimensions | 64,584 | 64,584 |
| Atoms retaining repeated scalar values | 17,748 | 17,748 |
| Atom negations | 226,128 | 226,128 |
| Canonical clause constructions | 226,128 | 226,128 |
| Constant classifications/negations | 6,000 | 6,000 |
| Generated formula cases, each built with stored and normalized inputs | 400 | 400 |
| Exact stored-versus-normalized comparisons | 400 | 400 |
| Direct mixed Boolean method calls | 5,850 | 5,850 |
| Supported native Boolean calls checked | 3,770 | 5,850 |
| Known old-R mixed-dispatch exclusions | 2,080 | 0 |
| Result/oracle comparisons in composition | 31,838 | 33,918 |
| Assignment rows in composition comparisons | 487,514 | 520,794 |
| Kernel entries checked by the secondary gate | 18,716 | 20,705 |
| Known raw-TRUE/Clause result-class losses | 52 | 52 |
| New wrong truth, noncanonical clause, or unexpected error results | 0 | 0 |

The mixed-call pool has 15 operands and checks every ordered pair under both
operators for 13 shapes. The 52 class losses are prior findings and have
correct Boolean truth. The old-R exclusions concern native dispatch only;
the direct selected CNF methods still pass those cases. The differing result
and valuation counts arise solely from the extra native calls on current R.

The 400 random cases use seed 908216. They contain up to three symbols with
two through four distinct domain values and one through five proper input
clauses. Both construction variants together include 578 nonconstant formula
negations and 222 contradiction negations. Each contradiction negation returns
TRUE. These cases supplement the source theorem; they do not exhaust formulas.

There are 6,250 saved exact payload records per composition run. Their payloads
agree between both runtimes, including unsorted literal values; complete
records agree between primary and gated execution on each runtime.
`compare_results.log` records that check. There are 182 harmless cross-version
metadata differences, all explicitly checked: raw logical matrices have
implicit class `"matrix"` on R 3.6 and `c("matrix", "array")` on R 4.6. Their
payloads agree exactly, and all CNF result-class records also agree.
The small RDS files contain result payloads without universe environments,
not normalized/sorted substitutes for returned ranges.

## Reproduction

Run from the repository root in the existing environment:

```sh
Rscript attic/cnf_verify3/constructor_domain_closure/primitives.R
Rscript attic/cnf_verify3/constructor_domain_closure/boundary_cases.R
Rscript attic/cnf_verify3/constructor_domain_closure/composition.R
CNF_CLOSURE_GATE=1 Rscript attic/cnf_verify3/constructor_domain_closure/composition.R

bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/constructor_domain_closure/primitives.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/constructor_domain_closure/boundary_cases.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/constructor_domain_closure/composition.R
podman exec -e CNF_CLOSURE_GATE=1 cnf-review-r46 Rscript attic/cnf_verify3/constructor_domain_closure/composition.R

Rscript attic/cnf_verify3/constructor_domain_closure/compare_results.R
```

The existing current-R container is described in
[`R46_ENVIRONMENT.md`](../review_semantics/R46_ENVIRONMENT.md). No package
installation was needed for this stream. JSON reports save exact counts,
versions and source MD5 hashes. `base_contract_r*.txt` preserves the inspected
primitive bodies and coercion decisions. These are standalone focused audit
scripts; no full package test suite was run and no production code was edited.

Production SHA-256 checksums at review time:

```
34ceb2fb392db49ba47adf2a970a824222dfa792b93a895147f0a437b5bdd13d  R/CnfUniverse.R
19a159daa63d2bafb8f5238cfd91d9940a81c08f366bbc5ecb81eca3a3807341  R/CnfSymbol.R
d03da14a7bce4989477efa09303e099a3d5b7a8abb3be3526ee850161a59dce6  R/CnfAtom.R
40d021025c290a5ad6522fadcda166ff55d83bdd1d27eb0f4fd1fbc6222f12f8  R/CnfClause.R
e94aabfb277cf3bc2c951a09571658fd7dba0b14bce484f8c7d642e8d7e41f60  R/CnfFormula.R
7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc  R/CnfFormula_simplify.R
```

No production or other-stream file was edited, and no commits were created.
