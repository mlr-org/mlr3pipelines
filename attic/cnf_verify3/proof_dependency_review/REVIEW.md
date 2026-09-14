# Independent audit of proof dependencies and exact contracts

Reviewed 2026-09-06 against the unchanged source hashes in
`SOURCE_HASHES.sha256`. The principal statements are
`../root/TOTAL_CORRECTNESS_COMPOSITION.md` and
`../boolean_occurrence_totality/PROOF.md`. I also read their indexing,
composition, occurrence-semantic, canonical-semantic, lifecycle, recursion,
and signature-potential reviews and checked the relevant source paths.

## Verdict

I found no circular dependency or counterexample to either stated
source-level total-correctness conclusion under its representation and
ordinary-operation/resource contract. In particular, no needed finite-prefix
shape or progress invariant is justified only by assuming the current whole
execution returns normally or preserves its truth function.

There is one premise-alignment clarification for the canonical composition:
its Section 1 explicitly describes the literal values as distinct nonmissing
characters, but describes each universe domain only as a "fixed nonempty
domain." The imported indexing proof explicitly requires the domains
themselves to be ordinary finite sets/vectors of distinct nonmissing character
values. My verdict retains that common stronger domain contract, made explicit
below. Repeating it in the composition would remove the ambiguity. This is
not a demonstrated counterexample to the source. The Boolean extension already
states its two-element domain condition explicitly.

The Boolean extension correctly replaces two false canonical premises:
registry membership may omit retained occurrences, and the canonical unit-HLA
lazy-row count equality can fail. Physical counts still establish pivot
scalarity. The separate structural proof that unit HLA cannot start supplies
the missing shape guarantee. That no-start proof does not need the semantic
grouped-containment argument or orphan birth, so its use by the semantic
theorem is noncircular.

`DEPENDENCIES.md` gives a minimal source-lemma graph and an additional simpler
termination proof using **all stored value occurrences**, including eliminated
slots. It establishes `helper_depth <= 6*Q0+11` without fiber closure,
signature freezing, truth preservation, or a live-target descent premise.
The existing sharper bounds remain justified; the new bound is a weaker
independent route to finite execution.

This is a source-level mathematical audit with small executable controls,
not a mechanization of R. No production edits, other-stream edits or commits
were made.

## Exact scope retained by this verdict

| Requirement | Canonical kernel | Homogeneous Boolean occurrence extension |
| --- | --- | --- |
| Entry point | `simplify_cnf(entries, universe)` on an already normalized kernel representation. | The same kernel representation, also reached by the specified direct `CnfFormula(list_of_proper_clauses)` path. |
| Clauses | Finite nonempty ordinary named lists, with one occurrence of each valid nonempty nonmissing name. Duplicate whole clauses and arbitrary input order are allowed. | Finite nonempty ordinary named lists; a name may occur repeatedly, with equal singleton ranges at every initial copy in that clause. |
| Literal values | Nonempty proper finite subsets of a fixed domain; ordinary distinct nonmissing character values. | Singletons belonging to ordinary two-element domains of distinct nonmissing character values. Opposite copies under one name in a single clause are excluded. |
| Universe | Every occurring name resolves consistently in one unchanged finite symbol-to-domain map; each domain is an ordinary nonempty finite vector/set of distinct nonmissing character values. | The same, with two values per occurring symbol, and every proper public clause must carry the identical shared universe. |
| Constants | Separately normalized scalar TRUE or FALSE; the empty conjunction is allowed. This excludes logical NA. | The direct list consists of proper clauses; empty input separately returns TRUE. Arbitrary mixed/nested constant/operator paths are outside the extension statement. |
| Ordinary execution | Ordinary set membership, indexing and copy semantics; no overriding method, active binding, external mutation or arbitrary caller computation changes the inspected operations. | The same. Standard constructor-created representation is permitted; arbitrary overloaded objects are not. |
| Finite resources | Source arithmetic/counts and storage sizes/indices are representable; primitive operations have sufficient allocation and stack resources. | The same. Finite input alone does not imply sufficient physical resources. |

The representation assumptions do not presume that the *next source
subscript* is valid. That is proved. Likewise, the resource assumptions
exclude actual interpreter resource limits but do not presume finite
source control flow. That is separately proved. A mathematical ghost
potential need not itself be computed in an R machine integer.

The direct public bridge is narrow and checkable: for a proper `CnfClause`,
`R/CnfFormula.R:153` obtains `c(cl)`, line 164 checks the shared universe,
line 169 appends that one ordinary clause, and line 178 enters the kernel.
Repeated positions are retained. This is not a theorem about every object
that has a `Cnf*` class or every accepted selector/constructor result.

Excluded conclusions include unconditional machine error-freedom, arbitrary
malformed duplicate-name input, multivalued unequal repeated ranges,
logical-NA selectors, universe mutation, mixed encodings with inconsistent
equality, overflow, external argument evaluation, completeness, a canonical
output, idempotence, order-independent output, and a Boolean occurrence cost
bound depending on clause count alone. The known selector counterexamples,
scheduling gaps, and recorded stack failures are unaffected.

## Four tempting strengthenings that are false or insufficient

1. **Physical row counts are sufficient for semantic donor premises.** They
   are sufficient for physical cardinality only. With a row `(X=TRUE,
   Y=TRUE)` and selected names `(X,X)`, the repeated matched-position sum is
   two, equal to the full row sum, while the unrelated Y exception remains.
   Each separately selected name is still scalar. The occurrence SSE2 proof
   correctly needs its separate Boolean case split.
2. **Every retained occurrence is currently registered.** Removing one copy
   clears all current registry entries for that clause/name. A later saved
   snapshot occurrence can take the cache skip and leave an orphan. The
   valid claim is sound current membership plus the separate multiset and
   orphan facts.
3. **Grouped FALSE containment is raw across all HLA targets.** Source HLA
   writes concern the current virtual target. On the known input
   `(X1 OR X2), (X1 OR X3), (!X3 OR X4)`, an X3 FALSE comparison can describe
   the expanded virtual first target while that symbol remains absent from
   its stored clause. Later targets use their own untouched rows. The same
   issue occurs with repeated X1 in the first target.
4. **The canonical lazy-row count invariant extends to Boolean occurrences.**
   The signed-word input `(-1,2), (3,-2), (1,1,1), (-3,-1)` reaches unit HLA
   with unit `X1=0`, an orphan donor `(X1=1 OR X1=1)`, no current X1 registry
   members, count two and a hypothetical all-FALSE row. The actual selection
   is NA, so the row is never consumed. Assuming the nonexistent equality
   would be wrong; the nonreachability proof is essential.

The last two witnesses are identified review fixtures, explicitly reused as
calibration inputs rather than claimed as new discoveries. All statements
above are compatible with preservation and finite normal return in the two
valid input classes.

## Focused executable checks

`check_dependencies.R` sources only the unchanged simplifier and base R.
It independently extracts the 13-helper call graph, adds the deferred
intersection-to-union edge, checks the acyclic canonical remainder and the
Boolean deletion-edge cut, and inserts six private read-only observer sites.
An observer adds a fresh private binding to each actual restriction frame
to record its entry potential; it does not force the deferred restriction
argument. All ordinary completed results are checked identically against a
separate unchanged-source call and against a positional truth-table oracle.

The five focused inputs are a reversed seven-link unit chain, the canonical
and repeated-name HLA calibration inputs, an equal-name SSE2 input, and the
unit-lazy mismatch calibration input. R 3.6.3 and R 4.6.1 both pass with
identical event counts:

| Observation across five completed cases | Each R version |
| --- | ---: |
| Exact observed/original result and positional truth-table comparisons | 5 |
| Restriction entries | 16 |
| Strict decreases against active restriction ancestors | 23 |
| Initialized physical pair-row/count equalities | 126 |
| Old-snapshot cache skips, each with absent current membership | 2 |
| Unit-HLA initializations, each unable to select a donor | 9 |
| Current-virtual-target FALSE comparisons that fail against stored target | 4 |
| Hypothetical lazy rows whose count is wrong | 1 |

The final two rows record expected counterexamples to the stronger
invariants, not failed checks. Current virtual containment and actual
unit-loop nonreachability both pass.

A sixth private invocation deliberately stops in the observer at the first
nested restriction after establishing `Q(parent entry)=15` and
`Q(child entry)=14`. Its whole result is never returned. The controlled
stop demonstrates that these observations can be made on a proper finite
prefix without using eventual output equality. It is not a claim that an
interrupted sample proves the universal prefix theorem.

The static canonical remainder's longest path has five vertices; the Boolean
cut graph has six. A separate ordinary-R repeated-match countermodel checks
the first invalid strengthening above. No branch guard or production
assignment is corrupted for these controls, and no broad random campaign is
used.

Reproduction from the repository root:

```sh
Rscript attic/cnf_verify3/proof_dependency_review/check_dependencies.R \
  > attic/cnf_verify3/proof_dependency_review/controls_r36.log 2>&1
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/proof_dependency_review/check_dependencies.R \
  > attic/cnf_verify3/proof_dependency_review/controls_r46.log 2>&1
```

`controls_*.rds` preserves the source MD5, R version, helper graphs, event
counts, positional calibration states and interrupted prefix. `controls_*.log`
contains the concise printed summaries. The finite controls test the stated
dependencies; the unbounded result follows from the source arguments. The two
saved RDS result objects are exactly identical after removing their version
labels, and all six recorded production SHA-256 hashes still match.
