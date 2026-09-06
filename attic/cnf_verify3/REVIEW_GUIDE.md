# Reading and reproducing the CNF investigation

Start with [FINDINGS.md](FINDINGS.md) for the current finding ledger and exact
proof scopes. [README.md](README.md) is the chronological investigation journal.
Production CNF source is unchanged from `09770eaa`; coherent commits contain
research scripts, lossless records, source proofs, independent reviews, and
focused tests protecting already-correct behavior.

## Small reproductions before large enumerations

Run from the repository root. Host `Rscript` is R 3.6.3. For R 4.6.1, replace
`Rscript` with `bash attic/cnf_verify3/review_semantics/run_r46.sh`. The existing
container and actual dependency setup are described in
[R46_ENVIRONMENT.md](review_semantics/R46_ENVIRONMENT.md).

| Result | Small entry point | What the record establishes |
| --- | --- | --- |
| Matrix clause selector changes truth | `Rscript attic/cnf_verify3/root/reproduce_selector_semantics.R` | A contradictory four-clause input returns the canonical satisfiable result X=c,Y=a. |
| Unicode name changes truth under C CTYPE | `Rscript attic/cnf_verify3/character_identity_review/reproduce_public.R` | A contradictory canonical three-clause input loses its unit and gains two models; the UTF-8-locale control returns FALSE. |
| Minimal selected-clause failures | `Rscript attic/cnf_verify3/small_selectors/reproduce.R` | Two-clause empty-mask runtime error and stale duplicate-value semantics, with positional interpretation. |
| Canonical scheduling omissions | `Rscript attic/cnf_verify3/root/replay_findings.R` | Reduced public examples preserve truth but change on another pass; separate raw rules identify the omitted operation. |
| Ordinary Unicode comparison failures | `Rscript attic/cnf_verify3/root/comparison_normalization.R` | Encoding-dependent digest alignment and locale collation ties produce false differences. |
| Dimensional atom comparison failure | `Rscript attic/cnf_verify3/root/atom_shape_comparison.R` | Accepted repeated scalar values select the same atom set but compare unequal; Clause conversion normalizes them. |
| Four arbitrarily wide clauses leave SSE1 unfinished | `Rscript attic/cnf_verify3/wide_four_boundary/reproduce.R 1` | Appending common proper literals retains the specific productive second-pass restriction. Increase the argument for wider clauses. |

These are investigation reproductions, not one combined production regression
suite. Some explicitly assert that the recorded baseline defect is present so
that an accidentally changed fixture cannot silently pass. A future production
repair should convert its semantic expectation into a regression test rather
than preserve the wrong-result assertion. Exact bodies, assertions, expected
outputs and scope are documented next to each entry point.

## Proofs to read by concern

The common semantic contract uses finite ordinary sets, unique consistent
symbol identities, immutable domains, supported indices/arithmetic, and the
stated runtime resources. The faithful symbol-identity premise is substantive:
the C-locale public counterexample meets syntactic canonicality and violates
environment-name enumeration identity.

| Concern | Main proof and independent challenge |
| --- | --- |
| Are individual restrictions/deletions logically sound? | [Semantic composition](independent_solver/SEMANTIC_PRESERVATION_MAP.md), [independent semantic review](review_semantics/REVIEW.md), and [HLA review](review_hla/REVIEW.md). |
| Can canonical execution fail through indices or run indefinitely? | [Total-correctness composition](root/TOTAL_CORRECTNESS_COMPOSITION.md), [index review](index_contract_review/REVIEW.md), and [dependency audit](proof_dependency_review/REVIEW.md). |
| Do proofs accidentally assume successful whole-call return? | [Explicit dependency graph and stored-value potential](proof_dependency_review/DEPENDENCIES.md). Prefix invariants and finite progress precede the semantic conclusion. |
| Which constructors establish the required actual storage? | [Constructor closure](constructor_domain_closure/PROOF.md), [independent review](constructor_closure_review/REVIEW.md), and [domain storage proof](domain_storage_contract/PROOF.md). Known constants/selectors/dispatch failures are separate. |
| Why can a valid Unicode name defeat the theorem? | [Native-key finding](character_identity_review/FINDING.md), [conditional identity simulation](character_identity_review/PROOF.md), and [independent causal/candidate review](ctype_semantics_review/REVIEW.md). |
| Can comparison incorrectly report TRUE? | [Default comparison soundness](comparison_soundness/PROOF.md) excludes this for proper content in a consistent common universe. The [Unicode](comparison_review/REVIEW.md) and [dimensional-atom](atom_shape_review/REVIEW.md) failures are false negatives. |
| Are repeated names always dangerous? | [Homogeneous Boolean occurrence proof](boolean_occurrences/PROOF_ATTEMPT.md), [semantic review](occurrence_review/REVIEW.md), and [totality extension](boolean_occurrence_totality/PROOF.md). Arbitrary multivalued duplicates remain outside the result. |
| Does one call saturate small wide formulas? | [Three-wide-clause proof](wide_three_saturation/WIDE_THREE_SATURATION.md) and [independent source review](wide_three_review/REVIEW.md). This covers arbitrary supports and initial widths at least three. |
| Is that three-clause boundary sharp even at large width? | [Four-clause family](wide_four_boundary/PROOF.md) and [independent review](wide_four_review/REVIEW.md) prove a useful SSE1 restriction can remain at every initial width at least three. |
| Does repeated application have a fixed small pass bound? | [Unbounded family](repeated_passes/PROOF.md) and [review](quotient_review/REPEATED_PASS_REVIEW.md); [finite input-dependent bound](pass_bound_review/PROOF.md) and [review](pass_bound_check/REVIEW.md). |
| What logical classes are completely decided? | [Semilattice/GAC theorem](root/SEMILATTICE_COMPLETENESS.md) and [relational review](semilattice_review/REVIEW.md); [Boolean binary graph criterion](structural_classes/BOOLEAN_2CNF_GRAPH.md) and [review](boolean_graph_review/REVIEW.md). |
| What is the smallest missed Boolean binary contradiction? | [Eight-clause/four-variable boundary](binary_minimal_boundary/README.md) and [independent second formulation](root/BINARY_MINIMUM_REVIEW.md). |
| Are different equivalent outputs necessarily a scheduler defect? | [Fixed-point saturation](root/FIXED_POINT_SATURATION.md), [pure implication graphs](root/PURE_IMPLICATION_GRAPH.md), and [independent review](fixed_graph_review/REVIEW.md). Irredundant alternative normal forms exist. |

These are source-level mathematical proofs with independent review and
executable controls. They are not a machine formalization of R. Local
saturation, logical equivalence, exact object equality, satisfiability
recognition, and minimum-size representation are kept separate throughout.

## Validation records and practical limits

The scoped package test command is:

```sh
bash attic/cnf_verify3/review_semantics/run_r46.sh -e 'devtools::test(filter = "^CnfFormula_simplify$", stop_on_failure = TRUE)'
```

The final saved execution of that test file has 1,542 passing expectations;
its log is `root/stale_snapshot_tests_r46.log`. The earlier all-CNF test run
passed 3,815 expectations before the later two focused callback controls were
added. Neither is a whole-package test result. No production source was
modified between those runs and the subsequent investigation artifacts.

The two large complete quotients have separate, noninterchangeable scopes:
[three-symbol `(3,3,2)`](root/THREE_SYMBOL_QUOTIENT.md) and
[full aligned XYZ](root/FULL_THREE_SYMBOL_QUOTIENT.md). Their calibrated
assignment grids, complete loop reports and reviewed lifting arguments matter
more than combining their counts. They cover truth preservation; saturation
for arbitrary internal orders is a separate source proof.

Similarly, runtime comparisons in [execution_modes](execution_modes/NOTES.md)
and [the new native-name package replay](root/CTYPE_UNIT_REVIEW.md) support
specific saved cases in actual installed namespaces. They are not universal
claims about R compilers. Saved solver timeouts, interpreter stack limits,
observer development mistakes and rejected stronger invariants remain in
their respective notes instead of being counted as successful exclusions.

Private selector, scheduler, unit-queue and comparator changes are diagnostic
candidates. They were evaluated alongside unchanged production and were never
installed as package fixes. The native-name direct-lookup candidate, for
example, resolves failed enumeration under stable injective keys but does not
repair accepted aliases that split or collide at construction.
