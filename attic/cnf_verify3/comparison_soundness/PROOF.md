# One-sided soundness of default CNF comparison

Independent source review, 2026-09-06, of the unchanged methods in
`R/CnfAtom.R:163`, `R/CnfClause.R:248`, and `R/CnfFormula.R:266`, and actual
base R 3.6.3 / R 4.6.1 comparison bodies.

**Result.** For proper ordinary CNF objects in a consistent common universe,
using default `all.equal` arguments, a returned TRUE implies equal stored
symbol-to-value-set content for atoms/clauses, or equal multisets of that
clause content for formulas. Consequently the objects have equal truth on
every valuation. This implication does not require a collision-free digest
or a canonical locale ordering. Its converse fails in the already documented
sorting/encoding cases.

Here proper nonconstant clauses have distinct registered names and nonempty
proper ordinary character ranges. Atom values may retain ordinary dimensions
or repetitions, provided sorting succeeds and preserves their nonmissing
scalar values. Symbol/value equality agrees with the intended domain
semantics; ambiguous native binding aliases, custom sorting/comparison
dispatch, malformed objects and missing values are outside the claim.
Literal order, incidental attributes and encoding marks are not semantic
set content. Clause **multiplicity** is part of formula multiset content.

## Base comparison lemma

On both inspected runtimes, default `all.equal.list` first checks attributes,
including names, through `attr.all.equal`. It reports unequal list lengths
and compares **every paired integer position** recursively. It does not
match components by name; `use.names` controls diagnostic labels, while
default attribute checking supplies the actual name comparison.

For ordinary character leaves, `all.equal.character` reports unequal lengths,
different NA patterns, or any position where `target != current`. With the
nonmissing-value premise, a returned TRUE therefore implies equal character
values at every position. Numeric tolerance does not relax these character
comparisons. Equivalent supported encodings can denote the same string;
collation ties between distinct strings do not make them equal here.

Recursively, a successful default comparison of the relevant ordinary lists
therefore implies equal list lengths, equal names at corresponding positions,
and equal scalar character sequences at all payload leaves. Additional
attribute checks can reject otherwise matching content, but cannot conceal
a differing core name or character leaf. This induction uses the actual
base character/list methods, whose failures are character diagnostics; it
does not assume an arbitrary user-supplied `all.equal` method is sound.

The current-R `check.class` additions do not weaken this default character
test. The saved `base_contract_r36.txt` and `base_contract_r46.txt` preserve
the inspected bodies rather than assuming both runtimes have identical APIs.

## Normalization preserves the relevant content

**Atom.** Sorting each values object permutes its nonmissing scalar values;
it does not change its support. The `symbol` component is left in place.
A TRUE final list comparison requires the same symbol string and the same
sorted character sequence, hence the same selected set. Scalar repetitions
may cause extra rejections because sort does not deduplicate them.

**Clause.** `order(names(clause))` is a permutation. The RHS of `clause[] =`
moves each associated range to that symbol's selected position and sorts
its values. Ordinary full-list replacement leaves the old names in place;
the immediately following `names(clause) = names(clause)[reorder]` applies
the same permutation to those names. Thus the symbol/range association is
preserved. By the base lemma, TRUE implies equal normalized name/value
pairs at every position, hence equal original symbol-to-set maps.

**Formula.** The first normalization assignment sorts each clause's names
while carrying its corresponding ranges, then sorts each range. It therefore
preserves each clause map. The digest-derived keys select an `order`
permutation of entire clause occurrences; `formula[] = formula[reorder]`
does not add, remove or rewrite those occurrences. A TRUE final comparison
implies that the two resulting lists have equal length and equal clause maps
at every position. Pairing those positions gives a bijection of the original
clause occurrences, proving equality of their multisets, including repeated
clauses.

This last argument works for **any** permutation of each clause list. A hash
collision, ambiguous concatenated key, locale tie, or encoding-dependent
digest can make the two permutations fail to align equal input multisets.
That can produce a false negative. It cannot make unequal input multisets
become equal after permutation, because the final comparison checks their
actual payloads, not merely the ordering keys. No actual digest collision
is asserted by this review.

## Logical branches and limits of the implication

Each CNF method handles two logical operands before its class guard. It
returns TRUE only if `identical(c(target), c(current))` succeeds. For proper
scalar Boolean constants this implies the same Boolean value, even across
CNF classes or different universe attributes. The branches intentionally do
not promise identical class/universe metadata. Names retained by `c` can
still reject two constants having the same truth. A proper/logical pair is
rejected, as is a wrong-class proper current operand.

Equal structural content implies equal Boolean truth in the common universe.
The reverse implication is much broader and is **not** supplied by these
methods. For example, A AND A has the same truth as A, and A AND A AND B has
the same truth as A AND B AND B, but their clause multisets differ. Rejecting
those pairs is correct for multiset comparison. It is not evidence of a
false positive or a broken test for arbitrary logical equivalence. Similarly,
TRUE comparison does not validate the correctness of earlier construction or
simplification steps that produced an object.

Nor does this result imply byte-for-byte object identity: ordinary sorting
can discard shape/metadata, supported encodings can compare equal, default
numeric comparisons of incidental attributes use tolerance, and the constant
branches intentionally ignore class/universe differences.

## Deliberately weaker caller policies

The default names check is essential because clause symbol identities live
in the list's names attribute. With X and Y in one universe and identical
domains, compare the clauses `[X in {a}]` and `[Y in {a}]`:

| Arguments | Returned TRUE? |
| --- | --- |
| Defaults | No |
| `check.attributes = FALSE` | Yes |
| `check.names = FALSE` | Yes |
| `use.names = FALSE` alone | No |

The first two nondefault policies explicitly remove the distinguishing
symbol names from the comparison. The same applies to one-clause Formulas.
They invalidate an extension of the theorem to those options, but are
intentional caller-requested weaker comparisons, **not new implementation
bugs**. Atom symbols are character payload components, so merely ignoring
attributes does not suppress their symbol comparison.

Arbitrary custom sort or leaf-comparison methods likewise fall outside the
proof: they need not preserve supports or report differing character values.
Removing that restriction would require a separate contract for each method.

## Independent evidence

[`checks.R`](checks.R) uses the six unchanged production CNF files, real
checkmate 2.3.4 and digest 0.6.39. It independently matches scalar value sets,
symbol associations and clause occurrences without sorting, hashing or using
CNF `all.equal` as its content oracle. Proper unsimplified Formula fixtures
test multiplicity, since constructors simplify duplicate clauses before
comparison.

On both runtimes, all 22 default CNF controls behaved as asserted: every TRUE
result preserved content. Controls reject changed values, symbols,
associations and clause multiplicities, while accepting ordinary permutations,
equal repeated multisets and equivalent encodings. They also reproduce the
known equal-set collation rejection and repeated-atom-storage rejection.
Six caller-option controls and four direct base comparison controls passed.
These finite checks support the source argument; they do not replace it.

Run from the repository root:

```sh
Rscript attic/cnf_verify3/comparison_soundness/checks.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/comparison_soundness/checks.R
```

Logs, complete comparison diagnostics, package versions, production source
MD5s and base function bodies are saved in this directory. No production
files, package tests or other review directories were edited. No commits
were made.
