Independent comparison-normalization review, 2026-09-06.

Both reported defects are confirmed on native R 3.6.3 and the existing
`cnf-review-r46` container running R 4.6.1. The UTF-8/radix proposal fixes the
demonstrated proper-object payload comparisons and preserves meaningful unequal
controls. A further ordinary collation failure remains when comparing different
universes: base R's delegated environment comparison can misalign equal symbol
bindings. This is a residual limitation of payload-only normalization, not another
cause in the CNF payload normalizers.

Production source was neither edited nor replaced in the running S3 method table.
All files created by this review are in this directory. There were no commits.

**Reproduced source causes and exact results.**

The independent [reproduce.R](reproduce.R) uses real checkmate, unchanged public
constructors and coercions, and direct structural and finite-truth oracles. It
does not source or rerun the parent's experiments. The three private candidate
function definitions are extracted from the parent's script without evaluating
its other expressions or writing to its directory.

For each runtime, the reduced suite makes 39 comparisons in `C`, `C.UTF-8`, and
`en_US.UTF-8`: 24 expected equal and 15 expected unequal. Production has exactly
13 false negatives and zero false positives. Both the parent's prototype and
the private copies preserving the original guards have zero disagreements and
zero errors on these 39 proper-object comparisons.

1. `R/CnfAtom.R:180`, `R/CnfClause.R:267`, and `R/CnfFormula.R:285` use ordinary
   locale sorting for value sets or symbol names. The distinct strings
   `"\u00e9"` and `"e\u0301"` tie under the measured Unicode collations.
   Reversing their input order therefore gives different sorted vectors.
   Atoms, clauses, and one-clause formulas containing the same value set compare
   unequal. Reversing the order of those two distinct symbol names also makes
   equal clauses and one-clause formulas compare unequal. These five pairs fail
   in each Unicode locale and pass in `C`, accounting for ten false negatives.
2. `R/CnfFormula.R:289` constructs whole-clause ordering keys with a serialized
   digest before string encodings have been canonicalized. For the parent's
   two-clause UTF-8 versus Latin-1 example, `identical(left, right)` is `TRUE`,
   both corresponding clause comparisons return `TRUE`, and whole-formula
   comparison fails in all three locales. The exact keys are the same in both
   measured runtimes:

   | Representation | First clause key | Second clause key | Sorted clause positions |
   | --- | --- | --- | --- |
   | UTF-8 | `X.__.Y642497de15345ba5` | `X.__.Y828c0d82bc749a8f` | 1, 2 |
   | Latin-1 | `X.__.Yaafaa05d165a4a0c` | `X.__.Y7f88ef91f2f6f955` | 2, 1 |

   All four keys differ; no hash collision is involved. Direct evaluation finds
   the same truth result on all nine assignments, with two satisfying
   assignments. The discrepancy is entirely in pairing the stored clauses for
   comparison.

The reduced negative controls compare the distinct NFC/NFD singleton sets and
swap which of the distinct symbols receives `"0"` versus `"1"`. All three CNF
classes reject the unequal finite-set controls; clauses and formulas reject the
unequal-association controls. No Unicode normalization such as NFC or NFD is
performed by the candidate, and doing so would incorrectly merge these distinct
R values.

The observed behavior is consistent with R's primary documentation:
[`identical`](https://stat.ethz.ch/R-manual/R-patched/library/base/html/identical.html)
compares ordinary marked strings after translation;
[`Encoding`](https://stat.ethz.ch/R-manual/R-patched/library/base/html/Encoding.html)
documents `enc2utf8` as encoding conversion; and
[`sort`](https://stat.ethz.ch/R-manual/R-patched/library/base/html/sort.html)
distinguishes locale sorting from radix byte ordering. The particular ties,
keys, and comparison results above were independently measured.

**Public permutations and encoding mixtures.**

[permutations.R](permutations.R) constructs 432 atoms, 216 clauses, and 648
three-clause formulas. It exhausts the listed value/symbol/clause permutation
and encoding schedules in that script, rather than claiming exhaustive coverage
of all public inputs. Every family includes UTF-8, Latin-1, and valid unmarked
native strings. The native strings are produced through ordinary
`iconv(..., to = "", mark = FALSE)` conversion, not raw-byte manipulation.
Mixed marks occur in symbol names and within selected value ranges. Symbol name
variants are obtained through the public `$.CnfUniverse` method.

Each variant is checked against its reference in both directions, against itself,
and against an unequal control. The independent structural oracle compares exact
R membership and matches symbols with their associated value sets; formulas use
one-to-one clause matching. This oracle does not sort, recode, hash, or call
`all.equal`. It also checks that every constructed formula has exactly the
reference's three-clause multiset. Therefore, none of the positive claims depend
on different simplified normal forms having equal truth values.

The larger controls replace one member of a four-element atom set, swap two
symbol-associated ranges within a clause, and swap two associations in one clause
of the formula. The latter formula control is independently evaluated on all
125 assignments: the reference has 16 models, the control has 29, and they differ
on 25 assignments. The clause association control retains the same flat multiset
of values, so comparing only unassociated ranges would be detected.

The following counts hold separately on both R versions. Each row's check count
includes equal, reverse, reflexive, and unequal cases.

| Class | LC_COLLATE | Checks | Production false negatives | Production false positives | Prototype disagreements | Guard-preserving disagreements |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| Atom | C | 1,728 | 0 | 0 | 0 | 0 |
| Clause | C | 864 | 0 | 0 | 0 | 0 |
| Formula | C | 2,592 | 1,152 | 0 | 0 | 0 |
| Atom | C.UTF-8 | 1,728 | 432 | 0 | 0 | 0 |
| Clause | C.UTF-8 | 864 | 324 | 0 | 0 | 0 |
| Formula | C.UTF-8 | 2,592 | 1,296 | 0 | 0 | 0 |
| Atom | en_US.UTF-8 | 1,728 | 432 | 0 | 0 | 0 |
| Clause | en_US.UTF-8 | 864 | 324 | 0 | 0 | 0 |
| Formula | en_US.UTF-8 | 2,592 | 1,296 | 0 | 0 | 0 |
| Total | three locales | 15,552 | 5,256 | 0 | 0 | 0 |

There are 11,664 expected-equal comparisons and 3,888 expected-unequal controls
per runtime in this larger suite. Full comparison rows are retained in
`permutations_checks_r36.csv` and `permutations_checks_r46.csv`, with compressed
lossless RDS equivalents. JSON summaries include representative original
failure messages.

**Concrete integration guidance and conditional correctness argument.**

The parent's `candidate_equal` is intentionally a small prototype, not a complete
replacement for the public methods. Calling it on logical `CnfAtom` objects
errors because `$` is invalid on atomic vectors. It also treats a TRUE
`CnfClause` with a universe attribute as unequal to TRUE without that attribute,
where the public method deliberately ignores the universe. These are prototype
scope limitations, not failures of the proposed proper-object normalization.

[guarded_candidate.R](guarded_candidate.R) instead copies the three original
comparison functions in memory and changes only their proper-object
normalization paths. The original logical/type guards and `...` forwarding stay
in their bodies. The atom copy only normalizes its selected values: recoding its
single symbol is unnecessary because that symbol is neither sorted nor hashed.
The clause/formula copies normalize values and names to UTF-8, use radix for all
ordering, and hash clauses only after that normalization. The production methods
remain callable and unchanged alongside these copies.

These copies pass both suites above. [scope.R](scope.R) checks 36 additional
scope cases, including named constants, TRUE versus FALSE, universe metadata,
proper versus logical, wrong classes, reversed domain order, and the residual
environment case below. The private copies return exactly the same captured
result or error as production in all 36 cases. Three further checks establish
that `check.attributes = FALSE` still forwards through `...`.

For plain, valid ordinary character strings, unique symbol names, finite proper
value sets, and the default structural comparison policy:

1. UTF-8 conversion maps equivalent supported string encodings to one byte
   representation without changing code points. Distinct code-point sequences
   remain distinct. Byte ordering is total on these distinct representations,
   so a range's sorted UTF-8 vector is independent of its incoming order.
2. Sorting symbol names while moving their associated ranges with them gives
   the same normalized clause exactly when their symbol-to-set maps agree.
   This cannot identify the unequal-association controls.
3. Normalizing clauses before creating keys makes each key invariant under
   the encoding and order variations above. If distinct normalized clauses
   have distinct ordering keys, sorting the formula aligns equivalent clause
   occurrences. Duplicates with identical payloads would not require a tie
   breaker because exchanging them changes nothing.
4. The final list comparison still checks actual symbol names and values.
   An ordering-key coincidence cannot alone create a false positive under this
   default policy: sorting only permutes the normalized payloads. It can still
   create a false negative if unequal clauses share a key and their opposite
   incoming orders survive sorting. Therefore, retaining a 64-bit digest is a
   practical ordering aid, not a collision-free proof of canonical form.

The formula argument is consequently conditional on the ordering-key property
and on the universe/other attribute comparison succeeding. The tests do not
claim arbitrary logical equivalence, completeness of simplification, or a unique
logical normal form. They concern the same stored structural content modulo
orders and equivalent character encodings.

**Demonstrated residual environment comparison.**

The search in [scope.R](scope.R) found an example after 39 ordinary name pairs.
It is further reduced in [universe_residual.R](universe_residual.R):

```r
symbol_names = c("38\u00e9", "38e\u0301")
make_universe = function(insertion_order) {
  u = CnfUniverse()
  for (i in insertion_order) CnfSymbol(u, symbol_names[[i]], c("0", "1"))
  u
}
u = make_universe(1:2)
v = make_universe(2:1)
a = `$.CnfUniverse`(u, symbol_names[[1L]]) %among% "0"
b = `$.CnfUniverse`(v, symbol_names[[1L]]) %among% "0"
all.equal(a, b)
```

The two universes have the same binding names and identical domain vectors at
each name. The atoms have the same proper payload. Under `C.UTF-8` and
`en_US.UTF-8` on both runtimes, however, the last expression returns:

```text
Attributes: < Component “universe”: Names: 2 string mismatches >
```

The same holds for corresponding clauses and formulas, for the parent's
prototype, and for the copies preserving the original guards. All nine
class/locale comparisons pass only for the three `C` locale cases and fail for
the six Unicode locale cases. The two names are distinct strings with tied
collation weights; `as.list.environment(..., sorted = TRUE)` preserves opposite
native binding enumeration orders. Base `all.equal.environment` delegates to
that sorted-list comparison. Its implementation was inspected directly in both
R processes. Normalizing only CNF payloads cannot affect this later comparison
of their universe attributes.

This is ordinary public construction with no universe mutation, fabricated CNF
objects, unsupported encodings, or changed domains. It establishes a limitation
of any payload-only fix across separate universes. It does not alter the
same-universe success demonstrated above or establish a simplifier defect.

**Other boundaries and evidence files.**

Named flags accepted by `as.CnfAtom`, `as.CnfClause`, and `as.CnfFormula` retain
names. Their common constant branch uses `identical(c(target), c(current))`, and
`c()` retains those names. Named versus unnamed TRUE/FALSE, and differently named
TRUE, therefore compare unequal. This is recorded as metadata sensitivity and
a potential contract ambiguity, not another confirmed defect: ordinary base
comparison also distinguishes named vectors, and the comment explicitly exempts
universe metadata rather than promising to discard all metadata.

Likewise, `tests/testthat/test_CnfUniverse.R:83` explicitly expects universes with
reversed domain-vector order to compare unequal. The normalizer copies preserve
that policy. Treating domains as unordered during universe comparison would be
a separate behavioral change, not an automatic extension of this proposal.

All encoding mixtures in this review are valid under the measured UTF-8 native
locales on Linux. No claim is made for invalid strings, byte-marked strings,
custom character subclasses, malformed duplicated-symbol selector outputs, or
radix-unsupported long vectors. R documents radix restrictions on supported
encodings and long vectors in its sorting reference linked above. Windows and
non-UTF-8 native locales were not run here. No digest collision was sought or
observed.

Both runtimes use checkmate 2.3.4, digest 0.6.39, and jsonlite 2.0.0. The native
runtime uses mlr3misc 0.22.0; the container uses mlr3misc 0.23.0. Each result JSON
and RDS records R version, full locale, package versions, and MD5 hashes of the
parent candidate file and the three production comparator files.

The four entry-point scripts are `reproduce.R`, `permutations.R`, `scope.R`, and
`universe_residual.R`. From the repository root, run any of them using either:

```sh
Rscript attic/cnf_verify3/comparison_review/reproduce.R
podman exec cnf-review-r46 Rscript attic/cnf_verify3/comparison_review/reproduce.R
```

Substitute the other script names to rerun their checks. Each script writes only
to this directory, chooses its `_r36` or `_r46` suffix from the runtime, and prints
its results. The saved `.log` files contain the corresponding successful runs;
the `.json` files summarize them and the `.rds` files retain lossless R values.
These are focused standalone assertion checks against sourced production code
and private comparator copies; no full-package test run was performed.
