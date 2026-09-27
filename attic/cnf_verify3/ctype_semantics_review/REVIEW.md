# Independent review of the C CTYPE unit-propagation failure

2026-09-06. This review covers the public counterexample and observational
trace in `../character_identity_review/`, the private candidate in
[`root/ctype_unit_boundary.R`](../root/ctype_unit_boundary.R), and the
two-clause boundary. It does not repeat root's exhaustive grid or change
production code.

**Verdict.** The reported failure is confirmed. Line 502 first loses a unit
propagation obligation; line 738 initializes an incorrect physical-containment
count; line 777 first changes actual formula truth in the supplied example.
The proposed direct-lookup replacement addresses that cause under stable,
injective native-key identity and canonical input. The claim that at most two
actual canonical clauses preserve truth is sound within that same scope,
even when the original line 502 misses units. Three clauses are therefore
minimal in clause count for this failure under the stated restrictions.

## 1. Scope and terminology

Each semantic variable has one consistently encoded representative string.
All its occurrences use that representative, and distinct variables have
distinct ordinary character names. Let `phi(s)` be the native binding key
used by R for the representative `s`.

The reviewed scope requires:

* `phi` is fixed throughout construction and the call, and is injective on
  participating semantic variables. Direct lookup through each supplied
  name retrieves its intended binding. This includes the existing universe
  bindings as well as newly created private registries.
* List/matrix indices, character equality and set operations agree on these
  consistently represented names and ordinary domain values. Every actual
  input clause has unique symbol names and canonical nonempty proper ranges.
  Domains are finite and nonempty, with the ordinary operation/storage
  assumptions of the earlier proofs.
* Universes are not changed, no custom callbacks/methods alter primitive
  behavior, native-translation warnings are nonfatal, and execution has the
  supported capacity to finish.

Environment enumeration need **not** return character names equal to the
original representatives. In particular, under `LC_CTYPE=C`, UTF-8 `é` can
have native key and enumerated name `"<U+00E9>"`. Repeating direct lookup with
the original UTF-8 representative nevertheless addresses the same key.

This scope excludes constructing one variable through encoding aliases that
split into different native keys, or constructing different variables through
names that collide at one native key. It also excludes a locale change that
invalidates the mapping between an already created universe and subsequent
lookups. Merely saying that all names pass `assert_string` would not suffice.

## 2. Independent causal trace

The reviewed public example is

```
A = [X in {a}]
B = [X in {b}] OR [Y in {a}]
C = [X in {c}] OR [Y in {b}]
F = A AND B AND C,
```

where X is named by UTF-8 `\u00e9` and has domain `{a,b,c}`, and Y has ASCII
name `Y` and domain `{a,b}`. All original actual ranges are plain unique
proper vectors, all clause names are unique, and all domain values are ASCII.
Under A, B and C require incompatible Y values, so F is unsatisfiable.

Registration at lines 92–93 writes the intended unit range under the native
key. This translation is ordinary R behavior, and direct source lookups
still work. The first source decision that incorrectly identifies semantic
symbols is at line 502:

```r
clause_symbol_isct = char_intersect(names(entries[[clause_idx]]), names(unit_domains))
```

The table contains literal ASCII `"<U+00E9>"`; `%in%` does not consider it equal
to the original UTF-8 name. Thus both nonunits get an empty propagation
sequence, although direct lookup for their X name finds the unit. The
operation loses the obligation to restrict their X ranges. **Skipping this
rewrite does not itself change the formula's truth.** All three original
clauses remain present and still denote F.

The two nonunits differ on both symbols, so the intermediate first-order
and second-order checks do not change this example. Neither nonunit HLA
target has a donor with exactly one exceptional symbol. The unit HLA phase
then uses a premise that the failed preprocessing was meant to establish:

```r
not_subset_count = lengths(entries[remaining_nonunit_entries]) -
  (remaining_nonunit_entries %in% symbol_registry[[unitsymbol]])
```

At line 738 this gives `(1,1)`. The true physical exceptional-symbol counts
of B and C against A are `(2,2)`: both X and Y are exceptional in each donor.
The lazy masks at lines 750 and 769 likewise mark X as contained merely
because it is the unit symbol. This is the first false containment premise
consumed by the HLA implementation; it is distinct from the earlier name
membership failure.

Using B, line 755 computes virtual Y range `{b}`, the complement of B's `{a}`.
Against the hypothetical virtual target `[X in {a}] OR [Y in {b}]`, C still
has one physical exceptional symbol, X. The cached count instead falls to
zero after its Y bit is cleared. The line-775 condition succeeds and line
777 executes:

```r
eliminated[[clause_idx]] = TRUE
```

This deletes A. The remaining B AND C admits precisely `(X=c,Y=a)` and
`(X=b,Y=b)`. Thus **line 777 is the first truth-changing write to the actual
formula**. The unsound virtual-expansion reasoning occurs earlier, but a
scratch range is not itself a committed rewrite of `entries`.

The independent observer in [`checks.R`](checks.R) records every write to
`entries` or `eliminated`, plus snapshots at lines 502, 738 and before 777.
It finds exactly one truth-changing actual write, at the stated deletion:

| Point | Cached counts | Physical counts | Live models |
| --- | --- | --- | ---: |
| Before unit HLA | `(1,1)` | `(2,2)` | 0 |
| After virtual Y expansion, before deletion | `(1,0)` | `(2,1)` | 0 |
| After line 777 | unchanged | unit removed | 2 |

The observer evaluates each original assignment in its original lexical
frame and records positional truth before/after it. Its returned payload
agrees exactly with the unchanged public `CnfFormula` result on both runtimes.
The uninstrumented public result remains the primary reproduction.

## 3. Candidate scope and correctness argument

The private candidate replaces only the line-502 expression by:

```r
Filter(function(symbol) !is.null(unit_domains[[symbol]]),
  names(entries[[clause_idx]]))
```

Under the scope above, a non-NULL direct lookup means exactly that this
semantic variable has a registered unit. Unit ranges are nonempty, so NULL
is an unambiguous absence indicator. No lookup creates a binding. `Filter`
preserves clause-symbol order and returns the original representative strings.
The relevant registry is not mutated while this selection is computed.

Where original name enumeration is faithful, this produces exactly the
original ordered intersection. Where enumeration changes the spelling, it
still selects exactly those clause symbols that have live unit bindings.
In the example the candidate therefore restricts B by X=a, creating Y=a;
processing C then detects the contradiction through the usual direct unit
propagation and merging paths.

The source's only environment-name enumeration used for propagation is the
removed `names(unit_domains)` call at line 502. Other `names`/`colnames` calls
read clause or matrix labels that retain the representative strings.
`length(unit_domains)` counts bindings and remains correct under injectivity.
All other registry operations use direct supplied-name lookup/update.

Consequently, after this replacement, rename each semantic variable to a
distinct ASCII label and pair its registries through the injective map
`phi`. Direct registry keys, exact list/matrix positions, clause-symbol
comparisons and the new unit-selection sequence all correspond. The rest of
the kernel follows the same semantic source decisions on corresponding
states. This removes the faulty enumeration premise from the earlier
canonical correctness proof; it does not require changing the HLA algebra.

This is a **conditional source argument for the specific candidate**, not a
claim that every public Unicode input is repaired. The change does not fix
native-key collisions or splits, inconsistent aliases, malformed selectors,
constructor constant/owner bugs, comparison normalization, or native encoding
warnings. In particular, the alias control below still produces duplicate
ordinary clause names before this changed line is reached. No performance
or warning-behavior equivalence is claimed. No candidate was installed in
production.

## 4. Two-clause preservation and minimality

Count **actual canonical clauses supplied to a kernel call**, after successful
constructor constant handling/flattening. The number of public list members
is not that count: one nested Formula member can supply three or more actual
clauses. The following argument concerns truth on every valuation, not just
satisfiability.

For real native key enumeration, a false positive among participating
representatives would require a collision: if an enumerated native key for
s were equal to the original representative for distinct t, native insertion
of t would use that same already-native key. Injectivity excludes this. Thus
the original line 502 can miss a unit under this scope, but cannot select an
unrelated unit. This uses native key enumeration semantics; an arbitrary
fictional name enumerator would need this no-false-positive property stated
separately.

### All input clauses are units

Registration and merging use direct native-key lookup. Equal-symbol units
are intersected, and an empty intersection returns FALSE. Different symbols
retain independent constraints. The early return at line 491 runs before
name enumeration. Hence zero, one or two units preserve truth.

### There is no initial unit

The unit registry is empty during every line-502 selection. Preprocessing
cannot create a unit in this case, because it has no unit restriction to
apply. The enumeration mismatch is therefore never exercised.

Any unit created later by pairwise simplification immediately propagates
through `register_unit` and `symbol_registry[[nu]]`; those paths use direct
keys, not enumeration. With stable injective keys, the remaining run is the
same as a consistently renamed canonical run. The previously established
canonical semantics therefore applies. This covers one or two nonunits,
including pairs that create a unit during simplification.

### One initial unit and one initial nonunit

Let the unit be U = `[s in A]` and let the nonunit be C. If preprocessing
selects s, its restriction/elimination/possible unit birth has the usual
sound direct-lookup behavior. If it misses s, the original U AND C is still
present. In either case, if C remains nonunit, there is at most one nonunit
available for later phases.

There is no distinct nonunit pair for self-subsumption or resolution, and a
nonunit HLA target has no other nonunit donor. The only possible remaining
concern is using C as the sole donor against U:

1. If C does not mention s, its initial unit-HLA count equals its width,
   which is at least two. No donor is selected.
2. If C mentions s but has width at least three, its initialized count is
   width minus one, again at least two. No donor is selected.
3. If C has exactly the symbols s and t, its initialized count is one,
   whether or not its s range was actually restricted. The selected
   expansion is at t. The old virtual target has no t range, so the new
   range is exactly `D_t minus C_t`.

In case 3, `C_t` is nonempty and proper. Its complement is a proper domain
subset and cannot trigger the full-domain test at line 756. The only
nonunit in `symbol_registry[[t]]` is C itself; its nonempty range `C_t` is
disjoint from that complement and therefore is not contained in it. Its
count cannot fall to zero. After C is marked used, no further donor remains.
Only a local virtual range may have changed; the actual unit and nonunit
clauses are retained.

The reasoning also permits ordinary repeated stored domains under the
existing storage contract: removing the nonempty donor range omits at least
one physical domain occurrence, so the newly built complement is strictly
shorter than the stored domain, and it remains disjoint from the donor.

These cases exhaust at most two actual canonical clauses. The argument does
not assume that missed unit propagation nevertheless established physical
containment; case 3 explicitly allows that invariant to fail. The three-clause
public counterexample therefore establishes clause-count minimality within
this scope. It does not establish minimality for arbitrary malformed public
inputs, ambiguous aliases, domain size, number of symbols, or nested-list
member count.

## 5. Small independent controls

[`checks.R`](checks.R) loads real checkmate 2.3.4 and all six production CNF
files into a separate environment. The original public constructors keep
their original simplifier binding. A private closure copy supplies the
candidate, with an assertion that exactly one expression was replaced.

Fourteen directed structures are exercised under two CTYPE settings and five
name profiles: ASCII, UTF-8 e acute, Latin-1 e acute, Greek lambda, and two
distinct UTF-8 variable names. They cover empty/one-clause formulas, same-
and different-symbol units, successful/missed initial restriction, a wider
sole donor, later unit birth, and both satisfiable and unsatisfiable triples.
Every tested profile has explicitly checked injective native binding keys.

Both runtimes passed 140 directed cases. The original changed truth in eight
cases: the two selected bad triples for each of the four non-ASCII name
profiles under C CTYPE. Every original case with at most two clauses
preserved its full truth table. Every candidate result matched the expected
truth, and its exact payload agreed with the original wherever name
enumeration was faithful.

The positional oracle identifies variable names by their Unicode code-point
sequences and then compares scalar domain values. It does not use native
registry lookup or a CNF comparison method to determine truth. The controls
are evidence for the source argument, not an exhaustive proof by sampling.
Root's 33,684-call grids are a separate artifact and were not rerun here.

An explicitly excluded mixed-encoding alias control creates the same
ordinary name twice with different native keys under C. Both original and
candidate input paths retain a two-position clause with duplicated ordinary
names. The candidate's returned clause still has those duplicate names,
confirming the scope limitation rather than assuming aliases were repaired.

The trace and full records are saved in `evidence_r36.rds` and
`evidence_r46.rds`; logs and JSON summaries are alongside them. Cross-runtime
comparison removes only the explicitly recorded warning counts: current R
warns about native translation while old R does not. Semantic records and
write traces agree exactly.

Reproduce from the repository root:

```sh
Rscript attic/cnf_verify3/ctype_semantics_review/checks.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/ctype_semantics_review/checks.R
Rscript attic/cnf_verify3/ctype_semantics_review/compare.R
```

Reviewed SHA-256 snapshots:

```
7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc  R/CnfFormula_simplify.R
54bb8503b3c2a04eb408c7febe2a866b585b700407556f4701ec72777619274f  attic/cnf_verify3/root/ctype_unit_boundary.R
95629f47512fef40d129081fa09b8eb1f3163b9c939fe817d023d82127126e83  attic/cnf_verify3/character_identity_review/reproduce_public.R
61674322dbece6c818b134ac6bd5fef1233d20d5167a184acf6efae1d29d3605  attic/cnf_verify3/character_identity_review/ctype_counterexample.R
```

Only `ctype_semantics_review/` was written for this follow-up. The completed
constructor review, production sources and package tests were left untouched.
No commits were made.
