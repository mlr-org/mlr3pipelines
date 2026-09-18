# A valid public non-ASCII symbol can make an unsatisfiable formula satisfiable

Confirmed on 2026-09-06 with unchanged production CNF source, native R 3.6.3,
container `cnf-review-r46` R 4.6.1, and real checkmate 2.3.4. This is a
**kernel truth-preservation failure**, independent of every CNF comparison
helper. It is not merely a different presentation, a missed contradiction,
or a failure to compare universe environments.

## Small public reproduction

Set `LC_CTYPE` to `C`; `LC_COLLATE` is held at `C` in both the failure and
the UTF-8 control. Let `X` have the valid UTF-8 marked name `"\u00e9"`,
domain `{a,b,c}`, and let ASCII-named `Y` have domain `{a,b}`. Construct:

```r
u = CnfUniverse()
X = CnfSymbol(u, "\u00e9", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("a", "b"))
clauses = list(
  CnfClause(list(X %among% "a")),
  CnfClause(list(X %among% "b", Y %among% "a")),
  CnfClause(list(X %among% "c", Y %among% "b"))
)
CnfFormula(clauses)
```

The input is

```
X=a AND (X=b OR Y=a) AND (X=c OR Y=b).
```

It has no satisfying assignment: `X=a` requires both `Y=a` and `Y=b`.
Under `LC_CTYPE=C`, the result is the two nonunit clauses with `X=a`
deleted. It has exactly two models, `(X=c,Y=a)` and `(X=b,Y=b)`.
Under `LC_CTYPE=C.UTF-8`, the same public construction returns FALSE.

`reproduce_public.R` is the self-contained executable version. It loads the
six unchanged CNF source files and real dependencies, evaluates every one
of the six positional domain assignments, and asserts these results. It
uses one universe per formula, never changes a universe after creation,
does not fabricate CNF payloads, and does not call `all.equal`.

The example has distinct nonmissing symbol names in every clause, ordinary
unique proper literal ranges, and unique nonempty domains. **All domain
values are ASCII.** It requires neither mixed encodings in a formula nor
duplicate-name selector output. The UTF-8 name is valid, declared, accepted
by `assert_string`, and usable by every executed public operation.

## First lost obligation and first incorrect deletion

R environment binding keys use native encoding. Under C CTYPE, the name
cannot be represented natively and R substitutes a printable escape.
The private registry therefore has these concrete properties:

| Supplied symbol | Declared encoding / bytes | `names(unit_domains)` | Enumerated bytes |
| --- | --- | --- | --- |
| precomposed e acute | UTF-8 / `C3 A9` | literal ASCII `<U+00E9>` | `3C 55 2B 30 30 45 39 3E` |
| the encoding-equivalent string | Latin-1 / `E9` | literal ASCII `<e9>` | `3C 65 39 3E` |

These are the actual enumerated string values, not just R's escaped console
display. The JSON records include their byte vectors and encoding marks.

Direct `unit_domains[[original_name]]` still retrieves the registered range,
because it repeats the same native translation. But `%in%` regards the
original character and the ASCII escape as distinct. Hence:

1. Line 502 intersects original clause names with `names(unit_domains)`.
   Both nonunit clauses get an empty `clause_symbol_isct`, so line 506 never
   propagates `X=a`. This is the first failed lifecycle obligation; the
   formula has not yet undergone an unsound rewrite.
2. The two nonunits have mutually different singleton ranges at both
   symbols. They survive the intervening simplification phases.
3. At unit HLA entry, line 738 and the lazy masks at lines 750/769 assume
   that any remaining nonunit containing `X` is contained in the unit's
   range at `X`. Here their ranges are `{b}` and `{c}`, disjoint from `{a}`.
   The counter initializes them as though each had just one exceptional
   symbol, `Y`.
4. Using `(X=b OR Y=a)` adds virtual `Y=b` to the target `X=a` at line 755.
   `(X=c OR Y=b)` is then treated as contained. Its count reaches zero at
   lines 773–775, although its `X` range is still not contained.
5. **Line 777 first changes formula truth:** it eliminates the unit `X=a`.
   The returned formula then admits the two assignments above.

`ctype_counterexample.R` saves the complete private observational trace and
the unchanged public result in RDS. Its JSON records the empty early
propagation sequences and the TRUE line-775 branch. It repeats the failure
with both UTF-8 and Latin-1 marked names; both versions give the same truth
tables. R 3.6.3 issues no translation warnings. R 4.6.1 warns that the name
cannot be translated to native encoding and continues to the wrong result.

This diagnoses a specific missing premise of the earlier physical
containment arguments. Those arguments need the registry's enumerated
names to agree with clause-name identity. A finite successful ASCII or
UTF-8-locale campaign cannot establish that premise for every accepted name.

## A second accepted construction boundary

`alias_construction.R` uses encoding-equivalent UTF-8 and Latin-1 names
in one universe. `identical`, `==`, `%in%`, and `unique` all regard these
names as the same string. In C CTYPE, the two native escapes differ:

* Retrieving the UTF-8-created symbol through its Latin-1 alias fails.
* `CnfSymbol` accepts a second registration for that same ordinary R name.
* `CnfClause` exact named indexing also translates to native encoding; it
  retains two literal positions with duplicated ordinary R names.
* That public clause reaches `simplify_cnf` and is returned with both names.

In the C.UTF-8 control, the second registration rejects, alias retrieval
succeeds, and clause construction merges the ranges into one literal.
The small alias example records a representation-contract violation; it is
not needed for the separate canonical-shape wrong-truth example above.
Primitive controls also show a distinct ASCII name `<U+00E9>` colliding
with the escaped binding for the UTF-8 name under C.

## Scope and interpretation

`LC_COLLATE` changes alone did not produce this kernel failure. The positive
proof in `PROOF.md` covers equivalent encodings and collation changes when
native key translation preserves and reflects ordinary string identity.
That is a concrete, checkable additional premise. “Locale changes preserve
ordinary equality” alone is insufficient: `==`, `unique`, and `%in%` still
agree on the original valid names in the C counterexample.

The task excluded invalid, byte-marked, or unoperable strings. These names
are valid and all necessary public calls complete. If “unoperable” is
intended to exclude any failure of faithful native translation, that
exclusion removes this example from the positive theorem; it does not mean
the public API currently rejects the example. The accepted-input gap is
therefore recorded explicitly rather than hidden in that word.

No production change or repair is made here. Root independently rebuilt
and exhaustively checked the boundary in
`../root/ctype_unit_boundary*`; those separate results are not used as this
script's semantic oracle.
