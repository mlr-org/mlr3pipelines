# Independent review of conditional-outcome coverage

Reviewed 2026-09-06 against the unmodified kernel, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
I read the instrumentation, both saved coverage tables/RDS files, and the
production call sites before writing this argument. I did not read or modify
the root agent's parallel proof of these four branches.

## Verdict

The catalog contains every explicit `if` expression in this kernel, and the
saved witnesses establish every claimed reachable outcome. Under canonical
finite-set input and the inspected lifecycle/cache invariants, the four
missing TRUE outcomes are unreachable:

| Site | Source line | Missing TRUE outcome | Independent reason |
| --- | --- | --- | --- |
| 26 | 239 | deleting a symbol leaves zero symbols | The deletion helper starts with a live nonunit, so removing one symbol leaves at least one. |
| 75 | 473 | whole-clause elimination receives a live unit | Its callers submit live nonunits; retained units merge through a separate path. |
| 98 | 694 | a nonunit HLA extension fills its whole domain | A selected donor supplies a concrete missing value that its complement cannot add. |
| 104 | 756 | a unit HLA extension fills its whole domain | The same missing-value certificate applies, including correct lazy row initialization. |

The two runs therefore provide **all 212 feasible explicit-if outcomes out
of 216 syntactic outcomes**, conditional on these source-level proofs. This
is a precise outcome-coverage statement. It does not mean every execution
path, loop behavior, recursion depth, or individual `&&`/`||` operand has
been covered, and it is not a correctness proof by testing alone.

## 1. Independent catalog and data checks

`review.R` uses R's source parser (`getParseData` with source text), not the
root visitor, to enumerate all `IF` tokens. It finds exactly 108 tokens in
14 function definitions. For each token it obtains the condition from the
first expression child of the token's parent and identifies the innermost
enclosing function by its source span. The resulting source order, owner,
and deparsed condition match all 108 recorded sites exactly.

This checks possible blind spots of the root visitor. That visitor descends
call arguments but not pairlist defaults or call heads. This particular
source has no additional `if` hidden in those locations: the independent
parser count and every mapped condition agree. An analogous visitor should
be rechecked if the source later adds such constructs.

The saved TSV and RDS site metadata/counts agree for both runs. Both RDS files
say `complete = TRUE`, recording 10,017 and 20,017 checked inputs. Together
they record 62,634,404 condition evaluations. Every FALSE outcome is observed,
and exactly sites 26, 75, 98, and 104 lack a TRUE outcome. The recorded kernel
hash matches the currently inspected source.

I separately checked canonicality of every saved first input and replayed
every saved first-witness obligation. Deduplicating exact domain/clause
objects gives 57 inputs and 250 distinct input/outcome obligations. A fresh
observer reproduces each requested outcome and every one of the 212 claimed
outcomes in their union. Its returned objects are `identical()` to fresh
unmodified-kernel returns on all 57 inputs.

The replay also checks the local proof premises after the production function
has forced its original arguments:

```
2,165 present-symbol restriction targets were live nonunits;
  609 symbol-deletion entries were live nonunits of length at least two;
  454 whole-clause-deletion entries were live nonunits;
  599 nonunit HLA selections had exact selected rows and a missing donor value;
  119 unit HLA selections had exact selected rows and a missing donor value.
```

Every HLA certificate also verifies that the new range is unique, lies inside
the domain, omits the certified value, and is strictly shorter than the domain.
These checks support the source argument below; their finite count does not
by itself establish unreachability.

## 2. Instrumentation behavior and its limits

The wrapper receives each original condition as a promise captured in the
original evaluation environment and forces it once. Reading the forced
value again for its length, missingness, logical outcome, and final return
does not reexecute side effects such as a `register_unit()` call. All
conditions in these normally completed canonical runs produce a plain
scalar logical or numeric value. For those values, the wrapper's
`as.logical()` count selects the same branch as R's native `if`, and the
wrapper returns the original value to that native `if`.

The source has no stack-introspection operation in these conditions whose
answer would be changed by the extra wrapper frame. Conditions do not read
the observer's environment. The recorded cases use ordinary value-semantics
lists and character vectors, so there is no external active binding or
callback mutation to distinguish the two evaluations. The full root runs
also check exact returned-object equality separately for every input.

This is not a general semantics-preserving transformation for arbitrary R.
It changes stack usage, so resource-limit behavior can differ. It would also
replace some malformed-condition error behavior with its own assertion;
normal canonical conditions avoid those cases. Source-level reasoning and
the per-case equality checks justify the present usage.

The RDS metadata stores the contents of an existing hash manifest, rather
than computing each file hash during `save_progress()`. It also omits the
generator seed/profile. The exact saved first inputs make the reachability
claim independently reproducible despite those provenance limits; the
current kernel hash was independently recomputed for this review.

One generator detail does not affect validity but limits the word “dense”:
when `n_symbols == 2`, `sample(2:min(4L, n_symbols), 1L)` is `sample(2, 1)`.
R interprets its length-one numeric argument as `1:2`, so that profile can
generate units. It is not a unit-free generator. All saved witnesses still
meet the canonical representation contract.

## 3. The lifecycle contract needed for sites 26 and 75

The precise contract is:

> A restriction target that still contains the requested symbol is a live
> nonunit when `apply_domain_restriction()` starts changing it.

Do not strengthen this to “every entry into `apply_domain_restriction()` has
a nonunit target.” A stale registry snapshot can name a clause that has become
a unit on a **different** symbol. Its requested symbol is absent, and line
149 returns without a mutation. The replay finds this real path once: the
dense witness labelled `random:169` calls a restriction on `X1` for clause
23, which by then is the live unit `X3 in {v3,v1}`. The exact input/state is
saved as `stale_unit_example` in `review_results.rds`.

The contract follows from the complete caller list:

| Caller of `apply_domain_restriction` | Why a present-symbol target is a live nonunit |
| --- | --- |
| `register_unit`, line 135 | The retained representative has already left the symbol registry. A snapshot member becoming a same-symbol unit merges into that representative and is marked eliminated, so line 128 skips it. A member becoming a different-symbol unit has lost the propagated symbol and returns at line 149. |
| Initial preprocessing, line 506 | Unvisited clauses have not joined the registry and cannot be changed by earlier propagation. A current clause's conversion to a unit returns NULL and breaks its symbol loop. A still-nonunit restriction cannot recurse while the matrices are NULL. |
| First-order SSE, line 322 | Every entry to its ordinary handler has live nonunit operands. There is no intervening mutation before the call. |
| Second-order SSE, line 467 | The oneend/twoend handlers guard the target and donors before each trial and recheck them after every recursive result. |

The ordinary handler's own callers preserve that claim:

* Range-update and symbol-deletion callback loops check each target and stop
  if their source becomes inactive after a callback.
* Pair construction guards the current inner/outer operands; NULL and the
  explicit source/target checks stop or skip the appropriate subsequent work.
* The manual queue checks both operands anew immediately before each call.
* Second-order candidate snapshots are revalidated before a trial; the main
  source and target are checked after it, including before the next orientation.

This is the same caller topology audited in
`../proof_state/LIFECYCLE_PROOF.md`, Sections 1–2, independently rechecked
against the current source here. A first-bad-call argument avoids circularity:
unit birth removes registry membership and marks the candidate unit or
eliminated before any recursive callback; subsequent guarded calls cannot
be the first call that mutates an already registered unit as a nonunit.

### 3.1 Site 26 is unreachable

There is only one call to `eliminate_symbol_from_clause()`, at line 163.
The requested symbol exists, because line 149 already excluded absence, and
the preceding intersection just removed its values. By the caller contract,
the local clause still has at least two distinct symbols. No recursive call
occurs between reading/intersecting the local clause and this helper call.

The helper reads the stored clause, removes exactly the requested symbol,
and then reaches site 26. At least one symbol remains, so `!length(clause)`
is FALSE. If exactly one remains, the next branch registers a new unit.
Opposing units are detected by the empty intersection in `register_unit()`,
not by attempting to remove the last symbol from a live unit here.

### 3.2 Site 75 is unreachable for a stronger reason than normal return

TRUE at site 75 throws an error, so excluding it merely by assuming a normal
return would be tautological. The caller contract proves the stronger
structural claim that no canonical execution reaches this error guard with a
unit, subject to the ordinary finite-index and resource assumptions.

The helper has four syntactic call sites:

1. Line 156 follows an intersection-length check in `apply_domain_restriction`.
   Its target is a live nonunit by the preceding contract; no recursive call
   occurs before the elimination.
2. Line 310 handles zero exceptions in `on_updated_subset_relations`.
   The handler's target is a live nonunit on entry, and no mutation precedes
   this zero-count branch.
3. Line 698 is the nonunit-HLA full-domain branch. It is already excluded by
   the next section, but independently its target comes from the nonunit-only
   HLA target list and cannot become a unit during HLA.
4. Line 714 is nonunit-HLA hidden subsumption. It has the same nonunit-only
   target and runs before any previous deletion of that target; the loop
   immediately breaks after deleting it.

HLA does not change actual ranges or create units. Its initial split is exact:
there is one live unit per unit-domain entry, all live nonunits have length at
least two, and sorting by length places exactly those nonunits first. Unit
HLA deliberately writes the unit's eliminated flag directly and never calls
this nonunit registry helper.

## 4. A selected HLA donor prevents a full-domain extension

Let `C_s` be the current **virtual** target range and `D_s` the selected
donor's range at the chosen pivot. Exact donor selection says

```
D_s is not a subset of C_s.
```

Choose `v in D_s minus C_s`. Canonicality gives `v in U_s`, where `U_s` is
the original universe domain used by the implementation. The extension is

```
C'_s = C_s union (U_s minus (C_s union D_s)).
```

The old range omits `v`, and the added complement omits `v` because it belongs
to `D_s`. Thus `v` is still absent from `C'_s`. The range stays inside `U_s`
and stays unique: all appended values come from the unique domain and are
explicitly excluded from the old range. Therefore

```
length(C'_s) < length(U_s),
```

which makes both sites 98 and 104 FALSE. Properness of all initial literal
ranges is more than this local lemma needs; non-inclusion of the chosen donor,
uniqueness, and domain containment are its actual premises.

### 4.1 Why nonunit selection is exact

The quiescent comparison argument is essential; contextual FALSE certificates
alone would not justify a missing **physical** value.

* Initial pair comparisons are raw membership tests. Every later live
  contextual FALSE becomes a raw FALSE certificate at HLA entry because
  completed propagation physically contains both ranges inside every unit.
* A TRUE comparison can become raw-inaccurate only by a source shrink. Every
  such shrink owns an update loop covering the affected initialized targets;
  recursive later shrinks create their own update obligations. A loop either
  discharges them or its source becomes inactive. All such frames have
  unwound before HLA starts.
* Counts are maintained as sums of initialized comparison rows, with guarded
  one-time increments/decrements. Hence live rows and counts are raw-exact at
  HLA entry, as detailed in `QUIESCENT_MATRIX_PROOF.md`.

Within one HLA target, actual donor ranges remain fixed and only the virtual
target expands. Each expansion visits every live donor containing its changed
symbol and clears exactly the comparisons that became contained, adjusting
their counts. FALSE comparisons cannot become TRUE under expansion. The
selected count-one donor therefore has exactly one actual exceptional symbol
against the current virtual target, providing the witness above.

Writes for one virtual target affect only that target's row within each donor
matrix; later targets use different rows and the unchanged original count
matrix. Deleted targets are removed from the remaining donor set and registry.
This preserves the entry argument for every later target.

### 4.2 Why unit selection and lazy rows are also exact

At unit-HLA entry, every nonunit donor's range at the unit symbol is physically
contained in the actual unit range. Equality is harmless here; strict
containment is unnecessary. Thus the initial donor count is exactly its
length minus one if it contains the unit symbol, and its full length otherwise.

The lazily initialized row uses FALSE at the unit symbol and TRUE at every
other donor symbol. This row is exact when first materialized. If an earlier
virtual expansion had concerned any of its other symbols, the complete
symbol registry would already have visited and materialized that donor then.
If no such expansion occurred, all its nonunit-symbol virtual ranges are
still absent. Later expansions update materialized rows and counts exactly
as in the nonunit case.

Therefore a selected unit-HLA donor also has an actual value missing from the
current virtual target. This argument uses final physical containment proved
from frozen unit-birth certificates; it does not assume the target unit true
while trying to prove that unit redundant.

## 5. Saved artifacts and scope

* `review.R`, `review.log`, and `review_results.rds` preserve the parser audit,
  all saved-witness replays, local contracts, and concrete HLA/stale-unit cases.
* `independent_catalog.tsv` adds exact source locations to the combined counts.
* `check_hla_complements.py` independently exhausts every current/donor set
  pair on domains of sizes one through eight with a selected-donor exception:
  77,540 cases. Every new range omits its donor witness and is smaller than
  the domain. `hla_complement_results.json` records the totals.

The contract excludes malformed selector outputs with repeated/NA symbol names
or NULL ranges, external universe mutation, and resource/index failures.
It does not establish saturation or idempotence of the general simplifier;
known canonical scheduling gaps remain compatible with complete feasible
if-outcome coverage. No production files, root artifacts, or package metadata
were modified, and no commits were made for this review.
