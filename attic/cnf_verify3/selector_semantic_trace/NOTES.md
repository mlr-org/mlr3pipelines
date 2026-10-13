# Event-level semantics of repeated clause selectors

This independent trace locates the first semantic change in both interpretations
of the accepted four-clause selector example. The positional interpretation first
fails when the simplifier **deletes c2**. The first-occurrence projection remains
correct until a later operation **deletes c1's first X occurrence**, exposing its
stale second range. HLA causes neither change. The final result has unique names
and incorrectly admits `X=c,Y=a` in both interpretations.

All six production files are unchanged; `SOURCE_HASHES.sha256` records them.
`common.R` sources those files into private environments. A second private copy
of the simplifier contains only added observations: removing the observer calls
and one private frame binding recovers every original source line exactly. Every
instrumented result must equal a separate unchanged-source result, including
physical duplicate positions, range order, clause order, or the same runtime
error. No production files, root's selector candidate, or existing campaign
artifacts were edited. This stream made no commits.

## Interpretation and observation boundaries

For an ordered clause `[(s1,R1),...,(sk,Rk)]`, the positional oracle is
`OR_i (assignment[si] in Ri)`. It iterates integer positions and preserves all
copies. The diagnostic first-occurrence projection first chooses the earliest
position of each name and then applies the same positional oracle. It does not
use a named range lookup. Thus `[X={b}, X={b,c}]` means `X in {b,c}` positionally
and `X=b` under projection. The input oracle uses selected original raw atoms;
each public selected clause must have the same table before simplification.

Every observation records the entire ordered entries list, eliminated flags,
unit domains and registries, symbol registry, initialized caches, relevant call
frames, and both complete truth tables for the conjunction of **currently live
stored clauses**. The observations include all writes to stored entries,
elimination flags, unit domains, and HLA virtual ranges, plus the proposed HLA
ranges used for immediate hidden deletions. Unit-domain writes are additionally
checked against their registered physical clause and by independently verifying
that the live conjunction already entails the recorded unit.

For every stored clause change or deletion, the analysis separately compares:

1. the changed clause alone;
2. that clause under the other currently live physical units;
3. that clause under all other currently live clauses.

A raw change is not automatically an error: unit propagation and HLA generally
need their surrounding constraints. Conversely, an already eliminated clause
cannot be retained silently as a premise of a later equivalence check. The
positional and projection meanings are checked separately at each boundary.

`results_r36.rds` and `results_r46.rds` retain every state without normalizing
duplicate names. Their `runs` and `repaired` components are identical across R
versions. The TSV event files use numbered occurrence displays, not JSON objects
keyed by symbol name. The RDS is the authoritative lossless record.

## Four-clause derivation

Both domains are `{a,b,c}`. The public input is:

```
c1 = [X={b,c}, X={b,c}]       # one-row matrix(c(1L,1L), nrow=1L)
c2 = [X={b}, Y={c}]
c3 = [X={c}, Y={b}]
c4 = [X={a}, Y={a}]
```

The duplicate copies initially agree, so both interpretations are the original
formula. c1 requires X in `{b,c}`; c4 then requires Y=a; c2 requires X=b and c3
requires X=c. There are no models. All four physical widths are two, so stable
initial ordering preserves these clause indices and creates no initial units.

**First nonempty restriction, event 47, source line 167.** Second-order SSE
uses c2 and c4, resolves their disjoint Y ranges `{c}` and `{a}`, and restricts
c1's X range to the union of donor X ranges `{b,a}`. The call at line 467 is:

```
apply_domain_restriction(1L, "X", c("b", "a"), FALSE)
```

Line 148 selects the first matching position. Lines 152 and 167 consequently
write only its intersection, leaving:

```
c1 = [X={b}, X={b,c}]
c2 = [X={b}, Y={c}]
c3 = [X={c}, Y={b}]
c4 = [X={a}, Y={a}]
```

There is no live-formula divergence yet. Positionally this c1 change is a raw
no-op because the second copy still covers `{b,c}`. Under projection the raw
clause loses the three X=c rows, but the restriction is equivalent under the
still-live donors c2 and c4. That donor-relative claim is nonvacuous: both
`project(c1_old) & c2 & c4` and `project(c1_new) & c2 & c4` have exactly the one
model `X=b,Y=a`. Adding c3 makes the full formula unsatisfiable in either case.

This is where the representation first becomes heterogeneous: surviving copies
of X in c1 no longer have the same range. The stored positional meaning and its
projection differ even though their current full conjunctions still agree.

**First positional divergence, event 57, source line 474.** The range update
sets c1's first X cache bit toward c2 to FALSE at line 221 and reduces its count
to one at line 222. The relevant exact cache row and count are:

```
colnames(is_not_subset_of[[1L]])             = c("X", "X")
is_not_subset_of[[1L]][2L, ]                 = c(FALSE, TRUE)
not_subset_count[1L, 2L]                     = 1L
entries[[1L]]                              = list(X="b", X=c("b","c"))
entries[[2L]]                              = list(X="b", Y="c")
unit_domains                               = list()
```

These two bits can correctly describe the *two physical occurrences*: the
first `{b}` is contained in c2's X range, whereas the second `{b,c}` is not.
The failure is in converting the exceptional occurrence back to a symbol name
and then looking up that name's **first** range. Lines 314 and 322 choose X from
the trailing TRUE column but fetch `entries[[1]][["X"]]`, which is `{b}`.
The nested call is therefore:

```
apply_domain_restriction(2L, "X", "b", FALSE)
```

The intersection is `{b}`, exactly as long as the supplied restricting range.
Lines 153–156 take the coverage branch and call `eliminate_clause_update_sr(2L)`;
line 474 commits the deletion. The live state becomes:

```
c1 = [X={b}, X={b,c}]
c3 = [X={c}, Y={b}]
c4 = [X={a}, Y={a}]
```

Its positional table has the newly admitted model `X=c,Y=a` (assignment row 3).
Both remaining ordinary clauses and c1's trailing X occurrence are true there;
the deleted c2 was false. Thus this deletion is not equivalent even under all
the remaining live constraints, and there are no live units to qualify it.

The invalid source premise is **one symbol name identifies its entire donor
range**. With unique names, equal intersection and restricting-range lengths,
together with off-target containment, establish donor subsumption of the target.
Here the true exceptional occurrence has range `{b,c}`, not the looked-up `{b}`.
The positional donor does not imply c2. If its full X union `{b,c}` were used,
the target intersection would have length one while the restricting range had
length two, so this coverage deletion would not follow.

Under projection the same deletion is sound: projected c1 is X=b and implies
c2 directly. Its full projected state remains unsatisfiable. A claim that the
first range write itself already changed the complete input's truth table would
therefore locate the failure too early; a first-name-only oracle would locate
the actual positional failure too late.

**First projection divergence, event 70, source line 244.** Processing resumes
inside the earlier second-order handler, which now uses the still-live donors
c3 and c4. Their Y ranges `{b}` and `{a}` are disjoint, so the requested X
restriction is their X union `{c,a}`:

```
apply_domain_restriction(1L, "X", c("c", "a"), FALSE)
```

The first c1 range `{b}` intersects this to empty. The helper takes the symbol
removal branch, reloads c1, and executes `clause[["X"]] = NULL` at line 238.
Line 244 commits:

```
c1 = [X={b,c}]
c3 = [X={c}, Y={b}]
c4 = [X={a}, Y={a}]
```

Positionally the removed `{b}` occurrence was redundant beside its broader
copy. This step is a raw no-op and preserves the positional model already
introduced by c2's deletion. Under projection it changes X=b into X in `{b,c}`;
`X=c,Y=a` now also becomes a model of the projected conjunction. The exact failed
premise here is that removing the selected named occurrence removes that
symbol's range, rather than exposing another occurrence with an older range.

Keeping c2 in the context would hide this second divergence, because it rejects
`X=c,Y=a`. But c2 was already eliminated and is not an available premise. The
tests explicitly check both the real current context and this invalid restored
context to distinguish the two claims.

The remaining changes are ordinary consequences of the already weakened state:

| Event | Source line | Committed change | Positional live delta | Projection live delta |
| --- | ---: | --- | --- | --- |
| 47 | 167 | c1 becomes `[X={b}, X={b,c}]` | None | None |
| 57 | 474 | Delete c2 | Adds `(c,a)` | None |
| 70 | 244 | c1 becomes physical unit `[X={b,c}]` | None | Adds `(c,a)` |
| 82 | 244 | X unit removes X=a from c4, leaving Y=a | None | None |
| 92 | 244 | Y unit removes Y=b from c3, leaving X=c | None | None |
| 95 | 103 | Merge X units, narrowing retained c1 to X=c | None | None |
| 96 | 104 | Remove redundant incoming c3 unit | None | None |

The X={b,c}, Y=a and merged X=c unit-domain writes occur at events 73, 85 and 95.
Each is already entailed by the physical live clauses at its write. The final
formula is exactly `list(list(X="c"), list(Y="a"))`. No non-unit HLA extension
or unit-HLA extension occurs in this run.

## Smaller controls

**Two-clause stale range.** X has domain `{a,b}` and Y has `{a,b,c}`. The selected
inputs are:

```
a = [Y={b,c}, X={a}]
b = [Y={c,a}, X={a}, Y={c,a}]
```

Their conjunction is `X=a OR Y=c`. First-order SSE using a restricts b's first Y
to `{c}` at event 21 / line 167. The trailing `{c,a}` remains. Positionally this
is a raw no-op; under projection it is equivalent in a's still-live context.
The resulting b-to-a row is `[FALSE,FALSE,TRUE]` with column names `[Y,X,Y]`.
The trailing TRUE names Y, but named donor lookup obtains the first Y range
`{c}`. Coverage restriction of a then deletes all of a at event 28 / line 474.
Its positional meaning gains `X=b,Y=a` (row 2); the projection remains correct
through return. This is the same invalid exceptional-occurrence lookup premise
as the main example, but the result remains noncanonical and its projection
never exposes a stale first range.

**Two-clause unit-HLA runtime error.** X has domain `{a,b,c}` and the input is
`[X={a},X={a}] AND [X={a,b}]`, logically X=a. Sorting puts the physical broad
unit first. The two copies of `{a}` are strict subsets of `{a,b}`. Initial unit
propagation changes neither copy and does not delete the non-unit; its X
registry contains the repeated indices `c(2L,2L)`.

The last observation before the error has:

```
entries                         = list(list(X=c("a","b")), list(X="a",X="a"))
remaining_nonunit_entries        = 2L
unit_domains                    = list(X=c("a","b"))
symbol_registry                 = list(X=c(2L,2L))
not_subset_count                = 2L - as.integer(2L %in% c(2L,2L)) = 1L
is_not_subset_entry              = c(X=FALSE, X=FALSE)
symbol                          = character(0)
```

Line 738 subtracts Boolean registry membership from physical occurrence width.
Its comment assumes one occurrence of the unit symbol, so its one-exception
count disagrees with the actual mask built at line 750. Line 753 extracts no
exceptional symbol; line 754 evaluates `clause[[character(0)]]` and errors.
Neither interpretation changes at any preceding committed state. This is a
runtime failure, not a returned wrong formula. There is no completed HLA range
proposal or deletion to assign a semantic mismatch to.

## Why the reviewed Boolean occurrence theorem does not extend here

The scoped argument in `../boolean_occurrences/PROOF_ATTEMPT.md`, as qualified
by `../occurrence_review/REVIEW.md`, assumes one unchanged shared universe,
ordinary two-element domains, proper constructor-built clauses, and initially
equal singleton copies. Surviving stored copies then never develop unequal
ranges: an intersection either keeps a singleton or removes an occurrence.
The first nonempty strict shrink at event 47 is impossible in that scope.

With equal surviving Boolean copies, retrieving the first range also retrieves
the exceptional copy's value; deleting one copy while another survives is a raw
no-op in both interpretations. The separate grouped-cache/orphan and same-name
SSE2 arguments remain necessary, but the ternary heterogeneous-range premise
that fails here is absent. This trace does not challenge those Boolean claims.

The runtime control needs a proper `{a,b}` unit and a strict nonempty `{a}`
non-unit range, which likewise cannot coexist as proper Boolean ranges. In the
Boolean argument, after unit propagation no active non-unit remains currently
registered for a unit symbol. Orphan copies can survive, but the unit-HLA count
then starts at the full physical width, at least two, so this loop cannot begin.

HLA cache containment must also retain the review's phase/target qualification:
before HLA it concerns stored clauses; inside one HLA pass it concerns that
target's virtual expansion, and previously expanded target columns need not
retain raw containment in their unchanged stored targets. The independent
`hla_virtual_control` reproduces this harmless virtual expansion setting and
checks all three HLA proposals. `hla_orphan_deletion_control` checks two proposals
and an actual hidden deletion while repeated Boolean occurrences exist. Every
proposal preserves both conjunctions under the current other live clauses,
although its raw clause table can change. Neither control has a semantic error.

## Selector repair: sufficient for this entry path, with a missing-value detail

The private candidate makes exactly the two substitutions in root's diagnostic
candidate, without altering that file or any production definition:

```r
i = as.vector(unclass(i))
# ...
check_logical(i, len = true_length, any.missing = FALSE)
```

Flattening before `unique()` makes numeric and character uniqueness apply to
individual elements regardless of matrix/array shape. For an ordinary canonical
input clause, accepted numeric indices are floored, globally deduplicated and
range checked; accepted character indices are globally deduplicated existing
names; accepted nonmissing logical indices select each physical position at
most once. Empty selectors produce FALSE. Thus each accepted result selects a
subset of the original unique positions and preserves their proper ranges.
It cannot create the repeated-name or missing-name objects traced here. This is
a source argument for ordinary selectors of any finite shape/length, supported
by the finite check; the finite check is not its all-size substitute.

An additional observed boundary detail corrects the earlier claim that numeric
and character NA selectors are invariably rejected. With real checkmate 2.3.4,
`check_logical(NA_real_, len=1L)` and `check_logical(NA_character_, len=1L)` both
return TRUE. For a one-symbol proper clause, production accepts each of `NA`,
`NA_real_`, and `NA_character_` and returns an NA name with a NULL range. Matching
all-NA numeric/character vectors also enter through this permissive logical
validation alternative. The candidate's `any.missing=FALSE` rejects them on both
tested R versions.

Accordingly, **rejecting missing values in the logical validator** is sufficient
here; an external guard that rejects only values satisfying `is.logical(i) &&
anyNA(i)` would not cover the all-NA numeric/character path. The boundary audit
requires rejection for every missing selector, and compares the candidate to
production on explicitly flattened indices only for nonmissing selectors.

The candidate returns FALSE for the four-clause contradiction, canonical
`X=a OR Y=c` for the stale-range control, and canonical X=a for the unit-HLA
control. Every traced stored state of those repaired controls preserves the
input table and has unique symbol names. This repair prevents creation through
these selector paths from canonical clauses; it does not claim to repair an
already malformed input object, class spoofing/custom S3 behavior, mutable
universes, encoding mixtures, or other constructor defects.

## Verification and reproduction

Both R 3.6.3 and R 4.6.1, with real checkmate 2.3.4, produce identical complete
trace data. The five unchanged-source controls have 383 recorded observations,
12 stored clause changes/deletions, four unit-domain writes, five HLA proposals
and four committed virtual expansions. All returned objects and the runtime
error exactly match a separate production run. The repaired controls are also
independently traced and checked.

The separate selector boundary bank makes 1,703 calls per R version over one-
through three-symbol canonical clauses, selected numeric/character/logical words
through length three, empty/invalid/missing controls, plain and named vectors,
row/column matrices and three-dimensional arrays. It records 1,093 accepted
canonical, truth-correct results matching the explicitly flattened reference,
435 rejected missing-selector calls, and 339 noncanonical baseline acceptances.
These are counts of calls, including deliberately repeated controls, not claims
of unique inputs. The current-R focused test file passes 103 expectations.
Native R lacks testthat; native reproduction and boundary checks use stopifnot.

From the repository root:

```sh
Rscript attic/cnf_verify3/selector_semantic_trace/reproduce.R
Rscript attic/cnf_verify3/selector_semantic_trace/candidate_boundary.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/selector_semantic_trace/reproduce.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/selector_semantic_trace/candidate_boundary.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/selector_semantic_trace/test_audit.R
```

The scripts write version-separated RDS/TSV records only in this directory.
`reproduce_r36.log`, `reproduce_r46.log`, the two boundary logs, and
`test_audit_r46.log` preserve the checked runs. No broader minimality, arbitrary
three-clause safety, or multivalued duplicate-name semantic theorem is inferred.
