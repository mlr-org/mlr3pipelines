# Independent review of ordinary indexing and initialization safety

Reviewed 2026-09-06 against `../index_contract/PROOF.md`, `INVENTORY.md`,
`README.md`, and unchanged `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
Source line numbers below refer to that production file.

## Verdict and scope

I found no missing source-level lemma or canonical counterexample to the
proposed ordinary indexing/value-shape theorem. Its first-failing-operation
argument is justified under the stated canonical finite-storage assumptions.
It excludes an invalid consumed subscript, missing consumed index, wrong-rank
access, non-scalar/missing condition, undefined local, and the explicit
internal `stop` on these source paths. Successful whole execution is not
used as a premise.

The result requires ordinary finite lists, character sets and environments;
unique nonempty nonmissing symbol names in each clause; nonempty proper
literal ranges; a consistent fixed universe; and normalized constants.
External/custom dispatch, active bindings and caller computation, inconsistent
character equality, overflow, stack/allocation failure and malformed accepted
public representations are excluded. The known matrix-selector problem is
therefore unaffected. Semantic preservation, simplification completeness and
termination are separate claims; the bounded follow-up in
`COMPOSITION_REVIEW.md` reviews the proposed termination/safety/preservation
composition and helper-call count.

This is an independent human source review, supplemented by new observations
and reduced controls. I did not use the proposing stream's test helpers,
instrumentation, input generator or saved result counts to establish the
review's executable evidence. I did reuse its explicitly identified
five-clause stale-snapshot witness as one directed boundary input; the rest
of the portfolio is constructed here.

## 1. Prefix induction and exact transition boundaries

The stable actual index space has `m` entries after the initial permutation
(48). No subsequent actual-entry assignment changes this outer length.
`is_unit` and `eliminated` therefore have fixed valid scalar coordinates.
After 530, `available` is a fixed injection of the then-live nonunits, and
`available_inverse` is a fixed partial inverse. Activity can only disappear.
The actual ranges and symbol lists only shrink; matrix columns retain their
allocation-time names. These facts are preserved by the individual writes,
not inferred from a completed successful computation.

There are two kinds of intentional intermediate state which a valid
small-step argument must distinguish from a dispatch boundary:

* Literal deletion writes a singleton at 244, removes its last registry
  occurrence at 250, and only then enters registration at 251. The new
  singleton may still have `is_unit=FALSE` until 95. It is a pending unit,
  not a live nonunit requiring registry membership. Conversely, whole-clause
  elimination sets its flag at 474 before deleting its registry occurrences
  at 475–477. Neither interval contains an inference callback.
* A TRUE/FALSE bit is written before the associated count repair at
  186–188, 221–222, 265–266 and 271–272. During the interval the old count
  differs by exactly the pending signed change; the fixed coordinates and
  ordinary numeric values justify the repair without relying on count/row
  equality. No count-one/two pivot or inference callback consumes the
  half-repaired state. The same explicit discipline applies to local HLA
  bit/count repairs at 709–711 and 773–774.

At inference/selection boundaries, initialized counts equal the corresponding
TRUE-bit cardinalities. Construction sums the completed rows (608–610).
Reverse updates test FALSE before changing it to TRUE; forward updates
recheck TRUE after reentrancy before clearing it. Column deletion captures
exactly the initialized TRUE rows before clearing the column. Its reverse
refresh has no callback before all repairs finish. Thus count-one and
count-two selections yield exactly one and two ordinary column names,
respectively. Semantic exactness of a temporarily stale comparison is not
needed for this cardinality argument.

Suppose a first ordinary consuming operation were invalid. The preceding
finite prefix has the initial shapes and the preserved boundary invariants,
or is inside one of the explicitly checked finite repair transitions. The
applicable source case below then establishes the next operation's shape,
contradicting that it is the first failure. This reasoning remains valid if
one has not yet established that the whole invocation terminates.

## 2. The principal consumed-boundary obligations

| Boundary | Independent source discharge |
| --- | --- |
| Zero/one available nonunits | The all-unit equality at 491 prevents a descending initial sequence at 499, including the empty conjunction. Preprocessing may still leave zero nonunits. The allocations at 538–548 have their explicit `n x n` dimensions for `n=0`; the initial loop and the two-column `which(..., arr.ind=TRUE)` queue are empty. For `n=1`, the only count is the unused NA diagonal and nonunit-HLA donors/counts are empty vectors. The guarded match stops before a missing donor index is consumed. |
| Matrix allocation before initialized pairs | The active outer index gets its matrix before any callback (561–569). An earlier currently active index must also have been allocated: a skipped inactive index cannot later reactivate. Future active slots may still be NULL, so 118/133/176/261 need their explicit outer-index guards. A non-NA pair count independently certifies both endpoint matrices. The diagonal is excluded. |
| Other-symbol units in old registry snapshots | A line-124 snapshot contains live nonunits on the propagated symbol when captured. An eliminated or same-symbol merged candidate is skipped at 128. A retained different-symbol unit has lost the old symbol, so 148–149 returns before positional range access. The optional matrix read at 133 occurs earlier, but remains valid: for an index at/below the fixed outer bound its matrix and old column existed while the snapshot member was active; for a future index short-circuiting skips the read. Callbacks cannot advance `meta_idx_outer`. |
| Registration inverse/name | The initial unique unit queue and preprocessing run before matrices exist. All later registration calls are singleton births from an original nonunit in `available`. Both merged and retained candidates therefore have valid inverses when 115 is reached. The retained symbol belongs to any allocated old matrix. `inso_column` is assigned whenever `use_inso` becomes TRUE. |
| Restrictions and frozen columns | Each caller supplies one symbol: registration/preprocessing iterate names; first-order work has count one; second-order work selects a guarded target name. `match` at 148 is scalar and its NA guard precedes local `[[`. Local intersection and replacement precede callbacks. Current or just-deleted symbols at 194/263 still belong to their allocated frozen matrix columns. An absent named list lookup elsewhere is ordinary NULL, not a missing numeric index. |
| Deleted-symbol `sr` snapshot | The unit branch reassigns `sr` at 249 but unconditionally returns at 251/252. The nonunit branch reaching 267 retains the removed-symbol snapshot captured at 241. Its members have fixed inverses; the non-NA pair mask at 269 excludes unallocated future and diagonal rows. |
| Active pair callbacks and the internal stop | The five `on_updated_subset_relations` call sites at 224, 285, 612, 620 and 646 pass initialized distinct pairs and live nonunit operands. The row-loop callbacks recheck target status and immediately stop if their own source became inactive; pair construction and the manual queue have the corresponding guards. Deletion through count zero or restriction consequently targets a nonunit. During HLA only the current nonunit target calls the deletion helper. Thus `is_unit[[clause_idx]]` cannot be TRUE at 473. |
| Second-order snapshots | Candidate activity/count/bit checks at 381/386/390 and 412/427/431 refresh the consumed premises after recursion. Target symbol presence is checked immediately before each trial at 397/440. A donor may lose an old intersection symbol; its named range lookup at 462–464 yields NULL safely. The two saved names at 413–418 remain a two-element local snapshot even when actual clauses later change. |
| Deliberate NA candidates | The ordinary vector `[` operations at 382 and 424 may produce NA entries for uninitialized pairs. They are legal intermediate values. The `!is.na` filters at 383/425 remove them before the scalar `available[[...]]` reads at 387/430. Count and enable-matrix NA cells are not themselves consumed as unguarded scalar conditions. |
| Two-symbol matrix selectors | `match(..., nomatch=0L)` at 436 supplies exactly two valid positions or zeros. Zero is legal for ordinary `[`, including the zero-element result. `sum` of the selected zero/one/two values is scalar. Count one/two selections elsewhere never rely on recursive multi-element `[[`. |
| Nonunit-HLA self exclusion | Only the current target can be eliminated, so the saved `for` sequence never reaches a future target eliminated by an earlier iteration. Every other live registry index occurs in `remaining_other_entries` once. The target itself can appear in the registry update loop but its self row has stayed FALSE since 569; 707 prevents reaching the missing self-donor match at 710–711. |
| Nonunit-HLA local counts | Each target's initial local count vector corresponds to that target's row in each donor matrix (673). Earlier targets modified other row coordinates; global matrix counts are not reused for a modified row. Every local clear decrements that donor's local counter. Count one therefore supplies exactly one pivot at 690. No actual donor range changes in HLA. |
| Unit-HLA lazy rows | The initial count at 738 equals `sum(names(donor) != unitsymbol)` because donor names are unique and the registry records membership exactly. Every decrement first allocates its row (765–769), then clears a TRUE bit. An unallocated row has never been decremented, so count one implies one TRUE immediately after lazy construction; an allocated row has the maintained equality. This proves scalarity at 753 without importing a whole-execution semantic premise. |
| Delayed inverse | After 730 the surviving nonunit vector and symbol registry are immutable. Every registry index therefore occurs exactly once in the list used by the delayed `match`. Neither earlier unit eliminations nor local virtual range changes alter that list before the first force at 764. Its consumed row index is nonmissing. |
| Matrix-to-vector phase transition | The last pair callbacks return before HLA begins. After 738 overwrites `not_subset_count` with a vector, only local unit-HLA operations, the character leaf helper and final return execute. No pair helper consumes the now-vector binding. |

The normal helper return values are TRUE, FALSE or NULL. Every plain
`if (adr/ousr/hs2oo)` is either guarded against NULL or reached through an
explicitly Boolean helper return; other callers use `identical(..., TRUE)`.
The remaining conditions consist of scalar indices/flags/counts/name tests
or `length`, `all`, `any`, `is.null` and `identical`. With the preceding
index/cardinality facts they are defined scalar conditions. Early helpers
return while `is_not_subset_of` is NULL before reading later-only bindings.
Every callback that reads the outer-loop bound occurs after assignment of
that bound and allocation of the current outer matrix. The zero-slot path
has no such callback. No undefined-local obligation remains.

## 3. Independent inventory and observer

`observe_boundaries.R` lexes the unchanged source with base R's parser.
The independently generated `lexical_sites_r36.csv` and
`lexical_sites_r46.csv` contain identical line/column inventories:

| Original source constructs | Count |
| --- | ---: |
| `[` tokens | 105 |
| `[[` tokens | 235 |
| `if` sites | 108 |
| `&&` sites | 8 |
| `||` sites | 26 |
| Explicit internal `stop` | 1 |

The observer injects read-only checks immediately before selected original
source statements and independently wraps all original scalar conditions
and short-circuit operands. It checks the exact permitted signed row/count
deltas between bit and count assignments, then checks repaired equality.
It also checks registry lifecycle, actual/meta index domains, pair allocation,
filtered candidate consumption, optional old/future matrices, self rows,
local donor counts, lazy rows and the immutable delayed-inverse domain.
The production expressions and assignments are preserved. Conditions are
evaluated once, and the `&&`/`||` wrappers preserve short-circuit evaluation.

This is boundary instrumentation, not a claim that every original indexing
operation has its own dynamic wrapper or that every control-flow outcome was
covered. In particular, exact counts below are observations at these selected
boundaries. The general indexing conclusion comes from the source audit and
prefix argument, not from a coverage percentage.

Each observed function return is compared identically with the untouched
function in the same universe. No independent semantic truth-table oracle is
claimed for this review: its purpose is storage/control safety, and the
separate normal-return semantic review supplies that different theorem.

The independently generated portfolio contains:

* Nine directed inputs: constants, the empty conjunction, one/duplicate/
  contradictory units, preprocessing with zero remaining nonunits, one
  nonunit, and the identified stale other-symbol singleton witness.
* All 18,278 ordered lists of one through three canonical clauses over three
  Boolean symbols: there are `3^3-1=26` clauses, so the count is
  `26+26^2+26^3`. Clause duplicates and different orders are included.
* 1,500 multivalued inputs using seed `202609061`, 2–7 symbols, domains of
  size 2–5 and 1–20 clauses. Most clauses are nonunits; explicit units are
  also generated. Symbol positions and value positions use `sample.int`,
  avoiding the length-one numeric `sample` pitfall.

R 3.6.3 and R 4.6.1 each passed the same **19,787 input executions**. This is
one portfolio replayed twice, not 39,574 distinct generated formulas. Every
named observation count agrees between versions; the raw count-list order
differs because it is collected from an environment. The CSV inventories and
the control rejection messages agree exactly.

| Observation | Each R version |
| --- | ---: |
| Observed/original return comparisons | 19,787 |
| Scalar `if` and short-circuit operand checks | 3,310,814 |
| Registry lifecycle boundaries | 241,825 |
| Matrix bookkeeping boundaries | 195,750 |
| Initialized matrix-row/count checks, including permitted pending deltas | 7,176,512 |
| Matrix bit/count repair blocks inspected before and after repair | 9,191 |
| Nonunit-HLA pending local repairs | 7,372 |
| Unit-HLA pending local repairs | 544 |
| Future singleton registrations | 412 |
| Optional future-index short circuits | 2,542 |
| Optional allocated old-matrix checks | 4,251 |
| Retained stale other-symbol-unit encounters | 7 |
| Intermediate candidate vectors containing NA | 15,023 |
| Filtered candidate indices consumed | 54,517 |
| Nonunit-HLA self-row guards | 2,439 |
| Lazy inverse consumptions | 2,368 |
| Unallocated lazy donor rows inspected | 11,385 |
| Allocated lazy donor rows inspected | 7,811 |

There are also **23 reduced ordinary-R shape assertions** in
`reduced_shapes.R` on each R version, including exact-source observations of
preprocessing that leaves zero and one allocated nonunit slots. These check
0-by-0/1-by-1 dimensions, the genuine two-column empty queue, empty donor
slices, scalar-NA match, legal zero column selectors, absent named lookup,
frozen-column lookup and filtered NA vector entries. The first draft of one
reduced assertion overlooked a harmless name attribute on a dropped 1-by-1
matrix value; that observer expectation was corrected to check scalar value
and rank. No source change or production failure was involved.

## 4. Eight corruption controls

The same observer accepts each original saved canonical fixture, then rejects
the corresponding deliberately corrupted in-memory source. All **eight
additional original fixture replays pass** and all **eight modified copies
are rejected**, on both R versions:

| Deliberate source change | Rejected boundary |
| --- | --- |
| Remove the missing-symbol guard at 149 | The stale retained unit reaches an invalid positional `[[` at 151. |
| Remove the optional future bound at 133 | A future NULL matrix result reaches the unary logical operation at 133 (`invalid argument type`). |
| Increment a reverse count by two at 188 | Repaired row/count equality fails. |
| Decrement a forward count by two at 222 | Repaired row/count equality fails. |
| Decrement deleted-column rows by two at 266 | Repaired row/count equality fails. |
| Initialize the self row TRUE at 569 | Self-row exclusion fails before the unsafe self-donor route can run. |
| Decrement a lazy row count by two at 774 | Repaired lazy row/count equality fails. |
| Give the delayed inverse an empty domain at 730 | Its consumed row position disagrees with the immutable survivor map. |

The controls establish sensitivity to these obligations. They are not
counterexamples to the unchanged source, nor are their early observer
failures claimed to be the eventual native error sites for all mutations.
The preceding portfolio counts exclude these extra control executions.

## 5. Reproduction and artifacts

From the repository root:

```sh
CNF_REVIEW_TAG=r36 Rscript attic/cnf_verify3/index_contract_review/observe_boundaries.R
CNF_REVIEW_TAG=r36 Rscript attic/cnf_verify3/index_contract_review/reduced_shapes.R

bash attic/cnf_verify3/review_semantics/run_r46.sh -e 'Sys.setenv(CNF_REVIEW_TAG="r46"); source("attic/cnf_verify3/index_contract_review/observe_boundaries.R")'
bash attic/cnf_verify3/review_semantics/run_r46.sh -e 'Sys.setenv(CNF_REVIEW_TAG="r46"); source("attic/cnf_verify3/index_contract_review/reduced_shapes.R")'
```

The `observations_*.log` files contain the full named counts and rejection
messages; `observations_*.rds` preserve those results, versions, source MD5,
parameters and first boundary fixtures. `reduced_shapes_*` retain the small
ordinary-R checks. The lexical CSV files give exact source positions.
No author's campaign was rerun or used as this review's independent test.
No production files, other research streams or git commits were changed.
