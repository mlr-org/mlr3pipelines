# Constructor and operator semantic closure on canonical CNF inputs

Independent source review, 2026-09-06. Production files are unchanged. The
experiments in this directory alter function objects in private R environments
only; they are diagnostics, not proposed production changes.

**Result.** On the precise representation and normal-return assumptions below,
the Formula methods for conjunction, disjunction, and negation preserve their
Boolean meaning and return canonical representations. In particular,

```
isTRUE(c(!F))  <=>  F has no satisfying valuation.
```

This is a completeness result for detecting an input contradiction by taking
its negation. It does **not** require the simplifier to recognize every
contradiction. Proper-clause representation and complete distribution suffice.
Public list constructors have additional success conditions: universe-free
constants and the known FALSE-flattening path prevent an unconditional claim
that arbitrary lists of otherwise canonical objects can be constructed.

## 1. Representation, semantics, and exact assumptions

Fix a finite symbol set S and an unmodified universe environment U. Each symbol
s has a finite, nonempty domain Omega_s containing unique ordinary character
values. A valuation independently selects one value in each domain. Thus the
semantic space is the full Cartesian product of the domains, including values
not mentioned in a formula. There are no implicit cross-symbol constraints.

A proper atom is membership s in A, where emptyset is a strict subset of A and
A is a strict subset of Omega_s. A proper clause C is a nonempty finite named
list of such ranges, with each symbol appearing once, denoting their
disjunction. A canonical formula is either a classed scalar Boolean constant
or a finite nonempty list of proper clauses, denoting their conjunction. Its
nonconstant clauses share the same registered universe. Duplicate whole
clauses and unreduced clauses are permitted: "canonical" describes the data
representation, not a unique or fully reduced normal form.

The assumptions applying throughout this review are:

1. The inputs obey this representation, or are the supported scalar logical
   constants/atoms/clauses explicitly admitted by a conversion. In particular,
   no missing names, duplicate symbol names, NA values, zero-length stored
   ranges, matrices, or forged classed selector results are admitted.
2. Membership, equality, and set operations use ordinary, total character
   semantics. Exotic mixed encodings that make R membership error are outside
   the theorem. Values and stored clauses are ordinary atomic vectors/lists,
   with no custom replacement methods or extra classes changing dispatch.
3. The universe and all input values remain fixed externally during the call.
   The universe's identity is significant for successful API combination;
   constants may carry U or NULL without changing their truth value.
4. Calls complete normally, with sufficient allocation, stack, and supported
   index/count capacity, without external interruption. A runtime error is
   not a returned Boolean result. The mathematical finite-loop argument does
   not imply execution within a fixed runtime stack limit.
5. The reviewed simplifier's normal-return semantic-preservation theorem holds
   under these same conditions. Its source obligations are assembled in
   [SEMANTIC_PRESERVATION_MAP.md](../independent_solver/SEMANTIC_PRESERVATION_MAP.md),
   with the independent callback and HLA reviews linked there. This proof does
   not replace that theorem with an inference from random testing.

The statements about Formula methods use direct S3 method invocation, or
ordinary supported operator dispatch. Before R 4.3, mixed-class binary Ops can
warn about incompatible methods and fall back; that execution path is not a
valid substitute for the Formula method. The experiments call those methods
directly where needed. Negation's internal reduction combines same-class
CnfFormula objects and does not have that mixed-class issue.

## 2. The proper-clause lemma

Let C be a proper clause. For every symbol it contains, choose a value outside
its range; properness ensures a choice exists. Choose arbitrary domain values
for all remaining symbols. Independence makes these choices one valuation,
and that valuation falsifies every atom of C. Therefore C is not a tautology.

Now take any nonconstant canonical formula F. Select just one of its clauses
C and use the preceding valuation. Since F is a conjunction containing C,
this valuation falsifies F, regardless of every other clause. Consequently:

```
a canonical formula is semantically TRUE iff its representation is TRUE.
```

This statement is asymmetric. A conjunction of individually proper clauses
can be contradictory, and FALSE is not the only canonical representation of
a contradiction. This asymmetry, rather than SAT completeness of any rewrite
rule, is the reason negation can expose every input contradiction.

The clause lemma also gives the exact normalization criterion for a
disjunction of unary ranges: the clause is tautological iff at least one
symbol's accumulated range equals its entire domain. Empty ranges contribute
FALSE and can be removed. If every range is empty the clause is FALSE.

## 3. The simplifier preserves proper representation

This is a smaller source obligation than semantic preservation, and it can be
checked directly. The physical assignments in `R/CnfFormula_simplify.R` are:

| Source operation | Representation consequence |
| --- | --- |
| Initial clause reordering, line 48 | Reorders already proper clauses only. |
| Unit merge, line 103 | Replaces a unit's range by a nonempty intersection; empty intersection returns the contradiction signal first. |
| Domain restriction, lines 146-167 | Intersects one range. An unchanged range stays proper; a nonempty changed range is a subset of the old proper range. An empty range is removed instead of being stored. |
| Symbol elimination, lines 236-245 | Deletes the named list member. An empty clause returns the contradiction signal before storing an empty clause. Otherwise the remaining names and ranges remain proper. |
| HLA, lines 663-784 | Expands a local virtual `clause`, never an entry in the physical `entries` list. The only physical effect is target deletion. |
| Return, lines 34-42, 491, 788 | Wraps the surviving clauses, or TRUE for no clauses, or scalar FALSE on a contradiction path. |

All other changes are to registries, elimination flags, comparisons, or
counts. Restricting a range by a donor union in SSE2 is still an intersection
with the old target range, so even a full-domain donor union cannot create a
full-domain physical range. None of these changes introduces repeated symbols
or repeated character values. Thus normally returned nonconstant output from
proper input has proper clauses, independently of any fixed-point claim.

## 4. Atom, clause, and formula constructors

`CnfAtom` at `R/CnfAtom.R:69` checks membership in the registered domain,
converts full coverage to TRUE and an empty selection to FALSE, and otherwise
stores `unique(values)`. On the stated character-valued domain this has exactly
membership semantics and returns the required representation.

`CnfClause` at `R/CnfClause.R:87` accumulates the disjunction. For a symbol
appearing in several atoms or clauses, the accumulated range is its set union.
A TRUE member or a full-domain accumulated range terminates with TRUE. FALSE
members contribute nothing. Empty accumulation returns FALSE. The clause
lemma proves that this catches all clause tautologies; it does not need
propositional or multivalued inference between clauses.

There is a precise universe boundary: the constructor chooses the first
member's owner, then checks owner identity **before** checking whether each
visited member is TRUE or FALSE. The owner check applies through the member
that first makes the accumulated disjunction tautological, inclusively;
members after that point are not visited. The upfront class assertion still
applies to the entire argument list. Thus, with a proper atom a owned by U:

```
CnfClause(list(as.CnfClause(FALSE), a))  # error: NULL versus U
CnfClause(list(a, as.CnfClause(FALSE))) # error: U versus NULL
CnfClause(list(a, as.CnfClause(TRUE)))  # error before recognizing TRUE
CnfClause(list(as.CnfClause(TRUE), a))  # TRUE; later owner check is bypassed
```

These errors prevent total constructor closure over canonical objects with
universe-free constants. For all visited inputs sharing the same owner,
the accumulation is a source-level proof of correct disjunction semantics.

`CnfFormula` at `R/CnfFormula.R:145` accumulates conjunction. It selects the
first member's owner but skips TRUE before checking owners and encounters
FALSE before checking owners. Proper CnfClause members append their unclassed
named lists to `entries`; proper CnfFormula members enter `other_entries`,
which is flattened by exactly one list level after the loop. On the successful
non-FALSE path, the kernel receives the union of all conjunct lists, after
tautologies are removed. Empty input denotes TRUE. Kernel soundness and
properness then give semantic and representation closure.

Two conditions are essential to describe the actual success boundary:

* Every nonconstant member before the first FALSE must share the first
  member's owner, even if that first member is a universe-free TRUE. Thus
  `CnfFormula(list(as.CnfFormula(TRUE), proper_clause))` errors, while the
  reversed order succeeds. TRUE members with different owners are themselves
  skipped without an owner check.
* If FALSE is encountered, no earlier **nonconstant CnfFormula member** may
  already be in `other_entries`. Otherwise `entries = FALSE` is followed by
  `c(FALSE, unlist(other_entries, recursive = FALSE))`. This is a malformed
  list containing the unnamed scalar FALSE as a clause. The kernel treats
  its length as a unit and errors when looking up its nonexistent symbol.
  All owners may be identical and this defect still occurs.

For example, if f is a nonconstant CnfFormula owned by U,

```
CnfFormula(list(f, as.CnfFormula(structure(FALSE, universe = U))))
```

errors on the reviewed source. FALSE before f succeeds; FALSE after only
CnfClause members succeeds. The malformed path is not a Boolean
counterexample on normal return, but it is a real failure of the promised
constructor operation. Negation below never uses this path.

The simple conversions are safe on the stated domain. `as.CnfFormula` of a
proper atom routes through a one-member CnfClause, and of a proper clause
through a one-member CnfFormula. These cannot encounter either list boundary
defect. Identity conversions preserve their objects; scalar logical
conversions only add the expected class and copy any owner attribute.

## 5. Formula conjunction and disjunction

The constant branches of `&.CnfFormula` (`R/CnfFormula.R:309`) implement
FALSE annihilation and TRUE identity after converting the returned operand
to CnfFormula. They run before checking the two nonconstant owners. The
nonconstant branch requires identical owners and concatenates the clause
lists with `c(e1, e2)`. Concatenation represents conjunction, and its proper
clause input meets the kernel's preconditions. This proves semantic and
representation closure for the Formula conjunction method.

For `|.CnfFormula` at line 323, the constant branches similarly implement TRUE
annihilation and FALSE identity while returning CnfFormula objects. Suppose
both converted operands are nonconstant, have the same owner, and are

```
F = conjunction_i C_i,    G = conjunction_j D_j.
```

Finite Boolean distributivity gives

```
F OR G = conjunction_(i,j) (C_i OR D_j).
```

One direct proof is pointwise: if either whole operand is true, every pair
clause is true. If both operands are false, some C_i and some D_j are false,
so that particular pair clause is false. This covers arbitrary finite domains
because it concerns truth values of the clauses, not a Boolean restriction
on their symbols.

The distribution loop computes each pair by unioning its ranges at each
symbol. An absent range is NULL, and `unique(c(left, NULL))` supplies the
left range. An existing right-only symbol remains unchanged. A full-domain
union makes the pair tautological and the `eliminated` vector discards that
pair; the clause lemma proves this criterion exact. Every retained range is
nonempty and proper, and names remain unique. Flattening the callback results
one level produces exactly the surviving pair clauses, or the empty list
when all pairs are tautologies. Applying the kernel gives the required
semantics and proper representation.

### The local R assignment premise is necessary

The potentially surprising statement is `e2[[i2]] = e2_clause` inside the
`lapply` callback. It does not change the enclosing function's e2. Every
callback invocation has a fresh evaluation frame. Ordinary complex assignment
reads an inherited object when needed, performs replacement, and binds the
result locally; `<<-` instead selects an enclosing binding. These are the
documented R evaluation and replacement rules, not a reliance on whether a
particular object happens to be physically duplicated by the runtime.
See the [R Language Definition, evaluation environment](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Evaluation-environment)
and [subset assignment](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Subset-assignment).

Let D denote the second formula in the enclosing frame, after the optional
operand swap. The per-row loop invariant is:

1. On entry to a callback for C_i, no local e2 binding exists; lookup yields D.
2. Before iteration j, local entries k < j contain their C_i-unions, while
   every entry k >= j equals D_k. Before the first assignment, the inherited
   D has the same property with an empty processed prefix.
3. Reading `e2[[j]]` therefore obtains D_j, and the nested symbol loop forms
   exactly C_i OR D_j. The complex assignment stores this at local position j.
4. The callback returns the local list with tautological pairs removed. Its
   local changes do not persist into the next callback's fresh frame.

Each position is processed once per row; earlier changed positions are not
used to compute later positions. No list node representing a caller operand
is observably mutated under ordinary R value semantics. The shared universe
is an environment, but the distribution code only reads it. Swapping local
operands changes neither this argument nor the Cartesian identity.

If the assignment were changed to `<<-`, step 4 would fail. Our executable
control uses four Boolean symbols with

```
F = (x = 0) AND (y = 0),  G = (z = 0) AND (w = 0).
```

The mutant leaks x = 0 into the second row, causing, for example,
`(x,y,z,w) = (0,1,1,1)` to satisfy its result although F OR G is false.
The original operands can still remain unchanged in that mutant, since its
erroneous mutation occurs in the operator's own frame. Testing only caller
operand immutability would therefore be insufficient.

## 6. Negation, including contradiction recognition

`!.CnfAtom` takes a domain complement for a proper atom, preserving a nonempty
proper range. Scalar constants are complemented directly. `!.CnfClause`
converts to a one-clause Formula and uses the following Formula proof.

`!.CnfFormula` at `R/CnfFormula.R:364` handles scalar constants directly. For
a proper nonconstant formula F with clauses C_1,...,C_m, it builds

```
N_i = conjunction_(s in C_i) (s in Omega_s \\ C_i[s]),
!F  = N_1 OR ... OR N_m.
```

Both identities are De Morgan's law with exact finite-domain complements.
There is at least one input clause and at least one symbol in each clause,
so neither `lapply` term construction nor `Reduce` is empty on this branch.

Every constructed complement is a proper unary CnfClause with the same owner
U. The argument to each `CnfFormula` is a list of these proper **clauses**,
never nested formulas or logical constants. Each symbol occurs once because
the original clause has unique symbol names. These unit conjunctions are
satisfiable: choose one value from every complement independently. The
constructor thus passes its owner checks, avoids FALSE flattening, and returns
the correct nonconstant conjunction. The subsequent reduction combines only
CnfFormula operands of owner U, with any intermediate TRUE handled by the
Formula OR constant branch. The OR proof inductively gives exact negation
semantics and proper-clause closure throughout.

Now suppose F is contradictory. Its returned negation is semantically TRUE;
by the proper-clause lemma, that return value must be literal TRUE. Conversely,
if the returned value is TRUE, sound negation semantics implies F has no
model. This proves the advertised iff without supposing the kernel recognizes
the contradiction in F or any intermediate conjunctive formula.

Indeed, the result remains true with a kernel that does **no reductions** and
merely wraps the distributed proper clauses, mapping an empty conjunction to
TRUE. Complete distribution plus exact pair-tautology filtering already has
the required semantics and proper representation. The identity-kernel
experiment below tests that stronger separation directly.

The implication is useful in both directions. If !F is nonconstant, falsifying
any one of its proper clauses constructs a satisfying valuation of F. If F is
contradictory but stored nonconstant, !F is TRUE and applying negation again
gives literal FALSE. There is no corresponding claim that ordinary one-pass
construction or simplification alone detects every contradiction.

## 7. Calibration and the stale documentation example

The example at `R/CnfFormula.R:130-140` labels this ternary formula an
unrecognized contradiction:

```
(X = a OR Y = d) AND (X = b OR Y = e) AND (X = c OR Y = f),
Omega_X = {a,b,c}, Omega_Y = {d,e,f}.
```

The current source already returns literal FALSE. That particular illustration
is stale, although the surrounding statement that the simplifier is not a
complete SAT procedure is correct. The six-clause Boolean pairwise-inequality
triangle is also recognized FALSE and is not a suitable unrecognized control.

For a real control, the structural-class stream supplied two opposed
four-edge implication cycles sharing x. This review copied the clauses
explicitly and independently evaluated all 128 valuations:

```
(x OR a1) AND (!a1 OR a2) AND (!a2 OR a3) AND (!a3 OR x)
AND
(!x OR b1) AND (!b1 OR b2) AND (!b2 OR b3) AND (!b3 OR !x).
```

The first group forces x: under x = FALSE, the chain forces a1,a2,a3 TRUE
and its last clause forces x TRUE. The second group similarly forces !x.
Hence their conjunction is contradictory. The truth table also verifies
that deleting any individual clause yields at least one model. Production
preserves all eight clauses unchanged, but !F is TRUE and !!F is FALSE.
The artifact from the other stream is
[opposed_cycles.json](../structural_classes/opposed_cycles.json), record
`cycle_sizes = [4,4]`; this review does not rely on its stored oracle result.

## 8. Executable evidence and its limits

Run from the repository root:

```sh
python3 attic/cnf_verify3/operator_proof/run_all.py
python3 attic/cnf_verify3/operator_proof/run_all.py --r46
```

The second command uses the already-running `cnf-review-r46` container. Each
script runs in a separate R process. Exact output is saved in the matching
`*_r36.log` and `*_r46.log` files. Both R 3.6.3 and R 4.6.1 passed, with the
same counts and outcomes:

| Script | Checks and observations per runtime |
| --- | --- |
| `constructor_boundaries.R` | All representative sequences through length 3: 820 Clause lists, 448 successful with correct truth and 372 expected owner errors; 585 Formula lists, 444 successful with correct truth, 96 expected owner errors, and 45 FALSE-flattening errors. Another 288 direct Formula binary calls gave 276 correct canonical returns and 12 expected nonconstant-owner errors. |
| `distribution_audit.R` | 795 nonconstant operand pairs, 1,428 callback rows, 4,428 Cartesian pair clauses, 3,262 retained and 1,166 tautological pairs; exact unsimplified clause-bag comparison, 60,878 valuations, and 279 operand swaps. Every row starts without local e2 and ends with a local e2; its enclosing original remains unchanged. Production and instrumented output agree. The cumulative-parent mutation is rejected with three disagreeing valuations. |
| `negation_closure.R` | 603 canonical formulas, 45,264 valuations, 58 contradictions including 28 represented nonconstant. Both production negation and the identity-kernel version give the exact complement and return TRUE iff contradictory. Among 544 nonconstant production negations, 1,088 explicit clause-falsifying witnesses across the two variants satisfy the input. The identity kernel was called 2,499 times, with at most 110 clauses in one call. |
| `negation_calibration.R` | Independent truth-table and irredundancy checks for the eight-clause control; current output and both negations; replay of the stale ternary example and recognized Boolean triangle. |

The random negation inputs alternate ordinary constructor outputs and proper
raw list representations which need not be reduced. Thus the count of
nonconstant contradictions includes deliberately unreduced inputs; the
eight-clause opposed-cycle control separately demonstrates a contradiction
left nonconstant by the production constructor itself. Domains in the random
experiments have two through five values, so neither test nor proof silently
specializes to binary domains.

The instrumentation checks the R-local-binding premise and exact Cartesian
clauses, not only final truth values. The identity-kernel experiment copies
production constructor and operator bodies in memory; it replaces only the
kernel and makes the reduction select its private OR copy directly. It
changes no global S3 registration or production function. These finite
experiments support the source derivation; they do not by themselves prove
the theorem for every input size.

## 9. What this theorem does not promise

The list-constructor failures above remain real API defects. There is also a
result-class boundary in `|.CnfClause`: when a raw logical TRUE is the left
operand, the identity branch can return it without converting to CnfClause.
For example `TRUE | proper_clause` has correct truth but returns raw logical
TRUE, and putting that result into `CnfFormula(list(...))` fails its class
assertion. This is separately reproduced here by direct method invocation;
the broader R 4.6 operator-contract experiment in
[operator_contracts_r46.log](../representation/operator_contracts_r46.log)
records the three representative failures. Formula methods themselves perform
the needed conversions, so the Formula closure proof does not inherit this
class failure. No claim is made that every supported selector or `as.list`
path preserves the canonical input conditions.

The output need not be a fixed point under SSE1, SSE2, subsumption, or a second
call to the simplifier, and no unique normal form is asserted. Those
incompleteness results are compatible with exact Boolean operator semantics.

Distribution materializes up to m*n clause pairs for a binary OR. Negating
clauses of widths k_1,...,k_m can produce up to the product of those widths
before tautology removal and simplification. This can be exponential in input
length. The recursive simplifier also has order-dependent runtime stack limits,
including non-unit paths untouched by the queue diagnostic; see
[unit_queue/README.md](../unit_queue/README.md). Therefore normal return and
adequate resources remain essential conditions, even for an operation that
is complete as a mathematical decision procedure.

Reviewed production SHA-256 values:

```
d03da14a7bce4989477efa09303e099a3d5b7a8abb3be3526ee850161a59dce6  R/CnfAtom.R
40d021025c290a5ad6522fadcda166ff55d83bdd1d27eb0f4fd1fbc6222f12f8  R/CnfClause.R
e94aabfb277cf3bc2c951a09571658fd7dba0b14bce484f8c7d642e8d7e41f60  R/CnfFormula.R
7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc  R/CnfFormula_simplify.R
```
