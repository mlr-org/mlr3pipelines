# A nonproductive call is a certificate of local-rule saturation

Independently reviewed source-level consequence of the static SSE2, HLA and
repeated-pass proofs. The input is canonical constructor-normalized finite
set storage in a fixed universe; each invocation returns normally. The
proper nonconstant representation has one unique valid symbol name per
entry, unique nonmissing domain values, and nonempty proper literal ranges.
Scalar TRUE/FALSE and the empty clause list are separate constant cases. The
rules are actual unit propagation and subsumption, direct subsumption, SSE1,
SSE2 under their full set premises, and domain-propagation refutation under
each target clause's negation. The claim is not arbitrary logical entailment
or SAT completeness.

## Claim

If one invocation makes no productive actual change to its input (ignoring
order but retaining clause multiplicity), its output is saturated under all
these rules. Consequently repeatedly applying the unchanged kernel until a
nonproductive pass yields a saturated result after finitely many normally
completing calls. The reviewed fiber or clause-count-only bounds apply to
the number of productive calls. This does not give a fixed constant number
of passes or an efficient implementation of complete scheduling.

## 1. No productive net change means no hidden actual change

Give each input clause its persistent ghost identity. All actual changes
remove values, remove symbols, or delete clauses. Units are existing clauses,
and a merge intersects an old unit while deleting the candidate. No actual
clause/value is added. Thus any productive intermediate operation lowers
the initial value-occurrence potential and cannot be undone later.

A nonproductive call therefore has no actual mutation anywhere. Its only
possible storage change is the initial stable clause-length sort. No later
callback creates a unit, shrinks a range, or eliminates a clause.

## 2. Initial units exclude every unit-related remainder

Initial units are merged before the nonunit stage. Two units on the same
symbol would be merged and one removed, contradicting nonproductivity.
For every existing unit, the initial nonunit scan has no subset cache and
cannot take the later equality-skip branch. A nonunit range outside the unit
would shrink; an equal or covering range would cause clause deletion. Thus
every final nonunit range on a unit symbol is a nonempty proper subset of
that unit, and no unit propagation or unit subsumption remains useful.

This step is why the known nested unit-equality omission cannot persist in
a nonproductive invocation. It does not assume that its skip is correct in
general or that raw comparisons are always exact during productive calls.

## 3. Static pair and second-order obligations are all visited

The surviving nonunits are unchanged for the entire call. Every distinct
pair is initialized once from its actual ranges in both directions. Because
no callback changes or removes an operand, each direct-subsumption or useful
count-one SSE1 pair is visited when initialized. Its useful operation would
make a productive change. Hence neither can remain.

Every count-two ordered pair is likewise unchanged from construction of the
manual second-order queue to its visit. It is present in that queue, remains
live, and is dispatched with the current two exceptional columns. Both pivot
orientations and all eligible other donors are considered. The independently
reviewed static candidate-coverage theorem then rules out a missed useful
SSE2 pattern containing a twoend donor.

All other abstract SSE2 arrangements reduce to subsumption, stronger SSE1,
or an HLA refutation by the relevant donors, according to that same static
set classification. Unit participants reduce to the already excluded unit
or earlier-rule cases. No dynamic notification premise is needed: by
hypothesis no actual range or participant ever changes.

## 4. HLA completes the remaining deletion obligations

The relevant pair matrices are raw-exact at the HLA boundary. The reviewed
single-use-donor argument makes each target's loop a complete domain-
propagation refutation check under its negation. No target is deleted during
this nonproductive invocation, so all original other clauses are available
when each is considered. Unit-HLA's lazy row argument applies, and the
strict unit containment from Section 2 excludes the known equality remainder.

Equivalently, the reviewed final-survivor classification says any remaining
refutable target would be directly subsumed by a surviving unit. Section 2
has already ruled that out. Thus no HLA/domain-refutation opportunity remains,
closing the final static SSE2 cases as well.

## 5. Finite repeated application

The reviewed descent argument bounds productive passes by a finite initial
potential. It also proves that after a first nonproductive pass, its sorted
output is an exact storage fixed point. The present argument additionally
identifies that output as fully saturated under the listed rules.

This excludes a permanently missed local rule at a fixed point of repeated
canonical simplification. It does not excuse incomplete first-call scheduling:
the explicit family needing n productive calls still rules out every fixed
number of repeated calls as a general repair. The exact opposed-four-cycle
Boolean contradiction also remains a fixed point, showing again that local
saturation is weaker than complete contradiction recognition.

The [independent source review](../fixed_graph_review/REVIEW.md) completes the
unit, static-pair, second-order, and HLA obligations. On each R version its
separate harness checked 21,429 fixed-point inputs, 20,601 productive calls,
21,429 nonproductive calls and exact next-call comparisons, and 20,267
nonconstant full local-rule audits. The finite checks support the source
argument; they do not replace it. This adds no production implementation or
tests that require unfixed first-call scheduling behavior.

Proper-range normalization is necessary. The manually supplied raw unit
`list(list(A=c("0","1")))` over domain `{0,1}` is returned unchanged by the
all-unit early exit, but is tautological and its negation is immediately
refutable. Ordinary construction makes it scalar TRUE before the kernel.
This raw input is a boundary control outside the stated premise.
