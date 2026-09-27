# An independent view of HLA through restricted domains

The cited primary paper distinguishes binary-donor HLA from unrestricted
asymmetric literal addition (ALA). Its Lemma 7 relates ALA tautologies to unit
propagation with every target literal assumed false. Thus this implementation's
general-donor loop is closer to ALA than the paper's narrower HLA terminology.
The paper also separates confluence, logical equivalence, and preservation of
unit propagation; these properties should not be conflated when interpreting
structurally different outputs. [Heule, Järvisalo and Biere, *Clause Elimination
Procedures for CNF Formulas*, 2010, §§4.1–5.2](https://fmv.jku.at/papers/HeuleJarvisaloBiere-LPAR10.pdf).

## Independent finite-domain formulation

For each symbol s, keep a set P_s of values still possible. Initially assume
the target clause T false, so `P_s = domain_s minus T_s`. Inspect every other
clause D. Its s literal is possible exactly when `D_s intersect P_s` is
nonempty. If no literal is possible, the assumptions contradict D. If exactly
one symbol s can satisfy D, restrict `P_s` to `P_s intersect D_s`. Repeat
until a contradiction or until no domain changes.

Each step is independently valid: with all other donor literals impossible,
every remaining satisfying valuation must satisfy its only possible literal.
Every proper step removes a value, so termination follows from finiteness.
A contradiction certifies that the other clauses imply T; removing T therefore
preserves all valuations. Failure to derive a contradiction proves nothing
about general entailment.

`domain_refutation.py` implements exactly this domain procedure. It does not
use virtual clause extensions, not-subset matrices, HLA donor counts, or the
production helper's scheduling. Every success contains donor-indexed domain
restriction steps and a final empty-possible-literals witness. A separate
replayer checks those literal-level certificates. Boolean SAT verifies every
residual redundancy reported by the experiment.

## Relating the two formulations

Let `C_s = domain_s minus P_s`. The condition that all donor literals outside s
are impossible is exactly `D_v subset C_v` for every v other than s. Restricting
P_s by D_s changes its complement to `C_s union (domain_s minus D_s)`, exactly
the virtual extension used in the production loop. A donor with no possible
literal is a subset of the virtual target, giving hidden subsumption.

A donor already used for a one-symbol restriction never needs another proper
restriction: subsequent domains only shrink, and its exceptional domain is
already inside that donor's range. It can later become wholly impossible,
which must still be checked. This independently explains the production
single-use optimization without relying on its boolean counters.

With exact live comparisons and no initially subsumed target, a complete HLA
loop must therefore agree with this domain refutation predicate for its donor
set. The initial-direct-subsumption qualification matters: production skips
count-zero candidates at HLA entry on the assumption earlier phases removed
them; the existing unit-equality gap violates part of that earlier saturation
expectation. The experiment records those separately.

## Why one target sweep can be enough for clause deletion

Suppose T has a domain-refutation proof using some donor set. Adding further
donors cannot invalidate the proof: every recorded restriction remains sound,
or a stronger restriction yields contradiction earlier. Conversely, removing
donors cannot create a new domain-refutation proof which was unavailable with
all original donors.

During the HLA phase, actual surviving clause ranges stay unchanged; only
whole clauses are removed. Thus if a target were redundant by this predicate
in the final formula, it was already redundant when visited with a superset
of its final donors. A complete target procedure should have removed it then.
This is a promising saturation theorem for the clause-deletion phase, despite
the proven lack of first-/second-order literal saturation in preceding phases.

The remaining implementation obligations are exact initial live comparisons,
complete selection of newly unit donors, and the handling of actual unit
clauses as context. These are being audited independently by the proof-state
agent; this document does not declare them established merely from the
algebraic correspondence.

## Calibration

`domain_refutation_calibration.py` checked 7,800 target/donor triples from all
three-clause subsets over three Boolean symbols. Every one of 1,836 derived
refutations passed both Boolean SAT entailment and all eight direct valuations.
One hundred independently constructed long chains also refuted their target,
with 1,384 recorded domain restrictions. The main probe applies the new
predicate to each surviving output clause across dense and structured inputs.
