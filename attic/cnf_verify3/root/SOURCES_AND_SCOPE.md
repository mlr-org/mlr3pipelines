# Primary references and interpretation boundaries

Consulted 2026-09-06. The mathematical arguments in campaign 3 are written
from this implementation and independently checked. The references below
clarify terminology and R behavior; they do not establish that this code
implements every condition correctly.

## Logical simplification terminology

Heule, Järvisalo and Biere, *Clause Elimination Procedures for CNF Formulas*,
LPAR 2010, pp. 357–371, DOI 10.1007/978-3-642-16242-8_26.
[Author-hosted paper](https://www.cs.helsinki.fi/matti.jarvisalo/papers/heule-jarvisalo-biere.lpar10.pdf).

The paper distinguishes logical equivalence, satisfiability preservation,
propagation preservation, and confluence. Its asymmetric literal addition
allows general donor clauses, whereas its restricted hidden literal addition
uses binary donors. The repository calls its more general procedure HLA and
acknowledges this distinction in its introductory comment. Lack of confluence
means different complete reduction orders can give different results; it
does not explain a remaining application of the same local rule after a
purported saturation pass. Also, preserving truth does not automatically
preserve all propagation consequences. Our core semantic theorem claims
truth preservation, with its explicit input and runtime assumptions.

## Why matrix selectors are a real normalization boundary

R's [official `unique` documentation](https://www.stat.ethz.ch/R-manual/R-patched/library/base/html/unique.html)
specifies different default behavior for vectors and matrices: matrix
deduplication compares rows and retains dimensions. Consequently a one-row
matrix containing two identical indices remains two elements after `unique`.
The simplifier's clause selector removes an explicit class with `unclass`,
which leaves dimensions. It then performs matrix-aware `unique` before list
subsetting. The duplicate symbol entries are produced by the public method,
without fabricating a CNF class or mutating a stored object.

R's [official extraction documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Extract.html)
describes list extraction as selecting a list of indexed elements. The
ordinary vector selection and matrix selection in our example denote the
same repeated input atom. Repeating an identical disjunct changes no Boolean
meaning. The output discrepancy therefore has an independent logical basis
even though a CNF implementation should normalize or reject the dimensional
selector before it reaches the simplifier.

`reproduce_selector_semantics.R` independently constructs the four clauses
and evaluates all nine assignments without the agents' helper code. For
`X=c, Y=a`, all input disjuncts are directly evaluated and the original
formula is false, but the returned canonical formula is true. Replacing the
matrix selector with its vector entries or reconstructing its clause restores
the correct FALSE result. Both are diagnostic evidence; production is not
changed by this investigation.

## Semilattices and domain propagation

Bodirsky, Macpherson and Thapper, *Constraint Satisfaction Tractability from
Semi-lattice Operations on Infinite Sets*, [arXiv:1111.6616](https://arxiv.org/abs/1111.6616).
Section 2 recalls the finite-domain connection between semilattice operations
and arc consistency. This is background for the independent
[source-prefix completeness argument](SEMILATTICE_COMPLETENESS.md), not evidence
that this particular implementation has the required propagation behavior.
The latter correspondence is proved directly from the pre-matrix source and
checked against a separate domain fixed-point calculation. No mathematical
novelty is claimed for the established algebraic tractability principle.
