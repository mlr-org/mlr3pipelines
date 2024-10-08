% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CnfFormula.R
\name{CnfFormula}
\alias{CnfFormula}
\alias{as.CnfFormula}
\title{CNF Formulas}
\usage{
CnfFormula(clauses)

as.CnfFormula(x)
}
\arguments{
\item{clauses}{(\code{list} of \code{\link{CnfClause}}) \cr
A list of \code{\link{CnfClause}} objects. The formula represents the conjunction of these clauses.}

\item{x}{(any) \cr
The object to be coerced to a \code{CnfFormula} by \code{as.CnfFormula}.
Only \code{logical(1)}, \code{\link{CnfAtom}}, \code{\link{CnfClause}}, and \code{CnfFormula} itself are currently supported.}
}
\value{
A new \code{CnfFormula} object.
}
\description{
A \code{CnfFormula} is a conjunction of \code{\link{CnfClause}} objects. It represents a statement
that is true if all of the clauses are true. These are for example of the form

\if{html}{\out{<div class="sourceCode r">}}\preformatted{  (X \%among\% "a" | Y \%among\% "d") & Z \%among\% "g"
}\if{html}{\out{</div>}}

\code{CnfFormula} objects can be constructed explicitly, using the \code{CnfFormula()} constructor,
or implicitly, by using the \code{&} operator on \code{\link{CnfAtom}}s, \code{\link{CnfClause}}s, or other \code{CnfFormula} objects.

To get individual clauses from a formula, \code{[[} should not be used; instead, use \code{as.list()}.
Note that the simplified form of a formula containing a tautology is the empty list.

Upon construction, the \code{CnfFormula} is simplified by using various heuristics.
This includes unit propagation, subsumption elimination, self/hidden subsumption elimination,
hidden tautology elimination, and resolution subsumption elimination (see examples).
Note that the order of clauses in a formula is not preserved.

Using \code{CnfFormula()} on lists that contain other \code{CnfFormula} objects will create
a formula that is the conjunction of all clauses in all formulas.
This may be somewhat more efficient than applying \code{&} many times in a row.

If a \code{CnfFormula} contains no clauses, or only \code{TRUE} clauses, it evaluates to \code{TRUE}.
If it contains at least one clause that is, by itself, always false, the formula evaluates to \code{FALSE}.
Not all contradictions between clauses are recognized, however.
These values can be converted to, and from, \code{logical(1)} values using \code{as.logical()}
and \code{as.CnfFormula()}.

\code{CnfFormula} objects can be negated using the \code{!} operator. Beware that this
may lead to an exponential blow-up in the number of clauses.

This is part of the CNF representation tooling, which is currently considered
experimental; it is for internal use.
}
\examples{
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e", "f"))
Z = CnfSymbol(u, "Z", c("g", "h", "i"))

frm = (X \%among\% c("a", "b") | Y \%among\% c("d", "e")) &
  Z \%among\% c("g", "h")
frm

# retrieve individual clauses
as.list(frm)

# Negation of a formula
# Note the parentheses, otherwise `!` would be applied to the first clause only.
!((X \%among\% c("a", "b") | Y \%among\% c("d", "e")) &
   Z \%among\% c("g", "h"))

## unit propagation
# The second clause can not be satisfied when X is "b", so "b" can be
# removed from the possibilities in the first clause.
(X \%among\% c("a", "b") | Y \%among\% c("d", "e")) &
  X \%among\% c("a", "c")

## subsumption elimination
# The first clause is a subset of the second clause; whenever the
# first clause is satisfied, the second clause is satisfied as well, so the
# second clause can be removed.
(X \%among\% "a" | Y \%among\% c("d", "e")) &
  (X \%among\% c("a", "b") | Y \%among\% c("d", "e") | Z \%among\% "g")

## self subsumption elimination
# If the first clause is satisfied but X is not "a", then Y must be "e".
# The `Y \%among\% "d"` part of the first clause can therefore be removed.
(X \%among\% c("a", "b") | Y \%among\% "d") &
  (X \%among\% "a" | Y \%among\% "e")

## resolution subsumption elimination
# The first two statements can only be satisfied if Y is either "d" or "e",
# since when X is "a" then Y must be "e", and when X is "b" then Y must be "d".
# The third statement is therefore implied by the first two, and can be
# removed.
(X \%among\% "a" | Y \%among\% "d") &
  (X \%among\% "b" | Y \%among\% "e") &
  (Y \%among\% c("d", "e"))

## hidden tautology elimination / hidden subsumption elimination
# When considering the first two clauses only, adding another atom
# `Z \%among\% "i"` to the first clause would not change the formula, since
# whenever Z is "i", the second clause would need to be satisfied in a way
# that would also satisfy the first clause, making this atom redundant
# ("hidden literal addition"). Considering the pairs of clause 1 and 3, and
# clauses 1 and 4, one could likewise add `Z \%among\% "g"` and
#' `Z \%among\% "h"`, respectively. This would reveal the first clausee to be
# a "hidden" tautology: it is equivalent to a clause containing the
# atom `Z \%among\% c("g", "h", "i")` == TRUE.
# Alternatively, one could perform "hidden" resolution subsumption using
# clause 4 after having added the atom `Z \%among\% c("g", "i")` to the first
# clause by using clauses 2 and 3.
(X \%among\% c("a", "b") | Y \%among\% c("d", "e")) &
  (X \%among\% "a" | Z \%among\% c("g", "h")) &
  (X \%among\% "b" | Z \%among\% c("h", "i")) &
  (Y \%among\% c("d", "e") | Z \%among\% c("g", "i"))

## Simple contradictions are recognized:
(X \%among\% "a") & (X \%among\% "b")
# Tautologies are preserved
(X \%among\% c("a", "b", "c")) & (Y \%among\% c("d", "e", "f"))

# But not all contradictions are recognized.
# Builtin heuristic CnfFormula preprocessing is not a SAT solver.
contradiction = (X \%among\% "a" | Y \%among\% "d") &
  (X \%among\% "b" | Y \%among\% "e") &
  (X \%among\% "c" | Y \%among\% "f")
contradiction

# Negation of a contradiction results in a tautology, which is recognized
# and simplified to TRUE. However, note that this operation (1) generally has
# exponential complexity in the number of terms and (2) is currently also not
# particularly well optimized
!contradiction
}
\seealso{
Other CNF representation objects: 
\code{\link{CnfAtom}()},
\code{\link{CnfClause}()},
\code{\link{CnfSymbol}()},
\code{\link{CnfUniverse}()}
}
\concept{CNF representation objects}
\keyword{internal}
