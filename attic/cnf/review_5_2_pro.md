The main change I would make is this:

**stop trying to get most of the extra power from clause elimination alone, and add a small bounded implied-clause learning phase.**

That matters because your objects are not merely SAT instances; they are semantic formulas you want to keep as formulas. In SAT preprocessing there is a sharp distinction between **logical-equivalence-preserving** transformations (strengthening, subsumption, vivification/distillation) and **equisatisfiability-only** transformations such as pure-literal unit addition, equivalent-literal substitution, bounded variable elimination, blocked-clause elimination variants, and covered-clause techniques. For your use case, the first group fits much better by default. ([CCA Informatik Freiburg][1])

A second reason is that elimination-only passes can reduce propagation strength. In the literature, hidden subsumption elimination is logically equivalent but **not** BCP-preserving. Modern SAT preprocessors therefore often get more leverage by learning a few short implied clauses—via failed-literal probing, hyper-binary resolution, vivification—than by adding ever more elaborate deletion criteria. ([CCA Informatik Freiburg][2])

## What I would add first

### 1) Bounded finite-domain resolution on one shared symbol

This is the most natural missing rule in your representation.

If you have

* `C1 = (X %among% A) | Tail1`
* `C2 = (X %among% B) | Tail2`

then the formula implies

* `R = (X %among% intersect(A, B)) | Tail1 | Tail2`

with the usual “same symbol inside one clause means union of ranges” rule applied inside `R`.

Why this is sound: if `Tail1` and `Tail2` are both false, then satisfying `C1` and `C2` requires `X ∈ A` and `X ∈ B`, hence `X ∈ A ∩ B`.

Two useful special cases:

* If `A ∩ B` is empty, `X` disappears from the resolvent.
* If `Tail1` and `Tail2` share a symbol, the resolvent can still shrink because the shared symbol is merged.

Example in your syntax:

* `(X %among% "a" | Y %among% "d")`
* `(X %among% "b" | Y %among% "e")`

implies

* `Y %among% c("d", "e")`

This is exactly your documented “resolution subsumption elimination” example, but expressed directly as a derived clause instead of as a specialized second-order side procedure.

I would **not** generate all such resolvents. I would generate them only when at least one of these holds:

* the resolvent is a unit or binary clause,
* the resolvent is strictly shorter than both parents,
* the resolvent subsumes an existing clause,
* the shared symbol’s range shrinks substantially, for example `length(A ∩ B) << min(length(A), length(B))`.

This is the finite-domain analogue of the SAT practice of learning only short implied clauses; unrestricted hyper-binary-resolution-style closure can help a lot, but if run to completion it can also blow up. ([CCA Informatik Freiburg][1])

**Practical recommendation:** this rule is probably a better replacement for most of your current `second_order_*` machinery than trying to make that machinery even richer.

---

### 2) Selective vivification / asymmetric literal elimination

After the bounded-resolvent pass, the next strongest exact simplifier is **clause vivification**.

Operationally, for a clause `a1 | a2 | ... | ak`, you negate literals one by one, run **only unit propagation**, and look for:

* a conflict → the clause can be shortened to the current prefix,
* propagation forcing the negation of another clause literal → that literal is redundant and can be removed.

In your setting, negating `X %among% A` is just a one-symbol clause restricting `X` to the complement of `A`, so the adaptation is straightforward.

This is stronger and more general than the current hidden-literal logic, and in SAT practice vivification has been effective enough to be used in top solvers—but it is normally scheduled **selectively**, not on every clause every time. ([CCA Informatik Freiburg][1])

I would run vivification only on clauses that are:

* recently changed,
* length 3 to 8, or some similar bounded range,
* inside small connected components,
* or on clauses that became candidates after the bounded-resolution pass.

**Important implementation point:** factor out a separate **propagation kernel** first. Do not call the full current `simplify_cnf()` recursively for vivification; that would be much too expensive and conceptually muddled.

---

### 3) If HLA is expensive, replace it with a cheaper binary-only variant

Your current final HLA/HSE-style pass is the expensive general form. If benchmarks show it dominates runtime, I would not make it more general. I would either:

* demote it to an “aggressive” mode, or
* replace it with a **binary-clause implication-graph** pass implementing binary-only hidden/asymmetric literal elimination.

That is a standard trade-off: hidden literal elimination is the restricted variant of asymmetric literal elimination that propagates only over binary clauses, which is much cheaper. ([CCA Informatik Freiburg][1])

In your representation, every binary clause `(A | B)` gives implications `!A -> B` and `!B -> A`, so there is a clean finite-domain version of this idea.

---

### 4) Limited failed-value probing

If domains are usually small, this is worth adding after the previous two.

For each symbol value `v`, temporarily assume `X = v` (or equivalently `X %among% v`) and run unit propagation:

* if that conflicts, then `v` is impossible and you can globally remove it from `X`’s domain;
* dually, if assuming `X != v` conflicts, then `X = v` is forced.

This is the finite-domain version of failed-literal probing. It is strictly stronger than plain unit propagation, but more expensive, so it should be restricted to small domains / small occurrence counts / recently changed components. Failed-literal probing is a standard exact strengthening rule, but it must be scheduled carefully. ([CCA Informatik Freiburg][1])

## What I would probably *not* add by default

I would **not** add these unless you are willing to move from equivalence to equisatisfiability plus reconstruction:

* blocked clause elimination / HBCE / ABCE,
* covered clause elimination,
* bounded variable elimination,
* equivalent-literal substitution,
* pure-literal assignment as added units.

These are all useful in SAT solving, but they change the meaning of the formula object unless you maintain reconstruction information. Bounded variable elimination is the classic “big hammer”, and in SAT it is often described as the most important preprocessing technique, but it does not preserve logical equivalence. ([CCA Informatik Freiburg][1])

## What I would change for speed

This part is at least as important as new rules.

### 1) Replace character sets with integer ids + bitsets

Most of your hot operations are:

* subset tests,
* intersection,
* union,
* complement,
* symbol lookup by name.

Right now those are done with character vectors, `%in%`, `match()`, `unique(c(...))`, `setdiff()`, and environment lookups. That is expensive in R.

A much better internal representation is:

* each symbol gets an integer id,
* each domain value gets a bit position,
* each atom range is a bitset,
* each clause is a small struct of parallel vectors `(sym_ids, masks)`.

Then:

* union/intersection/complement become bit operations,
* subset becomes `(mask1 & ~mask2) == 0`,
* tautology/contradiction checks become mask comparisons,
* negation is cheap.

Keep the S3 API if you want, but translate to a dense internal form at the boundary.

### 2) Replace the full pairwise matrix with occurrence-driven candidate generation

The biggest structural cost in `simplify_cnf()` is the global `is_not_subset_of` / `not_subset_count` machinery. It is clever, but it is large, stateful, and quadratic in the number of surviving clauses before you even get to second-order logic.

For subsumption / self-subsumption, use the standard SAT idea instead:

* index each clause by one **rarest symbol**,
* when a clause changes, only compare it against clauses in that symbol’s occurrence list,
* use a cheap signature to reject impossible candidates before exact checking.

Forward-subsumption schemes based on watched rarest literals/symbols and signatures are much more scalable than broad all-pairs checking. ([CCA Informatik Freiburg][1])

### 3) Decompose into connected components before heavy passes

Build the symbol co-occurrence graph once per formula and simplify each connected component independently. This is exact, cheap, and directly reduces the size of the candidate sets for every expensive pass. Connected-component decomposition is a standard and often useful preprocessing split. ([CCA Informatik Freiburg][1])

## Code-level weaknesses and bugs

These are the concrete issues I would fix even before adding more rules.

* **Mixed representations are hurting you.** `CnfAtom`, `CnfClause`, and `CnfFormula` are sometimes logical scalars and sometimes lists. That forces `isTRUE`/`isFALSE`/`unclass` branches everywhere and makes invariants hard to state. I would strongly prefer one uniform internal form plus explicit flags.

* **`as.list.CnfClause()` is broken for tautologies.** In the `TRUE` branch it returns `as.CnfAtom(x)`, but `x` is a `CnfClause`, and there is no `as.CnfAtom.CnfClause` method. S3 dispatch therefore falls through to `as.CnfAtom.default()` and errors.

* **`CnfClause()` and `CnfFormula()` infer the universe from the first element.** If the first element is a logical `TRUE`/`FALSE` wrapper with `universe = NULL`, and a later element belongs to a real universe, you spuriously get “All symbols must be in the same universe.” Constructors should pick the first **non-logical** element, or allow constants to adopt the common universe.

* **`as.list.CnfClause()` does not match its documentation.** The docs discuss named lists and commuting with name-based subsetting, but the implementation returns an unnamed list for the non-tautological case.

* **`CnfSymbol()` should reject duplicated domain values.** The entire algebra assumes domains are sets.

* **`simplify_cnf()` is too monolithic.** It mixes:

  * unit propagation,
  * clause elimination,
  * data-structure maintenance,
  * second-order reasoning,
  * HLA/HSE-like logic.
    Split it into explicit passes with explicit work queues.

* **The tri-state return convention is fragile.** Several helpers use `FALSE = no change`, `NULL = eliminated/turned unit`, `TRUE = contradiction`. That is easy to misuse. Use an explicit enum or small status object.

* **Too much hidden temporal coupling.** Several helpers rely on facts like “if `is_not_subset_of` is non-null then `meta_idx_outer` must exist” or “this function may be called only after removing the clause from the symbol registry”. That is a maintainability warning.

* **Hot loops do too many string and name operations.** `names()`, `match()`, `[[` by string, and environment lookups are everywhere in the simplifier. With integer ids, many of those disappear.

* **No semantic test oracle is visible.** Because the logic is finite-domain, you can brute-force small random universes and verify exact equivalence after every simplification. For this code, that is essential.

## If I had to prioritize only three changes

1. **Internal rewrite:** integer ids + bitsets + occurrence lists.
2. **Algorithmic change:** bounded finite-domain resolvent generation of short implied clauses.
3. **Rule substitution:** replace most of `second_order_*` and possibly the current full HLA default path with selective vivification plus cheaper binary-only hidden/asymmetric elimination.

That combination would likely make the system both **simpler** and **stronger**.

[1]: https://cca.informatik.uni-freiburg.de/papers/BiereJarvisaloKiesl-SAT-Handbook-2021-Preprocessing-Chapter-Manuscript.pdf "https://cca.informatik.uni-freiburg.de/papers/BiereJarvisaloKiesl-SAT-Handbook-2021-Preprocessing-Chapter-Manuscript.pdf"
[2]: https://cca.informatik.uni-freiburg.de/papers/HeuleJarvisaloBiere-LPAR10.pdf "https://cca.informatik.uni-freiburg.de/papers/HeuleJarvisaloBiere-LPAR10.pdf"

