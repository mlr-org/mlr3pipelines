## 1) Equivalence vs. equisatisfiability: a constraint that changes the “right” rules

If the object is a *logical function* over your symbols (not merely a satisfiability problem), then every simplification must preserve **logical equivalence**: the set of satisfying assignments must stay identical.

Under that constraint, several popular SAT-preprocessing rules are **not** admissible (pure-literal elimination, variable elimination by DP, blocked clause elimination, covered clause elimination, etc.), because they can change the model set even if they preserve satisfiability.

Your code already contains rules that are typically equivalence-preserving:

* **Unit propagation** (in your multi-valued “domain restriction” sense).
* **Clause subsumption elimination** (remove a clause implied by a stronger clause).
* **Eliminating tautological clauses** (TRUE clauses) and contradictions (FALSE clauses) is fine.

However, the part labeled **(Asymmetric) Hidden Literal Addition + hidden tautology / hidden subsumption elimination** is a red flag *under an equivalence requirement*. In SAT literature these are primarily **redundancy properties for satisfiability**, not necessarily logical consequence checks. Removing a clause because it becomes a hidden tautology after HLA can, in general, change the function (introduce new models). If equivalence is truly required, the safe version is:

> Use HLA (or any heuristic) only to *propose* redundancy candidates, but **only delete/strengthen** when you can prove the new constraint is a **logical consequence of the rest**.

The most practical “next step” is therefore to add a lightweight implication checker (see §2.1) and put the more aggressive rules behind it.

---

## 2) Next steps that improve simplification power without typical runtime blow-ups

### 2.1 Add a *certificate-like* redundancy test (RUP-style with your propagation)

A single mechanism unlocks several stronger but still equivalence-safe simplifications:

**RUP / reverse unit propagation check for clause implication**

To remove a clause (C) from (F), it suffices to show:
[
F \setminus {C} \models C.
]
A cheap sufficient test is:
[
(F \setminus {C}) \wedge \neg C ;; \text{unit-propagates to contradiction.}
]

In your representation, (\neg C) is a conjunction of unit restrictions “symbol ∈ complement(range-in-C)”. If your propagation engine can be run on a copy with temporary unit restrictions, you can prove implication without a full SAT solver.

This enables, equivalence-safely:

* **Clause deletion** (when implied by the rest).
* **Clause strengthening** (remove a literal / shrink a range) by proving the strengthened clause is implied by the rest.
* **Replacing HLA-based hidden tautology/subsumption elimination** with: “HLA proposes deletions; deletion only if RUP succeeds”.

This is usually the biggest *power-per-runtime* improvement: you get more correct deletions/strengthenings than purely syntactic rules, but most checks are still fast because they are just propagation.

### 2.2 Clause vivification (equivalence-safe when guarded by implication)

**Vivification**: try to strengthen a clause (C) by removing symbols or shrinking ranges, one at a time, using implication checks.

For a clause (C) and a candidate strengthened clause (C') (e.g. remove a symbol from (C), or shrink one symbol’s value-set), you can accept the strengthening if:
[
F \setminus {C} \models C'.
]
Again, use the propagation-based contradiction test on ((F \setminus {C}) \wedge \neg C').

Heuristic to keep runtime bounded:

* Only vivify clauses above a length threshold (or only newly added clauses).
* Stop after a small number of successful strengthenings per clause.
* Restrict to “high-value” candidates: symbols with large ranges, or symbols that occur frequently.

This tends to produce real simplifications in CNF size *without* global (O(n^2)) work.

### 2.3 Add **complement-resolution** in the multi-valued sense (bounded)

A sound, resolution-like rule in your literal language is the direct analogue of Boolean resolution:

If you have two clauses

* ( (X \in S) ,\lor, A )
* ( (X \in U\setminus S) ,\lor, B )

then the **resolvent**

* ( A \lor B )
  is a logical consequence of the formula.

In your representation, “(A \lor B)” is simply the disjunction of the remaining symbol-ranges (union per symbol).

How to keep runtime reasonable:

* Only resolve when the resolvent is **short** (e.g. size ≤ 2 or ≤ 3).
* Only keep a resolvent if it **subsumes** an existing clause (so it will delete something), or if it immediately makes another clause strengthenable by your existing subsumption machinery.
* Index candidates by `(symbol, range)` and look up exact complements efficiently (see §3.1).

This is a clean “resolution rule” that fits your data model and can yield improvements comparable to what SAT preprocessors get from bounded resolution, while still being equivalence-safe if you only *add implied resolvents* and then delete clauses that are implied by the added resolvent (or just let subsumption clean up).

### 2.4 Failed-value / failed-literal probing (optional, with strict cutoffs)

For a symbol (X) and a value (v), temporarily assume (X=v) (or (X \in S)) as a unit restriction and propagate. If you reach contradiction, then the formula implies (X \neq v) (or (X \notin S)), so you can permanently remove (v) from the domain restriction for (X) (i.e. add a unit restriction (X \in U\setminus{v})).

This is equivalence-safe (you are adding an implied constraint), but can be expensive. Use it only when:

* domains are small, or
* formula is small, or
* you have a strict budget (e.g. probe at most K values total).

---

## 3) Efficiency improvements that likely matter more than more rules

### 3.1 Stop using `character` set operations in the inner loops

Right now almost every “subset/intersect/union/complement” uses `%in%` over character vectors. That is often the main cost driver.

A typical next step is to represent each symbol’s domain as integers `1..d` and each range as one of:

* **Sorted integer vectors** (cheap subset checks using two-pointer merge; cheap intersection).
* **Bitsets** (fastest union/intersect/complement and subset checks, especially if domains are up to a few hundred).

This one change typically reduces runtime and makes bounded resolution/vivification feasible.

### 3.2 Canonicalize domains and ranges

For correctness and speed, enforce at symbol creation:

* `domain <- unique(domain)` and preferably fixed ordering.
* Store `domain_len` and a name→index map.

Then:

* “tautology check” becomes `length(range) == domain_len` (after canonicalization).
* “subset check” becomes bit/subset arithmetic rather than `%in%`.

### 3.3 Avoid global (O(n^2)) bookkeeping unless you know formulas stay small

`simplify_cnf()` builds and incrementally maintains an `n×n` structure (`not_subset_count` plus `is_not_subset_of` matrices). This is powerful but can dominate runtime and memory as `n` grows.

If formulas can grow beyond “small”, consider switching to:

* a **symbol-indexed candidate generation** approach for subsumption/self-subsumption (only compare clauses that share symbols), and/or
* phased passes: unit propagation → subsumption via hashing/signatures → bounded local strengthening, rather than maintaining a full pairwise state.

---

## 4) Concrete code issues / weaknesses

### 4.1 `as.list.CnfClause()` for TRUE clauses appears broken

```r
as.list.CnfClause = function(x, ...) {
  x_bare = unclass(x)
  if (isFALSE(x_bare)) return(list())
  if (isTRUE(x_bare)) return(as.CnfAtom(x))  # <-- likely dispatch error
  ...
}
```

A TRUE `CnfClause` has class `"CnfClause"` (not `"logical"`), so `as.CnfAtom(x)` will dispatch to `as.CnfAtom.default` and error. If you want a “tautology atom”, construct it directly, e.g. `structure(TRUE, universe=attr(x,"universe"), class="CnfAtom")`, or add `as.CnfAtom.CnfClause` handling TRUE/FALSE clauses explicitly.

### 4.2 `CnfUniverse` is an environment with a non-environment class

You create it as:

```r
CnfUniverse = function() structure(new.env(parent = emptyenv()), class = "CnfUniverse")
```

If any base methods rely on the `"environment"` class in S3 dispatch (e.g. `[[`, `names`, etc.), this can be fragile. A safer pattern is:

* `class = c("CnfUniverse", "environment")`, **or**
* explicitly define `[[.CnfUniverse` and other methods you intend to support and avoid relying on inherited environment methods.

Given your examples include `u[["Y"]]`, it is worth making this robust.

### 4.3 Several “length equality implies set equality/subset” checks rely on uniqueness

For example, in `apply_domain_restriction()`:

```r
if (length(clause[[symbol_idx]]) == length(restringent)) {
  eliminate_clause_update_sr(clause_idx)
  return(NULL)
}
```

This is only safe if both sides are unique sets and `clause[[symbol_idx]]` is known to be a superset of `restringent` after intersection. Enforce uniqueness/canonicalization of domains/ranges to make these checks correct and fast.

### 4.4 Equivalence risk: hidden tautology / hidden subsumption elimination

As noted, the HLA section deletes clauses based on hidden tautology/subsumption criteria. If strict equivalence is required, this should either be removed or guarded by an implication test (RUP-style propagation) before deletion. Otherwise, the system can change the represented function.

### 4.5 Maintainability: `simplify_cnf()` is extremely stateful

The event-driven design plus `<<-` updates and intertwined invariants makes it hard to validate and extend safely. If you add more rules, the risk of subtle invariant breakage rises quickly. Two practical mitigations:

* Introduce a **single internal “state” object** (environment/R6) and pass it explicitly to helpers rather than relying on free variables + `<<-`.
* Add property-based tests that randomly generate small universes/formulas and verify equivalence by brute-force enumeration (feasible for small domains) for every transformation.

---

## Summary

The most impactful “next step” is to add a **sound implication checker based on propagation** (RUP-style), and then use it to (i) make clause elimination/strengthening **equivalence-safe**, and (ii) implement **clause vivification** and **bounded complement-resolution** without risking incorrect model changes. In parallel, switching from `character`-based set operations to integer/bitset ranges is likely to yield the largest runtime improvement and will make additional rules affordable.

If you want, I can sketch a concrete propagation/implication-check API that fits your current objects (including how to represent temporary assumptions as unit-domain restrictions without mutating the original formula).

