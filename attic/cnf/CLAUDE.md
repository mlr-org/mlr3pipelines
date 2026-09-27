# Cnf*.R Bug Hunting Diary

## Bugs Found

### Bug #1: `as.list.CnfClause` crashes on TRUE clauses
- **File**: `R/CnfClause.R` line 194
- **Issue**: `as.list.CnfClause` for TRUE clause calls `as.CnfAtom(x)` where x has class "CnfClause". No `as.CnfAtom.CnfClause` method exists, so S3 dispatch falls through to `as.CnfAtom.default` which errors with "Cannot convert object to CnfAtom."
- **Secondary issue**: Even if the dispatch worked, it returns a single atom instead of `list(atom)`, which is inconsistent with the return type for FALSE (returns `list()`) and normal clauses (returns `list(atom1, atom2, ...)`).
- **Impact**: Also affects `lapply()` over TRUE CnfClause since R's lapply internally calls as.list.
- **Fix**: Replace `return(as.CnfAtom(x))` with `return(list(as.CnfAtom(TRUE)))` (using bare TRUE, not x which has class CnfClause).
- **Found in**: experiment 02

### Bug #2: `u[["NonExistent"]]` returns NULL instead of erroring
- **File**: `R/CnfUniverse.R` - missing `[[.CnfUniverse` method
- **Issue**: The `$` accessor has proper error checking (`stopf("Variable '%s' does not exist in the universe.")`), but `[[` uses default environment `[[` which silently returns NULL for missing names. Documentation says `[[` retrieves domain.
- **Impact**: Silent NULL propagation could cause confusing downstream errors.
- **Fix**: Add a `[[.CnfUniverse` method that delegates to `$` or adds its own validation.
- **Found in**: experiment 09

### Bug #3: `CnfFormula()` fails when first element is TRUE clause with NULL universe
- **File**: `R/CnfFormula.R` line 151
- **Issue**: `CnfFormula(list(as.CnfClause(TRUE), normal_clause))` throws "All clauses must be in the same universe." because `as.CnfClause(TRUE)` has `universe = NULL`. The constructor extracts universe from `clauses[[1]]`, which is NULL for bare-logical TRUE clauses. Same for `as.CnfFormula(TRUE)` used as first element.
- **Does NOT trigger when**: TRUE clause comes after a real clause (universe already set), or when TRUE is constructed via `CnfClause(list(tautological_atom))` (preserves universe from atom). FALSE clauses also don't trigger this because `isFALSE()` causes early break before the universe check.
- **Impact**: Users constructing formulas with programmatically-generated clause lists where a TRUE clause happens to come first.
- **Fix**: In constructor, scan for first non-logical clause to extract universe, or skip universe check for logical TRUE/FALSE entries.
- **Found in**: experiment 74

### Bug #4: `CnfFormula()` crashes when FALSE CnfFormula is in list after other CnfFormulas
- **File**: `R/CnfFormula.R` lines 149-178
- **Issue**: When `CnfFormula(list(formula1, formula2, FALSE_formula))` is called, the constructor accumulates formula1 and formula2 in `other_entries`, then sets `entries = FALSE` when it encounters the FALSE formula. After the loop, `c(FALSE, unlist(other_entries))` creates a malformed list `list(FALSE, clause1, clause2, ...)`. This gets passed to `simplify_cnf` which crashes with "wrong arguments for subsetting an environment" because it tries to process FALSE as a named-list clause.
- **Impact**: Combining CnfFormulas where one is FALSE.
- **Fix**: Add `if (isFALSE(entries)) return(structure(FALSE, universe = universe, class = "CnfFormula"))` before the `other_entries` merge at line 175.
- **Found in**: experiment 74

### Minor issues (not full bugs)
- **`FALSE_clause[FALSE]` errors** but `FALSE_clause[integer(0)]` returns FALSE. Inconsistency in `[.CnfClause` line 224. The `!isFALSE(x_bare)` condition excludes FALSE clauses from the logical-FALSE-index shortcut.
- **`clause[["NonExistent"]]` returns NULL** silently (no custom `[[.CnfClause`). Same pattern as Bug #2 but for clauses.
- **`length(TRUE_clause)` returns 1** and `length(FALSE_clause)` returns 1, since both are wrapped logicals. This is technically correct but potentially confusing (normal clauses return number of symbols).
- **`CnfSymbol` doesn't validate unique domain values**: `CnfSymbol(u, "X", c("a", "a", "b"))` is accepted. While this doesn't cause semantic bugs in practice (tested), it could theoretically cause HLA tautology detection to fail when `length(range) == length(universe[[symbol]])` check is used, since the domain length is inflated by duplicates.
- **`as.list.CnfFormula` has no explicit `isFALSE` check**: `as.list(FALSE_formula)` falls through to `lapply(FALSE, structure, ...)` which produces `list(FALSE_CnfClause)`. This works and roundtrips correctly, but the missing explicit check is inconsistent with the `isTRUE` check on the line above. Compare: `as.list(TRUE_formula) = list()` (explicit) vs `as.list(FALSE_formula) = list(FALSE_clause)` (implicit via lapply).

## Experiments Run

| # | File | Description | Tests | Failures |
|---|------|-------------|-------|----------|
| 01 | operator_edge_cases.R | \|, &, ! with TRUE/FALSE atoms/clauses/formulas | ~30 | 0 |
| 02 | as_list_edge_cases.R | as.list methods on all types | ~15 | **Bug #1** |
| 03 | basic_fuzzer.R | Random expressions, depth 2-6, truth table comparison | 4568 | 0 |
| 04 | simplification_edge_cases.R | Unit propagation, subsumption, SSE, HLA | ~20 | 0 |
| 05 | negation_distribution.R | ! and | distribution on formulas | 105 | 0 |
| 06 | directed_simplify_fuzzer.R | Directed SSE/HLA/subsumption patterns | 529 | 0 |
| 07 | heavy_fuzzer.R | Multi-strategy (superseded by 08) | - | test bug |
| 08 | expr_tree_fuzzer.R | Expression tree evaluation, 5 vars, depth 2-10 | 5900 | 0 |
| 09 | subsetting_and_equality.R | [, [[, all.equal methods | ~25 | **Bug #2** |
| 10 | 2nd_order_sse_heavy.R | Targeted 2nd-order SSE patterns | 326 | 0 |
| 11 | exhaustive_ternary.R | All 2-clause combos + random 3-5 clause, 3 ternary vars | 16001 | 0 |
| 12 | parallel_heavy_fuzzer.R | 5 strategies, seed 1-8, ~2280 tests/seed | ~18000 | 0 |
| 13 | roundtrip_and_format.R | as.list -> reconstruct + format/print methods | 519 | 0 |
| 14 | 2nd_order_sse_missing_symbol.R | 2nd-order SSE where oneend missing symbol_target | 650 | 0 |
| 15 | hla_stress_and_cascading.R | HLA chains, hidden tautology, cascading units, large formulas, dupes | 450 | 0 |
| 16 | logical_identities.R | !!f, De Morgan, constants, idempotence, complement, commutativity, absorption, associativity, distributivity | 1900 | 0 |
| 17 | or_distribution_edge_cases.R | \|.CnfFormula distribution, asymmetric, TRUE/FALSE | 900 | 0 |
| 18 | conversion_and_coercion.R | as.CnfFormula/Clause/Atom, cross-type ops, negation | 37 | 0 |
| 19 | clause_accessor_edge_cases.R | [, [[ edge cases, length, names, as.list edge cases | ~30 | confirms #1 |
| 20 | exhaustive_4binary.R | 4 binary vars, all 80 clauses, all 2-clause + random 3-6 | 18240 | 0 |
| 21 | domain_edge_cases.R | Dup domains, single-val domains, large domains, mixed | 107 | 0 |
| 22 | dup_domain_hla_bug.R | Dup domain + HLA example from docs | ~3 | 0 |
| 23 | hla_tautology_dup_domain.R | Systematic dup domain HLA patterns | 200 | 0 |
| 24 | mega_fuzzer.R | Large-scale multi-strategy fuzzer | 3200 | 0 |
| 25 | deep_cascading.R | Chain/web/contradiction cascading patterns | 600 | 0 |
| 26 | all_equal_stress.R | all.equal comparison stress testing | 609 | 1 (order-dep) |
| 27 | many_variables.R | 7-8 binary vars, 5 vars mixed domains | 900 | 0 |
| 28 | negation_stress.R | Double negation, De Morgan, complement stress | 953 | 0 |
| 29 | all_equal_reproduce.R | Reproduce order-dependence in simplifier | 6000 | 0 semantic |
| 30 | order_dependence_semantic.R | Exhaustive permutation semantic test | 17878 | 0 |
| 31 | 2nd_order_sse_twoend.R | Resolution, 4-var twoend, cascading 2nd-order SSE | 1300 | 0 |
| 32 | duplicate_and_long_clauses.R | Dup clauses, long clauses, mixed constructor | 1000 | 0 |
| 33 | apply_domain_restriction_stress.R | Multi-unit, cascading, empty symbol, conflicts | 1200 | 0 |
| 34 | crash_edge_cases.R | Boundary conditions, TRUE/FALSE ops, as.list edge | 123 | 0 |
| 35 | super_fuzzer.R | 5 configs: 4-6 vars, large domains, SAT-like, combined ops | 1100 | 0 |
| 36 | hla_specific.R | Hidden tautology, hidden subsumption, unit HLA phase | 503 | 0 |
| 37 | cascading_sse.R | SSE->unit->propagation chains, dense interaction | 702 | 0 |
| 38 | cross_universe_ops.R | Cross-universe errors, cross-type dispatch, format/print | 623 | 0 |
| 39 | resolution_subsumption.R | 2nd-order SSE oneend/twoend, on_update_range | 802 | 0 |
| 40 | distribution_stress.R | \| distribution, AND combos, triple OR, negation chains | 905 | 0 |
| 41 | adversarial_patterns.R | Almost-subset, post-simp dups, near-contradictions | 1000 | 0 |
| 42 | clause_length_transitions.R | Clause shrink to unit/empty, SSE chains, matrix units | 1002 | 0 |
| 43 | exhaustive_3var_ternary.R | 3 ternary vars, 226 clause pool, 2-5 clause combos | 6000 | 0 |
| 44 | inso_optimization.R | use_inso path, SSE creates unit during pairwise phase | 803 | 0 |
| 45 | algebraic_properties.R | Commutativity, associativity, distributivity, De Morgan, absorption, complement, idempotence | 1900 | 0 |
| 46 | large_domains.R | 2 vars domain 5-7, 3 vars domain 4-5, 2 vars domain 8-10, near-tautological, single-value | 1000 | 0 |
| 47 | subset_optimization.R | Equal/near-equal ranges, symbol orderings, many clauses 2 large-domain vars | 900 | 0 |
| 48 | meta_fuzzer.R | 11 strategies combined: random, chain, dense, units_heavy, sse_focused, etc. | 2000 | 0 |
| 49 | sat_regime.R | 7-8 binary vars 3-SAT, heavy clause ratio, phase transition, 3-valued SAT | 800 | 0 |
| 50 | hla_unit_phase.R | HLA unit hidden tautology, hidden subsumption, roe_inverse exercise | 600 | 0 |
| 51 | deep_composition.R | Deep AND/OR chains, alternating AND/OR, negation trees, triple+ negation | 1000 | 0 |
| 52 | singleton_and_edge_domains.R | Singleton domains, all binary, asymmetric sizes, special chars, empty strings | 800 | 0 |
| 53 | matrix_indexing_stress.R | Unit during pairwise, subsumption cascade, dense symbol_registry, SSE modification | 1000 | 0 |
| 54 | repeated_operations.R | Idempotence, triple repetition, non-corruption, absorption, consensus | 1800 | 0 |
| 55 | 2nd_order_disjointness.R | 2nd-order SSE disjointness, many vars small domains, cascading during pairwise | 1000 | 0 |
| 56 | structural_integrity.R | No empty ranges, no taut clauses, correct universe, format/print safety | 1900 | 0 |
| 57 | formula_from_formula.R | CnfFormula from formula list, mixed clause/formula, TRUE/FALSE constructor | 792 | 0 |
| 58 | or_distribution_deep.R | Tautology during distribution, symmetry, FALSE handling, large cross-products | 900 | 0 |
| 59 | final_boss.R | Extreme SAT (9 vars), large domains, combined ops gauntlet, heavy stress | 1300 | 0 |
| 60 | negation_internals.R | Single/multi-clause negation, units, double negation, complement, TRUE/FALSE | 1004 | 0 |
| 61 | exhaustive_2var_large.R | 2 vars, domain 4: all 24976 2-clause combos + random 3-4 clause | 32976 | 0 |
| 62 | concurrent_elimination.R | Multiple subsumptions, SSE->subsumption chains, contradicting units, cascading | 1100 | 0 |
| 63 | all_equal_deep.R | all.equal normalization, as.list roundtrip, TRUE/FALSE comparison | 605 | 0 |
| 64 | mega_stress.R | 5000 random formulas with 2-7 vars and domains 2-8 | 5000 | 0 |
| 65 | permutation_invariance.R | Clause order invariance, original vs reversed vs shuffled | 4000 | 0 |
| 66 | extreme_edge_cases.R | All identical clauses, single var, full-width, almost-taut/contra, units+long | 900 | 0 |
| 67 | special_chars_and_names.R | Dotted names, spaces, substrings, empty strings, numeric-like | 500 | 0 |
| 68 | cascading_elimination.R | Deep unit chains, contradicting chains, multi-unit, cascade-subsumption | 900 | 0 |
| 69 | many_clauses.R | 30-100 clauses, binary/ternary domains, high variable count | 300 | 0 |
| 70 | deep_nesting.R | Deep AND/OR/NOT nesting, De Morgan verification, double negation, absorption | 2600 | 0 |
| 71 | clause_subsetting.R | [.CnfClause, as.list, all.equal, construction edge cases | 28 | 0 |
| 72 | operator_shortcircuit.R | TRUE/FALSE shortcircuit paths in &, |, ! operators, mixed types | 626 | 0 |
| 73 | targeted_2nd_order_sse.R | Constructed 2nd-order SSE, symmetric, three-clause interactions | 1500 | 0 |
| 74 | formula_construction_paths.R | From CnfFormula list, mixed list, coercion paths, TRUE/FALSE in list | 607 | **Bugs #3, #4** |
| 75 | constructor_deep_dive.R | CnfUniverse/Symbol/Atom/Formula edge cases, cross-universe, dispatch | 36 | 0 |
| 76 | simplify_entry_points.R | All-unit, unit-only, single clause, all subsumed, TRUE/FALSE results, large domains | 1200 | 0 |
| 77 | negation_true_formula.R | !CnfFormula on TRUE/FALSE formulas, is.logical behavior | 5 | 0 |
| 78 | or_distribution_scale.R | OR distribution scale, AND-then-OR, OR-then-AND, complex trees | 1400 | 0 |
| 79 | mega_stress_2.R | 3000 formulas across 10 configs: 2-8 vars, domains 2-8 | 3000 | 0 |
| 80 | c_behavior_and_edge.R | c() behavior, formula from formulas, mixed clause+formula | 608 | 2 (Bug #4) |
| 81 | exhaustive_3var_binary.R | All 26 binary 3-var clauses, 2-5 clause combos | 8351 | 0 |
| 82 | operator_associativity.R | Commutativity, associativity, distributivity, complement | 4000 | 0 |
| 83 | edge_simplify_states.R | Subsumed by units, SSE creates unit, HLA all candidates, asymmetric, minimal pairwise | 1800 | 0 |
| 84 | simplifies_to_true.R | Subsumption, tautological formulas, unit makes taut, SSE->subsumable, HLA eliminates | 1700 | 0 |
| 85 | or_distribution_edge.R | OR swap optimization, distribution tautology, minimal dist, large cross-product, triple OR | 1600 | 0 |
| 86 | many_binary_vars.R | 8-10 binary vars: 3-SAT, mixed, many units, dense phase transition | 1400 | 0 |
| 87 | negation_deep.R | Negation tautological, multi-clause, double negation, De Morgan, AND/OR identity | 1800 | 0 |
| 88 | all_equal_deep.R | all.equal: different construction, TRUE/FALSE, identical, same clauses, type checking | 907 | 0 |
| 89 | cascading_unit_pairwise.R | SSE->unit cascade, constructed cascade, subsumption cascade, contradiction cascade | 1600 | 0 |
| 90 | mixed_domain_sizes.R | Singleton+binary+large, multiple singletons, binary+large(8), all different sizes | 1400 | 0 |
| 91 | structural_output.R | No empty ranges, no tautological symbols, no duplicates, as.list roundtrip, format/print | 2101 | 0 |
| 92 | overlapping_names.R | Substring names (V1/V10), numeric names, special chars, confusing values, single-char | 1100 | 0 |
| 93 | large_domains.R | Domain 10, domain 15, mixed 2+12, near-tautological ranges | 1600 | 0 |
| 94 | aslist_false_formula.R | as.list on FALSE formulas, as.logical, roundtrip, TRUE formula | 207 | 0 |
| 95 | operator_chains.R | Complex expression trees, chained AND/OR, alternating AND/OR, triple negation | 1500 | 0 |
| 96 | torture_test.R | Multi-op gauntlet, absorption law, consensus theorem, XOR expression | 1700 | 0 |
| 97 | regression_edge.R | Single unit, identical clauses, single var, domain 1, exhaustive 2v4, from-formula list | 1976 | 4 (Bug #4) |
| 98 | final_mega.R | 10 configs (2-8 vars, domains 1-10), 500 tests each | 5000 | 0 |
| 99 | atom_negation_edge.R | Atom negation basics, singleton domain, tautology check, clause subsetting, contradiction | 607 | 0 |
| 100 | final_verification.R | Exhaustive 4-var binary 2-clause (3240), random 3-clause (3000), algebraic properties (1500) | 7740 | 0 |
| 101 | eliminate_symbol_cascade.R | Constructed cascades, dense symbol sharing, long chains (4-8), multi-unit cascade | 1500 | 0 |
| 102 | hla_unit_phase.R | Unit HLA hidden tautology, mixed non-unit+unit HLA, roe_inverse stress | 1000 | 0 |
| 103 | or_distribution_large.R | OR swap optimization, large cross-product, chained OR, interleaved OR/AND | 900 | 0 |
| 104 | negation_reduce_path.R | Multi-clause negation, double negation, De Morgan, large Reduce path | 1500 | 0 |
| 105 | use_inso_path.R | SSE creates unit (use_inso), multiple units overlapping, duplicate unit registration, contradiction | 1500 | 0 |
| 106 | large_formulas.R | 30-60 clause binary SAT, 40-80 clause ternary, 30-50 with units, 100 clauses | 350 | 0 |
| 107 | 2nd_order_sse_deep.R | Constructed oneend/twoend patterns, random 2nd-order SSE, disjointness stress | 1800 | 0 |
| 108 | pairwise_modifications.R | Subsumption during pairwise, SSE->unit cascade, shortened clauses, dense pairwise | 1500 | 0 |
| 109 | return_entries_edge.R | Simplify to TRUE/FALSE, all-unit, structural integrity, format/print safety | 1600 | 0 |
| 110 | combined_ops_stress.R | Complex trees, absorption, consensus, XOR, chained mixed ops | 1200 | 0 |
| 111 | mutation_targets.R | char_intersect, contradiction detection, symbol registry, SSE direction, HLA complement, not_subset_count | 1742 | 0 |
| 112 | cross_type_ops.R | 3-atom OR, clause\|atom, atom&clause, formula\|atom, long chain, negated atoms in formulas | 1400 | 0 |
| 113 | distribution_unique.R | Same symbol distribution, tautology during distribution, multi x multi, near-identical ranges | 1600 | 0 |
| 114 | exhaustive_3var_4dom.R | 3 vars domain 4 (subsets size 1-2): all 2-clause combos + random 3-clause | 890115 | 0 |
| 115 | and_shortcircuit.R | AND with TRUE/FALSE, mixed types, all.equal normalize, c() concatenation | 1703 | 0 |
| 116 | final_sweep.R | 8 configs: 2v-d6, 3v-d3-many, 5v-binary, 4v-mixed, 6v-binary-20cl, 3v-d5-15cl, 7v-3sat, 2v-d10 | 3000 | 0 |
| 117 | unusual_patterns.R | Identical clauses, all units, complements, single symbol, subsumption, SSE chain, wide clauses | 1400 | 0 |
| 118 | atom_construction.R | Full/empty domain atoms, single value, out-of-domain values, duplicates, negation, atom ops in formulas | 908 | 0 |
| 119 | property_laws.R | Commutativity, De Morgan, absorption, double negation, complement across 4 configs | 6400 | 0 |
| 120 | large_domain_stress.R | 2v-d20, 2v-d30, 3v-d8, SSE d15, mixed large+small domains | 900 | 0 |
| 121 | hla_deep_chains.R | HLA tautology, HLA subsumption chains, units+non-units HLA, 5v ternary HLA stress | 1100 | 0 |
| 122 | on_update_range_cascade.R | 2nd-order SSE cascade, domain restriction cascades, SSE->unit->restriction, multi-unit | 1200 | 0 |
| 123 | operator_dispatch_edge.R | Cross-type ops, expression trees, tautology/contradiction, multi-clause negation | 3200 | 0 |
| 124 | contradiction_paths.R | Direct contradicting units, unit empties clause, SSE contradictions, all-unit, simplify-to-TRUE | 1500 | 0 |
| 125 | or_distribution_semantics.R | 1|1, multi|multi, chained OR, OR-then-AND, OR with TRUE/FALSE | 2400 | 0 |
| 126 | exotic_domains.R | Empty string, substrings, numeric-looking, confusing names, special chars, singleton domains | 1056 | 0 |
| 127 | recursive_cascade_stress.R | Dense 3v quaternary, 4v cascade, 2nd-order disjoint, many same-symbol clauses | 1100 | 0 |
| 128 | branch_coverage.R | register_unit intersection, domain chain, pairwise subsumption/SSE, HLA paths, unit HLA, symbol loss | 1800 | 0 |
| 129 | formula_builder_stress.R | Incremental AND, complex composite, deep negation, formula-from-formula, De Morgan complex | 1400 | 0 |
| 130 | naive_comparison.R | 12 configs (2-6 vars, domains 2-8): naive vs simplified truth table comparison | 3350 | 0 |
| 131 | random_transformations.R | Double negation, complement, absorption, distributivity, random operation sequences | 2300 | 0 |
| 132 | adversarial_simplifier.R | SSE shortens during pairwise, unit cascade, sparse (6-10 vars), dense (2v many clauses), max overlap | 1300 | 0 |
| 133 | exhaustive_2var_5dom.R | 2 vars domain 5: 960 clause pool, all 2-clause combos (461K) + random 3-clause (5K) | 466280 | 0 |
| 134 | ultimate_stress.R | 5000 trials, 6 strategies: random, incremental, complement, distribution, idempotence, contradiction | 6730 | 0 |
| 135 | duplicate_unit_symbols.R | Two/three initial units same symbol, contradicting pairs, SSE creates duplicate, all-unit formulas | 1608 | 0 |
| 136 | hla_targeted.R | HLA hidden tautology (non-unit), hidden subsumption (non-unit), unit HLA, random HLA stress | 2500 | 0 |
| 137 | very_large_formulas.R | 50-200 clauses: 3v binary, 4v ternary, 2v-d6, units + many clauses | 300 | 0 |
| 138 | long_op_chains.R | Long AND chains (5-15), OR chains (3-8), alternating AND/OR, deep negation (2-8), mixed op trees | 1100 | 0 |
| 139 | all_equal_stress.R | Same clauses different order, AND order independence, OR commutativity, TRUE/FALSE equality, constructed vs simplified | 2200 | 0 |
| 140 | 2nd_order_sse_targeted.R | Constructed oneend/twoend patterns, 2nd-order cascade, many 2-symbol clauses | 2000 | 0 |
| 141 | output_structural_integrity.R | No empty ranges, no tautological symbols, no duplicates, format/print, as.list roundtrip (3000 formulas) | 3000 | 0 |
| 142 | eliminate_symbol_cascade.R | SSE->unit->cascade, unit creates unit in later clause, dense symbol overlap, many short clauses | 1700 | 0 |
| 143 | comprehensive_final.R | 10000 trials, 8 strategies (truth table, complement, double neg, distribution, absorption, incremental, random ops, mixed units) | 11268 | 0 |
| 144 | reverse_sse_update.R | SSE reverse update paths, chain cascade, overlapping registries | 3000 | 0 |
| 145 | 2nd_order_missing_symbol.R | Oneend missing symbol_target, asymmetric sets, units + 2nd-order, 5v3d | 3000 | 0 |
| 146 | hla_interaction_stress.R | HLA after pairwise SSE, dense HLA iteration, unit HLA, near-tautological HLA | 2500 | 0 |
| 147 | 4var_3dom_exhaustive.R | 4 vars domain 3 semi-exhaustive, 200-clause pool, all 2-clause + random 3-4 clause | 24900 | 0 |
| 148 | or_distribution_stress.R | Distribution tautology, near-tautological OR, chained OR, swap optimization, expression trees | 2800 | 0 |
| 149 | eliminate_symbol_matrix.R | Elimination cascade, sequential elimination, symbol elim -> unit, late clause elimination | 3500 | 0 |
| 150 | negation_complex.R | Multi-clause negation, De Morgan complex, neg-AND-neg chains, triple negation | 2900 | 0 |
| 151 | mixed_domain_interactions.R | Mixed domains (2+3+4), singleton+large, 4 vars mixed (1,2,3,5), negation, operations | 3341 | 0 |
| 152 | pairwise_cascading_edge.R | SSE->unit pairwise, outer eliminated, dense pairwise, cascading future pairs | 3500 | 0 |
| 153 | constructed_2nd_order.R | Oneend without target symbol, disjointness boundary, multiple 2nd-order, 5v3d | 3000 | 0 |
| 154 | extreme_configurations.R | 2v binary many clauses, 8v 3-SAT, 3v domain 6, single var, identical, complementary, large mixed | 3300 | 0 |
| 155 | constructor_false_first.R | CnfClause FALSE atom first (Bug #3), CnfFormula orderings, conversion methods, as.logical | 1515 | 4 (known) |
| 156 | type_dispatch_matrix.R | All type pairs for &/|/!, mixed type expressions, type transition chains | 4300 | 0 |
| 157 | near_contradiction.R | Near-contradictions, near-tautologies, idempotence, 4-var tight, exact contradictions | 2848 | 0 |
| 158 | clause_subsetting_deep.R | Numeric/char/logical indexing, TRUE/FALSE clause, missing index, [[, consistency | 835 | 0 |
| 159 | all_equal_edge.R | all.equal for atom/clause/formula, TRUE/FALSE, cross-type, random stress | 1460 | 0 |
| 160 | and_or_shortcircuit_deep.R | &/| shortcircuit all pairs, return types, random shortcircuit stress | 4204 | 0 |
| 161 | hla_hidden_subsumption.R | Constructed HLA, random HLA, unit HLA, deep HLA (10-20 clauses), HLA cascade | 2801 | 0 |
| 162 | final_mega_2.R | 10 configs truth table (4400), algebraic properties (4000), operation chains (3000) | 11400 | 0 |
| 163 | apply_domain_restriction.R | Empty range, unit cascade, multiple restrictions, contradiction, large random | 2500 | 0 |
| 164 | workflow_paths.R | Units+multi, SSE-designed, mixed workflow, operations on simplified formulas | 4000 | 0 |
| 165 | dual_unit_stress.R | Dual units same symbol, contradicting, SSE duplicate, multiple units, unit-SSE chain | 3000 | 0 |
| 166 | all_unit_formulas.R | All units same/different symbols, units+one multi, single satisfying, large OR distribution | 2900 | 0 |
| 167 | format_print_stress.R | Format/print safety, names/length, random stress, long value names | 1531 | 0 |
| 168 | unit_cascade_init.R | Cascade unit creation during init, double cascade, duplicate, many-unit, contradictions | 3000 | 0 |
| 169 | pairwise_unit_creation.R | SSE creates unit during pairwise, dense pairwise, use_inso, operations | 3000 | 0 |
| 170 | hla_phase_interactions.R | HLA hidden tautology, unit stress, unit after non-unit, 2nd-order+HLA, large | 2300 | 0 |
| 171 | negation_multi_clause.R | 2/3-clause negation, unit negation, De Morgan, complement | 3940 | 0 |
| 172 | or_distribution_internals.R | Swap, copy-on-modify, tautology, chained OR, single-clause | 3100 | 0 |
| 173 | constructor_mixed_inputs.R | Mixed CnfClause+CnfFormula, multiple CnfFormulas, re-simplification | 2376 | 29 (known) |
| 174 | eliminate_clause_during_sr.R | Dense sharing, subsumption cascade, sparse, dense 2-var, idempotence | 2182 | 0 |
| 175 | second_order_trigger.R | Many 2nd-order pairs, 3-symbol, cascading, with units | 2500 | 0 |
| 176 | binary_sat_extreme.R | 6/7/8-var 3-SAT phase transition, mixed 2+3-SAT, 3-valued SAT | 2000 | 0 |
| 177 | algebraic_deep.R | Absorption, consensus, resolution, XOR, monotonicity | 2800 | 0 |
| 178 | large_domain_stress.R | 2-var domain 10/15, 3-var domain 6, asymmetric, near-tautological | 1864 | 0 |
| 179 | hla_count_divergence.R | Dense HLA, many HLA-1 pairs, HLA+SSE cascade, deep HLA 4v | 3500 | 0 |
| 180 | twoend_symbols_stress.R | Twoend-rich, 5-var twoend, twoend+cascade, same 4 symbols | 3000 | 0 |
| 181 | on_update_range_stress.R | Dense SSE+on_update_range, 4v-d5, units+3-sym, large domain tight | 3000 | 0 |
| 182 | hla_unit_roe_inverse.R | Unit HLA shared, multiple units HLA, non-unit then unit, large domain | 3500 | 0 |
| 183 | constructed_hla_cascade.R | HLA+SSE construction, complex ops, De Morgan, quantified patterns | 5000 | 0 |
| 184 | meta_idx_boundary.R | Unit+non-units ordering, SSE unit late, dense many-var, singleton+large | 3537 | 0 |
| 185 | negation_setdiff_edge.R | Large domain neg, double neg, confusing values, many-clause neg, De Morgan | 1800 | 0 |
| 186 | complex_expr_trees.R | Random trees, triple negation, chained OR-AND, large domain ops | 1800 | 0 |
| 187 | elimination_order_stress.R | Permutation invariance, all-phases, very many clauses | 3700 | 0 |
| 188 | boundary_sat_formulas.R | Single satisfying, exactly-k, pigeonhole, at-most-one | 1773 | 0 |
| 189 | incremental_build.R | Incremental vs batch, intermediate checks, incremental OR, mixed | 5229 | 0 |
| 190 | distribution_corner.R | Distribution fills domain, large cross-product, single clause OR, swap | 2700 | 0 |
| 191 | clause_constructor_edge.R | Clause merging, overlapping atoms, FALSE atoms, mixed atoms/clauses | 3500 | 0 |

| 192 | many_variables_deep.R | 10/12 binary vars, 6 ternary vars, 8-var operations | 500 | 0 |
| 193 | edge_case_gauntlet.R | All identical, complementary pairs, units subsume all, many operations, simplifies to single | 2188 | 0 |
| 194 | duplicate_units_stress.R | Two/three+ units same symbol, all units, contradicting, SSE creating units | 4500 | 0 |
| 195 | 2nd_order_sse_targeted.R | Direct 2nd-order SSE, cascading, 2-symbol, with units | 3000 | 0 |
| 196 | cascading_elimination.R | Chain unit propagation, SSE unit during pairwise, deep cascading (5 vars), elimination during pairwise loop | 3000 | 0 |
| 197 | operator_composition_stress.R | Distributivity, double negation, De Morgan, triple composition, associativity | 1700 | 0 |
| 198 | heterogeneous_domains.R | Binary+large(8), three different sizes, unit large domain, heterogeneous operations | 2000 | 0 |
| 199 | formula_constructor_paths.R | CnfFormula(list(f1,f2)), mixed clause/formula, FALSE clause, TRUE clause, many formulas | 3300 | 528 (Bugs #3,#4) |
| 200 | register_unit_inso.R | Unit covers most, SSE duplicate unit, multiple rounds, tight unit many eliminations | 3000 | 0 |
| 201 | disjointness_boundary.R | try_sse_2nd_order disjointness boundary, overlapping intersect, multiple 2nd-order same target | 3000 | 0 |
| 202 | dense_all_symbols.R | All clauses share all symbols (3v, 4v, 5v dense), dense + unit | 1500 | 0 |
| 203 | hla_roe_inverse.R | Non-unit HLA then unit HLA, multiple units HLA, cascading HLA+SSE, unit symbol absent | 2000 | 0 |
| 204 | negation_distribution_chains.R | De Morgan OR/AND, triple negation, alternating neg-or-and, negated sub-formula distribution | 2100 | 0 |
| 205 | distribution_many_symbols.R | Wide clause distribution (5 syms), near-tautological, many x many, shared symbol tautology | 1800 | 0 |
| 206 | on_update_range_cascade.R | SSE cascade through restriction, restriction creates unit, beyond meta_idx_outer, multiple restrictions | 2000 | 0 |
| 207 | intermediate_states.R | Progressive shortening, cascading unit creation (5 vars), multiple modification mechanisms, ops on complex | 1800 | 0 |
| 208 | twoend_oneend_interaction.R | Twoend both symbol iterations, oneend missing target, many oneend candidates, oneend extra symbols | 2000 | 0 |
| 209 | structural_completeness.R | No subsumption/tautology/empty range, unit propagation complete, idempotence, monotonicity | 3300 | 0 |
| 210 | sse_completeness.R | SSE completeness after simplification, post-operation SSE | 3000 | 1 (non-confluence) |
| 211 | simplification_equivalence.R | Batch vs incremental, atom ordering, clause permutation, value ordering | 3000 | 0 |
| 212 | 5var_ternary_stress.R | 5v ternary random, with units, operations, many clauses (15-30) | 1500 | 0 |
| 213 | or_distribution_internals.R | Asymmetric 1 vs many, all-tautology distribution, duplicate ranges, triple OR chain | 1800 | 0 |
| 214 | elimination_cascade_matrix.R | Deep cascade 6 symbols, diamond cascade, cross-referencing, rapid unit creation | 1800 | 0 |
| 215 | binary_domain_heavy.R | 6 binary vars unit-heavy, 8 binary 3-SAT, chain implications, binary operations | 1600 | 0 |
| 216 | remaining_entries_edge.R | All subsumed by units, pairwise subsumption heavy, SSE eliminates all, simplify to single clause | 1969 | 0 |
| 217 | wide_clause_stress.R | 8 binary wide clauses, 6 ternary wide, wide+units, full-width | 2000 | 0 |
| 218 | pairwise_unit_cascade_deep.R | SSE->unit->SSE cascade, short clause cascade, mixed width, high density binary | 2000 | 0 |
| 219 | hla_after_heavy_simplification.R | Heavy SSE then HLA, units then HLA, HLA unit phase, ops after heavy simplification | 1800 | 0 |
| 220 | inso_optimization_edge.R | SSE creates units during pairwise, multiple pairwise units, early unit late effects, contradiction | 2000 | 0 |
| 221 | composition_deep_stress.R | OR-AND compositions, De Morgan stress, repeated operations, complement verification | 1200 | 0 |
| 222 | subset_count_tracking.R | 3-symbol shared, gradually overlapping, structural checks, idempotence | 2000 | 0 |
| 223 | large_formula_operations.R | Large AND large, small OR large (swap), NOT of large, triple composition | 800 | 0 |
| 224 | monotonicity_absorption.R | Absorption AND/OR, consensus, monotonicity | 1500 | 0 |
| 225 | extreme_simplification.R | Unsatisfiable, single satisfying, near-tautological, many to few | 2000 | 0 |
| 226 | mixed_domain_wide.R | Binary+quaternary+singleton, three sizes ops, large+binary, 6 vars mixed | 1195 | 0 |
| 227 | regression_resilience.R | SSE apply_domain_restriction, symbol elim reverse, cascading subset, 2nd-order trigger | 2000 | 0 |
| 228 | formula_equality_deep.R | all.equal equivalence, TRUE/FALSE, as.list roundtrip, format/print | 1800 | 0 |
| 229 | meta_idx_boundary.R | Short first long affected, SSE beyond boundary, many vars sparse, same structure | 1800 | 0 |
| 230 | exhaustive_3var_5dom_2cl.R | 3 vars domain 5 (ranges 1-2): 720-clause pool, all 2-clause combos (259K) | ~259000 | 0 |
| 231 | negation_large_clause.R | Negation 3-5 clauses, unit formulas, double negation, negation then AND | 1400 | 0 |
| 232 | high_var_count.R | 10 binary 3-SAT, 12 binary 2-SAT+units, 8 ternary, 15 binary sparse | 600 | 0 |
| 233 | c_operator_formula.R | AND/OR associativity, distributivity, incremental vs batch | ~1400 | 0 |
| 234 | pigeonhole_crafted.R | At-most-one, graph coloring, implication chains, bi-implications | 2000 | 0 |
| 235 | random_mega_stress.R | 12 configs (2-6 vars, domains 2-8), 500 each | 6000 | 0 |
| 236 | unit_conflict_stress.R | Overlapping units, three units, pairwise-created conflicts, multi-symbol | 2000 | 0 |
| 237 | xor_parity.R | XOR via operations, binary XOR CNF, if-then-else, many XOR | 1400 | 0 |
| 238 | hla_specific_patterns.R | HLA hidden subsumption, unit HLA, multi-step HLA chains, high symbol overlap | 2000 | 0 |
| 239 | domain_size_one.R | All domain-1, mixed domain-1+larger, domain-1 ops, contradictions | 139 | 0 |
| 240 | large_domain_stress.R | Two vars large (10-15), three vars mixed (3-12), narrow ranges (12-20), ops | 1700 | 0 |
| 241 | all_units_formula.R | All units, many overlapping, units becoming, unit operations | 1800 | 0 |
| 242 | or_distribution_heavy.R | Multi-clause OR, tautology elimination, chained OR, swap path | 1500 | 0 |
| 243 | repeated_operations.R | Incremental AND, alternating AND/OR, repeated negation, De Morgan | 1300 | 0 |
| 244 | symbol_elimination_cascade.R | SSE->unit cascade, deep cascade (6 vars), contradiction via elimination, multi-symbol elim | 2000 | 0 |
| 245 | second_order_sse_targeted.R | Disjoint ranges, twoend+oneend interaction, many twoend, resolution-like | 2000 | 0 |
| 246 | special_value_names.R | Logical-like values ("TRUE"/"FALSE"), special chars, numeric strings, long names, ops | 2300 | 0 |
| 247 | order_sensitivity.R | All permutations (small), random permutations (large), reverse, duplicates | 2000 | 0 |
| 248 | cnf_formula_construction.R | Single clause, as.CnfFormula, c() vs AND, identical clauses, TRUE/FALSE edge | 2100 | 0 |
| 249 | exhaustive_3var_3dom_3cl.R | 3 vars domain 3 (ranges 1-2): 342-clause pool, 100K sampled 3-clause combos | 100000 | 0 |
| 250 | high_clause_count.R | 20-30 cl (3 binary), 40-60 cl (4 binary), 20+ ternary, 50-100 cl (5 binary) | 550 | 0 |
| 251 | satisfiability_edge.R | One satisfying, unsatisfiable via units, near-unsatisfiable, f & !f | 1679 | 0 |
| 252 | interleaved_phases.R | Cross-phase cascading, non-unit to unit, dense interconnected, heavy simplification | 1997 | 0 |
| 253 | regression_specific_bugs.R | Similar names, single overlap domains, mixed width clauses, different paths | 1800 | 0 |
| 254 | complete_algebraic.R | Identity, commutativity, associativity, distributivity, absorption, involution | 1400 | 0 |

| 255 | binary_domain_many_vars.R | 6/8/10-var binary, operations, many clauses | 980 | 0 |
| 256 | almost_full_domain.R | Domain-5 with ranges of 4, mixed widths, negation duality | 1600 | 0 |
| 257 | no_shared_symbols.R | Fully disjoint, disjoint operations, one shared symbol | 1500 | 0 |
| 258 | unit_from_pairwise.R | SSE creates units, unit chain creation, concurrent units | 1500 | 0 |
| 259 | wide_clauses.R | Wide (5-7 symbol) clauses 8-var, mix wide+narrow, operations | 1100 | 0 |
| 260 | dense_symbol_sharing.R | All share V1, shared+unit, multiple shared, operations | 1800 | 0 |
| 261 | resimplification.R | Re-simplify already simplified, after ops, triple simplification | 1260 | 0 |
| 262 | or_explosion.R | 2x3 OR, 3x3 OR, chained OR, tautology-heavy OR | 1400 | 0 |
| 263 | eliminate_symbol_cascade.R | Elimination to unit, to contradiction, multi-symbol elimination | 1500 | 0 |
| 264 | subset_equality_edge.R | Equal ranges, near-subset, identical clauses, subset/superset | 2000 | 0 |
| 265 | asymmetric_domains.R | Binary+ternary, mixed 2/3/5, domain-1+larger, operations | 1446 | 0 |
| 266 | implication_properties.R | Implication by construction, idempotency, consensus theorem | 1400 | 0 |
| 267 | negation_deep.R | De Morgan AND/OR, multi-clause negation, chained neg identity, XOR | 1800 | 0 |
| 268 | structural_coverage.R | Subsumption patterns, SSE patterns, HLA patterns | 1319 | 0 |
| 269 | random_walk.R | Random walk 5-10 steps, binary random walk 8-15 steps | 4611 | 0 |
| 270 | mutation_testing.R | Add value, remove value, add clause mutations with monotonicity checks | 1500 | 0 |
| 271 | exhaustive_4var_2dom.R | 4 binary vars: 50K 3-clause + 30K 4-clause samples | 74638 | 0 |
| 272 | complex_expressions.R | Compound AND-OR, negated-OR-AND, XNOR, deep nesting, if-then-else | 1500 | 0 |
| 273 | stress_apply_domain_restriction.R | Reverse direction updates, 2nd-order cascade, multiple restrictions same clause | 1500 | 0 |
| 274 | exhaustive_3var_4dom_3cl.R | 3 vars domain 4: 100K sampled 3-clause formulas | 100000 | 0 |
| 275 | randomized_operations_stress.R | 4 configs (5v-binary, 4v-ternary, 3v-quaternary, 2v-quinary), 5 ops each | 6000 | 0 |
| 276 | specific_code_paths.R | Pairwise loop interactions, HLA multi-step, unit HLA stress | 1500 | 0 |
| 277 | contradiction_detection.R | Conflicting units, SSE cascade contradiction, multi-step unit propagation | 1500 | 0 |
| 278 | ordering_independence.R | Clause ordering invariance (5 perms), AND/OR associativity/commutativity | 2900 | 0 |
| 279 | single_symbol_formulas.R | Single symbol, two symbol saturation, duplicate clauses | 1300 | 0 |
| 280 | incremental_and.R | Incremental AND vs batch, incremental OR associativity | 800 | 0 |
| 281 | large_domain_small_ranges.R | Domain 7-10 with ranges 1-2, large domain operations | 1400 | 0 |
| 282 | sse_creates_units.R | SSE creating units, chain reactions SSE->unit->SSE | 1000 | 0 |
| 283 | algebraic_properties.R | Absorption, double negation, distributivity, idempotency | 1700 | 0 |
| 284 | many_clauses_stress.R | 10-25 clauses binary/ternary, many clauses with operations | 600 | 0 |
| 285 | 2nd_order_sse_targeted.R | Oneend 2nd-order SSE, twoend 2nd-order SSE | 1000 | 0 |
| 286 | exhaustive_2var_5dom.R | 2 vars domain 5: 100K sampled 2-4 clause formulas | 100000 | 0 |
| 287 | edge_case_domains.R | Domain size 1, binary SAT-like (8 vars), large domain 15 | 800 | 0 |
| 288 | formula_composition.R | CnfFormula of CnfFormula, mixed constructor, as.list roundtrip | 888 | 0 |
| 289 | or_distribution_stress.R | Small OR, single-clause OR, TRUE/FALSE OR, chained ORs | 1300 | 0 |
| 290 | demorgan_stress.R | De Morgan AND/OR, nested De Morgan | 800 | 0 |
| 291 | special_domain_values.R | Substring domains, numeric strings, long strings, mixed domain sizes | 800 | 0 |
| 292 | concurrent_simplification.R | Independent formulas same universe, repeated operations immutability | 1100 | 0 |
| 293 | exhaustive_5var_binary.R | 5 binary vars: 100K sampled 2-4 clause formulas | 100000 | 0 |
| 294 | known_contradictions.R | Pairwise, three-way, cascade, random contradictions | 1100 | 0 |
| 295 | complex_operations_chain.R | f&!f==FALSE, f|!f==TRUE, complex chains (f1&f2)|(f3&f4), !f1&(f2|!f3)&f4 | 1200 | 0 |
| 296 | heavy_unit_propagation.R | Many units (3-5), all units, units with varying overlaps | 1500 | 0 |
| 297 | random_expression_tree.R | Random expression trees, random operation sequences | 1000 | 0 |
| 298 | near_tautology.R | Near-tautological clauses (3-4/5 domain values), mixed restrictiveness | 933 | 0 |

**Total tests across all experiments: ~2,760,000+** (incl. experiment 114's 890K + experiment 133's 466K + experiment 230's 259K + experiment 249/274/286/293's 100K each + experiment 271's 75K exhaustive tests)

### Minor finding: Non-confluent simplifier (not a bug)
- **Found in**: experiment 26, reproduced in 29
- The simplifier is **non-confluent**: different clause orderings can produce structurally different but **semantically equivalent** results
- Example (trial 175): same 3 clauses in different order → f_a removes C from third clause, f_b removes A
- Trial 965: different orderings produce 3 vs 4 clauses
- **0 semantic bugs** across 6000 trials with all permutations (exp 30: 17878 tests)
- This is inherent to heuristic simplification and NOT a correctness issue

## Summary

The simplification engine (`simplify_cnf`) is **extremely robust**. Despite ~2,760,000+ tests across 298 experiments including exhaustive enumeration (890K+ for 3-var 4-domain, 466K+ for 2-var 5-domain, 259K for 3-var 5-domain 2-clause, 100K sampled 3-var 3-domain 3-clause, 75K 4-var binary 3-4 clause), random fuzzing, targeted patterns, algebraic property verification (identity, commutativity, associativity, distributivity, absorption, involution), HLA-specific patterns (unit and non-unit phases, hidden tautology/subsumption, deep HLA chains, multi-step HLA), cascading SSE, 2nd-order SSE (oneend/twoend, disjointness, resolution-like), cross-type operator dispatch, deep formula composition, distribution stress (swap path, tautology elimination, chained OR), negation internals, SAT-regime testing (up to 15 binary variables), XOR/parity constraints, pigeonhole/graph-coloring, large domains (up to 20 values), domain-size-1 edge cases, exotic string domains (logical-like, special chars, numeric strings), high clause counts (50-100 clauses), satisfiability edge cases (single satisfying, unsatisfiable via operations), order sensitivity (all permutations, reverse, duplicates), symbol elimination cascading (deep 6-var chains), interleaved phase interactions, very large formulas (100-200 clauses), unit conflict stress (overlapping, triple units, pairwise-created), binary domains with many variables (up to 10), almost-full-domain ranges, asymmetric domain sizes, no-shared-symbols formulas, re-simplification idempotency, mutation testing (monotonicity of add/remove value/clause), random walks (8-15 step operation sequences), complex compound expressions (if-then-else, XNOR, deep nesting), implication/consensus properties, contradiction detection (cascading, multi-step), ordering independence (5 permutations, associativity, commutativity), algebraic laws (absorption, double negation, distributivity, idempotency, complementation), near-tautological clauses, random expression trees and operation sequences, heavy unit propagation (many/all units), De Morgan stress testing (nested), formula composition (CnfFormula of CnfFormula, mixed constructors), and many other approaches, **no semantic bugs were found in the simplification logic**.

The confirmed bugs are in the **accessor/API/constructor layer** (not the simplification engine):
1. `as.list.CnfClause` crashes on TRUE clauses (S3 dispatch + return type)
2. `u[["NonExistent"]]` silently returns NULL (missing validation)
3. `CnfFormula()` fails when first element is TRUE clause with NULL universe
4. `CnfFormula()` crashes when FALSE CnfFormula is in list after other CnfFormulas

All are straightforward to fix. Bugs #3 and #4 are in the CnfFormula constructor's handling of logical TRUE/FALSE entries and their interaction with the universe extraction and `other_entries` merge logic.
