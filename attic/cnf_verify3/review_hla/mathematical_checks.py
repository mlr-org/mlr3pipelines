"""Independent, standard-library finite-domain checks for the HLA review.

Run from the repository root. No earlier research harness is imported.
All masks denote sets; coordinate zero denotes an absent literal.
"""

import itertools
import json
from pathlib import Path


HERE = Path(__file__).resolve().parent


def available_literals(clause, domains):
    return [i for i, (literal, domain) in enumerate(zip(clause, domains))
            if literal & domain]


def propagate(donors, initial):
    """Repeated whole-formula support propagation; None is contradiction."""
    domains = tuple(initial)
    if 0 in domains:
        return None
    while True:
        before = domains
        for donor in donors:
            possible = available_literals(donor, domains)
            if not possible:
                return None
            if len(possible) == 1:
                pivot = possible[0]
                domains = tuple(domain & donor[i] if i == pivot else domain
                                for i, domain in enumerate(domains))
        if domains == before:
            return domains


def donor_operation(donor, domains):
    if domains is None:
        return None
    possible = available_literals(donor, domains)
    if not possible:
        return None
    if len(possible) > 1:
        return domains
    pivot = possible[0]
    return tuple(domain & donor[i] if i == pivot else domain
                 for i, domain in enumerate(domains))


def support_projection(donor, domains):
    """GAC by enumerating satisfying tuples, independently of unit counts."""
    values = [[i for i in range(domain.bit_length()) if domain & (1 << i)]
              for domain in domains]
    satisfying = [value for value in itertools.product(*values) if truth(donor, value)]
    if not satisfying:
        return None
    return tuple(sum(1 << value for value in set(v[i] for v in satisfying))
                 for i in range(len(domains)))


def boolean_unit_propagation(clauses):
    """Ordinary Boolean UP on integer literal lists; None means conflict."""
    assigned = set()
    while True:
        active = [tuple(literal for literal in clause if -literal not in assigned)
                  for clause in clauses if not any(literal in assigned for literal in clause)]
        if any(not clause for clause in active):
            return None
        units = {clause[0] for clause in active if len(clause) == 1}
        if not units:
            return assigned
        if any(-literal in assigned or -literal in units for literal in units):
            return None
        assigned.update(units)


def below(smaller, larger):
    return smaller is None or (larger is not None and all(
        small & ~large == 0 for small, large in zip(smaller, larger)))


def virtual_once(donors, target, universe, initial_zero_check=True):
    """Fresh comparisons, then the same one-use mathematical scheduling as R.

    Unlike the support propagator this uses expanding forbidden sets and
    explicit donor history. It checks the stronger used-donor invariant.
    """
    virtual = tuple(target)
    used = {}
    trace = []
    if any(value == full for value, full in zip(virtual, universe)):
        return dict(refuted=True, trace=[dict(kind="initial_empty")])
    while True:
        exceptions = [tuple(i for i, (literal, forbidden) in
                            enumerate(zip(donor, virtual)) if literal & ~forbidden)
                      for donor in donors]
        for j, pivot in used.items():
            assert exceptions[j] == (pivot,), (donors, target, virtual, used, j)
            possible = universe[pivot] & ~virtual[pivot]
            assert possible and possible & ~donors[j][pivot] == 0
        if initial_zero_check or trace:
            conflicts = [j for j, exceptional in enumerate(exceptions) if not exceptional]
            if conflicts:
                assert conflicts[0] not in used
                trace.append(dict(kind="conflict", donor=conflicts[0]))
                return dict(refuted=True, trace=trace)
        candidates = [j for j, exceptional in enumerate(exceptions)
                      if len(exceptional) == 1 and j not in used]
        if not candidates:
            return dict(refuted=False, trace=trace, domains=tuple(
                full & ~forbidden for full, forbidden in zip(universe, virtual)))
        j = candidates[0]
        pivot = exceptions[j][0]
        previous = virtual[pivot]
        new_range = previous | (universe[pivot] & ~donors[j][pivot])
        assert new_range != universe[pivot], "The selected-donor HTE branch is unreachable"
        virtual = tuple(new_range if i == pivot else value for i, value in enumerate(virtual))
        trace.append(dict(kind="extension", donor=j, pivot=pivot,
                          before=previous, after=new_range))
        used[j] = pivot


def refutable(universe, donors, target):
    initial = tuple(full & ~literal for full, literal in zip(universe, target))
    return propagate(donors, initial) is None


def truth(clause, valuation):
    return any(literal & (1 << value) for literal, value in zip(clause, valuation))


def entails(universe, donors, target):
    sizes = [full.bit_length() for full in universe]
    return all(not all(truth(donor, value) for donor in donors) or truth(target, value)
               for value in itertools.product(*(range(size) for size in sizes)))


def sequential_delete(universe, formula, order):
    remaining = list(range(len(formula)))
    for target in order:
        donors = [formula[i] for i in remaining if i != target]
        if refutable(universe, donors, formula[target]):
            remaining.remove(target)
    assert not any(refutable(universe, [formula[j] for j in remaining if j != i], formula[i])
                   for i in remaining)
    return tuple(remaining)


def exhaustive_checks():
    universe = (7, 7)
    clauses = [c for c in itertools.product(range(7), repeat=2) if any(c)]
    states = list(itertools.product(range(1, 8), repeat=2))
    comparable = [(small, large) for small in states for large in states if below(small, large)]
    monotonicity = 0
    support_checks = 0
    for donor in clauses:
        for state in states:
            assert donor_operation(donor, state) == support_projection(donor, state)
            support_checks += 1
        for small, large in comparable:
            assert below(donor_operation(donor, small), donor_operation(donor, large))
            monotonicity += 1

    target_pairs = 0
    donor_orders = 0
    refutations = 0
    for target in clauses:
        for donors in itertools.combinations(clauses, 2):
            initial = tuple(full & ~literal for full, literal in zip(universe, target))
            expected = propagate(donors, initial)
            refutations += expected is None
            if expected is None:
                assert entails(universe, donors, target)
            for ordered in (donors, donors[::-1]):
                result = virtual_once(ordered, target, universe)
                assert result["refuted"] == (expected is None)
                if expected is not None:
                    assert tuple(result["domains"]) == expected
                donor_orders += 1
            target_pairs += 1

    # Each donor coordinate physically lies in its corresponding context unit.
    # Use all proper units and the unrestricted/no-unit option on either symbol.
    unit_cases = 0
    for restrictions in itertools.product(range(1, 8), repeat=2):
        units = [tuple(value if j == i else 0 for j in range(2))
                 for i, value in enumerate(restrictions) if value != 7]
        nonunits = [c for c in clauses if all(c) and below(c, restrictions)]
        for target in clauses:
            if any(all(literal & ~forbidden == 0 for literal, forbidden in zip(unit, target))
                   for unit in units):
                continue
            for donors in itertools.combinations(nonunits, 2):
                initial = tuple(full & ~literal for full, literal in zip(universe, target))
                unrestricted = propagate(donors, initial)
                restricted = propagate(tuple(units) + donors, initial)
                assert (unrestricted is None) == (restricted is None)
                if unrestricted is not None:
                    assert restricted == tuple(value & unit for value, unit in
                                               zip(unrestricted, restrictions))
                unit_cases += 1
    return dict(gac_support_projection_checks=support_checks,
                monotone_operation_pairs=monotonicity, target_donor_pairs=target_pairs,
                donor_order_checks=donor_orders, refutations=refutations,
                unit_omission_cases=unit_cases)


def directed_examples():
    # Two mutually redundant target clauses: deleting one makes the other necessary.
    # A=x|y, B=x|z, C=!y|z, D=y|!z, where mask 2 is the positive literal.
    universe = (3, 3, 3)
    formula = [(2, 2, 0), (2, 0, 2), (0, 1, 2), (0, 2, 1)]
    initially = [i for i in range(4) if refutable(
        universe, formula[:i] + formula[i + 1:], formula[i])]
    assert initially == [0, 1]
    outputs = sorted(set(sequential_delete(universe, formula, order)
                         for order in itertools.permutations(range(4))))
    assert outputs == [(0, 2, 3), (1, 2, 3)]

    # A unit target on four-valued x; every donor x literal is a PROPER subset
    # of it. The y domain is restricted first to {0,1}, then to {1}.
    multi_universe = (15, 7, 7)
    multi_target = (3, 0, 0)
    multi_donors = [(1, 3, 0), (0, 6, 1), (0, 5, 2), (2, 0, 4)]
    multi_trace = virtual_once(multi_donors, multi_target, multi_universe)
    assert multi_trace["refuted"]
    assert [x.get("donor") for x in multi_trace["trace"]] == [0, 3, 1, 2]
    assert entails(multi_universe, multi_donors, multi_target)

    # The SSE2 oneend/oneend arrangement: both donors differ only on s,
    # overlap there only in the target, and jointly imply the whole target.
    oneend_target = (2, 7)
    oneend_donors = [(3, 1), (6, 2)]
    assert refutable((7, 15), oneend_donors, oneend_target)
    assert entails((7, 15), oneend_donors, oneend_target)

    # Omitting a context unit fails if donor containment is absent.
    # T=(x=0), U=(y=0), D=(x=0 or y=1).
    omitted_units_universe = (3, 3)
    omitted_units_target = (1, 0)
    omitted_units_donors = [(1, 2)]
    assert not refutable(omitted_units_universe, omitted_units_donors, omitted_units_target)
    assert refutable(omitted_units_universe, [(0, 1)] + omitted_units_donors,
                     omitted_units_target)

    # Initial subsumption is a necessary separate entry check for count-one code.
    no_zero = virtual_once([(1, 1)], (1, 3), (7, 7), initial_zero_check=False)
    assert not no_zero["refuted"]
    assert refutable((7, 7), [(1, 1)], (1, 3))

    # Logical entailment can require branching: all four clauses force x but
    # after assuming !x every donor still has two possible Boolean literals.
    entailed_target = (2, 0, 0)
    hard_donors = [(2, y, z) for y in (1, 2) for z in (1, 2)]
    assert entails((3, 3, 3), hard_donors, entailed_target)
    assert not refutable((3, 3, 3), hard_donors, entailed_target)

    # Naive Boolean value indicators with ordinary unit propagation miss a
    # ternary set-intersection step. The finite-domain oracle handles it.
    # q in {0,1}, q in {1,2}, and q in {0,2} have no common value.
    assert propagate([(3,), (6,), (5,)], (7,)) is None
    direct_encoding = [(1, 2, 3), (-1, -2), (-1, -3), (-2, -3),
                       (1, 2), (2, 3), (1, 3)]
    assert boolean_unit_propagation(direct_encoding) == set()
    assert not any(all(any((literal > 0) == value[abs(literal) - 1] for literal in clause)
                       for clause in direct_encoding)
                   for value in itertools.product((False, True), repeat=3))
    return dict(deletion_order=dict(initially_refutable=initially, distinct_outputs=outputs),
                multivalued_chain=multi_trace,
                sse2_oneend_oneend="two-step domain refutation checked",
                naive_value_indicator_encoding="unsatisfiable without Boolean UP conflict",
                unit_omission_without_containment="counterexample checked",
                initial_zero_without_entry_check="counterexample checked",
                entailment_without_domain_refutation="counterexample checked")


if __name__ == "__main__":
    result = dict(exhaustive=exhaustive_checks(), examples=directed_examples())
    (HERE / "mathematical_results.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))
