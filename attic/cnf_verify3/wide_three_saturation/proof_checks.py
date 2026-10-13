"""Finite challenges to the support and coordinate lemmas of the wider proof.

The support enumeration is deliberately separate from production execution.
Z3 checks finite-set local implications with empty ranges allowed, so absent
symbols are covered. These checks do not encode callback scheduling.
"""
from collections import Counter
import itertools
import json
from pathlib import Path

import z3

HERE = Path(__file__).resolve().parent


def matching(supports):
    return any(len(set(choice)) == len(supports)
               for choice in itertools.product(*supports))


def support_checks():
    counts = Counter()
    for n in range(3, 7):
        symbols = tuple(range(n))
        supports = [frozenset(c) for width in range(3, n + 1)
                    for c in itertools.combinations(symbols, width)]
        for a, t, b in itertools.product(supports, repeat=3):
            assert matching((a, t, b))
            counts["satisfiable_support_triples"] += 1
            for q in range(n + 1):  # The final q is absent from every clause.
                failed = not matching((a - {q}, t - {q}, b - {q}))
                assert failed == (a == t == b and len(a) == 3 and q in a)
                counts["fixed_symbol_support_checks"] += 1
                counts["common_triple_matching_exceptions"] += failed
            if b <= a and b <= t and len(a) <= len(t) <= len(b):
                assert a == t == b
                counts["sorted_relay_support_checks"] += 1
    return counts


def unary_projection_checks():
    values = range(3)
    ranges = [frozenset(c) for width in (1, 2)
              for c in itertools.combinations(values, width)]
    families = list(itertools.product(ranges, repeat=3))
    counts = Counter()
    for left, right in itertools.product(families, repeat=2):
        overlaps = any(family[i] & family[j] for family in (left, right)
                       for i, j in itertools.combinations(range(3), 2))
        for fixed_membership in range(8):
            has_model = any(all(fixed_membership & (1 << i) or x in left[i] or y in right[i]
                                for i in range(3))
                            for x, y in itertools.product(values, repeat=2))
            assert has_model == (bool(fixed_membership) or overlaps)
            counts["common_triple_projection_checks"] += 1
            counts["fixed_values_excluded_by_disjoint_residual_ranges"] += not has_model
    return counts


def local_checks():
    width = 8
    zero, value = z3.BitVecVal(0, width), z3.BitVecVal(1, width)
    results = []

    def subset(a, b):
        return a & ~b == zero

    def prove(name, premises, conclusion):
        solver = z3.Solver()
        solver.add(premises, z3.Not(conclusion))
        answer = solver.check()
        results.append(dict(name=name, result=str(answer)))
        assert answer == z3.unsat, (name, answer, str(solver.model()))

    for n in (3, 4, 5):
        a, b, target = state = [[z3.BitVec(f"n{n}_c{i}_q{q}", width)
                               for q in range(n)] for i in range(3)]
        private = z3.And(a[0] & value == zero, b[0] & value == zero,
                         target[0] & value != zero)
        single_absence = z3.And(a[0] & value != zero, b[0] & value == zero,
                                target[0] & value != zero)
        transitions = []
        for donor, receiver in itertools.permutations(range(3), 2):
            for q in range(n):
                premise = z3.And(*(subset(state[donor][p], state[receiver][p])
                                   for p in range(n) if p != q))
                transitions.append((f"sse1_{donor}_{receiver}_{q}", receiver, q,
                                    premise, state[donor][q]))
        for receiver in range(3):
            left, right = [i for i in range(3) if i != receiver]
            for pivot, q in itertools.permutations(range(n), 2):
                premise = z3.And(
                    subset(state[left][pivot] & state[right][pivot], state[receiver][pivot]),
                    *(subset(state[d][p], state[receiver][p]) for d in (left, right)
                      for p in range(n) if p not in (pivot, q)))
                transitions.append((f"sse2_{left}_{right}_{receiver}_{pivot}_{q}",
                                    receiver, q, premise, state[left][q] | state[right][q]))

        for name, receiver, q, premise, bound in transitions:
            after = [list(c) for c in state]
            after[receiver][q] = state[receiver][q] & bound
            survives = after[2][0] & value != zero
            prove(f"n{n}_private_intersection_{name}", z3.And(private, premise, survives),
                  z3.And(*(after[0][p] & after[1][p] == a[p] & b[p]
                           for p in range(1, n))))
            if receiver == 1 and q != 0:
                prove(f"n{n}_single_absence_freezes_{name}", single_absence, z3.Not(premise))
            if receiver == 0 and q not in (0, 1):
                # Removing any outside coordinate is a special case of this
                # stronger premise: every legal bound already needs B_s <= A_s.
                prove(f"n{n}_outside_update_requires_pivot_dominance_{name}",
                      z3.And(private, premise), subset(b[1], a[1]))
            prove(f"n{n}_pivot_dominance_persists_{name}",
                  z3.And(private, subset(b[1], a[1]), premise, survives),
                  subset(after[1][1], after[0][1]))

        prove(f"n{n}_union_bound_preserves_target_pivot_containment",
              subset(a[1], target[1]), subset(a[1], target[1] & (a[1] | b[1])))
    return results


def main():
    report = dict(scope="bounded support checks and exact local set implications; no scheduler model",
                  support_counts=dict(support_checks()), local_set_width=8,
                  projection_counts=dict(unary_projection_checks()),
                  obligations=local_checks())
    (HERE / "proof_checks.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(dict(support_counts=report["support_counts"],
                          projection_counts=report["projection_counts"],
                          local_obligations=len(report["obligations"]))))


if __name__ == "__main__":
    main()
