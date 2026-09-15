"""Set-algebra challenges for local three-clause saturation proof steps.

Each coordinate uses arbitrary subsets of eight values. A local step contains
only three pre-state sets at that coordinate and deterministic set operations,
so its membership quotient has at most eight atoms. Countermodels over ordinary
finite domains therefore lift to this exact local bound. This program checks
individual algebraic obligations; it does not model the production scheduler.
"""

import itertools
import json
from pathlib import Path
import z3

HERE = Path(__file__).resolve().parent
WIDTH = 8
ZERO = z3.BitVecVal(0, WIDTH)
ONE = z3.BitVecVal(1, WIDTH)
S = [[z3.BitVec("c%d_s%d" % (c, s), WIDTH) for s in range(3)] for c in range(3)]
A, B, T = 0, 1, 2


def subset(left, right):
    return left & ~right == ZERO


def clause_subset(state, left, right):
    return z3.And(*(subset(state[left][s], state[right][s]) for s in range(3)))


def sse1(state, donor, target, restricted):
    premise = z3.And(*(subset(state[donor][s], state[target][s])
                       for s in range(3) if s != restricted))
    return premise, state[donor][restricted]


def sse2(state, donor_a, donor_b, target, pivot, restricted):
    premise = z3.And(
        *(subset(state[d][s], state[target][s]) for d in (donor_a, donor_b)
          for s in range(3) if s not in (pivot, restricted)),
        subset(state[donor_a][pivot] & state[donor_b][pivot], state[target][pivot]))
    return premise, state[donor_a][restricted] | state[donor_b][restricted]


def relay(state):
    return z3.And(
        subset(state[A][0], state[B][0]),
        state[T][0] & ONE != ZERO, state[B][0] & ONE == ZERO,
        z3.Not(subset(state[A][0], state[T][0])),
        *(subset(state[B][s], state[A][s]) for s in (1, 2)),
        *(subset(state[A][s], state[T][s]) for s in (1, 2)),
        z3.Or(*(z3.Not(subset(state[A][s], state[B][s])) for s in (1, 2))),
        *(state[B][s] != ZERO for s in (1, 2)))


def main():
    report = dict(scope="exact local set implications; no execution-schedule model",
                  set_width=WIDTH, obligations=[], countermodels=[])

    def prove(name, assumptions, counterexample):
        solver = z3.Solver()
        solver.add(assumptions, counterexample)
        answer = solver.check()
        report["obligations"].append(dict(name=name, result=str(answer)))
        if answer != z3.unsat:
            report["countermodels"].append(dict(name=name, model=str(solver.model())))

    # With a value present in A,T but absent from B at symbol 0, neither A/T
    # alone nor their combination can restrict B at either other symbol.
    lone_absence = z3.And(S[A][0] & ONE != ZERO, S[T][0] & ONE != ZERO,
                          S[B][0] & ONE == ZERO)
    for restricted in (1, 2):
        for donor in (A, T):
            premise, _ = sse1(S, donor, B, restricted)
            prove("lone_absence_sse1_%d_%d" % (donor, restricted), lone_absence, premise)
        for pivot in range(3):
            if pivot == restricted:
                continue
            premise, _ = sse2(S, A, T, B, pivot, restricted)
            prove("lone_absence_sse2_%d_%d" % (pivot, restricted), lone_absence, premise)

    # The two-donor SSE2 t-range omission requires a shared exceptional pivot.
    # If B alone can remove a witness from A_t by SSE1, then B_s is contained
    # in A_s. The independent SSE2 intersection premise makes B_s contained
    # in T_s, contrary to that required exceptional pivot.
    premise, _ = sse1(S, B, A, 1)
    prove("contained_donor_shrink_cannot_enable_shared_pivot", premise,
          z3.And(subset(S[A][0] & S[B][0], S[T][0]),
                 z3.Not(subset(S[B][0], S[T][0]))))

    # Check every direct clause deletion and range restriction against the
    # relay invariant. A/T deletion or a newly pending direct subsumption of
    # A/T resolves the putative final-survivor witness; so does removing v.
    for donor, target in itertools.permutations(range(3), 2):
        prove("relay_direct_delete_%d_%d" % (donor, target), relay(S),
              z3.And(clause_subset(S, donor, target), z3.BoolVal(target == B)))

    transitions = []
    for donor, target in itertools.permutations(range(3), 2):
        for restricted in range(3):
            premise, bound = sse1(S, donor, target, restricted)
            transitions.append(("sse1_%d_%d_%d" % (donor, target, restricted),
                                target, restricted, premise, bound))
    for target in range(3):
        donors = [i for i in range(3) if i != target]
        for pivot, restricted in itertools.permutations(range(3), 2):
            premise, bound = sse2(S, *donors, target, pivot, restricted)
            transitions.append(("sse2_%d_%d_%d" % (target, pivot, restricted),
                                target, restricted, premise, bound))

    for name, target, restricted, premise, bound in transitions:
        after = [list(clause) for clause in S]
        after[target][restricted] = S[target][restricted] & bound
        eliminates = subset(bound, S[target][restricted])
        resolved = z3.Or(
            z3.And(eliminates, z3.BoolVal(target != B)),
            z3.And(z3.Not(eliminates), z3.Or(
                relay(after), after[T][0] & ONE == ZERO,
                clause_subset(after, A, T), clause_subset(after, B, A),
                clause_subset(after, B, T))))
        prove("relay_" + name, z3.And(relay(S), premise), z3.Not(resolved))

    # After B removes v from A_0 by SSE1, B's other coordinates are contained
    # in A's. While A,B omit v and T retains it, this dominance survives every
    # possible nonunit range step. A restriction of A off coordinate 0 cannot
    # come from T alone; every other bound includes B's corresponding range.
    dominance = z3.And(
        S[A][0] & ONE == ZERO, S[B][0] & ONE == ZERO, S[T][0] & ONE != ZERO,
        *(subset(S[B][s], S[A][s]) for s in (1, 2)))
    for name, target, restricted, premise, bound in transitions:
        after = [list(clause) for clause in S]
        after[target][restricted] = S[target][restricted] & bound
        prove("persistent_off_pivot_dominance_" + name, z3.And(dominance, premise),
              z3.Or(*(z3.Not(subset(after[B][s], after[A][s])) for s in (1, 2))))

    (HERE / "local_transition_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(dict(obligations=len(report["obligations"]),
                          countermodels=len(report["countermodels"]))))
    assert not report["countermodels"]


if __name__ == "__main__":
    main()
