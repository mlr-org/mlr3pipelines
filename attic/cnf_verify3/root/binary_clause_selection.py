"""Finite clause-selection encoding of seed-free contradictory Boolean 2-CNF.

UNSAT is encoded by covering every truth assignment with a falsified selected
clause. Short implication paths are directly forbidden clause subsets. Neither
constraint uses an R simplification result or another SAT encoding of 2-SAT.
"""
import argparse
import itertools
import json
from pathlib import Path
import time

import z3

HERE = Path(__file__).resolve().parent


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--symbols", type=int, default=8)
    parser.add_argument("--max-clauses", type=int, default=8)
    parser.add_argument("--min-clauses", type=int, default=4)
    parser.add_argument("--minimal-core", action="store_true")
    args = parser.parse_args()
    n = args.symbols
    literals = range(2 * n)
    clauses = [(a, b) for a, b in itertools.combinations(literals, 2) if a // 2 != b // 2]
    index = {clause: i for i, clause in enumerate(clauses)}
    selected = [z3.Bool(f"c{i}") for i in range(len(clauses))]
    solver = z3.Solver()
    solver.set(timeout=120000)
    # Every assignment must falsify at least one selected clause.
    for assignment in itertools.product((0, 1), repeat=n):
        false_clauses = [selected[i] for i, (a, b) in enumerate(clauses)
                         if assignment[a // 2] != a % 2 and assignment[b // 2] != b % 2]
        solver.add(z3.Or(false_clauses))
    edge_clause = {(a ^ 1, b): i for i, (a, b) in enumerate(clauses)}
    edge_clause.update({(b ^ 1, a): i for i, (a, b) in enumerate(clauses)})
    forbidden = set()
    for start in literals:
        end = start ^ 1
        for middle in literals:
            if (start, middle) in edge_clause and (middle, end) in edge_clause:
                forbidden.add(frozenset((edge_clause[start, middle], edge_clause[middle, end])))
            for second in literals:
                edges = ((start, middle), (middle, second), (second, end))
                if all(edge in edge_clause for edge in edges):
                    forbidden.add(frozenset(edge_clause[edge] for edge in edges))
    for combination in sorted(forbidden, key=lambda x: (len(x), tuple(sorted(x)))):
        solver.add(z3.Not(z3.And([selected[i] for i in combination])))
    # Symbol/polarity renaming lets us require one particular proper clause.
    solver.add(selected[index[0, 2]])
    if args.minimal_core:
        # A used variable of a minimally unsatisfiable core cannot be pure.
        # This option concerns exactly n used variables, not unused padding.
        for literal in literals:
            solver.add(z3.Or([selected[i] for i, clause in enumerate(clauses) if literal in clause]))
    base_constraints = len(solver.assertions())
    reports = []
    for bound in range(args.min_clauses, args.max_clauses + 1):
        solver.push()
        solver.add(z3.PbLe([(choice, 1) for choice in selected], bound))
        started = time.monotonic()
        verdict = solver.check()
        report = dict(symbols=n, clause_bound=bound, verdict=str(verdict),
                      seconds=time.monotonic() - started)
        if verdict == z3.sat:
            model = solver.model()
            chosen = [clauses[i] for i, choice in enumerate(selected) if z3.is_true(model.eval(choice))]
            report["signed_clauses"] = [[(v // 2 + 1) * (1 if v % 2 else -1) for v in clause]
                                         for clause in chosen]
            report["clauses"] = [{f"X{v // 2}": [str(v % 2)] for v in clause} for clause in chosen]
            report["domains"] = {f"X{s}": ["0", "1"] for s in range(n)}
            # Direct ordinary graph calibration of every extracted witness.
            graph = {literal: set() for literal in literals}
            for a, b in chosen:
                graph[a ^ 1].add(b)
                graph[b ^ 1].add(a)
            for start in literals:
                reached = {start}
                for unused in range(3):
                    reached |= {v for u in tuple(reached) for v in graph[u]}
                assert start ^ 1 not in reached
            assert all(any(assignment[a // 2] != a % 2 and assignment[b // 2] != b % 2
                           for a, b in chosen)
                       for assignment in itertools.product((0, 1), repeat=n))
        elif verdict == z3.unknown:
            report["reason"] = solver.reason_unknown()
        reports.append(report)
        print(json.dumps(report), flush=True)
        solver.pop()
        if verdict == z3.sat:
            break
    result = dict(reports=reports, symbols=n, selected_clause_variables=len(clauses),
                  every_symbol_both_polarities=args.minimal_core,
                  short_path_forbidden_subsets=len(forbidden),
                  base_assertions=base_constraints, z3_version=z3.get_version_string(),
                  scope="Proper binary clauses; no short complement path anywhere; exact full assignment coverage")
    suffix = "_core" if args.minimal_core else ""
    (HERE / f"binary_clause_selection_n{n}{suffix}.json").write_text(json.dumps(result, indent=2) + "\n")


if __name__ == "__main__":
    main()
