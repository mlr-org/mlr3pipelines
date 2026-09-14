"""Independent matrix graph oracle, complete local guard checks, and R traces."""

import argparse
import collections
import itertools
import json
import pathlib
import random
import subprocess
import time

HERE = pathlib.Path(__file__).resolve().parent


def literals(clause):
    return frozenset((s, int(v[0]) if isinstance(v, list) else int(v)) for s, v in clause.items())


def clause_key(clause):
    return tuple(sorted(literals(clause)))


def complement(literal):
    return literal[0], literal[1] ^ 1


def matrix_characterization(symbols, clauses):
    """Bounded adjacency powers and Warshall closure, not the sibling's BFS."""
    if isinstance(clauses, bool):
        return set(), set(), not clauses
    vertices = [(s, v) for s in symbols for v in (0, 1)]
    index = {literal: i for i, literal in enumerate(vertices)}
    rows = [0] * len(vertices)
    initial = set()
    for clause in clauses:
        values = tuple(literals(clause))
        if len(values) == 1:
            initial.add(values[0])
            continue
        assert len(values) == 2
        a, b = values
        rows[index[complement(a)]] |= 1 << index[b]
        rows[index[complement(b)]] |= 1 << index[a]
    powers = list(rows)
    for _ in range(2):
        expanded = []
        for row in powers:
            extra = row
            for j, next_row in enumerate(rows):
                if row & (1 << j):
                    extra |= next_row
            expanded.append(extra)
        powers = expanded
    seeds = {literal for literal in vertices
             if powers[index[complement(literal)]] & (1 << index[literal])}
    reach = [row | (1 << i) for i, row in enumerate(rows)]
    for k in range(len(vertices)):
        for i in range(len(vertices)):
            if reach[i] & (1 << k):
                reach[i] |= reach[k]
    closure_bits = 0
    for literal in initial | seeds:
        closure_bits |= reach[index[literal]]
    closure = {literal for i, literal in enumerate(vertices) if closure_bits & (1 << i)}
    return seeds, closure, any(complement(literal) in closure for literal in closure)


def exceptions(source, target):
    return {s for s, value in source.items() if target.get(s) != value}


def local_guard_checks():
    counts = collections.Counter()
    for n in (2, 3, 4):
        symbols = tuple("rpqt"[:n])
        pool = [dict(zip(pair, values)) for pair in itertools.combinations(symbols, 2)
                for values in itertools.product((0, 1), repeat=2)]
        for donor, target in itertools.product(pool, repeat=2):
            mismatch = exceptions(donor, target)
            if not mismatch:
                assert donor == target
                counts["subsumptions_are_duplicates"] += 1
            if len(mismatch) != 1:
                continue
            pivot = next(iter(mismatch))
            if pivot not in target:
                counts["sse1_absent_pivot_noops"] += 1
                continue
            assert donor[pivot] == (target[pivot] ^ 1)
            surviving = next((s, v) for s, v in target.items() if s != pivot)
            seeds, closure, _ = matrix_characterization(symbols, [donor, target])
            assert surviving in seeds
            counts["sse1_two_edge_seeds"] += 1
        for one, two, target in itertools.product(pool, repeat=3):
            two_exceptions = exceptions(two, target)
            if len(two_exceptions) != 2:
                continue
            for intersect, restrict in itertools.permutations(two_exceptions):
                counts["twoend_orientations"] += 1
                if restrict not in target or intersect not in one:
                    continue
                if not exceptions(one, target) <= {intersect, restrict}:
                    continue
                # Exact inverse guards and raw intersection-outside-target guard.
                if one.get(restrict) == target[restrict] or two.get(restrict) == target[restrict]:
                    continue
                if one[intersect] == two[intersect] and one[intersect] != target.get(intersect):
                    continue
                assert restrict in two
                restricting_values = {clause[restrict] for clause in (one, two) if restrict in clause}
                assert restricting_values == {target[restrict] ^ 1}
                assert one != two
                assert len(set(one) | set(two) | set(target)) <= 3
                surviving = next((s, v) for s, v in target.items() if s != restrict)
                seeds, closure, _ = matrix_characterization(symbols, [one, two, target])
                assert surviving in closure
                counts["productive_sse2_patterns"] += 1
                counts["sse2_intersection_is_survivor" if intersect == surviving[0]
                       else "sse2_intersection_is_third_symbol"] += 1
                if restrict in one:
                    intermediate = (restrict, target[restrict] ^ 1)
                    short, _, _ = matrix_characterization(symbols, [one, two])
                    assert intermediate in short
                    counts["sse2_from_two_edge_seed"] += 1
                else:
                    assert surviving in seeds
                    assert intersect != surviving[0]
                    counts["sse2_three_edge_seeds"] += 1
    return counts


class Bridge:
    def __init__(self, r46=False):
        command = ["podman", "exec", "-i", "cnf-review-r46"] if r46 else []
        command += ["Rscript", str(HERE / "bridge.R") if not r46 else
                    "attic/cnf_verify3/boolean_graph_review/bridge.R"]
        self.process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                        text=True, bufsize=1)

    def simplify(self, symbols, clauses):
        domains = {s: ["0", "1"] for s in symbols}
        encoded = clauses if isinstance(clauses, bool) else [
            {s: [str(v[0] if isinstance(v, list) else v)] for s, v in clause.items()} for clause in clauses]
        self.process.stdin.write(json.dumps(dict(domains=domains, clauses=encoded)) + "\n")
        self.process.stdin.flush()
        line = self.process.stdout.readline()
        assert line, "R bridge terminated"
        answer = json.loads(line)
        assert "error" not in answer, (clauses, answer)
        return answer

    def close(self):
        self.process.stdin.close()
        assert self.process.wait(timeout=20) == 0


def unit_set(formula):
    return set() if isinstance(formula, bool) else {
        next(iter(literals(clause))) for clause in formula if len(clause) == 1}


def check_production(bridge, symbols, clauses, counts):
    seeds, closure, contradiction = matrix_characterization(symbols, clauses)
    answer = bridge.simplify(symbols, clauses)
    result = answer["result"]
    assert (result is False) == contradiction, (clauses, seeds, closure, answer)
    if result is not False:
        assert unit_set(result) == closure, (clauses, closure, answer)
    for event in answer["births"]:
        unit = next(iter(literals(event["unit"])))
        assert unit in closure, ("birth outside K", clauses, unit, closure)
        counts["unit_births_in_K"] += 1
    boundary = answer["boundary"]
    if boundary is not None:
        units = unit_set(boundary)
        assert units == unit_set(result) == closure
        binaries = {clause_key(clause) for clause in boundary if len(clause) == 2}
        for clause in clauses:
            clause_literals = literals(clause)
            if len(clause_literals) != 2:
                continue
            assert clause_literals & units or clause_key(clause) in binaries
            counts["original_edge_representation"] += 1
            for antecedent, consequent in ((complement(a), b)
                                           for a, b in itertools.permutations(clause_literals)):
                assert antecedent not in units or consequent in units
                counts["input_edge_closure"] += 1
        assert seeds <= units
        counts["boundaries_with_all_seeds"] += 1
        counts["units_preserved_through_hla"] += len(units)
    for key, value in answer["counts"].items():
        counts[key] += value
    counts["cases"] += 1
    counts["contradictions"] += contradiction
    return answer


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--r46", action="store_true")
    parser.add_argument("--random-cases", type=int, default=2000)
    args = parser.parse_args()
    started = time.time()
    local = local_guard_checks()
    print("Local guard checks:", dict(local), flush=True)
    counts = collections.Counter()
    bridge = Bridge(args.r46)
    try:
        # Every sign, clause order, and within-clause symbol order of the exact
        # two- and three-edge forcing patterns; these are independent source
        # scheduler checks, not just tests of the graph oracle.
        for length in (2, 3):
            symbols = ("r", "p") if length == 2 else ("r", "p", "q")
            for values in itertools.product((0, 1), repeat=length):
                r, p = values[:2]
                clauses = [dict(r=r, p=p), dict(p=p ^ 1, r=r)] if length == 2 else [
                    dict(r=r, p=p), dict(p=p ^ 1, q=values[2]), dict(q=values[2] ^ 1, r=r)]
                for ordered in itertools.permutations(clauses):
                    for orientations in itertools.product((False, True), repeat=length):
                        oriented = [dict(reversed(list(clause.items()))) if reverse else clause
                                    for clause, reverse in zip(ordered, orientations)]
                        answer = check_production(bridge, symbols, oriented, counts)
                        assert unit_set(answer["result"]) == {("r", r)}
                        counts["short_path_scheduler_cases"] += 1
        rng = random.Random(20260908)
        for iteration in range(args.random_cases):
            symbols = ["s%d" % i for i in range(rng.randint(3, 9))]
            clauses = []
            for _ in range(rng.randint(1, 3 * len(symbols))):
                pair = rng.sample(symbols, 2)
                clauses.append({s: rng.randrange(2) for s in pair})
            for _ in range(rng.randint(0, 3)):
                clauses.insert(rng.randrange(len(clauses) + 1), {rng.choice(symbols): rng.randrange(2)})
            check_production(bridge, symbols, clauses, counts)
            if iteration % 3 == 0:
                check_production(bridge, symbols, list(reversed(clauses)), counts)
            if (iteration + 1) % 500 == 0:
                print("Random inputs:", iteration + 1, "; cases:", counts["cases"], flush=True)
        # Explicit normalized constants require a separate graph-theorem case.
        for terminal in (True, False, [], [dict(r=0), dict(r=0)], [dict(r=0), dict(r=1)]):
            check_production(bridge, ["r"], terminal, counts)
        report = dict(runtime="r46" if args.r46 else "r36", local=dict(local),
                      production=dict(counts), seconds=time.time() - started)
        output = HERE / ("results_r46.json" if args.r46 else "results_r36.json")
        output.write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(report, indent=2), flush=True)
    finally:
        bridge.close()


if __name__ == "__main__":
    main()
