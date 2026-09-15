#!/usr/bin/env python3
"""Independent finite lower-bound check, using multigraphs and truth tables.

No SAT solver, graph library, sibling graph oracle, or production R code is
imported here. Run from any working directory; results are written beside
this file. The accompanying proof explains why its symmetry reductions cover
every minimally unsatisfiable, short-seed-free proper Boolean binary formula.
"""

import argparse
from collections import Counter, deque
from itertools import combinations, product
import json
from pathlib import Path
import time


DIRECTORY = Path(__file__).resolve().parent


def graph(clauses, n):
    adjacency = [set() for _ in range(2 * n)]
    for a, b in clauses:
        assert a // 2 != b // 2
        adjacency[a ^ 1].add(b)
        adjacency[b ^ 1].add(a)
    return adjacency


def complement_distances(clauses, n):
    adjacency = graph(clauses, n)
    distances = []
    for start in range(2 * n):
        seen = {start: 0}
        queue = deque([start])
        while queue:
            u = queue.popleft()
            for v in adjacency[u]:
                if v not in seen:
                    seen[v] = seen[u] + 1
                    queue.append(v)
        distances.append(seen.get(start ^ 1))
    return distances


def has_short_seed(clauses, n):
    adjacency = graph(clauses, n)
    for start in range(2 * n):
        level = {start}
        for _ in range(3):
            level = set().union(*(adjacency[u] for u in level))
            if start ^ 1 in level:
                return True
    return False


def model_count(clauses, n):
    return sum(
        all(any(assignment[lit // 2] == lit % 2 for lit in clause)
            for clause in clauses)
        for assignment in product(range(2), repeat=n)
    )


def false_masks(n):
    masks = {}
    for a, b in combinations(range(2 * n), 2):
        if a // 2 == b // 2:
            continue
        masks[a, b] = sum(
            1 << assignment
            for assignment in range(1 << n)
            if ((assignment >> (a // 2)) & 1) != a % 2
            and ((assignment >> (b // 2)) & 1) != b % 2
        )
    return masks


def is_unsatisfiable(clauses, masks, n):
    covered = 0
    for clause in clauses:
        covered |= masks[clause]
    return covered == (1 << (1 << n)) - 1


def connected(edges, n):
    seen = {0}
    changed = True
    while changed:
        changed = False
        for a, b in edges:
            if (a in seen) != (b in seen):
                seen.update((a, b))
                changed = True
    return len(seen) == n


def multigraphs(n, m, counters):
    """All degree-sorted connected multigraphs, edge multiplicities 1 or 2."""
    pairs = tuple(combinations(range(n), 2))
    for edge_count in range((m + 1) // 2, min(m, len(pairs)) + 1):
        for edges in combinations(pairs, edge_count):
            support_degrees = [0] * n
            for a, b in edges:
                support_degrees[a] += 1
                support_degrees[b] += 1
            if 0 in support_degrees or not connected(edges, n):
                continue
            for doubled in combinations(range(edge_count), m - edge_count):
                degrees = support_degrees.copy()
                for i in doubled:
                    a, b = edges[i]
                    degrees[a] += 1
                    degrees[b] += 1
                if min(degrees) < 2 or degrees != sorted(degrees):
                    continue
                counters["multigraphs"] += 1
                yield edges, frozenset(doubled)


def signings(edges, doubled, n, counters):
    """Fix the first literal of each variable to zero by variable complementation.

    Only the first clause on a doubled edge needs signs: its mate is the exact
    literal complement, since every other distinct pair creates a 2-edge seed.
    """
    first = set()
    free = []
    flat_variables = [v for edge in edges for v in edge]
    for position, v in enumerate(flat_variables):
        if v not in first:
            first.add(v)
        else:
            free.append(position)
    assert len(first) == n
    for bits in product(range(2), repeat=len(free)):
        counters["gauge_fixed_signings"] += 1
        signs = [0] * (2 * len(edges))
        for position, bit in zip(free, bits):
            signs[position] = bit
        clauses = []
        polarities = [0] * n
        for i, (a, b) in enumerate(edges):
            x, y = 2 * a + signs[2 * i], 2 * b + signs[2 * i + 1]
            clauses.append((x, y))
            polarities[a] |= 1 << (x % 2)
            polarities[b] |= 1 << (y % 2)
            if i in doubled:
                clauses.append((x ^ 1, y ^ 1))
                polarities[a] = polarities[b] = 3
        if all(mask == 3 for mask in polarities):
            counters["signings_without_pure_literals"] += 1
            yield tuple(clauses)


def parity_cycle(n):
    clauses = []
    for i in range(n - 1):
        # Equality of successive variables.
        clauses.extend(((2 * i, 2 * (i + 1) + 1),
                        (2 * i + 1, 2 * (i + 1))))
    # Opposite endpoint values contradict the chain of equalities.
    clauses.extend(((0, 2 * (n - 1)), (1, 2 * (n - 1) + 1)))
    return tuple(clauses)


def calibrate():
    calibrations = []
    for n in (2, 3, 4):
        clauses = parity_cycle(n)
        distances = complement_distances(clauses, n)
        assert model_count(clauses, n) == 0
        assert is_unsatisfiable(clauses, false_masks(n), n)
        assert distances == [n] * (2 * n)
        assert has_short_seed(clauses, n) == (n <= 3)
        deletion_counts = [model_count(clauses[:i] + clauses[i + 1:], n)
                           for i in range(len(clauses))]
        assert deletion_counts == [1] * (2 * n)
        calibrations.append({"variables": n, "clauses": len(clauses),
                             "complement_distances": distances,
                             "one_clause_deletion_model_counts": deletion_counts})
    # An unsatisfiable 5-clause core can repeat a clause between opposing
    # complement paths: counting their lengths as distinct clauses is invalid.
    repeated = ((0, 2), (1, 2), (3, 4), (0, 5), (1, 5))
    assert model_count(repeated, 3) == 0
    assert all(model_count(repeated[:i] + repeated[i + 1:], 3) > 0
               for i in range(len(repeated)))
    # Exhaustively calibrate the independent truth-table bitset on every
    # proper clause subset over two variables, including the empty formula.
    all_two = tuple(false_masks(2))
    for size in range(len(all_two) + 1):
        for clauses in combinations(all_two, size):
            assert is_unsatisfiable(clauses, false_masks(2), 2) == (
                model_count(clauses, 2) == 0)
    return calibrations


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--min-clauses", type=int, default=1)
    parser.add_argument("--max-clauses", type=int, default=7)
    parser.add_argument("--min-variables", type=int, default=2)
    parser.add_argument("--max-variables", type=int, default=7)
    parser.add_argument("--output", default="results.json")
    args = parser.parse_args()
    start = time.monotonic()
    result = {"calibrations": calibrate(), "scopes": []}
    for m in range(args.min_clauses, args.max_clauses + 1):
        for n in range(args.min_variables, min(m, args.max_variables) + 1):
            counters = Counter()
            masks = false_masks(n)
            witnesses = []
            for edges, doubled in multigraphs(n, m, counters):
                for clauses in signings(edges, doubled, n, counters):
                    if has_short_seed(clauses, n):
                        continue
                    counters["short_seed_free_signings"] += 1
                    if is_unsatisfiable(clauses, masks, n):
                        counters["unrecognized_contradictions"] += 1
                        if len(witnesses) < 3:
                            assert model_count(clauses, n) == 0
                            witnesses.append(clauses)
            scope = {"clauses": m, "variables": n,
                     "counts": dict(counters), "witnesses": witnesses}
            result["scopes"].append(scope)
            print(json.dumps(scope), flush=True)
    result["elapsed_seconds"] = time.monotonic() - start
    result["totals"] = dict(sum((Counter(scope["counts"])
                                 for scope in result["scopes"]), Counter()))
    result["requested_bounds"] = vars(args)
    (DIRECTORY / args.output).write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps({"totals": result["totals"],
                      "elapsed_seconds": result["elapsed_seconds"]}), flush=True)


if __name__ == "__main__":
    main()
