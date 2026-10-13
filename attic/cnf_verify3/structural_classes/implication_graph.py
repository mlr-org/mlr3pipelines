"""An independent proposed exact unit/contradiction oracle for Boolean 2-CNF."""

import collections
import itertools
import json
import time
from probe import HERE, Bridge, forcing_cycle, values


def opposite(literal):
    return literal[0], str(1 - int(literal[1]))


def characterize(domains, clauses):
    if isinstance(clauses, bool):
        return dict(contradiction=not clauses, units=[], short_seeds=[])
    graph = {(s, v): set() for s in domains for v in ("0", "1")}
    initial = set()
    for clause in clauses:
        literals = [(s, values(v)[0]) for s, v in clause.items()]
        if len(literals) == 1:
            initial.add(literals[0])
        else:
            assert len(literals) == 2
            a, b = literals
            graph[opposite(a)].add(b)
            graph[opposite(b)].add(a)
    short = set()
    for literal in graph:
        reached = {opposite(literal)}
        for _ in range(3):
            reached |= {other for before in tuple(reached) for other in graph[before]}
        if literal in reached:
            short.add(literal)
    closure = initial | short
    pending = list(closure)
    while pending:
        for other in graph[pending.pop()]:
            if other not in closure:
                closure.add(other)
                pending.append(other)
    return dict(contradiction=any(opposite(lit) in closure for lit in closure),
                units=sorted(closure), short_seeds=sorted(short),
                initial_units=sorted(initial))


def compare(bridge, domains, clauses, stats):
    prediction = characterize(domains, clauses)
    answer = bridge.simplify(domains, clauses)
    result = answer["result"]
    assert (result is False) == prediction["contradiction"], (
        domains, clauses, prediction, answer)
    if not isinstance(result, bool):
        actual_units = sorted((s, values(v)[0]) for c in result if len(c) == 1
                              for s, v in c.items())
        assert actual_units == prediction["units"], (
            domains, clauses, prediction, answer, actual_units)
    elif result is True:
        assert not prediction["units"]
    stats["cases"] += 1
    stats["contradiction"] += prediction["contradiction"]
    stats["short_seed_occurrences"] += len(prediction["short_seeds"])
    stats["closure_unit_occurrences"] += len(prediction["units"])
    return prediction


def main():
    bridge = Bridge()
    started = time.time()
    stats = collections.Counter()
    try:
        domains = {s: ["0", "1"] for s in "xyz"}
        pool = [{a: [av], b: [bv]}
                for a, b in itertools.combinations(domains, 2)
                for av, bv in itertools.product(("0", "1"), repeat=2)]
        states = list(itertools.product((None, "0", "1"), repeat=3))
        for mask in range(1 << len(pool)):
            clauses = [c for i, c in enumerate(pool) if mask & (1 << i)]
            compare(bridge, domains, clauses, stats)
            selected = states[mask % len(states)]
            units = [{s: [v]} for s, v in zip(domains, selected) if v is not None]
            compare(bridge, domains, list(reversed(clauses)) + units, stats)
            if (mask + 1) % 1024 == 0:
                print("masks %d/4096: %s" % (mask + 1, dict(stats)), flush=True)
        controls = []
        for p, q in itertools.product(range(3, 9), repeat=2):
            clauses = forcing_cycle(p, "1", "a") + forcing_cycle(q, "0", "b")
            domains = {s: ["0", "1"] for c in clauses for s in c}
            prediction = compare(bridge, domains, clauses, stats)
            assert prediction["contradiction"] == (min(p, q) == 3)
            controls.append(dict(cycle_sizes=[p, q], prediction=prediction))
        # Initial roots can trigger arbitrarily long implication propagation.
        for length in (4, 8, 16, 32, 64):
            symbols = ["x%d" % i for i in range(length)]
            domains = {s: ["0", "1"] for s in symbols}
            body = [{a: ["0"], b: ["1"]} for a, b in zip(symbols, symbols[1:])]
            for contradiction in (False, True):
                clauses = list(reversed(body)) + [{symbols[0]: ["1"]}]
                if contradiction:
                    clauses.append({symbols[-1]: ["0"]})
                prediction = compare(bridge, domains, clauses, stats)
                assert prediction["contradiction"] == contradiction
        report = dict(stats=stats, cycle_controls=controls, seconds=time.time() - started)
        (HERE / "implication_graph_results.json").write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(dict(stats=stats, seconds=report["seconds"])), flush=True)
    finally:
        bridge.close()


if __name__ == "__main__":
    main()
