"""Constructed graph classes, direct truth tables, and independent rule tests."""

import collections
import itertools
import json
import pathlib
import sys
import time

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "repeated_passes"))
from repeated import Bridge, canonical, normalize
from oracle import OneHotOracle, MDDOracle, evaluate, values
from fixed_point import opportunities
from domain_refutation import redundancy_opportunities


def assignments(domains):
    return [dict(zip(domains, row)) for row in itertools.product(*domains.values())]


def truth(formula, rows):
    return tuple(evaluate(formula, row) for row in rows)


def cycle_ranks(formula):
    if isinstance(formula, bool):
        return []
    graph = collections.defaultdict(set)
    for i, clause in enumerate(formula):
        for s in clause:
            graph[("c", i)].add(("s", s))
            graph[("s", s)].add(("c", i))
    unseen = set(graph)
    result = []
    while unseen:
        start = unseen.pop()
        component, pending = {start}, [start]
        while pending:
            for node in graph[pending.pop()]:
                if node not in component:
                    component.add(node)
                    unseen.remove(node)
                    pending.append(node)
        edges = sum(len(graph[node]) for node in component) // 2
        result.append(edges - len(component) + 1)
    return sorted(result)


def essentiality(formula, rows):
    """Return a witness against every clause/value deletion, or the first gap."""
    if isinstance(formula, bool):
        return dict(clauses=0, values=0)
    formula = normalize(formula)
    initial = truth(formula, rows)
    counts = dict(clauses=0, values=0)
    for i, clause in enumerate(formula):
        weakened = formula[:i] + formula[i + 1:]
        assert initial != truth(weakened, rows), ("redundant clause", formula, i)
        counts["clauses"] += 1
        for symbol, domain in clause.items():
            for value in domain:
                reduced = dict(clause)
                reduced[symbol] = [v for v in domain if v != value]
                if not reduced[symbol]:
                    del reduced[symbol]
                strengthened = list(formula)
                strengthened[i] = reduced
                assert initial != truth(strengthened, rows), (
                    "redundant value", formula, i, symbol, value)
                counts["values"] += 1
    return counts


def forcing_cycle(size, forced="1", stem="a"):
    assert size >= 3
    internal = [stem + str(i) for i in range(1, size)]
    result = [{"x": [forced], internal[0]: ["1"]}]
    result += [{a: ["0"], b: ["1"]} for a, b in zip(internal, internal[1:])]
    result += [{internal[-1]: ["0"], "x": [forced]}]
    return result


def check_output(bridge, domains, clauses, check_sat=False, all_values=False):
    rows = assignments(domains)
    before = truth(clauses, rows)
    answer = bridge.simplify(domains, clauses)
    result = answer["result"]
    assert truth(result, rows) == before, (domains, clauses, answer)
    assert not opportunities(result), ("local rule", domains, clauses, answer)
    assert not redundancy_opportunities(domains, result), ("hla", domains, clauses, answer)
    second = bridge.simplify(domains, result)["result"]
    assert canonical(second) == canonical(result), ("second pass", domains, clauses, result, second)
    if check_sat:
        assert (result is False) == (not any(before)), ("missed unsat", domains, clauses, answer)
    certificates = essentiality(result, rows) if all_values else {}
    return result, certificates, not any(before)


def counterexamples(bridge):
    records = []
    for a, b in ((3, 3), (3, 4), (4, 4), (4, 5), (5, 5), (4, 8)):
        clauses = forcing_cycle(a, "1", "a") + forcing_cycle(b, "0", "b")
        domains = {s: ["0", "1"] for c in clauses for s in c}
        sat, mdd = OneHotOracle(domains), MDDOracle(domains)
        assert sat.difference(clauses, False) is None
        assert mdd.equivalent(clauses, False)
        # Deleting any one clause must restore satisfiability.
        for i in range(len(clauses)):
            assert sat.difference(clauses[:i] + clauses[i + 1:], False) is not None
        answer = bridge.simplify(domains, clauses, audit=True)
        result = answer["result"]
        assert sat.difference(clauses, result) is None
        assert not opportunities(result)
        assert not redundancy_opportunities(domains, result)
        assert canonical(bridge.simplify(domains, result)["result"]) == canonical(result)
        if min(a, b) >= 4:
            assert canonical(clauses) == canonical(result)
        else:
            assert result is False
        records.append(dict(cycle_sizes=[a, b], domains=domains, clauses=clauses,
                            result=result, incidence_cycle_ranks=cycle_ranks(clauses),
                            events=answer["events"]))
        print("opposed cycles %d,%d: returned %s" %
              (a, b, "FALSE" if result is False else "unchanged unsatisfiable clauses"), flush=True)
    (HERE / "opposed_cycles.json").write_text(json.dumps(records, indent=2) + "\n")


def exhaustive_boolean_binary(bridge):
    domains = {s: ["0", "1"] for s in "xyz"}
    universe = [{a: [av], b: [bv]}
                for a, b in itertools.combinations(domains, 2)
                for av, bv in itertools.product(("0", "1"), repeat=2)]
    stats = collections.Counter()
    for mask in range(1 << len(universe)):
        clauses = [c for i, c in enumerate(universe) if mask & (1 << i)]
        for ordered in (clauses, list(reversed(clauses))):
            result, _, unsat = check_output(bridge, domains, ordered)
            stats["cases"] += 1
            stats["unsatisfiable"] += unsat
            stats["recognized_unsatisfiable"] += result is False
        if (mask + 1) % 512 == 0:
            print("Boolean binary masks %d/4096; %s" % (mask + 1, dict(stats)), flush=True)
    return dict(stats)


def ternary_forests(bridge):
    domains = {s: ["0", "1", "2"] for s in "xyz"}
    ranges = [list(c) for size in (1, 2) for c in itertools.combinations(domains["x"], size)]
    stats = collections.Counter()
    unit_choices = ([], [{"x": ["0", "1"]}],
                    [{"x": ["0", "1"]}, {"z": ["1", "2"]}],
                    [{"y": ["0", "1"]}, {"y": ["1", "2"]}])
    for index, selected in enumerate(itertools.product(ranges, repeat=4)):
        body = [{"x": selected[0], "y": selected[1]},
                {"y": selected[2], "z": selected[3]}]
        # Every edge labeling gets every directed boundary condition.
        for units in unit_choices:
            clauses = body + units
            assert all(rank == 0 for rank in cycle_ranks(clauses))
            result, certificates, unsat = check_output(
                bridge, domains, clauses, check_sat=True, all_values=True)
            stats["cases"] += 1
            stats["unsatisfiable"] += unsat
            stats.update({"essential_" + k: v for k, v in certificates.items()})
        if (index + 1) % 216 == 0:
            print("ternary forest labels %d/1296; %s" % (index + 1, dict(stats)), flush=True)
    return dict(stats)


def main():
    bridge = Bridge()
    started = time.time()
    report = {}
    try:
        counterexamples(bridge)
        report["boolean_binary"] = exhaustive_boolean_binary(bridge)
        (HERE / "probe_results.json").write_text(json.dumps(report, indent=2) + "\n")
        report["ternary_forests"] = ternary_forests(bridge)
        report["seconds"] = time.time() - started
        (HERE / "probe_results.json").write_text(json.dumps(report, indent=2) + "\n")
    finally:
        bridge.close()
    print(json.dumps(report), flush=True)


if __name__ == "__main__":
    main()
