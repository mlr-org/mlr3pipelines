"""Focused checks of units, larger hyperedges, range blocks, and Horn signs."""

import collections
import itertools
import json
import time
from probe import (HERE, Bridge, assignments, truth, check_output, cycle_ranks,
                   opportunities, redundancy_opportunities, canonical)


def partition_blocks(bridge):
    domains = {s: ["a0", "a1", "b0", "b1", "c0", "c1"] for s in "xy"}
    blocks = [[letter + "0", letter + "1"] for letter in "abc"]
    clauses = [{"x": a, "y": b} for a, b in itertools.product(blocks, repeat=2)]
    stats = collections.Counter()
    for mask in range(1 << len(clauses)):
        body = [c for i, c in enumerate(clauses) if mask & (1 << i)]
        for units in ([], [{"x": blocks[0]}]):
            result, _, unsat = check_output(bridge, domains, body + units)
            stats["cases"] += 1
            stats["unsatisfiable"] += unsat
            stats["recognized_unsatisfiable"] += result is False
    return dict(stats)


def larger_forest_clauses(bridge):
    domains = {s: ["0", "1"] for s in "abcde"}
    stats = collections.Counter()
    for signs in itertools.product(("0", "1"), repeat=6):
        body = [dict(zip("abc", [[s] for s in signs[:3]])),
                dict(zip("cde", [[s] for s in signs[3:]]))]
        for units in ([], [{"a": ["0"]}, {"e": ["1"]}],
                      [{"a": ["0"]}, {"b": ["1"]}, {"e": ["1"]}]):
            clauses = body + units
            assert cycle_ranks(clauses) == [0]
            _, certificates, unsat = check_output(
                bridge, domains, clauses, check_sat=True, all_values=True)
            stats["cases"] += 1
            stats["unsatisfiable"] += unsat
            stats.update({"essential_" + k: v for k, v in certificates.items()})
    return dict(stats)


def signed_horn(bridge):
    domains = {s: ["0", "1"] for s in "abcd"}
    body = [{"a": ["0"], "b": ["0"], "c": ["1"]},
            {"c": ["0"], "d": ["1"]},
            {"d": ["0"], "a": ["1"]}]
    rows = assignments(domains)
    stats = collections.Counter()
    for unit_states in itertools.product((None, "0", "1"), repeat=4):
        units = [{s: [v]} for s, v in zip(domains, unit_states) if v is not None]
        for flip in itertools.product((False, True), repeat=4):
            flip_map = dict(zip(domains, flip))
            renamed = [{s: [str(1 - int(v[0])) if flip_map[s] else v[0]]
                        for s, v in clause.items()} for clause in body + units]
            for clauses in (renamed, list(reversed(renamed))):
                expected = truth(clauses, rows)
                result = bridge.simplify(domains, clauses)["result"]
                assert truth(result, rows) == expected
                assert (result is False) == (not any(expected))
                stats["cases"] += 1
                stats["unsatisfiable"] += not any(expected)
    return dict(stats)


def overlapping_pseudoforests(bridge):
    domains = {s: ["0", "1", "2"] for s in "xyz"}
    ranges = [list(c) for size in (1, 2) for c in itertools.combinations(domains["x"], size)]
    stats = collections.Counter()
    findings = []
    for a, b, c in itertools.product(range(6), repeat=3):
        body = [{"x": ranges[a], "y": ranges[b]},
                {"y": ranges[c], "z": ranges[(a + 1) % 6]},
                {"z": ranges[(b + 3) % 6], "x": ranges[(c + 2) % 6]}]
        for unit in [None] + ranges:
            clauses = body + ([{"x": unit}] if unit is not None else [])
            assert cycle_ranks(clauses) == [1]
            rows = assignments(domains)
            expected = truth(clauses, rows)
            result = bridge.simplify(domains, clauses)["result"]
            assert truth(result, rows) == expected
            assert (result is False) == (not any(expected))
            pending = opportunities(result)
            pending_hla = redundancy_opportunities(domains, result)
            second = bridge.simplify(domains, result)["result"]
            changed = canonical(second) != canonical(result)
            if pending or pending_hla or changed:
                findings.append(dict(domains=domains, clauses=clauses, result=result,
                                     pending=pending, pending_hla=pending_hla, second=second))
            stats["cases"] += 1
            stats["unsatisfiable"] += not any(expected)
            stats["residual_rule"] += bool(pending or pending_hla)
            stats["second_productive"] += changed
    (HERE / "pseudoforest_findings.json").write_text(json.dumps(findings, indent=2) + "\n")
    return dict(stats)


bridge = Bridge()
report = {}
start = time.time()
try:
    for name, operation in (("partition_blocks", partition_blocks),
                            ("larger_forest_clauses", larger_forest_clauses),
                            ("signed_horn", signed_horn),
                            ("overlapping_pseudoforests", overlapping_pseudoforests)):
        report[name] = operation(bridge)
        print(name + ": " + json.dumps(report[name]), flush=True)
        (HERE / "controls_results.json").write_text(json.dumps(report, indent=2) + "\n")
finally:
    bridge.close()
report["seconds"] = time.time() - start
(HERE / "controls_results.json").write_text(json.dumps(report, indent=2) + "\n")
print(json.dumps(report), flush=True)
