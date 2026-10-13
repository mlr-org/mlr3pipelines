"""Exhaustive three-donor calibration and an independent omitted-unit check."""
import collections
import itertools
import json
import time

from domain_refutation import refute_negated_clause
from horn_refutation import horn_refute, verify_horn_certificate
from oracle import HERE, evaluate


def compare(domains, donors, target, stats):
    horn = horn_refute(domains, donors, target)
    domain = refute_negated_clause(domains, donors, target)
    assert horn["refuted"] == domain["refuted"], (domains, donors, target, horn, domain)
    if horn["refuted"]:
        assert verify_horn_certificate(domains, donors, target, horn["trace"])
        for row in itertools.product(*domains.values()):
            assignment = dict(zip(domains, row))
            assert not evaluate(donors, assignment) or evaluate([target], assignment)
        stats["refutations"] += 1
    else:
        assert all(set(domains[s]) - set(horn["excluded"][s]) == set(domain["residual_domains"][s])
                   for s in domains), (donors, target, horn, domain)
    stats["queries"] += 1
    stats["horn_steps"] += len(horn["trace"])
    return horn


def main():
    start, stats = time.time(), collections.Counter()
    domains = {s: ["0", "1"] for s in "XYZ"}
    clauses = [dict((s, [str(v)]) for s, v in zip(domains, row) if v != 2)
               for row in itertools.product(range(3), repeat=3) if row != (2, 2, 2)]
    for size in range(4):
        for donors in itertools.combinations(clauses, size):
            for target in clauses:
                compare(domains, list(donors), target, stats)
        print(json.dumps(dict(donors=size, stats=stats, elapsed=time.time() - start)), flush=True)

    omitted_stats = collections.Counter()
    domains = {s: ["0", "1", "2"] for s in "XY"}
    units = [{"X": ["0", "1"]}, {"Y": ["0", "1"]}]
    ranges = [["0"], ["1"], ["0", "1"]]
    donors = [dict(zip(domains, row)) for row in itertools.product(ranges, repeat=2)]
    targets = [dict((s, r) for s, r in zip(domains, row) if r)
               for row in itertools.product([[], ["0"], ["1"]], repeat=2) if any(row)]
    for size in range(4):
        for selected in itertools.combinations(donors, size):
            for target in targets + units:
                omitted = [u for u in units if u != target]
                before = compare(domains, list(selected), target, omitted_stats)
                after = compare(domains, list(selected) + omitted, target, omitted_stats)
                assert before["refuted"] == after["refuted"], (selected, target, before, after)
                if not before["refuted"]:
                    for symbol in domains:
                        p = set(domains[symbol]) - set(before["excluded"][symbol])
                        q = set(domains[symbol]) - set(after["excluded"][symbol])
                        unit = next((u[symbol] for u in omitted if symbol in u), domains[symbol])
                        assert q == p & set(unit)
                omitted_stats["omission_pairs"] += 1
    report = dict(binary=stats, omitted_units=omitted_stats, elapsed=time.time() - start)
    (HERE / "horn_calibration_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report), flush=True)


if __name__ == "__main__":
    main()
