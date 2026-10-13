"""Exhaustive three-clause selector family up to X-value membership patterns.

For every nonempty subset of the eight X membership patterns, enumerate all
six clause orders and all eight within-clause symbol orders. This covers
arbitrary domain cardinalities in this family modulo value refinement.
"""
import collections
import itertools
import json
import time

from fixed_point import opportunities
from oracle import HERE, MDDOracle, OneHotOracle, RBridge, audit_events


def main():
    bridge = RBridge()
    patterns = list(itertools.product((0, 1), repeat=3))
    stats = collections.Counter()
    findings = []
    start = time.time()
    for mask in range(1, 256):
        chosen = [p for i, p in enumerate(patterns) if (mask >> i) & 1]
        labels = ["p" + "".join(map(str, p)) for p in chosen]
        domains = {"X": labels, "Y": ["a", "b", "c"]}
        ranges = [[v for v, p in zip(labels, chosen) if p[i]] for i in range(3)]
        sat = OneHotOracle(domains)
        for permutation in itertools.permutations(range(3)):
            for orientation in itertools.product((0, 1), repeat=3):
                clauses = []
                for i in permutation:
                    pairs = [("X", ranges[i]), ("Y", [domains["Y"][i]])]
                    if orientation[i]:
                        pairs.reverse()
                    clauses.append(dict(pairs))
                answer = bridge.simplify(domains, clauses, audit="skip")
                assert answer["ok"], (domains, clauses, answer)
                assert sat.difference(clauses, answer["result"]) is None, (domains, clauses, answer)
                assert MDDOracle(domains).equivalent(clauses, answer["result"]), (domains, clauses, answer)
                stats["formulas"] += 1
                stats.update(audit_events(answer["events"], domains))
                found = opportunities(answer["result"])
                if found:
                    stats["residual_formula"] += 1
                    for kind in set(x["kind"] for x in found):
                        stats["residual_" + kind] += 1
                    if len(findings) < 20:
                        findings.append(dict(mask=mask, permutation=permutation, orientation=orientation,
                                             domains=domains, clauses=clauses, result=answer["result"],
                                             opportunities=found, events=answer["events"]))
        if mask % 16 == 0:
            print(json.dumps(dict(mask=mask, elapsed=time.time() - start, stats=stats)), flush=True)
    bridge.close()
    report = dict(stats=stats, elapsed=time.time() - start, findings=findings)
    (HERE / "membership_cells_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(dict(elapsed=time.time() - start, stats=stats)), flush=True)


if __name__ == "__main__":
    main()
