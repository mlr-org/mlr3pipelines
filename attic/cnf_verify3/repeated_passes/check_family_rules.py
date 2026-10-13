"""Check every local rule on each prefix state of the constructed family.

This uses Python set semantics and no production matrices or event reports.
It checks the finite instances of the symbolic classification in PROOF.md.
"""

from itertools import combinations_with_replacement
import json
from repeated import HERE, Bridge, chain_family, canonical, normalize


def holds(clause, valuation):
    return any(valuation[s] in allowed for s, allowed in clause.items())


def clauses_by_name(n, clauses):
    result = {"A0": clauses[0]}
    result.update({"B%d" % i: clauses[1 + i] for i in range(n)})
    result.update({"A%d" % i: clauses[n + 1 + n - i] for i in range(1, n + 1)})
    return result


def frontier(n, k):
    domains, clauses = chain_family(n)
    labelled = clauses_by_name(n, clauses)
    for i in range(1, k + 1):
        del labelled["A%d" % i]["T"]
    return domains, {name: {s: set(v) for s, v in clause.items()}
                     for name, clause in labelled.items()}


def irredundancy_witness(n, target):
    if target.startswith("A"):
        i = int(target[1:])
        answer = {"G": "g%d" % n, "T": "1"}
        answer.update({"S%d" % j: "1" if j in (i, i + 1) else "0"
                       for j in range(n + 2)})
    else:
        i = int(target[1:])
        answer = {"G": "g1" if i == 0 else "g0", "T": "0"}
        answer.update({"S%d" % j: "0" if j in ((0, 1) if i == 0 else (0, i)) else "1"
                       for j in range(n + 2)})
    return answer


def local_reductions(domains, clauses):
    empty = set()
    reductions = set()
    for target_name, target in clauses.items():
        others = [(name, clause) for name, clause in clauses.items() if name != target_name]
        exceptions = {name: {s for s in domains if not clause.get(s, empty) <= target.get(s, empty)}
                      for name, clause in others}
        for name, clause in others:
            assert exceptions[name], ("direct subsumption", name, target_name)
            for pivot in target:
                if exceptions[name] <= {pivot} and target[pivot] - clause.get(pivot, empty):
                    reductions.add(("sse1", name, target_name, pivot))
        for (an, a), (bn, b) in combinations_with_replacement(others, 2):
            for pivot in target:
                removed = target[pivot] - a.get(pivot, empty) - b.get(pivot, empty)
                if not removed:
                    continue
                for intersect in domains:
                    if intersect == pivot:
                        continue
                    if not (exceptions[an] | exceptions[bn]) <= {pivot, intersect}:
                        continue
                    if not a.get(intersect, empty) & b.get(intersect, empty) <= target.get(intersect, empty):
                        continue
                    reductions.add(("sse2", tuple(sorted((an, bn))), target_name, pivot, intersect))
    return reductions


counts = dict(frontier_states=0, irredundancy_witnesses=0, public_first_passes=0)
bridge = Bridge()
try:
    for n in range(1, 11):
        domains, raw = chain_family(n)
        direct = bridge.simplify(domains, raw)["result"]
        public = bridge.simplify(domains, raw, direct=False)["result"]
        assert canonical(direct) == canonical(public)
        counts["public_first_passes"] += 1
        for k in range(n + 1):
            domains, clauses = frontier(n, k)
            for target_name, target in clauses.items():
                witness = irredundancy_witness(n, target_name)
                assert all(witness[s] in domains[s] for s in domains)
                assert not holds(target, witness), (n, k, target_name, witness)
                assert all(holds(c, witness) for name, c in clauses.items() if name != target_name)
                counts["irredundancy_witnesses"] += 1
            reductions = local_reductions(domains, clauses)
            expected = set() if k == n else {
                ("sse2", tuple(sorted(("A%d" % k, "B%d" % k))),
                 "A%d" % (k + 1), "T", "S%d" % k)}
            assert reductions == expected, (n, k, reductions, expected)
            counts["frontier_states"] += 1
        print("n=%d: all %d prefix states have exactly the claimed local opportunity" %
              (n, n + 1), flush=True)
finally:
    bridge.close()
(HERE / "family_rule_results.json").write_text(json.dumps(counts, indent=2) + "\n")
print(counts, flush=True)
