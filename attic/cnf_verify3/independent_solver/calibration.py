"""Calibrate distinct oracles, malformed-result sensitivity, and R transport."""
import itertools
import json
import random

from oracle import MDDOracle, OneHotOracle, RBridge, audit_events, evaluate


def truth_table(domains, formula):
    return tuple(evaluate(formula, dict(zip(domains, row)))
                 for row in itertools.product(*domains.values()))


def all_clauses(domains):
    options = []
    for symbol, domain in domains.items():
        options.append([{}] + [{symbol: list(subset)}
                               for size in range(1, len(domain))
                               for subset in itertools.combinations(domain, size)])
    for row in itertools.product(*options):
        clause = {s: v for part in row for s, v in part.items()}
        if clause:
            yield clause


def main():
    rng = random.Random(7129183)
    totals = dict(formulas=0, equivalence_comparisons=0, false_claims_rejected=0,
                  production_outputs=0, audited_events=0)
    bridge = RBridge()
    for domains in ({"X": ["a", "b", "c"]}, {"X": ["0", "1"], "Y": ["0", "1"]}):
        pool = list(all_clauses(domains))
        formulas = [True, False, [], [{}]] + [list(subset)
                    for size in range(len(pool) + 1) for subset in itertools.combinations(pool, size)]
        sat, mdd = OneHotOracle(domains), MDDOracle(domains)
        tables = [truth_table(domains, f) for f in formulas]
        for i, left in enumerate(formulas):
            totals["formulas"] += 1
            # Every pair for 1-variable domain-3; systematic spread for the
            # larger 2-variable binary formula universe.
            right_ids = range(len(formulas)) if len(domains) == 1 else range(i % 13, len(formulas), 13)
            for j in right_ids:
                right = formulas[j]
                expected = tables[i] == tables[j]
                assert mdd.equivalent(left, right) == expected, (left, right)
                assert (sat.difference(left, right) is None) == expected, (left, right)
                totals["equivalence_comparisons"] += 1
                totals["false_claims_rejected"] += not expected
            if isinstance(left, list) and all(left):
                answer = bridge.simplify(domains, left, audit=True)
                assert answer["ok"], answer
                assert truth_table(domains, answer["result"]) == tables[i]
                totals["audited_events"] += sum(audit_events(answer["events"], domains).values())
                totals["production_outputs"] += 1
    for i in range(200):
        domains = {"X%d" % s: [str(v) for v in range(rng.randint(2, 4))] for s in range(3)}
        pool = list(all_clauses(domains))
        left = rng.sample(pool, rng.randint(1, 12))
        right = rng.sample(pool, rng.randint(1, 12))
        expected = truth_table(domains, left) == truth_table(domains, right)
        sat, mdd = OneHotOracle(domains), MDDOracle(domains)
        assert (sat.difference(left, right) is None) == expected
        assert mdd.equivalent(left, right) == expected
        answer = bridge.simplify(domains, left, audit=True)
        assert answer["ok"], answer
        assert truth_table(domains, left) == truth_table(domains, answer["result"])
        totals["audited_events"] += sum(audit_events(answer["events"], domains).values())
        totals["production_outputs"] += 1
        totals["formulas"] += 2
        totals["equivalence_comparisons"] += 1
        totals["false_claims_rejected"] += not expected
    bridge.close()
    print(json.dumps(totals, indent=2))


if __name__ == "__main__":
    main()
