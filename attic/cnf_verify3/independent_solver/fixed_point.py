"""Independent check for residual advertised simplification operations."""
import argparse
import collections
import itertools
import json
import random
import time

from oracle import HERE, OneHotOracle, RBridge, normalize, values
from structured_families import canonical, second_order_bundle


def opportunities(formula, second=True):
    formula = normalize(formula)
    if isinstance(formula, bool):
        return []
    clauses = [{s: set(v) for s, v in c.items()} for c in formula]
    results = []
    for j, target in enumerate(clauses):
        donors = [(i, c) for i, c in enumerate(clauses) if i != j]
        mismatches = {i: {s for s, r in c.items() if not r <= target.get(s, set())}
                      for i, c in donors}
        for i, donor in donors:
            exceptions = mismatches[i]
            if not exceptions:
                results.append(dict(kind="subsumption", donor=i, target=j))
            elif len(exceptions) == 1:
                s = next(iter(exceptions))
                removed = target.get(s, set()) - donor[s]
                if removed:
                    results.append(dict(kind="sse1", donor=i, target=j, symbol=s, removed=sorted(removed)))
        if not second:
            continue
        for (ia, a), (ib, b) in itertools.combinations(donors, 2):
            exceptions = mismatches[ia] | mismatches[ib]
            if len(exceptions) > 2:
                continue
            for s in a.keys() & b.keys():
                if not a[s] & b[s] <= target.get(s, set()):
                    continue
                for t in target:
                    if s == t or not exceptions <= {s, t}:
                        continue
                    removed = target[t] - (a.get(t, set()) | b.get(t, set()))
                    if removed:
                        results.append(dict(kind="sse2", donor_a=ia, donor_b=ib, target=j,
                                            intersect_symbol=s, restrict_symbol=t, removed=sorted(removed)))
    return results


def random_dense(rng, trial):
    n = rng.randint(2, 5)
    size = rng.randint(3, 7)
    domains = {"X%d" % j: [str(v) for v in range(size)] for j in range(n)}
    clauses = []
    for j in range(rng.randint(3, 15)):
        chosen = rng.sample(list(domains), rng.randint(2, n))
        clause = {s: rng.sample(domains[s], rng.randint(1, size - 1)) for s in chosen}
        clauses.append(clause)
    return domains, clauses


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--trials", type=int, default=30000)
    parser.add_argument("--seed", type=int, default=6671829)
    parser.add_argument("--out", default="fixed_point_results.json")
    args = parser.parse_args()
    rng, bridge = random.Random(args.seed), RBridge()
    stats = collections.Counter()
    start = time.time()
    failures = []
    for i in range(args.trials):
        domains, clauses = random_dense(rng, i)
        answer = bridge.simplify(domains, clauses)
        assert answer["ok"], answer
        result = answer["result"]
        found = opportunities(result)
        stats["trials"] += 1
        if found:
            resimplified = bridge.simplify(domains, result)
            assert resimplified["ok"], resimplified
            record = dict(trial=i, domains=domains, clauses=clauses, result=result,
                          opportunities=found, resimplified=resimplified["result"])
            sat = OneHotOracle(domains)
            assert sat.difference(clauses, result) is None
            assert sat.difference(clauses, record["resimplified"]) is None
            record["same_second_pass"] = canonical(result) == canonical(record["resimplified"])
            failures.append(record)
            stats["residual_opportunities"] += 1
            stats["same_second_pass"] += record["same_second_pass"]
            for kind in set(x["kind"] for x in found):
                stats["residual_" + kind] += 1
            print(json.dumps(record), flush=True)
            if len(failures) >= 10:
                break
        if (i + 1) % 1000 == 0:
            print(json.dumps(dict(elapsed=time.time() - start, stats=stats)), flush=True)
    bridge.close()
    report = dict(seed=args.seed, stats=stats, elapsed=time.time() - start, findings=failures)
    (HERE / args.out).write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(dict(elapsed=time.time() - start, stats=stats)), flush=True)


if __name__ == "__main__":
    main()
