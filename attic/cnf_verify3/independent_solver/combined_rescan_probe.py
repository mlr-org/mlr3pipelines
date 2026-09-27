"""Challenge the combined diagnostic repairs semantically and structurally."""
import collections
import json
import random
import time

from domain_refutation import redundancy_opportunities
from fixed_point import opportunities, random_dense
from oracle import HERE, MDDOracle, OneHotOracle, RBridge
from structured_families import canonical, hla_chain, second_order_bundle, permutation_cycle


def main():
    rng, bridge = random.Random(89055199), RBridge()
    stats, failures = collections.Counter(), []
    start = time.time()
    for i in range(5000):
        if i % 12 == 0:
            ds, cs, _ = hla_chain(rng, rng.randint(3, 18), unit=bool(i % 24), branches=rng.randint(1, 2))
        elif i % 12 == 1:
            ds, cs, _ = second_order_bundle(rng, rng.randint(2, 7))
        elif i % 12 == 2:
            ds, cs, _ = permutation_cycle(rng, rng.randint(3, 9), rng.randint(3, 7))
        else:
            ds, cs = random_dense(rng, i)
        baseline = bridge.simplify(ds, cs)
        answer = bridge.simplify(ds, cs, variant="combined_rescan")
        assert baseline["ok"] and answer["ok"], dict(trial=i, domains=ds, clauses=cs, baseline=baseline, candidate=answer)
        result = answer["result"]
        sat, mdd = OneHotOracle(ds), MDDOracle(ds)
        assert sat.difference(cs, result) is None
        assert mdd.equivalent(cs, result)
        second = bridge.simplify(ds, result, variant="combined_rescan")
        assert second["ok"], second
        residual = opportunities(result)
        redundant = redundancy_opportunities(ds, result)
        stable = canonical(result) == canonical(second["result"])
        stats["formulas"] += 1
        stats["baseline_seconds"] += baseline["seconds"]
        stats["candidate_seconds"] += answer["seconds"]
        stats["second_pass_seconds"] += second["seconds"]
        stats["changed_output"] += canonical(baseline["result"]) != canonical(result)
        if residual or redundant or not stable:
            record = dict(trial=i, domains=ds, clauses=cs, baseline=baseline,
                          candidate=answer, second=second, residual=residual,
                          redundant=redundant, stable=stable)
            failures.append(record)
            for kind in {o["kind"] for o in residual}:
                stats["residual_" + kind] += 1
            stats["redundancy_failure"] += bool(redundant)
            stats["idempotence_failure"] += not stable
            print(json.dumps(record), flush=True)
            break
        if (i + 1) % 500 == 0:
            print(json.dumps(dict(stats=stats, elapsed=time.time() - start)), flush=True)
    bridge.close()
    report = dict(stats=stats, failures=failures, elapsed=time.time() - start)
    (HERE / "combined_rescan_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({k:v for k,v in report.items() if k != "failures"}), flush=True)


if __name__ == "__main__":
    main()
