"""Diagnostic callback repair, evaluated without editing production sources."""
import collections
import json
import random
import time

from fixed_point import opportunities, random_dense
from oracle import HERE, MDDOracle, OneHotOracle, RBridge
from structured_families import hla_chain, second_order_bundle, permutation_cycle


def main():
    rng, bridge = random.Random(9108052), RBridge()
    stats, fixtures = collections.Counter(), []
    start = time.time()
    for filename in ("minimized_sse1.json", "minimized_first_order_phase_sse1.json",
                     "minimized_sse2.json", "directed_oneend_shrink_min.json",
                     "minimized_subsumption.json", "minimized_deferred_skip.json"):
        record = json.loads((HERE / filename).read_text())
        ds, cs = record["domains"], record["clauses"]
        answer = bridge.simplify(ds, cs, variant="sse1_changed_range")
        assert answer["ok"], dict(filename=filename, answer=answer)
        assert OneHotOracle(ds).difference(cs, answer["result"]) is None
        assert MDDOracle(ds).equivalent(cs, answer["result"])
        fixtures.append(dict(file=filename, result=answer["result"], residual=opportunities(answer["result"])))
    for i in range(5000):
        if i % 20 == 0:
            ds, cs, _ = hla_chain(rng, rng.randint(3, 14), unit=bool(i % 40))
        elif i % 20 == 1:
            ds, cs, _ = second_order_bundle(rng, rng.randint(2, 6))
        elif i % 20 == 2:
            ds, cs, _ = permutation_cycle(rng, rng.randint(3, 8), rng.randint(3, 7))
        else:
            ds, cs = random_dense(rng, i)
        baseline = bridge.simplify(ds, cs)
        answer = bridge.simplify(ds, cs, variant="sse1_changed_range")
        assert baseline["ok"] and answer["ok"], dict(trial=i, domains=ds, clauses=cs, baseline=baseline, candidate=answer)
        assert OneHotOracle(ds).difference(cs, answer["result"]) is None
        assert MDDOracle(ds).equivalent(cs, answer["result"])
        stats["formulas"] += 1
        stats["baseline_seconds"] += baseline["seconds"]
        stats["candidate_seconds"] += answer["seconds"]
        residual = opportunities(answer["result"])
        for kind in {p["kind"] for p in residual}:
            stats["residual_" + kind] += 1
        if any(p["kind"] == "sse1" for p in residual):
            record = dict(trial=i, domains=ds, clauses=cs, baseline=baseline,
                          candidate=answer, residual=residual)
            (HERE / "sse1_candidate_failure.json").write_text(json.dumps(record, indent=2) + "\n")
            break
        if (i + 1) % 500 == 0:
            print(json.dumps(dict(stats=stats, elapsed=time.time() - start)), flush=True)
    bridge.close()
    report = dict(stats=stats, fixtures=fixtures, elapsed=time.time() - start)
    (HERE / "sse1_candidate_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report), flush=True)


if __name__ == "__main__":
    main()
