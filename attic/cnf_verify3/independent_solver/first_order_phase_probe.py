"""Does the pairwise first-order phase alone reach its own local fixed point?"""
import collections
import json
import random
import time

from fixed_point import opportunities, random_dense
from oracle import HERE, MDDOracle, OneHotOracle, RBridge


def main():
    rng, bridge = random.Random(41156200), RBridge()
    stats, findings = collections.Counter(), []
    start = time.time()
    for i in range(30000):
        ds, cs = random_dense(rng, i)
        answer = bridge.simplify(ds, cs, variant="first_order_phase")
        assert answer["ok"], dict(domains=ds, clauses=cs, answer=answer)
        result = answer["result"]
        found = opportunities(result, second=False)
        stats["formulas"] += 1
        if found:
            stats["residual_formulas"] += 1
            for kind in {p["kind"] for p in found}:
                stats["residual_" + kind] += 1
            assert OneHotOracle(ds).difference(cs, result) is None
            assert MDDOracle(ds).equivalent(cs, result)
            record = dict(trial=i, domains=ds, clauses=cs, result=result, opportunities=found)
            findings.append(record)
            print(json.dumps(record), flush=True)
            if any(p["kind"] == "sse1" for p in found) or len(findings) == 20:
                break
        if (i + 1) % 1000 == 0:
            print(json.dumps(dict(elapsed=time.time() - start, stats=stats)), flush=True)
    bridge.close()
    report = dict(stats=stats, findings=findings, elapsed=time.time() - start)
    (HERE / "first_order_phase_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({k: v for k, v in report.items() if k != "findings"}), flush=True)


if __name__ == "__main__":
    main()
