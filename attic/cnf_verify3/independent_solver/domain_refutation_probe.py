"""Independent check of final HLA redundancy saturation via domain refutation."""
import collections
import itertools
import json
import random
import time

from domain_refutation import redundancy_opportunities
from fixed_point import opportunities, random_dense
from oracle import HERE, MDDOracle, OneHotOracle, RBridge, evaluate
from structured_families import hla_chain, second_order_bundle, permutation_cycle


def main():
    rng, bridge = random.Random(9024842), RBridge()
    stats, findings = collections.Counter(), []
    start = time.time()
    for i in range(10000):
        if i % 20 == 0:
            ds, cs, _ = hla_chain(rng, rng.randint(3, 12), unit=False)
        elif i % 20 == 1:
            ds, cs, _ = hla_chain(rng, rng.randint(3, 12), unit=True)
        elif i % 20 == 2:
            ds, cs, _ = second_order_bundle(rng, rng.randint(2, 6))
        elif i % 20 == 3:
            ds, cs, _ = permutation_cycle(rng, rng.randint(3, 7), rng.randint(3, 6))
        else:
            ds, cs = random_dense(rng, i)
        answer = bridge.simplify(ds, cs)
        assert answer["ok"], dict(domains=ds, clauses=cs, answer=answer)
        result = answer["result"]
        stats["formulas"] += 1
        stats["output_clauses"] += 0 if isinstance(result, bool) else len(result)
        found = redundancy_opportunities(ds, result)
        if found:
            sat, mdd = OneHotOracle(ds), MDDOracle(ds)
            assert sat.difference(cs, result) is None
            assert mdd.equivalent(cs, result)
            known_unit_targets = {o["target"] for o in opportunities(result, second=False)
                                  if o["kind"] == "subsumption" and len(result[o["donor"]]) == 1}
            novel = []
            for item in found:
                j = item["target"]
                donors = result[:j] + result[j + 1:]
                assert sat.implies(donors, [result[j]])
                if j in known_unit_targets:
                    stats["known_unit_subsumption"] += 1
                else:
                    novel.append(item)
            if novel:
                record = dict(trial=i, domains=ds, clauses=cs, result=result,
                              opportunities=found, novel=novel)
                findings.append(record)
                stats["novel_formulas"] += 1
                print(json.dumps(record), flush=True)
                if len(findings) == 10:
                    break
        if (i + 1) % 500 == 0:
            print(json.dumps(dict(elapsed=time.time() - start, stats=stats)), flush=True)
    bridge.close()
    report = dict(stats=stats, findings=findings, elapsed=time.time() - start)
    (HERE / "domain_refutation_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({k: v for k, v in report.items() if k != "findings"}), flush=True)


if __name__ == "__main__":
    main()
