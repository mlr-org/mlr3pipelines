"""Isolate real HLA loops and compare their output with Horn refutations.

The finite family is every formula of at most four distinct clauses over
three Boolean symbols. Unit propagation and direct subsumption remain enabled;
first- and second-order SSE are disabled in the diagnostic source copy.
"""
import collections
import concurrent.futures
import itertools
import json
import time

from horn_refutation import horn_refute, verify_horn_certificate
from oracle import HERE, MDDOracle, OneHotOracle, RBridge, normalize


def shard(shard_id):
    domains = {s: ["0", "1"] for s in "XYZ"}
    clauses = [dict((s, [str(v)]) for s, v in zip(domains, row) if v != 2)
               for row in itertools.product(range(3), repeat=3) if row != (2, 2, 2)]
    bridge, sat, mdd = RBridge(), OneHotOracle(domains), MDDOracle(domains)
    stats = collections.Counter()
    start = time.time()
    for size in range(5):
        for i, selected in enumerate(itertools.combinations(clauses, size)):
            if i % 4 != shard_id:
                continue
            formula = list(selected)
            # A deterministic per-case permutation exercises both symbol and
            # equal-length clause orders without changing the family covered.
            if i % 2:
                formula.reverse()
            if i % 3:
                formula = [dict(reversed(list(c.items()))) for c in formula]
            answer = bridge.simplify(domains, formula, variant="hla_only")
            assert answer["ok"], (formula, answer)
            result = normalize(answer["result"])
            assert sat.difference(formula, result) is None, (formula, answer)
            assert mdd.equivalent(formula, result), (formula, answer)
            for event in answer["events"]:
                stats[event["kind"]] += 1
                assert event["kind"] in {"subsumption", "hla", "unit_hla"}
            if not isinstance(result, bool):
                for target_idx, target in enumerate(result):
                    donors = result[:target_idx] + result[target_idx + 1:]
                    horn = horn_refute(domains, donors, target)
                    if horn["refuted"]:
                        assert verify_horn_certificate(domains, donors, target, horn["trace"])
                        record = dict(domains=domains, clauses=formula, answer=answer,
                                      target=target_idx, horn=horn)
                        (HERE / ("hla_only_failure_%d.json" % shard_id)).write_text(json.dumps(record, indent=2) + "\n")
                        raise AssertionError(record)
                    stats["surviving_targets"] += 1
            stats["formulas"] += 1
    bridge.close()
    return dict(shard=shard_id, stats=stats, elapsed=time.time() - start)


def main():
    start = time.time()
    with concurrent.futures.ProcessPoolExecutor(max_workers=4) as executor:
        reports = list(executor.map(shard, range(4)))
    totals = collections.Counter()
    for report in reports:
        totals.update(report["stats"])
    report = dict(totals=totals, shards=reports, elapsed=time.time() - start)
    (HERE / "hla_only_exhaustive_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report), flush=True)


if __name__ == "__main__":
    main()
