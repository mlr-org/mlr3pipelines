"""Targeted audit of lifecycle assumptions in the unit-containment argument."""
import collections
import json
import random
import time

from fixed_point import random_dense
from oracle import HERE, MDDOracle, OneHotOracle, RBridge, audit_events
from structured_families import hla_chain, permutation_cycle, second_order_bundle


def main():
    rng, bridge = random.Random(384923), RBridge()
    stats = collections.Counter()
    start = time.time()
    saved_outside = False
    for i in range(5000):
        if i % 10 == 0:
            domains, clauses, _ = permutation_cycle(rng, rng.randint(3, 10), rng.randint(3, 7))
        elif i % 10 == 1:
            domains, clauses, _ = second_order_bundle(rng, rng.randint(2, 8))
        else:
            domains, clauses = random_dense(rng, i)
        answer = bridge.simplify(domains, clauses, audit=True, variant="lifecycle")
        assert answer["ok"], dict(trial=i, domains=domains, clauses=clauses, answer=answer)
        try:
            counts = audit_events(answer["events"], domains)
            stats.update(counts)
            if counts.get("unit_skip_outside") and not saved_outside and OneHotOracle(domains).difference(clauses, False) is not None:
                detailed = bridge.simplify(domains, clauses, audit=True, detailed=True, variant="lifecycle")
                record = dict(trial=i, domains=domains, clauses=clauses, answer=detailed)
                (HERE / "outside_skip_satisfiable_dense.json").write_text(json.dumps(record, indent=2) + "\n")
                saved_outside = True
        except AssertionError as error:
            record = dict(trial=i, domains=domains, clauses=clauses, answer=answer, message=str(error))
            (HERE / "lifecycle_failure.json").write_text(json.dumps(record, indent=2) + "\n")
            raise
        assert OneHotOracle(domains).difference(clauses, answer["result"]) is None
        assert MDDOracle(domains).equivalent(clauses, answer["result"])
        stats["formulas"] += 1
        if (i + 1) % 250 == 0:
            print(json.dumps(dict(elapsed=time.time() - start, stats=stats)), flush=True)
    bridge.close()
    report = dict(elapsed=time.time() - start, stats=stats)
    (HERE / "lifecycle_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report), flush=True)


if __name__ == "__main__":
    main()
