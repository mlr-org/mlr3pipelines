"""Falsification probe for the unbounded two-clause saturation argument."""
import collections
import itertools
import json
import multiprocessing
import random
import time

from fixed_point import opportunities
from oracle import HERE, MDDOracle, OneHotOracle, RBridge
from structured_families import canonical

PATTERNS = list(itertools.product((0, 1), repeat=2))


def domain(mask):
    ps = [p for i, p in enumerate(PATTERNS) if (mask >> i) & 1]
    labels = ["p" + "".join(map(str, p)) for p in ps]
    ranges = [[label for label, p in zip(labels, ps) if p[i]] for i in range(2)]
    return labels, ranges


def worker(task):
    count, first_mask = task
    rng = random.Random(937150 + 19 * count + first_mask)
    bridge, stats, errors = RBridge(), collections.Counter(), []
    for remaining in itertools.product(range(1, 16), repeat=count - 1):
        masks = (first_mask,) + remaining
        ds, ranges = {}, []
        for i, mask in enumerate(masks):
            d, r = domain(mask)
            ds["S%d" % i] = d
            ranges.append(r)
        sat, mdd = OneHotOracle(ds), MDDOracle(ds)
        for orientation in (0, 1):
            cs = []
            for j in (0, 1):
                pairs = [("S%d" % i, r[j]) for i, r in enumerate(ranges)]
                rng.shuffle(pairs)
                cs.append(dict(pairs))
            if orientation:
                cs.reverse()
            result = bridge.simplify(ds, cs)
            stats["formulas"] += 1
            if not result["ok"]:
                errors.append(dict(kind="error", domains=ds, clauses=cs, answer=result))
                continue
            second = bridge.simplify(ds, result["result"])
            witness = sat.difference(cs, result["result"])
            mdd_equal = mdd.equivalent(cs, result["result"])
            residual = opportunities(result["result"])
            if witness is not None or not mdd_equal or residual or not second["ok"] or canonical(result["result"]) != canonical(second["result"]):
                errors.append(dict(kind="claim_failure", domains=ds, clauses=cs,
                                   result=result, second=second, witness=witness,
                                   mdd_equal=mdd_equal, residual=residual))
            stats["constant_results"] += isinstance(result["result"], bool)
    bridge.close()
    return dict(task=task, stats=stats, errors=errors)


def main():
    start = time.time()
    totals, errors, completed = collections.Counter(), [], []
    tasks = [(n, first) for n in range(1, 5) for first in range(1, 16)]
    with multiprocessing.Pool(4) as pool:
        for result in pool.imap_unordered(worker, tasks):
            totals.update(result["stats"])
            errors.extend(result["errors"])
            completed.append(result["task"])
            if len(completed) % 5 == 0 or result["errors"]:
                print(json.dumps(dict(completed=len(completed), stats=totals,
                                      errors=len(errors), elapsed=time.time() - start)), flush=True)
    report = dict(stats=totals, errors=errors, tasks=completed, elapsed=time.time() - start)
    (HERE / "two_clause_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({k: v for k, v in report.items() if k != "tasks"}), flush=True)


if __name__ == "__main__":
    main()
