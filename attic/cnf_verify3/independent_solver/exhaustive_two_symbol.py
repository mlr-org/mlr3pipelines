"""All <=3-clause / <=2-symbol formulas modulo value-membership equivalence.

255 domain-pattern subsets per symbol x 255 x 8 literal orders = 520,200.
Clause order is encoded by bit-coordinate order, already exhaustively covered.
Domain/range value order cannot affect any set-predicate control decision.
"""
import argparse
import collections
import itertools
import json
import multiprocessing
import time

from fixed_point import opportunities
from oracle import HERE, MDDOracle, OneHotOracle, RBridge, audit_events

PATTERNS = list(itertools.product((0, 1), repeat=3))
ORIENTATIONS = list(itertools.product((0, 1), repeat=3))


def domain(mask):
    chosen = [p for i, p in enumerate(PATTERNS) if (mask >> i) & 1]
    labels = ["p" + "".join(map(str, p)) for p in chosen]
    ranges = [[label for label, p in zip(labels, chosen) if p[i]] for i in range(3)]
    return labels, ranges


def worker(mask_x):
    stats = collections.Counter()
    findings, errors = [], []
    bridge = RBridge()
    dx, rx = domain(mask_x)
    for mask_y in range(1, 256):
        dy, ry = domain(mask_y)
        domains = {"X": dx, "Y": dy}
        sat, mdd = OneHotOracle(domains), MDDOracle(domains)
        for orientation in ORIENTATIONS:
            clauses = []
            for i in range(3):
                pairs = [("X", rx[i]), ("Y", ry[i])]
                if orientation[i]:
                    pairs.reverse()
                clauses.append(dict(pairs))
            answer = bridge.simplify(domains, clauses, audit="skip")
            stats["formulas"] += 1
            if not answer["ok"]:
                errors.append(dict(kind="production_error", mask_x=mask_x, mask_y=mask_y,
                                   orientation=orientation, domains=domains, clauses=clauses, answer=answer))
                continue
            result = answer["result"]
            witness = sat.difference(clauses, result)
            mdd_equal = mdd.equivalent(clauses, result)
            if witness is not None or not mdd_equal:
                errors.append(dict(kind="semantic", mask_x=mask_x, mask_y=mask_y,
                                   orientation=orientation, domains=domains, clauses=clauses,
                                   result=result, witness=witness, mdd_equal=mdd_equal))
            stats.update(audit_events(answer["events"], domains))
            found = opportunities(result)
            if found:
                stats["residual_formula"] += 1
                for kind in set(f["kind"] for f in found):
                    stats["residual_" + kind] += 1
                if len(findings) < 12:
                    findings.append(dict(mask_x=mask_x, mask_y=mask_y, orientation=orientation,
                                         domains=domains, clauses=clauses, result=result,
                                         opportunities=found, events=answer["events"]))
    bridge.close()
    return dict(mask_x=mask_x, stats=stats, errors=errors, findings=findings)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--workers", type=int, default=8)
    parser.add_argument("--start-mask", type=int, default=1)
    parser.add_argument("--end-mask", type=int, default=255)
    parser.add_argument("--out", default="exhaustive_two_symbol_results.json")
    args = parser.parse_args()
    start = time.time()
    stats, findings, errors = collections.Counter(), [], []
    completed = []
    with multiprocessing.Pool(args.workers) as pool:
        for result in pool.imap_unordered(worker, range(args.start_mask, args.end_mask + 1)):
            completed.append(result["mask_x"])
            stats.update(result["stats"])
            errors.extend(result["errors"])
            # Preserve representative findings from every outer mask. Typical
            # output is small; unexpected semantic errors are never truncated.
            findings.extend(result["findings"])
            if len(completed) % 8 == 0 or result["errors"]:
                print(json.dumps(dict(completed_masks=len(completed), latest_mask=result["mask_x"],
                                      elapsed=time.time() - start, stats=stats, errors=len(errors))), flush=True)
    report = dict(workers=args.workers, completed_masks=sorted(completed), stats=stats,
                  errors=errors, findings=findings, elapsed=time.time() - start)
    (HERE / args.out).write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(dict(completed_masks=len(completed), elapsed=time.time() - start,
                          stats=stats, errors=len(errors))), flush=True)


if __name__ == "__main__":
    main()
