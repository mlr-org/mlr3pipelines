"""All proper three-clause/three-symbol profiles in aligned symbol order.

Each symbol occurs in every clause. This checks 193**3 range-profile triples,
including every repeated-pass output, but keeps X,Y,Z order in each clause.
It is not an enumeration of arbitrary within-clause symbol permutations.
"""
import argparse
from collections import Counter
from functools import lru_cache
import hashlib
import itertools
import json
import multiprocessing
from pathlib import Path
import sys
import time

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]
sys.path.insert(0, str(HERE.parent / "independent_solver"))
from oracle import MDDOracle, OneHotOracle, RBridge  # noqa: E402
from three_symbol_quotient import BitTruth, PROFILES3  # noqa: E402


@lru_cache(maxsize=None)
def positional_masks(nx, ny, nz):
    x = tuple(((1 << (ny * nz)) - 1) << (i * ny * nz) for i in range(nx))
    y = tuple(sum(((1 << nz) - 1) << ((i * ny + j) * nz) for i in range(nx))
              for j in range(ny))
    z = tuple(sum(1 << (k * nz + j) for k in range(nx * ny)) for j in range(nz))
    return x, y, z


class FastTruth:
    def __init__(self, domains):
        self.domains = domains
        self.rows = 1
        for domain in domains.values():
            self.rows *= len(domain)
        self.all_rows = (1 << self.rows) - 1
        masks = positional_masks(*(len(domain) for domain in domains.values()))
        self.membership = {symbol: dict(zip(domain, bits))
                           for (symbol, domain), bits in zip(domains.items(), masks)}

    def formula(self, clauses):
        if isinstance(clauses, bool):
            return self.all_rows if clauses else 0
        result = self.all_rows
        for clause in clauses:
            disjunction = 0
            for symbol, values in clause.items():
                for value in [values] if isinstance(values, str) else values:
                    disjunction |= self.membership[symbol][value]
            result &= disjunction
        return result

    def witness(self, difference):
        row = (difference & -difference).bit_length() - 1
        assignment = next(itertools.islice(itertools.product(*self.domains.values()), row, None))
        return dict(zip(self.domains, assignment))


def weight(formula):
    if isinstance(formula, bool):
        return 0
    return sum(1 if isinstance(values, str) else len(values)
               for clause in formula for values in clause.values())


def normalize(formula):
    if isinstance(formula, bool):
        return formula
    return tuple(sorted(tuple(sorted((symbol, tuple(sorted([values] if isinstance(values, str) else values)))
                                     for symbol, values in clause.items())) for clause in formula))


def worker(task):
    ix, y_limit, z_limit = task
    mx, dx, rx = PROFILES3[ix]
    bridge = RBridge()
    stats = Counter()
    pass_counts = Counter()
    examples = {}
    for my, dy, ry in PROFILES3[:y_limit]:
        for mz, dz, rz in PROFILES3[:z_limit]:
            domains = dict(X=dx, Y=dy, Z=dz)
            original = [dict(X=rx[c], Y=ry[c], Z=rz[c]) for c in range(3)]
            oracle = FastTruth(domains)
            expected = oracle.formula(original)
            # Matching each clause to a different symbol proves satisfiability.
            assert expected != 0 and expected != oracle.all_rows
            current = original
            productive = 0
            first = None
            stats["inputs"] += 1
            while True:
                answer = bridge.simplify(domains, current)
                stats["executions"] += 1
                stats["valuations"] += oracle.rows
                if not answer["ok"]:
                    return dict(ix=ix, stats=dict(stats), error=dict(domains=domains, clauses=original,
                                current=current, answer=answer))
                result = answer["result"]
                actual = oracle.formula(result)
                if actual != expected:
                    return dict(ix=ix, stats=dict(stats), error=dict(domains=domains, clauses=original,
                                current=current, result=result, witness=oracle.witness(actual ^ expected)))
                if first is None:
                    first = result
                before, after = weight(current), weight(result)
                assert after <= before
                if after == before:
                    assert normalize(result) == normalize(current)
                    break
                productive += 1
                assert productive <= weight(original)
                current = result
            pass_counts[productive] += 1
            if productive >= 2 and str(productive) not in examples:
                examples[str(productive)] = dict(masks=[mx, my, mz], domains=domains, clauses=original,
                                                 first=first, fixed=result, productive=productive)
            if stats["inputs"] % 4093 == 1:
                slow = BitTruth(domains)
                assert slow.membership == oracle.membership
                assert slow.formula(original) == expected
                sat, mdd = OneHotOracle(domains), MDDOracle(domains)
                assert sat.difference(original, first) is None
                assert sat.difference(original, result) is None
                assert mdd.equivalent(original, first) and mdd.equivalent(original, result)
                stats["independent_calibrations"] += 1
    bridge.close()
    return dict(ix=ix, stats=dict(stats), productive_pass_counts=dict(pass_counts), examples=examples)


def calibrate_masks():
    for sizes in itertools.product(range(2, 9), repeat=3):
        domains = {symbol: [str(i) for i in range(n)] for symbol, n in zip("XYZ", sizes)}
        slow, fast = BitTruth(domains), FastTruth(domains)
        assert slow.membership == fast.membership
        assert slow.all_rows == fast.all_rows
    return 7 ** 3


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--workers", type=int, default=16)
    parser.add_argument("--x-start", type=int, default=0)
    parser.add_argument("--x-stop", type=int, default=193)
    parser.add_argument("--y-limit", type=int, default=193)
    parser.add_argument("--z-limit", type=int, default=193)
    parser.add_argument("--out", default="full_three_symbol_results.json")
    args = parser.parse_args()
    manifest = HERE.parent / "independent_solver/SOURCE.sha256"
    for line in manifest.read_text().splitlines():
        digest, filename = line.split()
        assert hashlib.sha256((REPO / filename).read_bytes()).hexdigest() == digest
    expected_inputs = (args.x_stop - args.x_start) * args.y_limit * args.z_limit
    start = time.time()
    report = dict(scope="three clauses, three symbols in every clause; aligned X,Y,Z occurrence order",
                  parameters=vars(args), source_manifest=manifest.read_text(), expected_inputs=expected_inputs,
                  mask_shape_calibrations=calibrate_masks(), completed=[], stats={},
                  productive_pass_counts={}, examples={}, errors=[], started_utc=time.strftime("%F %T UTC", time.gmtime()))
    print(json.dumps(dict(start=report["started_utc"], expected_inputs=expected_inputs)), flush=True)
    out = HERE / args.out
    with multiprocessing.Pool(args.workers) as pool:
        tasks = [(ix, args.y_limit, args.z_limit) for ix in range(args.x_start, args.x_stop)]
        for result in pool.imap_unordered(worker, tasks):
            report["completed"].append(result["ix"])
            for category in ("stats", "productive_pass_counts"):
                for key, value in result.get(category, {}).items():
                    key = str(key)
                    report[category][key] = report[category].get(key, 0) + value
            for key, example in result.get("examples", {}).items():
                report["examples"].setdefault(key, example)
            if "error" in result:
                report["errors"].append(result["error"])
            report["elapsed"] = time.time() - start
            out.write_text(json.dumps(report, indent=2) + "\n")
            print(json.dumps(dict(completed=len(report["completed"]), elapsed=report["elapsed"],
                                  stats=report["stats"], productive=report["productive_pass_counts"],
                                  errors=len(report["errors"]))), flush=True)
            if report["errors"]:
                raise RuntimeError("Saved a differing result")
    assert report["stats"]["inputs"] == expected_inputs
    report["completed"].sort()
    report["finished_utc"] = time.strftime("%F %T UTC", time.gmtime())
    out.write_text(json.dumps(report, indent=2) + "\n")


if __name__ == "__main__":
    main()
