"""Exhaust all (3,3,2)-occurrence three-symbol, three-clause set profiles.

There are 193 nondegenerate three-bit membership profiles for X and Y,
seven two-bit profiles for Z, and 36 remaining within-clause orders after
renaming X/Y to fix the shortest clause's order. This is 9,386,748 executions.
Every returned formula is evaluated on every assignment through bit vectors.
Only the JSON bridge is reused; the profile generator and bit-vector oracle
are independently defined here. SAT/MDD additionally calibrate sampled cases.
"""

import argparse
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


def profiles(n_clauses):
    result = []
    for mask in range(1, 1 << (1 << n_clauses)):
        values = [v for v in range(1 << n_clauses) if mask & (1 << v)]
        if all({(v >> c) & 1 for v in values} == {0, 1} for c in range(n_clauses)):
            domain = [str(v) for v in values]
            ranges = [[str(v) for v in values if v & (1 << c)] for c in range(n_clauses)]
            result.append((mask, domain, ranges))
    return result


PROFILES3 = profiles(3)
PROFILES2 = profiles(2)
ORDERS = list(itertools.permutations(("X", "Y", "Z")))
assert len(PROFILES3) == 193 and len(PROFILES2) == 7


class BitTruth:
    def __init__(self, domains):
        self.domains = domains
        self.symbols = list(domains)
        self.assignments = list(itertools.product(*domains.values()))
        self.all_rows = (1 << len(self.assignments)) - 1
        self.membership = {s: {v: 0 for v in d} for s, d in domains.items()}
        for row, assignment in enumerate(self.assignments):
            for symbol, value in zip(self.symbols, assignment):
                self.membership[symbol][value] |= 1 << row

    def formula(self, clauses):
        if isinstance(clauses, bool):
            return self.all_rows if clauses else 0
        result = self.all_rows
        for clause in clauses:
            disjunction = 0
            for symbol, values in clause.items():
                if isinstance(values, str):
                    values = [values]
                for value in values:
                    disjunction |= self.membership[symbol][value]
            result &= disjunction
        return result

    def witness(self, difference):
        row = (difference & -difference).bit_length() - 1
        return dict(zip(self.symbols, self.assignments[row]))


def worker(task):
    ix, y_limit = task
    mx, dx, rx = PROFILES3[ix]
    bridge = RBridge()
    stats = {"executions": 0, "valuations": 0, "sat_mdd_calibrations": 0,
             "constant_false": 0, "unit_subsumption_outputs": 0}
    findings = []
    for my, dy, ry in PROFILES3[:y_limit]:
        for mz, dz, rz in PROFILES2:
            domains = {"X": dx, "Y": dy, "Z": dz}
            ranges = [dict(X=rx[0], Y=ry[0]),
                      dict(X=rx[1], Y=ry[1], Z=rz[0]),
                      dict(X=rx[2], Y=ry[2], Z=rz[1])]
            oracle = BitTruth(domains)
            expected = oracle.formula(ranges)
            for o2, o3 in itertools.product(ORDERS, repeat=2):
                clauses = [ranges[0], {s: ranges[1][s] for s in o2}, {s: ranges[2][s] for s in o3}]
                answer = bridge.simplify(domains, clauses)
                stats["executions"] += 1
                stats["valuations"] += len(oracle.assignments)
                if not answer["ok"]:
                    bridge.close()
                    return dict(ix=ix, stats=stats, error=dict(domains=domains, clauses=clauses, answer=answer))
                result = answer["result"]
                actual = oracle.formula(result)
                if actual != expected:
                    bridge.close()
                    return dict(ix=ix, stats=stats, error=dict(domains=domains, clauses=clauses,
                                result=result, witness=oracle.witness(actual ^ expected)))
                if stats["executions"] % 4093 == 1:
                    sat = OneHotOracle(domains)
                    mdd = MDDOracle(domains)
                    assert sat.difference(clauses, result) is None
                    assert mdd.equivalent(clauses, result)
                    assert (expected == 0) == (sat.difference(clauses, False) is None)
                    stats["sat_mdd_calibrations"] += 1
                if result is False:
                    stats["constant_false"] += 1
                if isinstance(result, list):
                    units = [(next(iter(c)), next(iter(c.values()))) for c in result if len(c) == 1]
                    subsumed = False
                    for symbol, values in units:
                        values = {values} if isinstance(values, str) else set(values)
                        for clause in result:
                            if len(clause) <= 1 or symbol not in clause:
                                continue
                            target = clause[symbol]
                            target = {target} if isinstance(target, str) else set(target)
                            subsumed |= values <= target
                    if subsumed:
                        stats["unit_subsumption_outputs"] += 1
                        if len(findings) < 2:
                            findings.append(dict(masks=[mx, my, mz], domains=domains, clauses=clauses, result=result))
    bridge.close()
    return dict(ix=ix, stats=stats, findings=findings)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--workers", type=int, default=16)
    parser.add_argument("--x-start", type=int, default=0)
    parser.add_argument("--x-stop", type=int, default=193)
    parser.add_argument("--y-limit", type=int, default=193)
    parser.add_argument("--out", default="three_symbol_quotient_results.json")
    args = parser.parse_args()
    manifest = HERE.parent / "independent_solver" / "SOURCE.sha256"
    for line in manifest.read_text().splitlines():
        digest, filename = line.split()
        assert hashlib.sha256((REPO / filename).read_bytes()).hexdigest() == digest, filename
    start = time.time()
    report = dict(scope="three clauses, two symbols in all clauses, third symbol in two clauses",
                  parameters=vars(args), source_manifest=manifest.read_text(),
                  completed=[], stats={}, findings=[], errors=[])
    out = HERE / args.out
    print(json.dumps(dict(start=time.strftime("%Y-%m-%d %H:%M:%S UTC", time.gmtime()),
                          expected_executions=(args.x_stop - args.x_start) * args.y_limit * 7 * 36)), flush=True)
    with multiprocessing.Pool(args.workers) as pool:
        tasks = [(ix, args.y_limit) for ix in range(args.x_start, args.x_stop)]
        for result in pool.imap_unordered(worker, tasks):
            report["completed"].append(result["ix"])
            for key, value in result["stats"].items():
                report["stats"][key] = report["stats"].get(key, 0) + value
            report["findings"].extend(result.get("findings", []))
            if "error" in result:
                report["errors"].append(result["error"])
            report["elapsed"] = time.time() - start
            out.write_text(json.dumps(report, indent=2) + "\n")
            print(json.dumps(dict(completed=len(report["completed"]), elapsed=report["elapsed"],
                                  stats=report["stats"], errors=len(report["errors"]))), flush=True)
            if report["errors"]:
                raise RuntimeError("saved a differing result; inspect the report")
    report["completed"].sort()
    report["finished_utc"] = time.strftime("%Y-%m-%d %H:%M:%S UTC", time.gmtime())
    out.write_text(json.dumps(report, indent=2) + "\n")


if __name__ == "__main__":
    main()
