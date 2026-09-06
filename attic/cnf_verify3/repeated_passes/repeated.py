"""Utilities for repeated-pass experiments; production R remains unchanged."""

import json
import pathlib
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[2]
SOLVER = HERE.parent / "independent_solver"
sys.path.insert(0, str(SOLVER))
from oracle import OneHotOracle, MDDOracle, values


class Bridge:
    def __init__(self):
        self.process = subprocess.Popen([
            "podman", "exec", "-i", "cnf-review-r46", "Rscript",
            "/work/attic/cnf_verify3/independent_solver/r_bridge.R"],
            cwd=str(ROOT), stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            text=True, bufsize=1)

    def simplify(self, domains, clauses, audit=False, direct=True):
        request = dict(domains=domains, clauses=clauses, direct=direct, audit=audit)
        self.process.stdin.write(json.dumps(request, separators=(",", ":")) + "\n")
        self.process.stdin.flush()
        answer = self.process.stdout.readline()
        if not answer:
            raise RuntimeError("R process ended: %s" % self.process.poll())
        result = json.loads(answer)
        if not result["ok"]:
            raise RuntimeError(result["message"])
        return result

    def close(self):
        self.process.stdin.close()
        self.process.wait(timeout=20)


def canonical(formula, ignore_multiplicity=False):
    if isinstance(formula, bool):
        return formula
    clauses = [tuple(sorted((s, tuple(sorted(values(v)))) for s, v in clause.items()))
               for clause in formula]
    if ignore_multiplicity:
        clauses = set(clauses)
    return tuple(sorted(clauses))


def mass(formula):
    if isinstance(formula, bool):
        return 0
    return sum(len(values(v)) for clause in formula for v in clause.values())


def storage_key(formula):
    if isinstance(formula, bool):
        return formula
    return tuple(tuple((s, tuple(values(v))) for s, v in clause.items())
                 for clause in formula)


def normalize(formula):
    if isinstance(formula, bool):
        return formula
    return [{s: list(values(v)) for s, v in clause.items()} for clause in formula]


def iterate(bridge, domains, clauses, limit=100, verify=True, audit=False):
    current = normalize(clauses)
    sat = OneHotOracle(domains) if verify else None
    mdd = MDDOracle(domains) if verify else None
    trace = [current]
    events = []
    seconds = []
    productive = 0
    order_only = 0
    for _ in range(limit):
        answer = bridge.simplify(domains, current, audit=audit)
        result = normalize(answer["result"])
        if verify:
            assert sat.difference(clauses, result) is None
            assert mdd.equivalent(clauses, result)
        assert mass(result) <= mass(current)
        changed = canonical(current) != canonical(result)
        if changed:
            assert mass(result) < mass(current)
            productive += 1
        elif storage_key(current) != storage_key(result):
            order_only += 1
        trace.append(result)
        events.append(answer["events"])
        seconds.append(answer["seconds"])
        if storage_key(current) == storage_key(result):
            return dict(trace=trace, events=events, seconds=seconds,
                        productive=productive, order_only=order_only,
                        calls=len(trace) - 1, mass=[mass(x) for x in trace])
        current = result
    raise RuntimeError("No exact fixed point within %d passes" % limit)


def chain_family(n):
    """A proposed one-stage-per-pass chain of already-contained SSE2 losses.

    A0 = S0=0 or S1=0.
    Bi = Si=1 or T=1 or G=gi, for 0 <= i < n.
    Ai = Si=0 or S(i+1)=0 or T=0 or G in {g0,...,g(i-1)}.
    The initially four-symbol Ai clauses are supplied in decreasing i order.
    """
    domains = {"G": ["g%d" % i for i in range(n + 1)], "T": ["0", "1"]}
    domains.update({"S%d" % i: ["0", "1"] for i in range(n + 2)})
    seed = {"S0": ["0"], "S1": ["0"]}
    blockers = [{"S%d" % i: ["1"], "T": ["1"], "G": ["g%d" % i]}
                for i in range(n)]
    targets = [{"S%d" % i: ["0"], "S%d" % (i + 1): ["0"],
                "T": ["0"], "G": ["g%d" % j for j in range(i)]}
               for i in range(n, 0, -1)]
    return domains, [seed] + blockers + targets


def main():
    bridge = Bridge()
    records = []
    try:
        for n in (1, 2, 3, 4, 5, 8, 12):
            domains, clauses = chain_family(n)
            result = iterate(bridge, domains, clauses, limit=n + 10, audit=n <= 4)
            record = dict(n=n, domains=domains, clauses=clauses, **result)
            records.append(record)
            print("n=%d productive=%d order_only=%d mass=%s" %
                  (n, result["productive"], result["order_only"], result["mass"]), flush=True)
            (HERE / "chain_pilot.json").write_text(json.dumps(records, indent=2) + "\n")
    finally:
        bridge.close()


if __name__ == "__main__":
    main()
