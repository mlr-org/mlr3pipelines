"""Explore first-call saturation for three clauses of width at least three.

This is a wider input class than the completed all-three-symbol theorem:
supports may differ and may contain arbitrarily many distinct symbols.
"""
import argparse
from collections import Counter
import itertools
import json
from pathlib import Path
import random
import subprocess
import sys
import time

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "independent_solver"))
from oracle import MDDOracle, OneHotOracle, RBridge, evaluate  # noqa: E402


def weight(formula):
    return 0 if isinstance(formula, bool) else sum(
        1 if isinstance(v, str) else len(v) for c in formula for v in c.values())


def make_case(rng, mode):
    n = rng.randrange(3, 9)
    symbols = [f"S{i}" for i in range(n)]
    domains = {s: list(map(str, range(rng.randrange(2, 7)))) for s in symbols}
    if mode == 0:
        supports = [rng.sample(symbols, rng.randrange(3, n + 1)) for unused in range(3)]
    elif mode == 1:
        supports = [rng.sample(symbols, 3) for unused in range(3)]
    elif mode == 2:
        supports = [symbols[:3] + rng.sample(symbols[3:], rng.randrange(n - 2)) for unused in range(3)]
    else:
        supports = [symbols[:] for unused in range(3)]
    orders = {s: rng.sample(d, len(d)) for s, d in domains.items()}
    clauses = []
    for support in supports:
        rng.shuffle(support)
        clause = {}
        for s in support:
            if mode in (2, 3):
                clause[s] = orders[s][:rng.randrange(1, len(domains[s]))]
            else:
                clause[s] = rng.sample(domains[s], rng.randrange(1, len(domains[s])))
            rng.shuffle(clause[s])
        clauses.append(clause)
    rng.shuffle(clauses)
    return domains, clauses


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--cases", type=int, default=20000)
    parser.add_argument("--r46", action="store_true")
    args = parser.parse_args()
    rng = random.Random(2609061709)
    if args.r46:
        bridge = RBridge.__new__(RBridge)
        bridge.process = subprocess.Popen(["podman", "exec", "-i", "cnf-review-r46", "Rscript",
            "/work/attic/cnf_verify3/independent_solver/r_bridge.R"], stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, text=True, bufsize=1)
    else:
        bridge = RBridge()
    stats, examples = Counter(), {}
    started = time.monotonic()
    for index in range(args.cases):
        domains, clauses = make_case(rng, index % 4)
        stats["inputs"] += 1
        stats["different_supports"] += not (set(clauses[0]) == set(clauses[1]) == set(clauses[2]))
        # Explicit independent matching/model certificate for every input.
        matching = next(t for t in itertools.product(*(tuple(c) for c in clauses)) if len(set(t)) == 3)
        model = {s: d[0] for s, d in domains.items()}
        model.update({s: clause[s][0] for s, clause in zip(matching, clauses)})
        assert evaluate(clauses, model)
        first = bridge.simplify(domains, clauses)
        assert first["ok"] and first["result"] is not False, (domains, clauses, first)
        second = bridge.simplify(domains, first["result"])
        assert second["ok"] and second["result"] is not False, (domains, clauses, first, second)
        stats["source_calls"] += 2
        stats["first_productive"] += weight(first["result"]) < weight(clauses)
        stats["second_productive"] += weight(second["result"]) < weight(first["result"])
        stats["retained_units"] += sum(len(c) == 1 for c in first["result"])
        oracle = MDDOracle(domains)
        assert oracle.equivalent(clauses, first["result"])
        assert oracle.equivalent(clauses, second["result"])
        stats["mdd_comparisons"] += 2
        if index % 257 == 0:
            assert OneHotOracle(domains).difference(clauses, first["result"]) is None
            stats["sat_calibrations"] += 1
        if weight(second["result"]) < weight(first["result"]):
            examples["productive_second_call"] = dict(domains=domains, clauses=clauses,
                first=first["result"], second=second["result"])
            break
        if (index + 1) % 1000 == 0:
            print(json.dumps(dict(stats=stats, seconds=time.monotonic() - started)), flush=True)
    bridge.close()
    result = dict(stats=stats, examples=examples, seed=2609061709,
                  seconds=time.monotonic() - started, runtime="R4.6.1" if args.r46 else "R3.6.3")
    suffix = "r46" if args.r46 else "r36"
    (HERE / f"three_wide_clauses_{suffix}.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
