"""Greedy reduction of residual-rule findings; preserves a requested rule kind."""
import copy
import json
import sys
import time

from fixed_point import opportunities
from oracle import HERE, MDDOracle, OneHotOracle, RBridge, normalize
from structured_families import canonical


def main():
    kind = sys.argv[1]
    source_name = sys.argv[2] if len(sys.argv) > 2 else "fixed_point_results.json"
    variant = sys.argv[3] if len(sys.argv) > 3 else None
    output_name = sys.argv[4] if len(sys.argv) > 4 else "minimized_" + kind + ".json"
    reports = json.loads((HERE / source_name).read_text())["findings"]
    report = next(r for r in reports if any(x["kind"] == kind for x in r["opportunities"]))
    domains, clauses = report["domains"], normalize(report["clauses"])
    bridge = RBridge()
    calls = 0
    start = time.time()
    def reproduces(ds, cs):
        nonlocal calls
        calls += 1
        answer = bridge.simplify(ds, cs, variant=variant)
        return answer["ok"] and any(x["kind"] == kind for x in opportunities(answer["result"]))
    assert reproduces(domains, clauses)
    changed = True
    while changed:
        changed = False
        for i in range(len(clauses) - 1, -1, -1):
            candidate = clauses[:i] + clauses[i + 1:]
            if reproduces(domains, candidate):
                clauses = candidate
                changed = True
        for i in range(len(clauses)):
            for symbol in list(clauses[i]):
                candidate = copy.deepcopy(clauses)
                del candidate[i][symbol]
                if reproduces(domains, candidate):
                    clauses = candidate
                    changed = True
                    continue
                for value in list(clauses[i][symbol]):
                    candidate = copy.deepcopy(clauses)
                    candidate[i][symbol] = tuple(v for v in candidate[i][symbol] if v != value)
                    if reproduces(domains, candidate):
                        clauses = candidate
                        changed = True
        for symbol in list(domains):
            for value in list(domains[symbol]):
                if len(domains[symbol]) == 1:
                    break
                ds = copy.deepcopy(domains)
                ds[symbol].remove(value)
                cs = [{s: tuple(v for v in vals if not (s == symbol and v == value))
                       for s, vals in c.items()} for c in clauses]
                if reproduces(ds, cs):
                    domains, clauses = ds, cs
                    changed = True
        print(json.dumps(dict(calls=calls, clauses=len(clauses), symbols=sum(map(len, clauses)),
                              values=sum(len(v) for c in clauses for v in c.values()), domains=list(map(len, domains.values())))), flush=True)
    answer = bridge.simplify(domains, clauses, audit=True, variant=variant)
    second = bridge.simplify(domains, answer["result"], audit=True, variant=variant)
    bridge.close()
    assert answer["ok"] and second["ok"]
    sat, mdd = OneHotOracle(domains), MDDOracle(domains)
    assert sat.difference(clauses, answer["result"]) is None
    assert sat.difference(clauses, second["result"]) is None
    assert mdd.equivalent(clauses, answer["result"])
    assert mdd.equivalent(clauses, second["result"])
    result = dict(kind=kind, variant=variant, domains=domains, clauses=clauses, result=answer["result"],
                  opportunities=opportunities(answer["result"]), second_pass=second["result"],
                  idempotent=canonical(answer["result"]) == canonical(second["result"]),
                  events=answer["events"], second_pass_events=second["events"], calls=calls, elapsed=time.time() - start)
    (HERE / output_name).write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps({k: v for k, v in result.items() if "events" not in k}, indent=2))


if __name__ == "__main__":
    main()
