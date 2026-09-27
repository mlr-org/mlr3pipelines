"""Reduce a satisfiable case which physically defers a unit restriction.

The property is an instrumented branch observation, not a claimed bug.
SAT and MDD independently check the final reduced case.
"""
import copy
import json
import time

from oracle import HERE, MDDOracle, OneHotOracle, RBridge, audit_events, normalize


def main():
    report = json.loads((HERE / "outside_skip_satisfiable_dense.json").read_text())
    domains, clauses = report["domains"], normalize(report["clauses"])
    bridge = RBridge()
    calls, start = 0, time.time()

    def reproduces(ds, cs):
        nonlocal calls
        calls += 1
        answer = bridge.simplify(ds, cs, audit=True, variant="lifecycle")
        if not answer["ok"]:
            return False
        counts = audit_events(answer["events"], ds)
        return counts.get("unit_skip_outside", 0) and OneHotOracle(ds).difference(cs, False) is not None

    assert reproduces(domains, clauses)
    changed = True
    while changed:
        changed = False
        for i in range(len(clauses) - 1, -1, -1):
            candidate = clauses[:i] + clauses[i + 1:]
            if reproduces(domains, candidate):
                clauses, changed = candidate, True
        for i in range(len(clauses)):
            for symbol in list(clauses[i]):
                candidate = copy.deepcopy(clauses)
                del candidate[i][symbol]
                if reproduces(domains, candidate):
                    clauses, changed = candidate, True
                    continue
                for value in list(clauses[i][symbol]):
                    candidate = copy.deepcopy(clauses)
                    candidate[i][symbol] = tuple(v for v in candidate[i][symbol] if v != value)
                    if reproduces(domains, candidate):
                        clauses, changed = candidate, True
        for symbol in list(domains):
            for value in list(domains[symbol]):
                if len(domains[symbol]) == 1:
                    break
                ds = copy.deepcopy(domains)
                ds[symbol].remove(value)
                cs = [{s: tuple(v for v in vals if not (s == symbol and v == value))
                       for s, vals in c.items()} for c in clauses]
                if reproduces(ds, cs):
                    domains, clauses, changed = ds, cs, True
        print(json.dumps(dict(calls=calls, clauses=len(clauses),
                              values=sum(len(v) for c in clauses for v in c.values()),
                              domains=list(map(len, domains.values())))), flush=True)
    answer = bridge.simplify(domains, clauses, audit=True, detailed=True, variant="lifecycle")
    bridge.close()
    assert OneHotOracle(domains).difference(clauses, answer["result"]) is None
    assert MDDOracle(domains).equivalent(clauses, answer["result"])
    result = dict(domains=domains, clauses=clauses, answer=answer,
                  counts=audit_events(answer["events"], domains), calls=calls, elapsed=time.time() - start)
    (HERE / "minimized_deferred_skip.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps({k: v for k, v in result.items() if k != "answer"}, indent=2), flush=True)


if __name__ == "__main__":
    main()
