"""Preserve an SSE2 leftover caused by complete removal of a donor literal."""
import copy
import json
import time

from fixed_point import opportunities
from oracle import HERE, MDDOracle, OneHotOracle, RBridge, normalize, values


def plain_sets(clause):
    return {s: frozenset(values(v)) for s, v in clause.items()}


def desired(answer):
    if not answer["ok"]:
        return False
    found = opportunities(answer["result"])
    if any(o["kind"] == "sse1" for o in found):
        return False
    for o in found:
        if o["kind"] != "sse2":
            continue
        t = o["restrict_symbol"]
        donors = [plain_sets(answer["result"][o[k]]) for k in ("donor_a", "donor_b")]
        for event in answer["events"]:
            if event["kind"] != "sse2" or event["restrict_symbol"] != t:
                continue
            before = plain_sets(event["target"])
            if len(before) < 3 or t not in before:
                continue
            a, b = plain_sets(event["donor_a"]), plain_sets(event["donor_b"])
            if before[t] & (a.get(t, set()) | b.get(t, set())):
                continue
            del before[t]
            if before in donors:
                return True
    return False


def main():
    record = json.loads((HERE / "directed_oneend_symbol_removal_asymmetric.json").read_text())
    domains, clauses = record["domains"], normalize(record["clauses"])
    bridge, calls, start = RBridge(), 0, time.time()
    def reproduces(ds, cs):
        nonlocal calls
        calls += 1
        return desired(bridge.simplify(ds, cs, audit=True))
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
        print(json.dumps(dict(calls=calls, clauses=len(clauses), values=sum(len(v) for c in clauses for v in c.values()), domains=list(map(len, domains.values())))), flush=True)
    answer = bridge.simplify(domains, clauses, audit=True, detailed=True)
    second = bridge.simplify(domains, answer["result"], audit=True)
    bridge.close()
    assert OneHotOracle(domains).difference(clauses, answer["result"]) is None
    assert MDDOracle(domains).equivalent(clauses, answer["result"])
    assert OneHotOracle(domains).difference(clauses, second["result"]) is None
    record = dict(domains=domains, clauses=clauses, answer=answer, second=second,
                  opportunities=opportunities(answer["result"]), calls=calls, elapsed=time.time() - start)
    (HERE / "minimized_oneend_symbol_removal.json").write_text(json.dumps(record, indent=2) + "\n")
    print(json.dumps({k:v for k,v in record.items() if k not in ("answer", "second")}, indent=2))


if __name__ == "__main__":
    main()
