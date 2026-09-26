"""Greedily minimize a constructed witness with at least three useful passes."""

import copy
import json
from repeated import HERE, Bridge, chain_family, iterate, normalize, mass


def canonical_input(domains, clauses):
    # Keep every literal proper, so public CnfClause construction does not
    # turn a clause into TRUE before the simplification kernel sees it.
    return (bool(clauses) and all(clauses) and all(
        values and set(values) < set(domains[s])
        for clause in clauses for s, values in clause.items()))


def clean_domains(domains, clauses):
    used = set().union(*(set(clause) for clause in clauses))
    return {s: domain for s, domain in domains.items() if s in used}


bridge = Bridge()
domains, clauses = chain_family(3)
calls = 0
accepted = []


def preserves(ds, cs, action):
    global calls, domains, clauses
    if not canonical_input(ds, cs):
        return False
    ds = clean_domains(ds, cs)
    calls += 1
    result = iterate(bridge, ds, cs, limit=mass(cs) + 2, verify=False)
    if result["productive"] < 3:
        return False
    checked = iterate(bridge, ds, cs, limit=mass(cs) + 2, verify=True)
    domains, clauses = ds, cs
    accepted.append(dict(action=action, productive=checked["productive"],
                         clauses=len(cs), symbols=len(ds), mass=mass(cs)))
    print(accepted[-1], flush=True)
    return True


try:
    changed = True
    while changed:
        changed = False
        for i in range(len(clauses)):
            if preserves(domains, clauses[:i] + clauses[i + 1:], "delete clause %d" % i):
                changed = True
                break
        if changed:
            continue
        for i, clause in enumerate(clauses):
            for symbol, values in clause.items():
                proposal = copy.deepcopy(clauses)
                del proposal[i][symbol]
                if preserves(domains, proposal, "delete literal %d:%s" % (i, symbol)):
                    changed = True
                    break
                if len(values) > 1:
                    for value in values:
                        proposal = copy.deepcopy(clauses)
                        proposal[i][symbol].remove(value)
                        if preserves(domains, proposal,
                                     "delete literal value %d:%s=%s" % (i, symbol, value)):
                            changed = True
                            break
                if changed:
                    break
            if changed:
                break
        if changed:
            continue
        for symbol, domain in domains.items():
            if len(domain) <= 2:
                continue
            for value in domain:
                ds = copy.deepcopy(domains)
                ds[symbol].remove(value)
                proposal = copy.deepcopy(clauses)
                for clause in proposal:
                    if value in clause.get(symbol, ()):
                        clause[symbol].remove(value)
                        if not clause[symbol]:
                            del clause[symbol]
                if preserves(ds, proposal, "delete domain value %s=%s" % (symbol, value)):
                    changed = True
                    break
            if changed:
                break
    result = iterate(bridge, domains, clauses, limit=mass(clauses) + 2,
                     verify=True, audit=True)
    record = dict(domains=domains, clauses=clauses, accepted=accepted,
                  minimization_calls=calls, **result)
    (HERE / "minimized_three_pass.json").write_text(json.dumps(record, indent=2) + "\n")
    print("FINAL", {k: result[k] for k in ("productive", "order_only", "mass")}, flush=True)
    print(json.dumps(dict(domains=domains, clauses=clauses), indent=2), flush=True)
finally:
    bridge.close()
