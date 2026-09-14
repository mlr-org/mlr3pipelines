"""Clause redundancy by domain propagation under the clause's negation.

This works on admissible value domains, not virtual clauses or subset counts.
Every successful result includes an elementary propagation certificate.
"""
from oracle import normalize, values


def refute_negated_clause(domains, donors, target):
    allowed = {s: set(d) - set(values(target.get(s))) for s, d in domains.items()}
    trace = []
    if any(not d for d in allowed.values()):
        return dict(refuted=True, trace=[dict(kind="initial_empty")])
    donors = [{s: set(v) for s, v in c.items()} for c in normalize(donors)]
    changed = True
    while changed:
        changed = False
        for i, donor in enumerate(donors):
            possible = [(s, v & allowed[s]) for s, v in donor.items() if v & allowed[s]]
            if not possible:
                trace.append(dict(kind="conflict", donor=i))
                return dict(refuted=True, trace=trace)
            if len(possible) == 1:
                symbol, restricted = possible[0]
                if restricted != allowed[symbol]:
                    trace.append(dict(kind="unit", donor=i, symbol=symbol,
                                      removed=sorted(allowed[symbol] - restricted)))
                    allowed[symbol] = restricted
                    changed = True
    return dict(refuted=False, trace=trace,
                residual_domains={s: sorted(d) for s, d in allowed.items()})


def verify_refutation(domains, donors, target, certificate):
    """Replay a claimed contradiction directly from each donor's literals."""
    allowed = {s: set(d) - set(values(target.get(s))) for s, d in domains.items()}
    if certificate == [dict(kind="initial_empty")]:
        assert any(not d for d in allowed.values())
        return True
    assert all(allowed.values())
    for n, step in enumerate(certificate):
        donor = donors[step["donor"]]
        if step["kind"] == "conflict":
            assert all(not (set(values(v)) & allowed[s]) for s, v in donor.items())
            assert n == len(certificate) - 1
            return True
        assert step["kind"] == "unit"
        symbol = step["symbol"]
        assert symbol in donor
        assert all(not (set(values(v)) & allowed[s]) for s, v in donor.items() if s != symbol)
        removed = set(step["removed"])
        assert removed
        assert removed <= allowed[symbol] - set(values(donor[symbol]))
        allowed[symbol] -= removed
        assert allowed[symbol]
    raise AssertionError("No final conflict in the supplied certificate")


def redundancy_opportunities(domains, formula):
    if isinstance(formula, bool):
        return []
    formula = normalize(formula)
    result = []
    for i, target in enumerate(formula):
        donors = formula[:i] + formula[i + 1:]
        report = refute_negated_clause(domains, donors, target)
        if report["refuted"]:
            assert verify_refutation(domains, donors, target, report["trace"])
            result.append(dict(target=i, certificate=report["trace"]))
    return result
