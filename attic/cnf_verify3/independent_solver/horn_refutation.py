"""Compile negated-target propagation to propositional Horn implications.

An atom (s, v) means that value v has been excluded from symbol s. Unlike the
domain oracle, this engine never counts a donor's possible literals. It uses
only ordinary Horn-rule forward chaining with an incidence queue.
"""
from collections import defaultdict, deque

from oracle import normalize, values


def horn_refute(domains, donors, target):
    donors = normalize(donors)
    facts = {(s, v) for s, r in target.items() for v in values(r)}
    for s, domain in domains.items():
        if all((s, v) in facts for v in domain):
            return dict(refuted=True, trace=[dict(kind="initial_empty", symbol=s)])
    rules = []
    for i, donor in enumerate(donors):
        literals = {s: {(s, v) for v in r} for s, r in donor.items()}
        body = set().union(*literals.values())
        rules.append((body, None, dict(kind="conflict", donor=i)))
        for symbol, own in literals.items():
            antecedent = body - own
            for value in domains[symbol]:
                atom = (symbol, value)
                if atom not in own:
                    rules.append((antecedent, atom, dict(kind="exclude", donor=i,
                                                        symbol=symbol, value=value)))
    waiting, missing, queue = defaultdict(list), [], deque()
    for i, (body, _, _) in enumerate(rules):
        unfired = body - facts
        missing.append(len(unfired))
        if not unfired:
            queue.append(i)
        for atom in unfired:
            waiting[atom].append(i)
    trace = []
    while queue:
        i = queue.popleft()
        _, head, step = rules[i]
        if head is None:
            trace.append(step)
            return dict(refuted=True, trace=trace)
        if head in facts:
            continue
        facts.add(head)
        trace.append(step)
        for dependent in waiting[head]:
            missing[dependent] -= 1
            if not missing[dependent]:
                queue.append(dependent)
    return dict(refuted=False, trace=trace,
                excluded={s: sorted(v for v in d if (s, v) in facts)
                          for s, d in domains.items()})


def verify_horn_certificate(domains, donors, target, trace):
    """Check value exclusions directly from clause literals, without a compiler."""
    excluded = {s: set(values(target.get(s))) for s in domains}
    if len(trace) == 1 and trace[0]["kind"] == "initial_empty":
        assert set(domains[trace[0]["symbol"]]) <= excluded[trace[0]["symbol"]]
        return True
    for n, step in enumerate(trace):
        donor = donors[step["donor"]]
        if step["kind"] == "conflict":
            assert all(set(values(r)) <= excluded[s] for s, r in donor.items())
            assert n == len(trace) - 1
            return True
        assert step["kind"] == "exclude"
        symbol, value = step["symbol"], step["value"]
        assert symbol in donor and value in domains[symbol]
        assert value not in donor[symbol] and value not in excluded[symbol]
        assert all(set(values(r)) <= excluded[s] for s, r in donor.items() if s != symbol)
        excluded[symbol].add(value)
    raise AssertionError("The Horn certificate does not end in a contradiction")
