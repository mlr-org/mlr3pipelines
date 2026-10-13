"""Calibrate the quotient experiment's bit-vector evaluator independently."""
import json
import random

from three_symbol_quotient import BitTruth, HERE, MDDOracle, OneHotOracle

rng = random.Random(6904)
comparisons = changed_models = 0
for trial in range(2000):
    domains = {s: [str(i) for i in range(rng.randrange(2, 6))] for s in ("X", "Y", "Z")}
    clauses = []
    for _ in range(rng.randrange(1, 7)):
        clause = {}
        for s in rng.sample(list(domains), rng.randrange(1, 4)):
            clause[s] = rng.sample(domains[s], rng.randrange(1, len(domains[s])))
        clauses.append(clause)
    oracle = BitTruth(domains)
    expected = 0
    for row, values in enumerate(oracle.assignments):
        assignment = dict(zip(domains, values))
        satisfied = all(any(assignment[s] in allowed for s, allowed in c.items()) for c in clauses)
        if satisfied:
            expected |= 1 << row
    assert oracle.formula(clauses) == expected
    assert oracle.formula(True) == (1 << len(oracle.assignments)) - 1
    assert oracle.formula(False) == 0
    comparisons += 1
    if expected:
        bit = expected & -expected
        point = oracle.witness(bit)
        blocker = {s: [v for v in d if v != point[s]] for s, d in domains.items()}
        altered = clauses + [blocker]
        assert oracle.formula(altered) == expected ^ bit
        changed_models += 1
        if trial % 13 == 0:
            sat, mdd = OneHotOracle(domains), MDDOracle(domains)
            assert sat.difference(clauses, altered) is not None
            assert not mdd.equivalent(clauses, altered)

report = dict(seed=6904, direct_truth_comparisons=comparisons, deliberate_single_model_changes=changed_models)
(HERE / "three_symbol_truth_checks.json").write_text(json.dumps(report, indent=2) + "\n")
print(json.dumps(report))
