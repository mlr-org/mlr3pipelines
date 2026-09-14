"""Small negative controls for tempting strengthenings of the reviewed claims."""

import json
from itertools import combinations_with_replacement

from checks import (Bridge, HERE, downset, meet_closed, normalized_clauses,
                    prefix_domains, relational_meet_closed, truth_models)


def run(bridge):
    domains = {s: ["0", "1"] for s in "XY"}
    clauses = [{"X": ["0"], "Y": ["1"]},
               {"X": ["1"], "Y": ["0"]},
               {"X": ["1"], "Y": ["1"]}]
    models = truth_models(domains, normalized_clauses(clauses))
    assert models == {("1", "1")}
    assert all(tuple(min(x, y) for x, y in zip(a, b)) in models
               for a, b in combinations_with_replacement(models, 2))
    answer = bridge.ask({"domains": domains, "clauses": clauses, "full": True})
    assert prefix_domains(domains, answer) == {s: frozenset(values) for s, values in domains.items()}
    assert ("0", "0") not in models

    diamond = tuple(tuple(x & y for y in range(4)) for x in range(4))
    nonclosed = frozenset([1, 2])
    assert not meet_closed(diamond, nonclosed)
    assert not downset(diamond, nonclosed)
    # Properness is essential for the iff characterization: the other literal
    # is full, so the relation is a tautology despite this nonclosed range.
    assert relational_meet_closed(diamond, [frozenset(range(4)), nonclosed])

    unit_domains = {"X": list(map(str, range(4)))}
    unit_clauses = [{"X": ["1", "2"]}]
    unit_answer = bridge.ask({"domains": unit_domains, "clauses": unit_clauses})
    assert prefix_domains(unit_domains, unit_answer) == {"X": frozenset(["1", "2"])}
    assert ("0",) not in truth_models(unit_domains, normalized_clauses(unit_clauses))
    return {
        "version": bridge.version,
        "whole_relation_meet_closed_does_not_suffice": {
            "domains": domains, "clauses": clauses, "models": sorted(models),
            "initial_coordinate_minima": ["0", "0"], "answer": answer,
        },
        "full_literal_hides_nonclosed_range": {
            "meet": diamond, "ranges": [list(range(4)), sorted(nonclosed)],
            "relation_closed": True, "every_range_closed": False,
        },
        "range_closure_is_needed_for_model_extraction": {
            "domains": unit_domains, "clauses": unit_clauses,
            "meet_of_surviving_domain": "0", "answer": unit_answer,
        },
        "status": "all negative controls reproduced",
    }


for r46 in (False, True):
    bridge = Bridge(r46)
    try:
        result = run(bridge)
    finally:
        bridge.close()
    suffix = "r46" if r46 else "r36"
    (HERE / f"limits_{suffix}.json").write_text(json.dumps(result, indent=2) + "\n")
    print(result["version"]["version"], result["status"])
