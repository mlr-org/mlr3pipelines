"""Calibrate propagation certificates against SAT and direct literal semantics."""
import collections
import itertools
import json
import random

from calibration import all_clauses
from domain_refutation import refute_negated_clause, verify_refutation
from oracle import HERE, OneHotOracle, evaluate
from structured_families import hla_chain


def main():
    stats = collections.Counter()
    ds = {s: ["0", "1"] for s in ("X", "Y", "Z")}
    pool = list(all_clauses(ds))
    sat = OneHotOracle(ds)
    for cs in itertools.combinations(pool, 3):
        for i, target in enumerate(cs):
            donors = list(cs[:i] + cs[i + 1:])
            report = refute_negated_clause(ds, donors, target)
            stats["queries"] += 1
            if report["refuted"]:
                assert verify_refutation(ds, donors, target, report["trace"])
                assert sat.implies(donors, [target])
                for row in itertools.product(*ds.values()):
                    a = dict(zip(ds, row))
                    assert not evaluate(donors, a) or evaluate([target], a)
                stats["verified_refutations"] += 1
    rng = random.Random(38859832)
    for i in range(100):
        ds, cs, _ = hla_chain(rng, rng.randint(3, 25), unit=bool(i % 2), branches=rng.randint(1, 3))
        report = refute_negated_clause(ds, cs[1:], cs[0])
        assert report["refuted"]
        assert verify_refutation(ds, cs[1:], cs[0], report["trace"])
        assert OneHotOracle(ds).implies(cs[1:], [cs[0]])
        stats["known_chain_refutations"] += 1
        stats["chain_unit_steps"] += sum(e["kind"] == "unit" for e in report["trace"])
    (HERE / "domain_refutation_calibration_results.json").write_text(json.dumps(stats, indent=2) + "\n")
    print(json.dumps(stats, indent=2))


if __name__ == "__main__":
    main()
