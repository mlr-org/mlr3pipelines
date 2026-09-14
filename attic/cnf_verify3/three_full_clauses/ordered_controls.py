"""Directed and seeded symbol-order controls for the three-full-clause proof.

This is a small falsification experiment, not another full profile enumeration.
All 216 independent within-clause orders are replayed for the directed cases;
seeded controls fix the first clause order and replay all 36 remaining orders.
The scalar evaluator and normalization below are independent of root's profile
generator and positional-mask truth oracle.
"""

from collections import Counter
import hashlib
import itertools
import json
from pathlib import Path
import random
import sys
import time

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]
sys.path.insert(0, str(HERE.parent / "independent_solver"))
from oracle import RBridge  # noqa: E402


def values(value):
    return [value] if isinstance(value, str) else value


def normalize(formula):
    if isinstance(formula, bool):
        return formula
    return tuple(sorted(tuple(sorted((s, tuple(sorted(values(v))))
                                     for s, v in c.items())) for c in formula))


def models(domains, formula):
    symbols = tuple(domains)
    return {assignment for assignment in itertools.product(*(domains[s] for s in symbols))
            if formula is True or formula is not False and all(
                any(assignment[symbols.index(s)] in values(v) for s, v in c.items())
                for c in formula)}


def clauses_from_columns(columns):
    return [{s: list(columns[s][i]) for s in "XYZ"} for i in range(3)]


def replay(bridge, domains, clauses, orders, scalar=False):
    outcomes = {}
    summary = Counter()
    expected = models(domains, clauses) if scalar else None
    for order in orders:
        original = [{s: c[s] for s in p} for c, p in zip(clauses, order)]
        first = bridge.simplify(domains, original)
        assert first["ok"], first
        second = bridge.simplify(domains, first["result"])
        assert second["ok"], second
        summary["executions"] += 2
        summary["arrangements"] += 1
        if scalar:
            assert models(domains, first["result"]) == expected
            assert models(domains, second["result"]) == expected
            summary["full_model_comparisons"] += 2
        first_key = normalize(first["result"])
        second_key = normalize(second["result"])
        outcomes.setdefault(repr(first_key), dict(order=order, first=first["result"]))
        if first_key != second_key:
            return summary, outcomes, dict(domains=domains, clauses=original,
                                            first=first["result"], second=second["result"])
    return summary, outcomes, None


def main():
    start = time.time()
    permutations = tuple(itertools.permutations("XYZ"))
    all_orders = tuple(itertools.product(permutations, repeat=3))
    reduced_orders = tuple((("X", "Y", "Z"), p, q)
                           for p in permutations for q in permutations)
    directed = [
        dict(name="unit_birth_no_range_propagation",
             domains={s: list("0123") for s in "XYZ"},
             clauses=clauses_from_columns(dict(X=("012", "01", "2"),
                                              Y=("0", "1", "2"),
                                              Z=("0", "1", "2")))),
        dict(name="two_symbol_equality_gap_with_common_third_literal",
             domains=dict(X=list("1245"), Y=list("235"), Z=list("01")),
             clauses=clauses_from_columns(dict(X=("24", "12", "25"),
                                              Y=("3", "5", "2"),
                                              Z=("0", "0", "0")))),
        dict(name="proposed_first_order_relay_deleted_donor_control",
             domains=dict(X=list("0123"), Y=list("012"), Z=list("01")),
             clauses=clauses_from_columns(dict(X=("01", "0", "12"),
                                              Y=("0", "01", "0"),
                                              Z=("0", "0", "0")))),
    ]
    report = dict(source_sha256=hashlib.sha256((REPO / "R/CnfFormula_simplify.R").read_bytes()).hexdigest(),
                  seed=309062026, scope="directed and seeded full-three-clause symbol-order controls",
                  directed=[], random_cases=0, summary={}, ordering_examples=[], failures=[])
    summary = Counter()
    bridge = RBridge()
    for case in directed:
        counts, outcomes, failure = replay(bridge, case["domains"], case["clauses"], all_orders, scalar=True)
        summary.update(counts)
        report["directed"].append(dict(**case, arrangements=216,
                                       distinct_outputs=len(outcomes), representatives=list(outcomes.values())))
        if failure:
            report["failures"].append(failure)
            break
    rng = random.Random(report["seed"])
    for index in range(1000):
        domains = {s: list("0123") for s in "XYZ"}
        columns = {}
        for s in "XYZ":
            column = []
            for i in range(3):
                if i and rng.randrange(4) == 0:
                    column.append(rng.choice(column))
                else:
                    column.append(rng.sample(domains[s], rng.randrange(1, 4)))
            columns[s] = column
        clauses = clauses_from_columns(columns)
        counts, outcomes, failure = replay(bridge, domains, clauses, reduced_orders,
                                           scalar=index % 25 == 0)
        summary.update(counts)
        report["random_cases"] += 1
        if len(outcomes) > 1 and len(report["ordering_examples"]) < 3:
            report["ordering_examples"].append(dict(index=index, domains=domains, clauses=clauses,
                                                    representatives=list(outcomes.values())))
        if failure:
            report["failures"].append(failure)
            break
    bridge.close()
    report["summary"] = dict(summary)
    report["elapsed_seconds"] = time.time() - start
    (HERE / "ordered_controls_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(dict(summary=report["summary"], random_cases=report["random_cases"],
                          ordering_examples=len(report["ordering_examples"]), failures=len(report["failures"]),
                          elapsed_seconds=report["elapsed_seconds"])))
    assert not report["failures"]


if __name__ == "__main__":
    main()
