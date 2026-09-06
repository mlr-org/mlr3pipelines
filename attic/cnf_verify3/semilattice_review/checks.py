"""Independent finite-relation review of the initial CNF propagation prefix.

No campaign oracle or domain-propagation implementation is imported. GAC is
computed by materializing satisfying tuples, deleting tuples outside the
current box, and projecting the surviving tuples onto each coordinate.
"""

import argparse
from collections import Counter
from functools import reduce
from itertools import combinations, combinations_with_replacement, permutations, product
import json
from pathlib import Path
import random
import subprocess
import time


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]


def subsets(values, proper=False):
    end = len(values) if proper else len(values) + 1
    return [frozenset(part) for n in range(1, end) for part in combinations(values, n)]


def validate_meet(table):
    n = len(table)
    return all(table[x][x] == x for x in range(n)) and all(
        table[x][y] == table[y][x] for x, y in product(range(n), repeat=2)
    ) and all(
        table[table[x][y]][z] == table[x][table[y][z]]
        for x, y, z in product(range(n), repeat=3)
    )


def labeled_semilattices(n):
    pairs = list(combinations(range(n), 2))
    for entries in product(range(n), repeat=len(pairs)):
        table = [[x if x == y else None for y in range(n)] for x in range(n)]
        for (x, y), meet in zip(pairs, entries):
            table[x][y] = table[y][x] = meet
        if validate_meet(table):
            yield tuple(map(tuple, table))


def canonical_table(table):
    n = len(table)
    encodings = []
    for order in permutations(range(n)):
        inverse = {x: i for i, x in enumerate(order)}
        encodings.append(tuple(inverse[table[x][y]] for x in order for y in order))
    return min(encodings)


def meet_closed(table, values):
    return all(table[x][y] in values for x, y in product(values, repeat=2))


def downset(table, values):
    return all(x in values for y in values for x in range(len(table)) if table[x][y] == x)


def syntactic_class(table, ranges):
    return all(meet_closed(table, r) for r in ranges) and sum(
        not downset(table, r) for r in ranges
    ) <= 1


def relational_meet_closed(table, ranges):
    """Evaluate the whole clause truth relation and every unordered tuple pair."""
    relation = {
        row for row in product(range(len(table)), repeat=len(ranges))
        if any(x in r for x, r in zip(row, ranges))
    }
    for left, right in combinations_with_replacement(relation, 2):
        if tuple(table[x][y] for x, y in zip(left, right)) not in relation:
            return False
    return True


def named_tables():
    # M3 is the five-element nondistributive diamond.
    m3 = tuple(tuple(x if x == y else y if x == 4 else x if y == 4 else 0
                     for y in range(5)) for x in range(5))
    # N5: 0 < 1 < 2 < 4, and 0 < 3 < 4; 3 is incomparable with 1 and 2.
    chains = [set([0]), set([0, 1]), set([0, 1, 2]), set([0, 3]), set(range(5))]
    n5 = tuple(tuple(max(chains[x] & chains[y], key=lambda z: len(chains[z]))
                     for y in range(5)) for x in range(5))
    return {
        "M3": m3,
        "N5": n5,
        "powerset_3": tuple(tuple(x & y for y in range(8)) for x in range(8)),
    }


def algebra_checks(rng):
    counts = Counter()
    representatives = {}
    labeled_counts = {}
    unlabeled_counts = {}
    for n in range(1, 5):
        tables = list(labeled_semilattices(n))
        labeled_counts[str(n)] = len(tables)
        unique = {}
        for table in tables:
            unique.setdefault(canonical_table(table), table)
            proper = subsets(tuple(range(n)), proper=True)
            for width in (1, 2):
                for ranges in product(proper, repeat=width):
                    assert relational_meet_closed(table, ranges) == syntactic_class(table, ranges)
                    counts["all_labeled_unary_binary"] += 1
        unlabeled_counts[str(n)] = len(unique)
        for i, table in enumerate(unique.values()):
            name = f"size_{n}_type_{i}"
            representatives[name] = table
            for ranges in product(subsets(tuple(range(n)), proper=True), repeat=3):
                assert relational_meet_closed(table, ranges) == syntactic_class(table, ranges)
                counts["all_unlabeled_ternary"] += 1
        print(json.dumps({"algebra_size_done": n, "counts": counts}), flush=True)
    assert labeled_counts == {"1": 1, "2": 2, "3": 9, "4": 76}
    assert unlabeled_counts == {"1": 1, "2": 1, "3": 2, "4": 5}
    for name, table in named_tables().items():
        assert validate_meet(table)
        representatives[name] = table
        proper = subsets(tuple(range(len(table))), proper=True)
        range_tuples = list(product(proper, repeat=2)) if len(table) == 5 else [
            tuple(rng.choice(proper) for _ in range(2)) for _ in range(1000)
        ]
        range_tuples += [tuple(rng.choice(proper) for _ in range(3)) for _ in range(300)]
        for ranges in range_tuples:
            assert relational_meet_closed(table, ranges) == syntactic_class(table, ranges)
            counts[name] += 1
        print(json.dumps({"algebra_named_done": name, "counts": counts}), flush=True)
    return representatives, dict(counts), labeled_counts, unlabeled_counts


def normalized_clauses(clauses):
    if isinstance(clauses, bool):
        return clauses
    return [{s: frozenset([r] if isinstance(r, str) else r) for s, r in c.items()}
            for c in clauses]


def relations(domains, clauses):
    result = []
    for clause in clauses:
        scope = tuple(clause)
        tuples = [row for row in product(*(domains[s] for s in scope))
                  if any(value in clause[s] for s, value in zip(scope, row))]
        result.append((scope, tuples))
    return result


def project_relation(scope, tuples, box):
    surviving = [row for row in tuples if all(x in box[s] for s, x in zip(scope, row))]
    if not surviving:
        return None
    return {s: frozenset(row[i] for row in surviving) for i, s in enumerate(scope)}


def relation_gac(domains, clause_relations):
    box = {s: frozenset(values) for s, values in domains.items()}
    while True:
        before = dict(box)
        for scope, tuples in clause_relations:
            supported = project_relation(scope, tuples, box)
            if supported is None:
                return None
            box.update(supported)
        if box == before:
            return box


def is_consistent_box(clause_relations, box):
    return all((projection := project_relation(scope, tuples, box)) is not None
               and all(projection[s] == box[s] for s in scope)
               for scope, tuples in clause_relations)


def enumerate_fixed_boxes(domains, clause_relations):
    symbols = tuple(domains)
    fixed = []
    examined = 0
    for parts in product(*(subsets(domains[s]) for s in symbols)):
        examined += 1
        box = dict(zip(symbols, parts))
        if is_consistent_box(clause_relations, box):
            fixed.append(box)
    if not fixed:
        return None, examined, 0
    greatest = {s: frozenset().union(*(box[s] for box in fixed)) for s in symbols}
    assert is_consistent_box(clause_relations, greatest)
    assert all(all(box[s] <= greatest[s] for s in symbols) for box in fixed)
    return greatest, examined, len(fixed)


def truth_models(domains, clauses):
    symbols = tuple(domains)
    result = set()
    for row in product(*(domains[s] for s in symbols)):
        assignment = dict(zip(symbols, row))
        if clauses is True or (clauses is not False and all(
            any(assignment[s] in r for s, r in clause.items()) for clause in clauses
        )):
            result.add(row)
    return result


def serializable(value):
    if isinstance(value, (set, frozenset)):
        return sorted(value)
    raise TypeError(type(value))


class Bridge:
    def __init__(self, r46):
        command = (["podman", "exec", "-i", "cnf-review-r46", "Rscript"] if r46 else ["Rscript"])
        self.process = subprocess.Popen(command + [str(HERE.relative_to(ROOT) / "bridge.R")],
                                        cwd=ROOT, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                        text=True, bufsize=1)
        self.version = self.ask({"version": True})

    def ask(self, request):
        self.process.stdin.write(json.dumps(request, default=serializable) + "\n")
        self.process.stdin.flush()
        line = self.process.stdout.readline()
        assert line, (self.process.poll(), request)
        result = json.loads(line)
        assert result["ok"], (request, result)
        return result

    def close(self):
        self.process.stdin.close()
        assert self.process.wait(timeout=10) == 0


def as_list(value):
    if value is None:
        return []
    return value if isinstance(value, list) else [value]


def prefix_domains(domains, answer):
    if answer["prefix"] is False:
        return None
    actual = {s: frozenset(values) for s, values in domains.items()}
    units = answer["final"]["unit_domains"]
    if units:
        actual.update({s: frozenset(as_list(values)) for s, values in units.items()})
    return actual


def check_boundary(domains, answer):
    if answer["prefix"] is False:
        return
    effective = prefix_domains(domains, answer)
    state = answer["final"]
    entries = normalized_clauses(state["entries"])
    if isinstance(entries, bool):
        return
    live_units = {}
    expected_registry = {s: [] for s in domains}
    eliminated = as_list(state["eliminated"])
    is_unit = as_list(state["is_unit"])
    for index, (clause, dead, unit) in enumerate(zip(entries, eliminated, is_unit), 1):
        if dead:
            continue
        if len(clause) == 1:
            assert unit
            symbol = next(iter(clause))
            assert symbol not in live_units
            assert clause[symbol] == effective[symbol]
            live_units[symbol] = index
        else:
            assert len(clause) >= 2 and not unit
            for symbol, values in clause.items():
                assert values and values < effective[symbol], (clause, effective)
                expected_registry[symbol].append(index)
    assert (state["unit_registry"] or {}) == live_units
    assert set(state["unit_domains"] or {}) == set(live_units)
    registry = state["symbol_registry"] or {}
    for symbol in domains:
        assert as_list(registry.get(symbol)) == expected_registry[symbol]


def check_case(bridge, domains, clauses, counts, family, tables=None, enumerate_boxes=False,
               trace=False, full=False):
    clauses = normalized_clauses(clauses)
    rels = relations(domains, clauses)
    expected = relation_gac(domains, rels)
    assert expected == relation_gac(domains, list(reversed(rels)))
    if enumerate_boxes:
        enumerated, examined, fixed = enumerate_fixed_boxes(domains, rels)
        assert expected == enumerated
        counts["boxes_enumerated"] += examined
        counts["fixed_boxes_found"] += fixed
    answer = bridge.ask({"domains": domains, "clauses": clauses, "trace": trace, "full": full})
    actual = prefix_domains(domains, answer)
    assert actual == expected, (family, domains, clauses, expected, answer)
    check_boundary(domains, answer)
    original_models = truth_models(domains, clauses)
    assert truth_models(domains, normalized_clauses(answer["prefix"])) == original_models
    if full:
        assert truth_models(domains, normalized_clauses(answer["full"])) == original_models
    if tables is not None:
        assert (actual is None) == (not original_models), (family, domains, clauses, answer)
        if actual is not None:
            least = tuple(str(reduce(lambda x, y: tables[s][x][y], map(int, sorted(actual[s]))))
                          for s in domains)
            assert least in original_models
            for model in original_models:
                assert all(tables[s][int(x)][int(y)] == int(x)
                           for s, x, y in zip(domains, least, model))
            for symbol, values in actual.items():
                assert meet_closed(tables[symbol], frozenset(map(int, values)))
            counts["least_models_verified"] += 1
        else:
            counts["semilattice_unsat_verified"] += 1
    elif actual is not None and not original_models:
        counts["general_gac_nonempty_but_unsat"] += 1
    counts["source_cases"] += 1
    counts[family] += 1
    return answer


def all_clauses(domains):
    result = []
    symbols = tuple(domains)
    for width in range(1, len(symbols) + 1):
        for scope in combinations(symbols, width):
            for ranges in product(*(subsets(domains[s], proper=True) for s in scope)):
                result.append(dict(zip(scope, ranges)))
    return result


def directed_checks(bridge, counts):
    domains = {s: [str(x) for x in range(3)] for s in "XYZW"}
    clauses = [{"X": ["2"]}, {"Y": ["0"], "Z": ["1", "2"]},
               {"Z": ["0"], "Y": ["2"]}, {"Y": ["0", "1"], "W": ["1", "2"]},
               {"X": ["0"], "Y": ["1", "2"]}]
    chain = tuple(tuple(min(x, y) for y in range(3)) for x in range(3))
    tables = {s: chain for s in domains}
    examples = {}
    answer = check_case(bridge, domains, clauses, counts, "nested_directed", tables=tables,
                        trace=True, full=True)
    examples["nested_current_unit_is_narrowed"] = {"domains": domains, "clauses": clauses, "answer": answer}
    assert prefix_domains(domains, answer) == {
        "X": frozenset(["2"]), "Y": frozenset(["2"]),
        "Z": frozenset(["1", "2"]), "W": frozenset(["1", "2"]),
    }
    snapshots = [event for event in answer["events"] if event["label"] == "captured_symbols"]
    assert snapshots[-1]["state"]["clause_symbol_isct"] == "X"
    for order in permutations(range(1, 5)):
        for reverse_literals in (False, True):
            variant = [clauses[0]] + [clauses[i] for i in order]
            if reverse_literals:
                variant = [dict(reversed(list(c.items()))) for c in variant]
            check_case(bridge, domains, variant, counts, "nested_permutations", tables=tables)
    # Duplicate original units, including duplicates with different storage order.
    for original_units in ([{"X": ["1", "2"]}, {"X": ["0", "2"]}, {"X": ["2", "0"]}],
                           [{"X": ["1", "2"]}, {"X": ["0"]}]):
        for order in permutations(original_units):
            check_case(bridge, domains, list(order) + clauses[1:], counts,
                       "duplicate_original_units", tables=tables, full=True)
    # The four combinations of two incomparable ideals cannot all be ordered-Horn
    # in any independent total orders of X and Y, though each is meet closed.
    diamond = tuple(tuple(x & y for y in range(4)) for x in range(4))
    nonchain_domains = {s: list(map(str, range(4))) for s in "XY"}
    ideals = [frozenset(["0", "1"]), frozenset(["0", "2"])]
    nonchain = [{"X": ["3"]}] + [{"X": x, "Y": y} for x, y in product(ideals, repeat=2)]
    for x_order, y_order in product(permutations(nonchain_domains["X"]), repeat=2):
        prefixes = {"X": [frozenset(x_order[:i]) for i in range(1, 4)],
                    "Y": [frozenset(y_order[:i]) for i in range(1, 4)]}
        assert any(sum(r not in prefixes[s] for s, r in clause.items()) > 1
                   for clause in nonchain[1:])
    answer = check_case(bridge, nonchain_domains, nonchain, counts, "nonchain_directed",
                        tables={s: diamond for s in nonchain_domains}, full=True)
    examples["nonchain_least_model"] = {
        "domains": nonchain_domains, "clauses": nonchain, "answer": answer,
        "independent_total_order_pairs_excluded": 576,
    }
    bool_domains = {s: ["0", "1"] for s in "XY"}
    unsat = [{"X": [x], "Y": [y]} for x, y in product(["0", "1"], repeat=2)]
    answer = check_case(bridge, bool_domains, unsat, counts, "outside_class_unsat", full=True)
    assert answer["prefix"] is not False and answer["full"] is False
    examples["outside_class_unsat"] = {"domains": bool_domains, "clauses": unsat, "answer": answer}
    smaller_domains = {s: domains[s] for s in "XY"}
    projection_gap = [{"X": ["0"], "Y": ["0"]}, {"X": ["0"], "Y": ["1"]}]
    answer = check_case(bridge, smaller_domains, projection_gap, counts, "projection_gap",
                        tables={s: chain for s in smaller_domains}, full=True)
    models = truth_models(smaller_domains, normalized_clauses(projection_gap))
    assert {row[0] for row in models} == {"0"}
    assert prefix_domains(smaller_domains, answer)["X"] == frozenset(domains["X"])
    examples["domains_are_not_model_projections"] = {
        "domains": smaller_domains, "clauses": projection_gap, "answer": answer,
    }
    return examples


def source_checks(bridge, representatives, rng):
    counts = Counter()
    examples = directed_checks(bridge, counts)
    domains = {s: ["0", "1"] for s in "XY"}
    clauses = all_clauses(domains)
    assert len(clauses) == 8
    for mask in range(1 << len(clauses)):
        selected = [c for i, c in enumerate(clauses) if mask & (1 << i)]
        check_case(bridge, domains, selected, counts, "all_boolean_two_symbol_sets", enumerate_boxes=True)
    domains = {s: ["0", "1", "2"] for s in "XY"}
    clauses = all_clauses(domains)
    assert len(clauses) == 48
    for length in range(3):
        for selected in combinations_with_replacement(clauses, length):
            check_case(bridge, domains, list(selected), counts,
                       "all_ternary_two_clause_multisets", enumerate_boxes=True)
    print(json.dumps({"source_exhaustive_done": dict(counts)}), flush=True)
    for i in range(750):
        domains = {s: list(map(str, range(rng.randrange(2, 6)))) for s in "XYZW"[:rng.randrange(1, 5)]}
        ranges = {s: subsets(values, proper=True) for s, values in domains.items()}
        clauses = []
        for _ in range(rng.randrange(0, 15)):
            scope = rng.sample(list(domains), rng.randrange(1, len(domains) + 1))
            clauses.append({s: rng.choice(ranges[s]) for s in scope})
        if clauses and rng.random() < 0.5:
            clauses += [rng.choice(clauses)]
        check_case(bridge, domains, clauses, counts, "general_random", full=i < 150)
    # Include all nine unlabeled semilattices through four values; size one is
    # exercised as an unused coordinate, because it has no nonempty proper range.
    available = {name: table for name, table in representatives.items() if len(table) > 1}
    for name, table in available.items():
        proper = subsets(tuple(range(len(table))), proper=True)
        ideals = [r for r in proper if downset(table, r)]
        closed = [r for r in proper if meet_closed(table, r)]
        for i in range(120):
            symbols = "XYZ"[:rng.randrange(1, 4)]
            domains = {s: list(map(str, range(len(table)))) for s in symbols}
            domains["unused"] = ["0"]
            tables = {s: table for s in symbols}
            tables["unused"] = ((0,),)
            clauses = []
            for _ in range(rng.randrange(0, 13)):
                scope = rng.sample(list(symbols), rng.randrange(1, len(symbols) + 1))
                head = rng.choice(scope + [None])
                clause = {s: frozenset(map(str, rng.choice(closed if s == head else ideals))) for s in scope}
                assert syntactic_class(table, [frozenset(map(int, r)) for r in clause.values()])
                clauses.append(clause)
            if clauses and rng.random() < 0.5:
                clauses += [rng.choice(clauses)]
            check_case(bridge, domains, clauses, counts, f"semilattice_{name}", tables=tables,
                       full=i < 15)
        print(json.dumps({"source_semilattice_done": name, "counts": dict(counts)}), flush=True)
    # Different coordinate operations, rather than one common order or domain.
    pool = list(available.values())
    for i in range(180):
        tables = {s: rng.choice(pool) for s in "XYZ"}
        domains = {s: list(map(str, range(len(t)))) for s, t in tables.items()}
        choices = {}
        for s, table in tables.items():
            proper = subsets(tuple(range(len(table))), proper=True)
            choices[s] = ([r for r in proper if downset(table, r)],
                          [r for r in proper if meet_closed(table, r)])
        clauses = []
        for _ in range(rng.randrange(0, 15)):
            scope = rng.sample(list(domains), rng.randrange(1, 4))
            head = rng.choice(scope + [None])
            clauses.append({s: frozenset(map(str, rng.choice(choices[s][int(s == head)]))) for s in scope})
        check_case(bridge, domains, clauses, counts, "mixed_semilattices", tables=tables, full=i < 20)
    return dict(counts), examples


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--r46", action="store_true")
    parser.add_argument("--skip-algebra", action="store_true")
    args = parser.parse_args()
    started = time.time()
    rng = random.Random(2026090627)
    if args.skip_algebra:
        saved = json.loads((HERE / "results_r36.json").read_text())
        representatives = {name: tuple(map(tuple, table)) for name, table in saved["tables"].items()}
        algebra = saved["algebra"]
    else:
        representatives, counts, labeled, unlabeled = algebra_checks(rng)
        algebra = {"counts": counts, "labeled_counts": labeled, "unlabeled_counts": unlabeled}
    # Use the same source corpus even when algebra checks are reused.
    rng = random.Random(2026090628)
    bridge = Bridge(args.r46)
    try:
        counts, examples = source_checks(bridge, representatives, rng)
    finally:
        bridge.close()
    result = {"version": bridge.version, "algebra": algebra, "source": counts,
              "tables": representatives, "elapsed_seconds": round(time.time() - started, 3),
              "status": "all checks passed"}
    suffix = "r46" if args.r46 else "r36"
    (HERE / f"results_{suffix}.json").write_text(json.dumps(result, indent=2) + "\n")
    (HERE / f"directed_{suffix}.json").write_text(json.dumps(examples, indent=2, default=serializable) + "\n")
    print(json.dumps(result, indent=2), flush=True)


if __name__ == "__main__":
    main()
