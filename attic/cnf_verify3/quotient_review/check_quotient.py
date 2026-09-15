"""Independent coverage, assignment-grid, and refinement review.

Reads the root's implementation but writes only in this review directory.
It does not duplicate the 9.4 million production execution campaign.
"""
import collections
import itertools
import json
import math
from pathlib import Path
import random
import sys

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "root"))
import three_symbol_quotient as reviewed


def independent_profiles(k):
    """Enumerate subsets of explicit Boolean tuples, rather than mask ranges."""
    points = list(itertools.product((False, True), repeat=k))
    answer = {}
    for count in range(1, len(points) + 1):
        for chosen in itertools.combinations(points, count):
            if any(all(point[c] == chosen[0][c] for point in chosen) for c in range(k)):
                continue
            labels = sorted(sum(int(bit) * 2 ** coordinate for coordinate, bit in enumerate(point))
                            for point in chosen)
            mask = sum(2 ** value for value in labels)
            answer[mask] = labels
    return answer


def point_grid(domains):
    """Independent mixed-radix assignment generator, no Cartesian-product API."""
    symbols = list(domains)
    total = math.prod(len(domains[s]) for s in symbols)
    answer = []
    for code in range(total):
        left = code
        point = {}
        for symbol in symbols:
            point[symbol] = domains[symbol][left % len(domains[symbol])]
            left //= len(domains[symbol])
        assert left == 0
        answer.append(point)
    return answer


def allowed(values):
    return [values] if isinstance(values, str) else values


def scalar(formula, point):
    if isinstance(formula, bool):
        return formula
    return all(any(point[s] in allowed(values) for s, values in clause.items()) for clause in formula)


def normalize_coverage(domains, clauses):
    """Map arbitrary input order/names/value refinement into the claimed cells."""
    clauses = sorted(clauses, key=len)  # Stable, matching the production length sort.
    assert list(map(len, clauses)) == [2, 3, 3]
    short = list(clauses[0])
    extra = next(s for s in domains if s not in short)
    rename = dict(zip(short + [extra], ("X", "Y", "Z")))
    quotient_domains, mappings, masks = {}, {}, {}
    for old, symbol in rename.items():
        coordinates = range(3) if symbol != "Z" else (1, 2)
        mappings[old] = {v: str(sum((1 << bit) for bit, ci in enumerate(coordinates)
                                    if v in clauses[ci].get(old, []))) for v in domains[old]}
        quotient_domains[symbol] = sorted(set(mappings[old].values()), key=int)
        masks[symbol] = sum(1 << int(v) for v in quotient_domains[symbol])
    result = [{rename[s]: sorted({mappings[s][v] for v in values}, key=int)
               for s, values in clause.items()} for clause in clauses]
    assert tuple(result[0]) == ("X", "Y")
    assert masks["X"] in independent3 and masks["Y"] in independent3 and masks["Z"] in independent2
    assert tuple(result[1]) in reviewed.ORDERS and tuple(result[2]) in reviewed.ORDERS
    return quotient_domains, result, rename, mappings, masks


def projected_output(output, domains, rename, mappings):
    if isinstance(output, bool):
        return output
    result = []
    for clause in output:
        projected = []
        for old, vals in clause.items():
            vals = set(allowed(vals))
            classes = {mappings[old][v] for v in vals}
            # Whole membership classes must survive together, not just project
            # to an accidentally matching representative after partial removal.
            assert vals == {v for v in domains[old] if mappings[old][v] in classes}
            projected.append((rename[old], tuple(sorted(classes))))
        result.append(tuple(projected))
    return tuple(result)


def output_key(output):
    if isinstance(output, bool):
        return output
    return tuple(tuple((s, tuple(sorted(allowed(v)))) for s, v in clause.items()) for clause in output)


def cheap_local_opportunities(formula):
    """Sufficient set-based direct/SSE1/SSE2 checks, not a normal-form claim."""
    if isinstance(formula, bool):
        return []
    formula = [{s: set(allowed(v)) for s, v in cl.items()} for cl in formula]
    symbols = set().union(*(set(c) for c in formula))
    empty = set()
    opportunities = []
    for ti, target in enumerate(formula):
        others = [(i, c) for i, c in enumerate(formula) if i != ti]
        for di, donor in others:
            outside = {s for s in symbols if not donor.get(s, empty) <= target.get(s, empty)}
            if not outside:
                opportunities.append(("subsumption", di, ti))
            for pivot in target:
                if outside == {pivot} and target[pivot] - donor.get(pivot, empty):
                    opportunities.append(("sse1", di, ti, pivot))
        for (ai, a), (bi, b) in itertools.combinations_with_replacement(others, 2):
            for pivot in target:
                if not target[pivot] - a.get(pivot, empty) - b.get(pivot, empty):
                    continue
                for intersect in symbols - {pivot}:
                    if not a.get(intersect, empty) & b.get(intersect, empty) <= target.get(intersect, empty):
                        continue
                    if any(not donor.get(s, empty) <= target.get(s, empty)
                           for donor in (a, b) for s in symbols - {pivot, intersect}):
                        continue
                    opportunities.append(("sse2", ai, bi, ti, pivot, intersect))
    return opportunities


independent3 = independent_profiles(3)
independent2 = independent_profiles(2)
assert len(independent3) == 193 and len(independent2) == 7
for independent, production, k in ((independent3, reviewed.PROFILES3, 3), (independent2, reviewed.PROFILES2, 2)):
    assert independent == {mask: list(map(int, domain)) for mask, domain, _ in production}
    for mask, domain, ranges in production:
        for coordinate in range(k):
            assert ranges[coordinate] == [v for v in domain if int(v) & (1 << coordinate)]
        for perm in itertools.permutations(range(k)):
            transformed = {sum(((v >> old) & 1) << new for new, old in enumerate(perm))
                           for v in independent[mask]}
            assert sum(1 << v for v in transformed) in independent

stats = collections.Counter(profile3=193, profile2=7)
rng = random.Random(816225)
# Includes the largest representative grid, ordinary strings with substring
# relationships, singleton domains, and constants/empty clauses.
grid_cases = [{"X": [str(i) for i in range(8)], "Y": [str(i) for i in range(8)], "Z": [str(i) for i in range(4)]},
              {"other": ["1", "10", "a b"], "X": ["x"], "Y": ["0", "00"]}]
for _ in range(100):
    grid_cases.append({s: ["value_%s_%d" % (s, i) for i in range(rng.randrange(1, 9))]
                       for s in rng.sample(["X", "Y", "Z"], 3)})
for domains in grid_cases:
    oracle = reviewed.BitTruth(domains)
    points = point_grid(domains)
    expected_points = {tuple(p[s] for s in domains) for p in points}
    assert len(points) == len(expected_points) == len(oracle.assignments)
    assert expected_points == set(oracle.assignments)
    observed_bits = 0
    for point in points:
        singleton = [{s: [v]} for s, v in point.items()]
        bit = oracle.formula(singleton)
        assert bit and bit & (bit - 1) == 0
        assert oracle.witness(bit) == point
        assert observed_bits & bit == 0
        observed_bits |= bit
        stats["singleton_points"] += 1
    assert observed_bits == oracle.all_rows
    for formula in ([], [{}], True, False):
        mask = oracle.formula(formula)
        assert all(bool(mask & (1 << i)) == scalar(formula, dict(zip(domains, assignment)))
                   for i, assignment in enumerate(oracle.assignments))
    # Build unrelated formulas, then compare their model SETS to a scalar oracle
    # on the independent mixed-radix grid, not to the oracle's assignment order.
    for _ in range(10):
        formula = []
        for _ in range(rng.randrange(0, 5)):
            clause = {}
            for s in rng.sample(list(domains), rng.randrange(0, 4)):
                values = rng.sample(domains[s], rng.randrange(len(domains[s]) + 1))
                clause[s] = values[0] if len(values) == 1 and rng.randrange(2) else values
            formula.append(clause)
        mask = oracle.formula(formula)
        actual = {tuple(assignment) for i, assignment in enumerate(oracle.assignments) if mask & (1 << i)}
        expected = {tuple(p[s] for s in domains) for p in points if scalar(formula, p)}
        assert actual == expected
        stats["independent_formula_checks"] += 1
    stats["grid_cases"] += 1

# Exhaust every one of the 6 * 2 * 6 * 6 syntactic clause/symbol arrangements
# for a fixed asymmetric profile triple. This independently exercises the
# normalizer that maps arbitrary inputs to one of the enumerated schedules.
px, py, pz = reviewed.PROFILES3[31], reviewed.PROFILES3[147], reviewed.PROFILES2[3]
domains = {"a": px[1], "b": py[1], "c": pz[1]}
base = [dict(a=px[2][0], b=py[2][0]), dict(a=px[2][1], b=py[2][1], c=pz[2][0]),
        dict(a=px[2][2], b=py[2][2], c=pz[2][1])]
covered = set()
for short_order in itertools.permutations(("a", "b")):
    for long1 in itertools.permutations(("a", "b", "c")):
        for long2 in itertools.permutations(("a", "b", "c")):
            ordered = [{s: base[i][s] for s in order} for i, order in enumerate((short_order, long1, long2))]
            for positions in itertools.permutations(range(3)):
                qd, qc, rename, mapping, masks = normalize_coverage(domains, [ordered[i] for i in positions])
                covered.add((masks["X"], masks["Y"], masks["Z"], tuple(qc[1]), tuple(qc[2])))
                stats["syntactic_order_checks"] += 1
assert stats["syntactic_order_checks"] == 432

# Independent production metamorphic checks: arbitrary clause order, symbol
# name and insertion order, nonuniform class refinement, and range order.
bridge = reviewed.RBridge()
opportunity_examples = []
try:
    for trial in range(300):
        xp, yp, zp = rng.choice(reviewed.PROFILES3), rng.choice(reviewed.PROFILES3), rng.choice(reviewed.PROFILES2)
        original_domains = {"X": xp[1], "Y": yp[1], "Z": zp[1]}
        original = [dict(X=xp[2][0], Y=yp[2][0]), dict(X=xp[2][1], Y=yp[2][1], Z=zp[2][0]),
                    dict(X=xp[2][2], Y=yp[2][2], Z=zp[2][1])]
        names = rng.sample(["zeta", "alpha", "middle name", "100", "é", "X10"], 3)
        rename = dict(zip(original_domains, names))
        copies = {s: {v: ["%s_cell%s_copy%d" % (s, v, i) for i in range(rng.randrange(1, 6))]
                      for v in domain} for s, domain in original_domains.items()}
        domains = {rename[s]: sum(copies[s].values(), []) for s in rng.sample(list(original_domains), 3)}
        for domain in domains.values():
            rng.shuffle(domain)
        clauses = [{rename[s]: sum((copies[s][v] for v in cl[s]), [])
                    for s in rng.sample(list(cl), len(cl))} for cl in original]
        for clause in clauses:
            for values in clause.values():
                rng.shuffle(values)
        rng.shuffle(clauses)
        qd, qc, renaming, mapping, masks = normalize_coverage(domains, clauses)
        concrete = bridge.simplify(domains, clauses)
        quotient = bridge.simplify(qd, qc)
        assert concrete["ok"] and quotient["ok"], (domains, clauses, concrete, quotient)
        assert projected_output(concrete["result"], domains, renaming, mapping) == output_key(quotient["result"])
        # Independent constructive satisfiability witness for this exact shape.
        short, first, second = sorted(clauses, key=len)
        x, y = list(short)
        z = next(s for s in second if s not in short)
        point = {x: short[x][0], y: first[y][0], z: second[z][0]}
        assert scalar(clauses, point) and scalar(concrete["result"], point)
        opportunities = cheap_local_opportunities(quotient["result"])
        if opportunities:
            stats["sample_outputs_with_local_reduction"] += 1
            if len(opportunity_examples) < 3:
                opportunity_examples.append(dict(domains=qd, clauses=qc, output=quotient["result"], opportunities=opportunities))
        stats["production_refinement_pairs"] += 1
finally:
    bridge.close()

report = dict(seed=816225, counts=dict(stats), distinct_cells_for_asymmetric_order_case=len(covered),
              local_reduction_examples=opportunity_examples)
(HERE / "quotient_checks.json").write_text(json.dumps(report, indent=2, ensure_ascii=False) + "\n")
print(json.dumps(report, indent=2, ensure_ascii=False))
