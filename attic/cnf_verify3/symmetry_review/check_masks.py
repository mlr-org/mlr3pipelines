"""Independent calibration of the compact masks; does not launch the R worker.

The reviewed definitions are extracted from their AST so the audit needs only
the Python standard library and cannot touch the live exhaustive run.
"""
import ast
from collections import Counter
from functools import lru_cache
import hashlib
import itertools
import json
from pathlib import Path
import random


HERE = Path(__file__).resolve().parent
ROOT = HERE.parent / "root"


def definitions(path, wanted, namespace):
    tree = ast.parse(path.read_text())
    selected = [node for node in tree.body
                if isinstance(node, (ast.FunctionDef, ast.ClassDef)) and node.name in wanted]
    assert len(selected) == len(wanted)
    exec(compile(ast.Module(body=selected, type_ignores=[]), str(path), "exec"), namespace)


namespace = dict(lru_cache=lru_cache, itertools=itertools)
definitions(ROOT / "full_three_symbol_quotient.py", {"positional_masks", "FastTruth"}, namespace)
definitions(ROOT / "three_symbol_quotient.py", {"profiles"}, namespace)
FastTruth = namespace["FastTruth"]


def valuations(domains):
    # Build assignments one symbol at a time, without either mask expression
    # or the imported BitTruth/product implementation.
    result = [{}]
    for symbol, values in domains.items():
        result = [dict(assignment, **{symbol: value}) for assignment in result for value in values]
    return result


def evaluate(formula, assignment):
    if isinstance(formula, bool):
        return formula
    for clause in formula:
        satisfied = False
        for symbol, raw_range in clause.items():
            allowed = [raw_range] if isinstance(raw_range, str) else raw_range
            if assignment[symbol] in allowed:
                satisfied = True
                break
        if not satisfied:
            return False
    return True


def truth_word(formula, assignments):
    return int("".join("1" if evaluate(formula, a) else "0" for a in reversed(assignments)), 2)


def independently_generate_profiles():
    patterns = tuple(itertools.product((False, True), repeat=3))
    result = set()
    by_size = Counter()
    for size in range(1, 9):
        for cells in itertools.combinations(patterns, size):
            if any(all(row[column] for row in cells) or not any(row[column] for row in cells)
                   for column in range(3)):
                continue
            values = tuple(sorted(sum((1 << c) for c in range(3) if row[c]) for row in cells))
            result.add(values)
            by_size[size] += 1
    return result, by_size


def main():
    rng = random.Random(906202691)
    profile_cells, by_size = independently_generate_profiles()
    reviewed = namespace["profiles"](3)
    assert len(profile_cells) == len(reviewed) == 193
    assert {tuple(map(int, domain)) for _, domain, _ in reviewed} == profile_cells
    assert len({mask for mask, _, _ in reviewed}) == 193
    for mask, domain, ranges in reviewed:
        assert mask == sum(1 << int(value) for value in domain)
        assert ranges == [[value for value in domain if int(value) & (1 << c)] for c in range(3)]

    shapes = membership_checks = formula_checks = witness_checks = row_checks = 0
    for sizes in itertools.product(range(1, 9), repeat=3):
        symbols = ["third", "first", "second"]
        domains = {symbol: rng.sample(["q_" + str(i) for i in range(size)], size)
                   for symbol, size in zip(symbols, sizes)}
        assignments = valuations(domains)
        oracle = FastTruth(domains)
        assert oracle.rows == len(assignments)
        assert oracle.all_rows == int("1" * len(assignments), 2)
        for symbol, domain in domains.items():
            for value in domain:
                expected_bits = [a[symbol] == value for a in assignments]
                actual_bits = [(oracle.membership[symbol][value] >> row) & 1
                               for row in range(len(assignments))]
                assert actual_bits == expected_bits
                membership_checks += 1
        formulas = [True, False, [], [{}]]
        for _ in range(16):
            formula = []
            for _ in range(rng.randrange(1, 6)):
                clause = {}
                for symbol in rng.sample(symbols, rng.randrange(4)):
                    allowed = rng.sample(domains[symbol], rng.randrange(len(domains[symbol]) + 1))
                    clause[symbol] = allowed[0] if len(allowed) == 1 and rng.randrange(2) else allowed
                formula.append(clause)
            formulas.append(formula)
        for formula in formulas:
            expected = truth_word(formula, assignments)
            assert expected == oracle.formula(formula)
            formula_checks += 1
            row_checks += len(assignments)
        for row in {0, len(assignments) // 2, len(assignments) - 1}:
            # A mixed word must select the lowest set bit, not its highest bit.
            difference = (1 << row) | (1 << (len(assignments) - 1))
            assert oracle.witness(difference) == assignments[row]
            witness_checks += 1
        shapes += 1

    result = dict(
        independent_profiles=len(profile_cells),
        profile_domain_size_counts=dict(sorted(by_size.items())),
        full_inputs=len(profile_cells) ** 3,
        initial_valuation_checks=sum(len(v) for v in profile_cells) ** 3,
        shape_calibrations=shapes,
        literal_membership_words=membership_checks,
        formula_truth_words=formula_checks,
        individual_formula_valuations=row_checks,
        witness_calibrations=witness_checks,
        errors=[],
        source_hashes={name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest()
                       for name in ("full_three_symbol_quotient.py", "three_symbol_quotient.py")})
    (HERE / "mask_results.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
