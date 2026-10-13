"""Run independent support checks and instrumented source checks against R.

This is a targeted premise audit, not a rerun of the repository's test suite.
The only imported test code is our separate mathematical set implementation.
"""

import hashlib
import itertools
import json
import random
import subprocess
from collections import Counter
from pathlib import Path

from mathematical_checks import HERE, entails, refutable


ROOT = HERE.parents[2]
SOURCE = ROOT / "R" / "CnfFormula_simplify.R"


def r_vector(values):
    return "c(" + ",".join(str(v) + "L" for v in values) + ")"


def request_line(sizes, formula):
    return "list(domains=" + r_vector(sizes) + ",clauses=list(" + ",".join(
        r_vector(c) for c in formula) + "))\n"


def parse_answer(line):
    parts = line.rstrip("\n").split("\t")
    assert parts[0] == "OK", line
    if parts[1] in ("TRUE", "FALSE"):
        formula = parts[1] == "TRUE"
    else:
        formula = [tuple(map(int, c.split(","))) for c in parts[1].split(";")]
    stats = dict((name, int(count)) for name, count in
                 (value.split("=") for value in parts[2].split(",")))
    return formula, stats


def candidates():
    # The known unit-equality gap, expressed with fresh small labels.
    yield "unit_equality", (4, 3), [(3, 1), (5, 2), (9, 4)]
    # Both initially redundant targets need not be removed.
    yield "deletion_order", (2, 2, 2), [(2, 2, 0), (2, 0, 2), (0, 1, 2), (0, 2, 1)]
    # Non-Boolean restricted domains with all donor x ranges proper in the unit.
    yield "multivalued_chain", (4, 3, 3), [(3, 0, 0), (1, 3, 0), (0, 6, 1), (0, 5, 2), (2, 0, 4)]
    # Systematically permute all 6 orders and symbol orders of the equality gap.
    for i, order in enumerate(itertools.permutations([(3, 1), (5, 2), (9, 4)])):
        yield "equality_order_" + str(i), (4, 3), list(order)
        yield "equality_transpose_" + str(i), (3, 4), [c[::-1] for c in order]
    # Fixed seed, moderate dimensions, deliberately mixed range cardinalities.
    rng = random.Random(692061)
    for trial in range(2000):
        sizes = tuple(rng.randint(2, 5) for _ in range(rng.randint(2, 6)))
        formula = []
        for _ in range(rng.randint(2, 16)):
            present = rng.sample(range(len(sizes)), rng.randint(1, len(sizes)))
            formula.append(tuple(rng.randint(1, (1 << size) - 2) if i in present else 0
                                 for i, size in enumerate(sizes)))
        yield "random_" + str(trial), sizes, formula


def main():
    before = hashlib.sha256(SOURCE.read_bytes()).hexdigest()
    stats = Counter()
    outputs = []
    residual = []
    process = subprocess.Popen(["Rscript", str(HERE / "production_bridge.R")], cwd=str(ROOT),
                               stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    try:
        for number, (label, sizes, formula) in enumerate(candidates(), 1):
            process.stdin.write(request_line(sizes, formula))
            process.stdin.flush()
            line = process.stdout.readline()
            if not line:
                raise AssertionError("R terminated while processing " + label)
            result, counters = parse_answer(line)
            stats.update(counters)
            stats["formulas"] += 1
            universe = tuple((1 << size) - 1 for size in sizes)
            if not isinstance(result, bool):
                for target, clause in enumerate(result):
                    donors = result[:target] + result[target + 1:]
                    stats["output_targets"] += 1
                    if refutable(universe, donors, clause):
                        units = [donor for donor in donors if sum(bool(v) for v in donor) == 1]
                        subsuming = [unit for unit in units if all(
                            literal & ~target_literal == 0
                            for literal, target_literal in zip(unit, clause))]
                        assert subsuming, (label, sizes, formula, result, target)
                        assert sum(bool(v) for v in clause) > 1
                        assert entails(universe, donors, clause)
                        residual.append(dict(label=label, target=target, result=result,
                                             subsuming_units=subsuming))
            if not label.startswith("random_"):
                outputs.append(dict(label=label, sizes=sizes, formula=formula, result=result))
            if number % 250 == 0:
                print("Audited", number, "formulas;", len(residual), "unit-subsumption residuals", flush=True)
    finally:
        process.stdin.close()
        process.wait(timeout=20)
    assert process.returncode == 0
    after = hashlib.sha256(SOURCE.read_bytes()).hexdigest()
    assert before == after
    result = dict(source_sha256=before, counts=dict(stats), residual_refutations=residual,
                  directed_outputs=outputs)
    (HERE / "production_results.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(dict(counts=dict(stats), residuals=len(residual)), indent=2))


if __name__ == "__main__":
    main()
