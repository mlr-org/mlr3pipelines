"""Saved canonical examples and directed/random semantic checks of queue scope.

Two established independent oracles (Boolean SAT and a multivalued decision
diagram) verify complete valuation equivalence; production does not act as oracle.
"""
import argparse
import collections
import hashlib
import itertools
import json
import pathlib
import random
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[2]
SOLVER = HERE.parent / "independent_solver"
sys.path.insert(0, str(SOLVER))
from oracle import MDDOracle, OneHotOracle, normalize, values
from domain_refutation import redundancy_opportunities
from fixed_point import opportunities


def canonical(formula):
    if isinstance(formula, bool):
        return formula
    return tuple(sorted(tuple(sorted((s, tuple(sorted(values(v)))) for s, v in c.items())) for c in formula))


def cases():
    patterns = ("minimized_*.json", "outside_skip*.json", "directed_oneend*.json")
    paths = sorted(set(itertools.chain.from_iterable(SOLVER.glob(pattern) for pattern in patterns)))
    for path in paths:
        case = json.loads(path.read_text())
        if not isinstance(case, dict) or "domains" not in case or "clauses" not in case:
            continue
        domains, formula = case["domains"], normalize(case["clauses"])
        if not isinstance(formula, list) or not all(c and all(
                set(v) and set(v) < set(domains[s]) for s, v in c.items()) for c in formula):
            continue
        yield path.name, domains, formula
    # Repeated same-symbol unit merges and fan-out during queued propagation.
    for n in (3, 8, 24):
        domains = {"X%d" % i: ["0", "1"] for i in range(n)}
        formula = [{"X0": ["1"]}]
        for i in range(n - 1, 0, -1):
            formula += [{"X%d" % (i - 1): ["0"], "X%d" % i: ["1"]}]
            if i > 1:
                formula += [{"X0": ["0"], "X%d" % i: ["1"]}]
        yield "fanout_%d" % n, domains, formula
    rng = random.Random(292601)
    for trial in range(1000):
        domains = {"S%d" % i: [str(v) for v in range(rng.randint(3, 6))]
                   for i in range(rng.randint(2, 6))}
        formula = []
        for _ in range(rng.randint(3, 16)):
            count = rng.randint(1 if trial % 5 == 0 else 2, len(domains))
            selected = rng.sample(list(domains), count)
            formula.append({s: rng.sample(domains[s], rng.randint(1, len(domains[s]) - 1))
                            for s in selected})
        yield "random_%d" % trial, domains, formula


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--r46", action="store_true")
    parser.add_argument("--saved-only", action="store_true")
    args = parser.parse_args()
    source = ROOT / "R" / "CnfFormula_simplify.R"
    before = hashlib.sha256(source.read_bytes()).hexdigest()
    command = (["podman", "exec", "-i", "cnf-review-r46", "Rscript",
                "attic/cnf_verify3/unit_queue/semantic_bridge.R"] if args.r46
               else ["Rscript", str(HERE / "semantic_bridge.R")])
    process = subprocess.Popen(command, cwd=str(ROOT),
                               stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    counters = {kind: collections.Counter() for kind in ("production", "no_skip", "queued")}
    report = []
    differences = []
    total = 0
    try:
        for label, domains, formula in cases():
            if args.saved_only and label.startswith("random_"):
                continue
            request = dict(domains=domains, clauses=formula)
            process.stdin.write(json.dumps(request, separators=(",", ":")) + "\n")
            process.stdin.flush()
            answer = json.loads(process.stdout.readline())
            if not answer["ok"]:
                (HERE / "semantic_failure.json").write_text(json.dumps(dict(label=label, **request,
                                                                          answer=answer), indent=2) + "\n")
                raise AssertionError((label, answer))
            mdd = MDDOracle(domains)
            sat = OneHotOracle(domains)
            summary = dict(label=label, input_clauses=len(formula), variants={})
            for kind, output in answer["outputs"].items():
                result = output["result"]
                assert mdd.equivalent(formula, result), (label, kind, "MDD")
                assert sat.difference(formula, result) is None, (label, kind, "SAT")
                counters[kind].update(output["statistics"])
                pending = opportunities(result)
                refutations = redundancy_opportunities(domains, result)
                if kind != "production":
                    assert not refutations, (label, kind, result, refutations)
                counters[kind]["residual_refutations"] += len(refutations)
                counters[kind]["residual_sse1"] += sum(p["kind"] == "sse1" for p in pending)
                counters[kind]["residual_sse2"] += sum(p["kind"] == "sse2" for p in pending)
                summary["variants"][kind] = dict(result=result, pending=pending, refutations=refutations)
            same_no_skip_queue = canonical(answer["outputs"]["no_skip"]["result"]) == canonical(
                answer["outputs"]["queued"]["result"])
            same_production_queue = canonical(answer["outputs"]["production"]["result"]) == canonical(
                answer["outputs"]["queued"]["result"])
            if not same_no_skip_queue or not same_production_queue:
                differences.append(dict(label=label, domains=domains, clauses=formula,
                                        **{key: value for key, value in summary.items() if key != "label"},
                                        queue_differs_from_no_skip=not same_no_skip_queue,
                                        queue_differs_from_production=not same_production_queue))
            if not label.startswith("random_"):
                report.append(summary)
            total += 1
            if total % 100 == 0:
                print("Checked", total, "formulas;", len(differences), "structural differences", flush=True)
    finally:
        process.stdin.close()
        process.wait(timeout=20)
    assert process.returncode == 0
    assert before == hashlib.sha256(source.read_bytes()).hexdigest()
    result = dict(formulas=total, r_version="4.6" if args.r46 else "3.6",
                  source_sha256=before, counters=counters,
                  saved_examples=report, differences=differences)
    destination = "semantic_results_r46.json" if args.r46 else "semantic_results.json"
    (HERE / destination).write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(dict(formulas=total, differences=len(differences), counters=counters), indent=2))


if __name__ == "__main__":
    main()
