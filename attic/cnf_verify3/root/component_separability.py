"""Independent-component execution checks against the unchanged R kernel."""
from collections import Counter
import argparse
import itertools
import json
from pathlib import Path
import random
import subprocess
import sys

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "independent_solver"))
from oracle import MDDOracle, OneHotOracle, RBridge  # noqa: E402


def exact_ranges(formula):
    if isinstance(formula, bool):
        return formula
    return tuple(tuple((symbol, tuple([values] if isinstance(values, str) else values))
                       for symbol, values in clause.items()) for clause in formula)


def weight(formula):
    if isinstance(formula, bool):
        return 0
    return sum(len([values] if isinstance(values, str) else values)
               for clause in formula for values in clause.values())


def project(formula, symbols):
    if isinstance(formula, bool):
        return formula
    result = []
    for clause in formula:
        intersect = set(clause).intersection(symbols)
        assert not intersect or intersect == set(clause), (clause, symbols)
        if intersect:
            result.append(clause)
    return result or True


def plant_component(rng, number, force_model):
    symbols = [f"C{number}_S{i}" for i in range(rng.randrange(1, 5))]
    domains = {s: list(map(str, range(rng.randrange(2, 6)))) for s in symbols}
    planted = {s: rng.choice(d) for s, d in domains.items()}
    clauses = []
    for unused in range(rng.randrange(1, 9)):
        names = rng.sample(symbols, rng.randrange(1, len(symbols) + 1))
        clause = {s: rng.sample(domains[s], rng.randrange(1, len(domains[s]))) for s in names}
        if force_model and not any(planted[s] in values for s, values in clause.items()):
            s = rng.choice(names)
            clause[s] = [planted[s]]
        clauses.append(clause)
    return domains, clauses


def check_case(bridge, components, order, stats, examples):
    domains = {s: d for component, unused in components for s, d in component.items()}
    tagged = [(c, clause) for c, (unused, clauses) in enumerate(components) for clause in clauses]
    tagged = [tagged[i] for i in order]
    original = [clause for unused, clause in tagged]
    isolated = [[clause for c, clause in tagged if c == k] or True
                for k in range(len(components))]
    global_state = original
    isolated_productive = [0] * len(components)
    global_productive = 0
    for pass_index in itertools.count():
        answer = bridge.simplify(domains, global_state)
        assert answer["ok"], (components, original, answer)
        stats["global_calls"] += 1
        new_global = answer["result"]
        new_isolated = []
        for k, (domain, unused) in enumerate(components):
            local = bridge.simplify(domain, isolated[k])
            assert local["ok"], (domain, isolated[k], local)
            stats["isolated_calls"] += 1
            result = local["result"]
            isolated_productive[k] += weight(result) < weight(isolated[k])
            new_isolated.append(result)
        any_false = any(result is False for result in new_isolated)
        assert (new_global is False) == any_false, (original, new_global, new_isolated)
        if any_false:
            stats["contradictory_cases"] += 1
            break
        for k, (domain, unused) in enumerate(components):
            assert exact_ranges(project(new_global, domain)) == exact_ranges(new_isolated[k]), (
                original, pass_index, k, new_global, new_isolated)
            stats["ordered_component_checks"] += 1
        if stats["cases"] % 137 == 0:
            assert OneHotOracle(domains).difference(original, new_global) is None
            assert MDDOracle(domains).equivalent(original, new_global)
            stats["independent_solver_checks"] += 1
        productive = weight(new_global) < weight(global_state)
        global_productive += productive
        if not productive:
            assert global_productive == max(isolated_productive, default=0)
            final = bridge.simplify(domains, new_global)
            assert final["ok"] and exact_ranges(final["result"]) == exact_ranges(new_global)
            stats["global_calls"] += 1
            stats["nonconstant_fixed_points"] += 1
            examples[str(global_productive)] = examples.get(str(global_productive), 0) + 1
            break
        assert pass_index <= weight(original)
        global_state, isolated = new_global, new_isolated
    stats["cases"] += 1


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--r46", action="store_true")
    args = parser.parse_args()
    rng = random.Random(772031)
    if args.r46:
        # Retain the independently used bridge protocol, changing only R runtime.
        bridge = RBridge.__new__(RBridge)
        bridge.process = subprocess.Popen([
            "podman", "exec", "-i", "cnf-review-r46", "Rscript",
            "/work/attic/cnf_verify3/independent_solver/r_bridge.R"
        ], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, bufsize=1)
    else:
        bridge = RBridge()
    stats, examples = Counter(), {}
    # Root scheduling controls combined with independently generated components.
    fixtures = []
    for filename in (HERE.parent / "independent_solver").glob("*min*.json"):
        data = json.loads(filename.read_text())
        if "domains" in data and isinstance(data.get("clauses"), list):
            fixtures.append(data)
    for case_index in range(2400):
        components = [plant_component(rng, k, case_index % 3 != 0)
                      for k in range(rng.randrange(2, 5))]
        if fixtures and case_index < 8 * len(fixtures):
            saved = fixtures[case_index % len(fixtures)]
            rename = {s: f"C0_{s}" for s in saved["domains"]}
            components[0] = ({rename[s]: d for s, d in saved["domains"].items()},
                             [{rename[s]: values for s, values in clause.items()}
                              for clause in saved["clauses"]])
        total = sum(len(clauses) for unused, clauses in components)
        order = list(range(total))
        rng.shuffle(order)
        check_case(bridge, components, order, stats, examples)
        if (case_index + 1) % 200 == 0:
            print(json.dumps(dict(stats=stats, productive=examples)), flush=True)
    bridge.close()
    result = dict(stats=stats, productive_pass_counts=examples,
                  source_scope="ordinary disjoint symbol groups; exact projected clause/range order",
                  fixture_count=len(fixtures), seed=772031,
                  runtime="R4.6.1 container" if args.r46 else "native R3.6.3")
    filename = "component_separability_r46.json" if args.r46 else "component_separability_results.json"
    (HERE / filename).write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
