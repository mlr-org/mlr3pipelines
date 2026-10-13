"""Finite meet-closed clause characterization and source UP completeness checks.

The independent propagation procedure rescans original clause relations over
domains. It has no source registries, simplified clause storage or callbacks.
"""
import argparse
from collections import Counter
import functools
import itertools
import json
from pathlib import Path
import random
import subprocess
import sys

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "independent_solver"))
from oracle import MDDOracle, OneHotOracle, evaluate, normalize  # noqa: E402


def subsets(domain):
    return [frozenset(c) for n in range(1, len(domain))
            for c in itertools.combinations(domain, n)]


def is_closed(values, meet):
    return all(meet(a, b) in values for a in values for b in values)


def is_ideal(values, domain, meet):
    return all(meet(a, b) in values for a in values for b in domain)


def class_member(domains, clauses, meets):
    if isinstance(clauses, bool):
        return True
    return all(all(is_closed(v, meets[s]) for s, v in clause.items()) and
               sum(not is_ideal(v, domains[s], meets[s]) for s, v in clause.items()) <= 1
               for clause in clauses)


def domain_closure(domains, clauses):
    """Greatest generalized arc-consistent subdomains, or None on emptiness."""
    current = {s: set(d) for s, d in domains.items()}
    if isinstance(clauses, bool):
        return current if clauses else None
    while True:
        changed = False
        for clause in clauses:
            possible = [(s, current[s].intersection(v)) for s, v in clause.items()
                        if current[s].intersection(v)]
            if not possible:
                return None
            if len(possible) == 1:
                s, restricted = possible[0]
                if restricted != current[s]:
                    current[s] = restricted
                    changed = True
        if not changed:
            return current


def prefix_domains(domains, prefix):
    if prefix is False:
        return None
    current = {s: set(d) for s, d in domains.items()}
    if prefix is not True:
        for clause in normalize(prefix):
            if len(clause) == 1:
                s, values = next(iter(clause.items()))
                current[s].intersection_update(values)
    return current


class Bridge:
    def __init__(self, r46):
        command = (["podman", "exec", "-i", "cnf-review-r46", "Rscript",
                    "/work/attic/cnf_verify3/root/semilattice_bridge.R"] if r46 else
                   ["Rscript", str(HERE / "semilattice_bridge.R")])
        self.process = subprocess.Popen(command, stdin=subprocess.PIPE,
                                        stdout=subprocess.PIPE, text=True, bufsize=1)

    def run(self, domains, clauses):
        self.process.stdin.write(json.dumps(dict(domains=domains, clauses=clauses)) + "\n")
        self.process.stdin.flush()
        text = self.process.stdout.readline()
        if not text:
            raise RuntimeError("R bridge exited")
        answer = json.loads(text)
        assert answer["ok"], (domains, clauses, answer)
        return answer["prefix"], answer["full"]

    def close(self):
        self.process.stdin.close()
        self.process.wait(timeout=10)


def characterize(stats):
    """Every proper two-literal relation on chains 2/3 and the 4-element diamond."""
    domains = [tuple(map(str, range(n))) for n in (2, 3, 4)]
    meets = [lambda a, b: str(min(int(a), int(b))),
             lambda a, b: str(min(int(a), int(b))),
             lambda a, b: str(int(a) & int(b))]
    for d1, m1 in zip(domains, meets):
        for d2, m2 in zip(domains, meets):
            for r1 in subsets(d1):
                for r2 in subsets(d2):
                    models = [(a, b) for a in d1 for b in d2 if a in r1 or b in r2]
                    relational = all(m1(a[0], b[0]) in r1 or m2(a[1], b[1]) in r2
                                     for a in models for b in models)
                    syntactic = (is_closed(r1, m1) and is_closed(r2, m2) and
                                 (is_ideal(r1, d1, m1) or is_ideal(r2, d2, m2)))
                    assert relational == syntactic, (d1, d2, r1, r2)
                    stats["characterized_relations"] += 1
                    stats["characterized_model_pairs"] += len(models) ** 2


def make_case(rng, index):
    symbols = [f"S{i}" for i in range(rng.randrange(1, 5))]
    domains, meets, ideals, heads = {}, {}, {}, {}
    family = "semilattice" if index % 3 == 0 else "chain"
    for s in symbols:
        size = rng.choice((4, 8)) if family == "semilattice" else rng.randrange(2, 7)
        domain = list(map(str, range(size)))
        rng.shuffle(domain)
        domains[s] = domain
        if family == "semilattice":
            meets[s] = lambda a, b: str(int(a) & int(b))
        else:
            rank = {v: i for i, v in enumerate(domain)}
            meets[s] = lambda a, b, rank=rank: a if rank[a] <= rank[b] else b
        ranges = subsets(domain)
        heads[s] = [r for r in ranges if is_closed(r, meets[s])]
        ideals[s] = [r for r in heads[s] if is_ideal(r, domain, meets[s])]
    clauses = []
    for unused in range(rng.randrange(1, 14)):
        names = rng.sample(symbols, rng.randrange(1, len(symbols) + 1))
        head = rng.choice(names + [None])
        clause = {s: sorted(rng.choice(heads[s] if s == head else ideals[s])) for s in names}
        for values in clause.values():
            rng.shuffle(values)
        clauses.append(clause)
    return domains, clauses, meets, family


def check_case(bridge, domains, clauses, meets, family, stats, examples):
    expected = domain_closure(domains, clauses)
    prefix, full = bridge.run(domains, clauses)
    actual = prefix_domains(domains, prefix)
    assert actual == expected, (domains, clauses, expected, prefix)
    stats["gac_domain_comparisons"] += 1
    oracle = MDDOracle(domains)
    assert oracle.equivalent(clauses, prefix), (domains, clauses, prefix)
    assert oracle.equivalent(clauses, full), (domains, clauses, full)
    stats["mdd_equivalence_checks"] += 2
    member = class_member(domains, clauses, meets)
    if family != "outside":
        assert member
    if stats["cases"] % 149 == 0:
        assert OneHotOracle(domains).difference(clauses, prefix) is None
        assert OneHotOracle(domains).difference(clauses, full) is None
        stats["sat_equivalence_checks"] += 2
    sat = oracle.encode(clauses) != 0
    if member:
        assert (prefix is False) == (not sat)
        assert (full is False) == (not sat)
        stats["complete_class_cases"] += 1
        stats["complete_class_unsat" if not sat else "complete_class_sat"] += 1
        if sat:
            least = {s: functools.reduce(meets[s], sorted(expected[s])) for s in domains}
            assert evaluate(clauses, least), (domains, clauses, expected, least)
            for s, values in expected.items():
                assert is_closed(values, meets[s]) and all(meets[s](least[s], v) == least[s] for v in values)
            stats["least_models"] += 1
            if stats["cases"] % 7 == 0:
                models = [dict(zip(domains, values)) for values in itertools.product(*domains.values())
                          if evaluate(clauses, dict(zip(domains, values)))]
                for model in models:
                    assert all(meets[s](least[s], model[s]) == least[s] for s in domains)
                stats["enumerated_least_model_checks"] += len(models)
            if family not in examples and any(set(expected[s]) != set(domains[s]) for s in domains):
                examples[family] = dict(domains=domains, clauses=clauses, prefix=prefix, least_model=least)
    elif prefix is not False and not sat:
        stats["outside_class_unrecognized_prefix_unsat"] += 1
    stats["cases"] += 1
    stats[family + "_cases"] += 1


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--r46", action="store_true")
    parser.add_argument("--random-cases", type=int, default=3000)
    args = parser.parse_args()
    rng = random.Random(2609061629)
    stats, examples = Counter(), {}
    characterize(stats)
    bridge = Bridge(args.r46)
    boolean_domains = {s: ["0", "1"] for s in ("X", "Y", "Z")}
    meets = {s: lambda a, b: min(a, b) for s in boolean_domains}
    all_clauses = [{s: [bit] for s, bit in zip(boolean_domains, choice) if bit is not None}
                   for choice in itertools.product((None, "0", "1"), repeat=3) if any(x is not None for x in choice)]
    horn_clauses = [c for c in all_clauses if class_member(boolean_domains, [c], meets)]
    for count in range(1, 4):
        for clauses in itertools.combinations_with_replacement(horn_clauses, count):
            check_case(bridge, boolean_domains, list(clauses), meets, "exhaustive_boolean", stats, examples)
    for clauses in (True, False, []):
        check_case(bridge, boolean_domains, clauses, meets, "constants", stats, examples)
    # Outside-class controls exercise general GAC correspondence, without a
    # claim that propagation alone decides arbitrary satisfiability.
    for count in range(1, 5):
        for clauses in itertools.combinations(all_clauses[:8], count):
            check_case(bridge, boolean_domains, list(clauses), meets, "outside", stats, examples)
    four = [{"X": [a], "Y": [b]} for a, b in itertools.product(("0", "1"), repeat=2)]
    check_case(bridge, boolean_domains, four, meets, "outside", stats, examples)
    unsupported = [{"X": ["0"], "Y": ["1"]}, {"X": ["0"], "Y": ["0"]}]
    check_case(bridge, boolean_domains, unsupported, meets, "horn_projection_control", stats, examples)
    assert domain_closure(boolean_domains, unsupported)["X"] == {"0", "1"}
    assert not any(evaluate(unsupported, dict(X="1", Y=y, Z="0")) for y in ("0", "1"))
    # Complete unordered multisets of up to three clauses on two ternary
    # chains. Unlike the random portfolio this exhausts all permitted ranges.
    chain_domains = {s: ["0", "1", "2"] for s in ("X", "Y")}
    chain_meets = {s: lambda a, b: min(a, b) for s in chain_domains}
    ranges = [()] + [tuple(sorted(r)) for r in subsets(chain_domains["X"])]
    chain_clauses = [{s: list(r) for s, r in zip(chain_domains, choice) if r}
                     for choice in itertools.product(ranges, repeat=2) if any(choice)]
    chain_clauses = [c for c in chain_clauses if class_member(chain_domains, [c], chain_meets)]
    for count in range(1, 4):
        for clauses in itertools.combinations_with_replacement(chain_clauses, count):
            check_case(bridge, chain_domains, list(clauses), chain_meets,
                       "exhaustive_ternary_chain", stats, examples)
    for unused in range(750):
        names = [f"A{i}" for i in range(rng.randrange(1, 5))]
        domains = {s: list(map(str, range(rng.randrange(2, 6)))) for s in names}
        meet_functions = {s: lambda a, b: min(a, b) for s in names}
        clauses = [{s: rng.sample(domains[s], rng.randrange(1, len(domains[s])))
                    for s in rng.sample(names, rng.randrange(1, len(names) + 1))}
                   for ignored in range(rng.randrange(1, 12))]
        check_case(bridge, domains, clauses, meet_functions, "outside", stats, examples)
    for index in range(args.random_cases):
        domains, clauses, meets, family = make_case(rng, index)
        check_case(bridge, domains, clauses, meets, family, stats, examples)
        if (index + 1) % 250 == 0:
            print(json.dumps(dict(random_done=index + 1, stats=stats)), flush=True)
    bridge.close()
    result = dict(stats=stats, examples=examples, seed=2609061629,
                  runtime="R4.6.1" if args.r46 else "R3.6.3",
                  source="Unchanged full kernel; private source copy returns at the initial UP boundary")
    filename = "semilattice_r46.json" if args.r46 else "semilattice_r36.json"
    (HERE / filename).write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
