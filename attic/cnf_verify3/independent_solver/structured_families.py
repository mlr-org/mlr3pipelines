"""New algebraic stress families, independent from prior R generators."""
import argparse
import collections
import json
import math
import pathlib
import random
import time

from oracle import HERE, MDDOracle, OneHotOracle, RBridge, audit_events, normalize


def hla_chain(rng, length, unit=False, branches=1):
    domain = [str(i) for i in range(rng.randint(4, 8))]
    domains = {"A": domain, "B": domain}
    target = {"A": ["0", "1"]}
    if not unit:
        target["B"] = ["0", "1"]
    clauses = [target]
    for branch in range(branches):
        names = ["Z%d_%d" % (branch, i) for i in range(length)]
        domains.update({s: domain for s in names})
        cut = rng.randint(2, len(domain) - 1)
        positive, negative = domain[:cut], domain[cut:]
        clauses.append({"A": [str(branch % 2)], names[0]: positive})
        for left, right in zip(names, names[1:]):
            clauses.append({left: [rng.choice(negative)], right: positive})
        clauses.append({names[-1]: [rng.choice(negative)], "A" if unit else "B": [str((branch + 1) % 2)]})
    # The explicit target is implied by the remaining clauses by resolving
    # backwards along any one chain, regardless of domain cardinality.
    return domains, clauses, clauses[1:]


def second_order_bundle(rng, width):
    domains = {s: [str(i) for i in range(6)] for s in ("S", "T", "C")}
    clauses = []
    for i in range(width):
        s, t, c = ("S%d" % i, "T%d" % i, "C%d" % (i % 3))
        domains.update({k: domains["S"] for k in (s, t, c)})
        vals_s, vals_t, vals_c = (rng.sample(domains["S"], 6) for _ in range(3))
        target = {s: vals_s[:1], t: vals_t[:3], c: vals_c[:3]}
        a = {s: vals_s[:2], t: [vals_t[0], vals_t[3]], c: vals_c[:1]}
        b = {s: [vals_s[0], vals_s[2]], t: [vals_t[1], vals_t[4]], c: vals_c[1:2]}
        clauses.extend([target, a, b])
        if i and rng.random() < 0.7:
            # Cross-link bundles through genuinely new ranges, creating nested
            # updates while preserving a moderate MDD frontier.
            previous = "T%d" % (i - 1)
            clauses.append({previous: rng.sample(domains[previous], 2), t: vals_t[2:5]})
    domains = {s: d for s, d in domains.items() if any(s in c for c in clauses)}
    rng.shuffle(clauses)
    return domains, clauses, None


def permutation_cycle(rng, length, size):
    domain = [str(i) for i in range(size)]
    domains = {"Q%d" % i: domain for i in range(length)}
    names = list(domains)
    transitions = [rng.sample(domain, size) for _ in range(length)]
    clauses = []
    for i, source in enumerate(names):
        target = names[(i + 1) % length]
        for a, b in zip(domain, transitions[i]):
            clauses.append({source: [v for v in domain if v != a], target: [b]})
    # Explicitly calculated fixed points of the composed transition around
    # the cycle give exact unary consequences, inserted as redundant clauses.
    fixed = []
    for start in domain:
        value = start
        for transition in transitions:
            value = transition[int(value)]
        if value == start:
            fixed.append(start)
    if fixed and len(fixed) < size:
        clauses.append({names[0]: fixed})
    rng.shuffle(clauses)
    return domains, clauses, False if not fixed else None


def lift_values(rng, domains, clauses):
    """Surjective value refinement with highly unequal membership-cell sizes."""
    mapping = {s: {v: ["%s.%d" % (v, j) for j in range(rng.randint(1, 7))]
                   for v in vals} for s, vals in domains.items()}
    lifted_domains = {s: [x for group in mapping[s].values() for x in group] for s in domains}
    lifted = [{s: [x for v in vals for x in mapping[s][v]] for s, vals in clause.items()}
              for clause in clauses]
    return lifted_domains, lifted, mapping


def canonical(formula):
    formula = normalize(formula)
    if isinstance(formula, bool):
        return formula
    return sorted(tuple(sorted((s, tuple(sorted(v))) for s, v in clause.items())) for clause in formula)


def lift_result(formula, mapping):
    formula = normalize(formula)
    if isinstance(formula, bool):
        return formula
    return [{s: [x for v in vals for x in mapping[s][v]] for s, vals in c.items()} for c in formula]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--trials", type=int, default=400)
    parser.add_argument("--seed", type=int, default=20260906)
    parser.add_argument("--out", default="structured_results.json")
    args = parser.parse_args()
    rng, bridge = random.Random(args.seed), RBridge()
    stats = collections.Counter()
    start = time.time()
    failures = []
    for i in range(args.trials):
        family = i % 4
        if family < 2:
            name = "unit_hla_chain" if family else "nonunit_hla_chain"
            domains, clauses, known = hla_chain(rng, rng.randint(2, 30), family == 1, rng.randint(1, 3))
        elif family == 2:
            name = "second_order_bundle"
            domains, clauses, known = second_order_bundle(rng, rng.randint(1, 15))
        else:
            name = "permutation_cycle"
            domains, clauses, known = permutation_cycle(rng, rng.randint(3, 18), rng.randint(2, 7))
        stats["family_" + name] += 1
        try:
            answer = bridge.simplify(domains, clauses, audit=True)
            assert answer["ok"], answer
            result = answer["result"]
            sat = OneHotOracle(domains)
            witness = sat.difference(clauses, result)
            assert witness is None, dict(kind="semantic", witness=witness)
            stats["sat_equivalences"] += 1
            if known is not None:
                assert sat.difference(clauses, known) is None, "generator's algebraic promise failed"
                stats["known_consequences"] += 1
            try:
                assert MDDOracle(domains, node_limit=300000).equivalent(clauses, result)
                stats["mdd_equivalences"] += 1
            except MemoryError:
                stats["mdd_budget"] += 1
            for kind, count in audit_events(answer["events"], domains).items():
                stats["event_" + kind] += count
            if i % 4 == 2 or i % 13 == 0:
                lifted_domains, lifted, mapping = lift_values(rng, domains, clauses)
                lifted_answer = bridge.simplify(lifted_domains, lifted, audit=True)
                assert lifted_answer["ok"], lifted_answer
                assert canonical(lifted_answer["result"]) == canonical(lift_result(result, mapping)), "domain refinement changes algorithmic result"
                audit_events(lifted_answer["events"], lifted_domains)
                stats["value_refinement_naturality"] += 1
            stats["max_variables"] = max(stats["max_variables"], len(domains))
            stats["max_clauses"] = max(stats["max_clauses"], len(clauses))
            stats["max_log10_assignments"] = max(stats["max_log10_assignments"], sum(math.log10(len(d)) for d in domains.values()))
        except (AssertionError, TimeoutError) as error:
            failure = dict(trial=i, family=name, domains=domains, clauses=clauses, error=str(error))
            if "answer" in locals():
                failure["answer"] = answer
            failures.append(failure)
            stats["failures"] += 1
            print(json.dumps(dict(failure=failures[-1])), flush=True)
            break
        if (i + 1) % 20 == 0:
            print(json.dumps(dict(trials=i + 1, elapsed=time.time() - start, stats=stats)), flush=True)
    bridge.close()
    report = dict(seed=args.seed, requested_trials=args.trials, elapsed=time.time() - start,
                  stats=stats, failures=failures)
    (HERE / args.out).write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
