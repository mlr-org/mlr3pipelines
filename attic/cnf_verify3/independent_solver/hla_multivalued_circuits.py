"""HLA-only stress with layered multi-premise, multi-valued implications.

Every gate needs two different donors to remove two distinct extra values.
Later gates require all values outside each predecessor's core to be excluded.
This is not a Boolean chain with indistinguishable duplicate domain values.
"""
import collections
import json
import math
import random
import time

from horn_refutation import horn_refute, verify_horn_certificate
from oracle import HERE, MDDOracle, OneHotOracle, RBridge, normalize


def circuit(rng, depth, width, unit):
    domain = list(map(str, range(8)))
    domains = {"A": domain}
    target = {"A": ["0", "1"]}
    if not unit:
        domains["B"] = domain
        target["B"] = ["0", "1"]
    cores, previous, clauses = {}, [], [target]
    for level in range(depth):
        current = []
        for j in range(width):
            symbol = "Z%02d_%d" % (level, j)
            domains[symbol] = domain
            shuffled = rng.sample(domain, len(domain))
            n_core = rng.randint(1, 4)
            core, extras = shuffled[:n_core], shuffled[n_core:n_core + 2]
            cores[symbol] = core
            current.append(symbol)
            if not level:
                first, second = {"A": ["0"]}, {"A" if unit else "B": ["1"]}
            else:
                premises = rng.sample(previous, rng.randint(1, len(previous)))
                first = {p: [v for v in domain if v not in cores[p]] for p in premises}
                second = dict(first)
            first[symbol] = core + [extras[0]]
            second[symbol] = core + [extras[1]]
            clauses.extend([first, second])
        if level and rng.random() < 0.8:
            # A feedback implication adds cycles without changing the known
            # propagation from the seeded predecessor layer.
            left, right = rng.choice(current), rng.choice(previous)
            clauses.append({left: [v for v in domain if v not in cores[left]],
                            right: cores[right]})
        previous = current
    terminal = rng.choice(previous)
    clauses.append({terminal: [v for v in domain if v not in cores[terminal]], "A": ["0"]})
    # The explicit target follows by intersecting the two gate outputs, layer
    # by layer, then contradicting the terminal donor under target negation.
    donors = clauses[1:]
    rng.shuffle(clauses)
    clauses = [dict(reversed(list(c.items()))) if rng.random() < 0.5 else c for c in clauses]
    return domains, clauses, target, donors


def main():
    rng, bridge = random.Random(1430906), RBridge()
    start, stats = time.time(), collections.Counter()
    for trial in range(200):
        unit = trial % 2 == 0
        domains, formula, target, donors = circuit(rng, rng.randint(3, 12), rng.randint(1, 4), unit)
        certificate = horn_refute(domains, donors, target)
        assert certificate["refuted"]
        assert verify_horn_certificate(domains, donors, target, certificate["trace"])
        stats["promised_horn_steps"] += len(certificate["trace"])
        answer = bridge.simplify(domains, formula, variant="hla_only")
        assert answer["ok"], (trial, answer)
        result = normalize(answer["result"])
        sat = OneHotOracle(domains)
        assert sat.difference(formula, result) is None, (trial, answer)
        assert sat.difference(formula, donors) is None, (trial, "generator promise")
        try:
            assert MDDOracle(domains, node_limit=300000).equivalent(formula, result)
            stats["mdd_equivalences"] += 1
        except MemoryError:
            stats["mdd_budget"] += 1
        assert not isinstance(result, bool), (trial, "known satisfiable circuit became constant", answer)
        if unit:
            assert all(len(c) != 1 for c in result), (trial, "redundant target unit survived", answer)
            stats["unit_targets_removed"] += 1
        for event in answer["events"]:
            stats[event["kind"]] += 1
            assert event["kind"] in {"subsumption", "hla", "unit_hla"}
        for target_idx, survivor in enumerate(result):
            horn = horn_refute(domains, result[:target_idx] + result[target_idx + 1:], survivor)
            if horn["refuted"]:
                record = dict(trial=trial, domains=domains, clauses=formula, answer=answer,
                              target=target_idx, horn=horn)
                (HERE / "hla_multivalued_circuit_failure.json").write_text(json.dumps(record, indent=2) + "\n")
                raise AssertionError(record)
            stats["surviving_targets"] += 1
        stats["formulas"] += 1
        stats["max_symbols"] = max(stats["max_symbols"], len(domains))
        stats["max_clauses"] = max(stats["max_clauses"], len(formula))
        stats["max_log10_assignments"] = max(stats["max_log10_assignments"], len(domains) * math.log10(8))
        if (trial + 1) % 10 == 0:
            print(json.dumps(dict(stats=stats, elapsed=time.time() - start)), flush=True)
    bridge.close()
    report = dict(stats=stats, elapsed=time.time() - start)
    (HERE / "hla_multivalued_circuit_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report), flush=True)


if __name__ == "__main__":
    main()
