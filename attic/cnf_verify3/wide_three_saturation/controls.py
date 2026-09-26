"""Directed wider-support controls; no random input bank.

Reuses the previously calibrated raw-set rule checker and unchanged-kernel
bridge. Inputs systematically append proper literals to selected clauses of
the saved scheduling-boundary examples and two explicit relay examples.
"""
import argparse
from collections import Counter
import hashlib
import itertools
import json
from pathlib import Path
import sys
import time

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
sys.path.insert(0, str(HERE.parent / "three_full_review"))
from checks import Bridge, formula, normal, opportunities, product_size, seq, truth  # noqa: E402


def variants():
    saved = json.loads((HERE.parent / "three_full_review/results_r36.json").read_text())
    bases = [dict(name=name, domains=saved["examples"][name]["domains"],
                  clauses=saved["examples"][name]["clauses"])
             for name in ("debt_enabled", "enabled_batch_pending_one")]
    bases.extend([
        dict(name="relay_nonempty", domains=dict(X=list("01234"), Y=list("0123"), Z=list("01")),
             clauses=[dict(X=list("01"), Y=list("01"), Z=["0"]),
                      dict(X=list("02"), Y=list("012"), Z=["0"]),
                      dict(X=list("13"), Y=["0"], Z=["0"])]),
        dict(name="relay_complete_pivot", domains=dict(X=list("01234"), Y=list("0123"), Z=list("01")),
             clauses=[dict(X=list("01"), Y=list("01"), Z=["0"]),
                      dict(X=list("02"), Y=list("012"), Z=["0"]),
                      dict(X=["3"], Y=["0"], Z=["0"])])])
    extensions = [(0, 0)] + [(mask, 0) for mask in range(1, 8)]
    extensions += [(mask, 7 ^ mask) for mask in range(1, 7)] + [(7, 7)]
    for base in bases:
        for mask_w, mask_v in extensions:
            domains = dict(base["domains"])
            clauses = [dict(c) for c in base["clauses"]]
            for symbol, mask in (("W", mask_w), ("V", mask_v)):
                if mask:
                    domains[symbol] = ["0", "1"]
                    for i in range(3):
                        if mask & (1 << i):
                            clauses[i][symbol] = ["0"]
            yield dict(name=base["name"], masks=[mask_w, mask_v], domains=domains, clauses=clauses)


def arranged(clauses, permutation, style):
    result = []
    for i in permutation:
        c = clauses[i]
        symbols = list(c)
        if style == 1:
            symbols.reverse()
        elif style in (2, 3):
            shift = style - 1
            symbols = symbols[shift:] + symbols[:shift]
        elif style == 4:
            if i % 2:
                symbols.reverse()
        elif style == 5:
            symbols = sorted(symbols)
        result.append({s: c[s] for s in symbols})
    return result


def observe(events, original, counts):
    original = sorted(formula(original), key=len)  # Kernel's initial stable width sort.
    common_triple = all(set(c) == set(original[0]) for c in original) and len(original[0]) == 3
    symbols = set().union(*(set(c) for c in original))
    for event in events:
        state = event["state"]
        entries = formula(state["entries"])
        active = [not a and not b for a, b in zip(seq(state["eliminated"]), seq(state["is_unit"]))]
        if not common_triple:
            assert not any(seq(state["is_unit"])), event
            counts["no_unit_state_checks"] += 1
        if len(active) == 3 and all(active):
            for target in range(3):
                a, b = [i for i in range(3) if i != target]
                for t in original[target]:
                    private = original[target][t] - (original[a].get(t, set()) | original[b].get(t, set()))
                    if not private & entries[target].get(t, set()):
                        continue
                    assert entries[target][t] == original[target][t]
                    counts["private_target_range_checks"] += 1
                    for s in symbols - {t}:
                        assert entries[a].get(s, set()) & entries[b].get(s, set()) == (
                            original[a].get(s, set()) & original[b].get(s, set()))
                        counts["constant_donor_intersection_checks"] += 1
        if not state["not_subset_count"]:
            continue
        available = [i - 1 for i in seq(state["available"])]
        for ai, a in enumerate(available):
            matrix = state["is_not_subset_of"][ai]
            if not active[a] or not matrix:
                continue
            for bi, b in enumerate(available):
                if a == b or not active[b] or state["not_subset_count"][ai][bi] is None:
                    continue
                bits = matrix[bi]
                assert sum(bits) == state["not_subset_count"][ai][bi]
                counts["matrix_count_checks"] += 1
                for s, bit in zip(seq(state["columns"][ai]), bits):
                    contained = entries[a].get(s, set()) <= entries[b].get(s, set())
                    if not bit:
                        assert contained
                    elif contained and state["second_order_enabled"]:
                        counts["enabled_conservative_true_observations"] += 1
        if event["label"] == "symbol_batch" and state["second_order_enabled"]:
            ai = event["local"]["meta_idx"] - 1
            for bi in [i - 1 for i in seq(event["local"]["rows_changed"])]:
                if active[available[bi]] and state["not_subset_count"][ai][bi] == 1:
                    counts["enabled_pending_count_one_observations"] += 1


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--current-r", action="store_true")
    args = parser.parse_args()
    started = time.monotonic()
    source_hash = hashlib.sha256((ROOT / "R/CnfFormula_simplify.R").read_bytes()).hexdigest()
    assert source_hash == "7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc"
    bridge = Bridge(args.current_r)
    counts = Counter()
    groups = {}
    report = dict(source_sha256=source_hash,
                  version=bridge.version, cases=[], negative_controls=[])
    for case in variants():
        domains, clauses = case["domains"], case["clauses"]
        case_counts = Counter()
        expected = truth(domains, formula(clauses))
        for permutation in itertools.permutations(range(3)):
            for style in range(6):
                initial = arranged(clauses, permutation, style)
                trace = permutation == (0, 1, 2) and style == 0
                result = bridge.request(dict(domains=domains, clauses=initial, trace=trace))
                first, second = formula(result["first"]), formula(result["second"])
                assert truth(domains, first) == expected == truth(domains, second)
                assert normal(first) == normal(second)
                assert not opportunities(domains, first), (initial, result)
                assert first is not False
                common_triple = all(set(c) == set(clauses[0]) for c in clauses) and len(clauses[0]) == 3
                if not common_triple and not isinstance(first, bool):
                    assert not any(len(c) == 1 for c in first)
                case_counts["arrangements"] += 1
                case_counts["ordinary_production_calls"] += 2
                case_counts["truth_valuation_rows"] += 2 * product_size(domains)
                if trace:
                    observe(result["events"], initial, case_counts)
                    case_counts["observed_calls_identical_to_plain"] += 1
        counts.update(case_counts)
        supports = [set(c) for c in clauses]
        group = ("common_triple" if supports[0] == supports[1] == supports[2] and len(supports[0]) == 3
                 else "common_wider" if supports[0] == supports[1] == supports[2]
                 else "different_supports")
        groups.setdefault(group, Counter()).update(case_counts)
        report["cases"].append(dict(**case, counts=dict(case_counts)))
    negative_controls = [dict(name="three_binary_unit_equality",
        domains=dict(X=list("1245"), Y=list("235")),
        clauses=[dict(X=list("24"), Y=["3"]), dict(Y=["5"], X=list("21")),
                 dict(Y=["2"], X=list("25"))])]
    saved = json.loads((HERE.parent / "independent_solver/first_order_sse1_production_replay.json").read_text())
    negative_controls.append(dict(name="four_clause_sse1_gap", domains=saved["domains"], clauses=saved["clauses"]))
    for case in negative_controls:
        result = bridge.request(dict(domains=case["domains"], clauses=case["clauses"], trace=False))
        residual = opportunities(case["domains"], formula(result["first"]))
        assert residual and normal(result["first"]) != normal(result["second"])
        assert truth(case["domains"], formula(case["clauses"])) == truth(case["domains"], formula(result["first"]))
        report["negative_controls"].append(dict(**case, first=result["first"], second=result["second"], residual=residual))
        counts["negative_controls_rejected"] += 1
        counts["ordinary_production_calls"] += 2
    bridge.close()
    assert counts["enabled_conservative_true_observations"]
    assert counts["enabled_pending_count_one_observations"]
    report["counts"] = dict(counts)
    report["group_counts"] = {name: dict(values) for name, values in groups.items()}
    report["seconds"] = time.monotonic() - started
    suffix = "r46" if args.current_r else "r36"
    (HERE / f"controls_{suffix}.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(dict(version=bridge.version, counts=counts, seconds=report["seconds"])))


if __name__ == "__main__":
    main()
