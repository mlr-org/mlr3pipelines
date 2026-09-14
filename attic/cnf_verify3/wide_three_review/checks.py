"""Small deterministic controls for the independent wide-three review.

The existing direct-kernel bridge is reused; the output checker below is
local and reads only raw sets. These controls do not prove callback coverage.
"""
from collections import Counter
import hashlib
import itertools
import json
from pathlib import Path
import sys
import time

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
sys.path.insert(0, str(HERE.parent / "three_full_review"))
from checks import Bridge  # noqa: E402


def decode(value):
    if isinstance(value, bool):
        return value
    return [{s: set(v if isinstance(v, list) else [v]) for s, v in c.items()} for c in value]


def normalized(value):
    clauses = decode(value)
    if isinstance(clauses, bool):
        return clauses
    return sorted(tuple(sorted((s, tuple(sorted(v))) for s, v in c.items())) for c in clauses)


def check_outputs(domains, original, first, second):
    clauses = [decode(c) for c in (original, first, second)]
    symbols = tuple(domains)
    rows = 0
    for values in itertools.product(*(domains[s] for s in symbols)):
        assignment = dict(zip(symbols, values))
        answers = [c if isinstance(c, bool) else
                   all(any(assignment[s] in v for s, v in clause.items()) for clause in c)
                   for c in clauses]
        assert answers[0] == answers[1] == answers[2], (assignment, clauses)
        rows += 1
    assert normalized(first) == normalized(second)
    assert not residuals(domains, clauses[1])
    return rows


def residuals(domains, clauses):
    if isinstance(clauses, bool):
        return []
    out = []
    for target_id, target in enumerate(clauses):
        donors = [(i, c) for i, c in enumerate(clauses) if i != target_id]
        for donor_id, donor in donors:
            exceptions = [s for s, values in donor.items() if not values <= target.get(s, set())]
            if not exceptions:
                out.append(("direct", donor_id, target_id))
            for t, values in target.items():
                if set(exceptions) <= {t} and values - donor.get(t, set()):
                    out.append(("sse1", donor_id, target_id, t))
        for (a_id, a), (b_id, b) in itertools.combinations(donors, 2):
            for s, t in itertools.permutations(domains, 2):
                if not target.get(t, set()) - (a.get(t, set()) | b.get(t, set())):
                    continue
                if (a.get(s, set()) & b.get(s, set())) - target.get(s, set()):
                    continue
                if all(values <= target.get(q, set()) for c in (a, b)
                       for q, values in c.items() if q not in (s, t)):
                    out.append(("sse2", a_id, b_id, target_id, s, t))
        allowed = {s: set(values) - target.get(s, set()) for s, values in domains.items()}
        while True:
            changed = False
            contradiction = any(not values for values in allowed.values())
            for _, donor in donors:
                live = [(s, values & allowed[s]) for s, values in donor.items() if values & allowed[s]]
                if not live:
                    contradiction = True
                    break
                if len(live) == 1:
                    s, values = live[0]
                    if values != allowed[s]:
                        allowed[s] = values
                        changed = True
            if contradiction:
                out.append(("hla", target_id))
                break
            if not changed:
                break
    return out


def cases():
    for width in (4, 7):
        for strict in (0, 1, 2):
            for complete in (False, True):
                domains = {"t": list("01234")}
                clauses = [{"t": list("01")}, {"t": list("02")},
                           {"t": list("3" if complete else "13")}]
                for i in range(width - 1):
                    s = "q%d" % i
                    if i < max(strict, 1):
                        domains[s] = list("0123")
                        ranges = [list("01" if i < strict else "0"), list("012"), list("0")]
                    else:
                        domains[s] = list("01")
                        ranges = [["0"]] * 3
                    for clause, values in zip(clauses, ranges):
                        clause[s] = values
                yield dict(name="relay_w%d_k%d_%s" % (width, strict, "empty" if complete else "partial"),
                           domains=domains, clauses=clauses)
    for strict in (1, 2):
        for complete in (False, True):
            domains = {"t": list("01234"), "q0": list("0123"), "q1": list("0123")}
            clauses = [{"t": list("01"), "q0": list("01"), "q1": list("01" if strict == 2 else "0")},
                       {"t": list("02"), "q0": list("012"), "q1": list("012")},
                       {"t": list("3" if complete else "13"), "q0": ["0"], "q1": ["0"]}]
            for i, mask in enumerate((3, 3, 2, 2, 2)):
                symbol = "extra%d" % i
                domains[symbol] = list("01")
                for clause_id in range(3):
                    if mask & (1 << clause_id):
                        clauses[clause_id][symbol] = ["0"]
            yield dict(name="unequal_sorted_relay_k%d_%s" % (strict, complete), domains=domains, clauses=clauses)
    saved = json.loads((HERE.parent / "three_full_review/results_r36.json").read_text())
    for base_name in ("debt_enabled", "enabled_batch_pending_one"):
        base = saved["examples"][base_name]
        for kind, masks in (("common9", (7,) * 6), ("equal_width_unequal_supports", (7, 7, 7, 3, 5, 6)),
                            ("unequal_widths", (7, 7, 7, 1, 3, 7))):
            domains = dict(base["domains"])
            clauses = [dict(c) for c in base["clauses"]]
            for i, mask in enumerate(masks):
                symbol = "outside%d" % i
                domains[symbol] = list("01")
                for clause_id in range(3):
                    if mask & (1 << clause_id):
                        clauses[clause_id][symbol] = ["0"]
            yield dict(name=base_name + "_" + kind, domains=domains, clauses=clauses)


def arrange(clauses, order, style):
    answer = []
    for i in order:
        names = list(clauses[i])
        if style:
            names = names[i + 1:] + names[:i + 1]
            if i == 1:
                names.reverse()
        answer.append({s: list(reversed(clauses[i][s])) if style else clauses[i][s] for s in names})
    return answer


def observe(events, counts):
    for event in events:
        state = event["state"]
        assert not any(state["is_unit"])
        counts["nonunit_checkpoints"] += 1
        if not state["second_order_enabled"] or not state["not_subset_count"]:
            continue
        active = [not dead for dead in state["eliminated"]]
        clauses = decode(state["entries"])
        available = [i - 1 for i in state["available"]]
        for ai, a in enumerate(available):
            if not active[a]:
                continue
            matrix = state["is_not_subset_of"][ai]
            for bi, b in enumerate(available):
                if a == b or not active[b] or state["not_subset_count"][ai][bi] is None:
                    continue
                assert sum(matrix[bi]) == state["not_subset_count"][ai][bi]
                for symbol, bit in zip(state["columns"][ai], matrix[bi]):
                    included = clauses[a].get(symbol, set()) <= clauses[b].get(symbol, set())
                    if not bit:
                        assert included
                    if bit and included:
                        counts["enabled_conservative_true_observations"] += 1
        if event["label"] == "symbol_batch":
            local = event["local"]
            ai = local["meta_idx"] - 1
            changed = local["rows_changed"]
            if isinstance(changed, int):
                changed = [changed]
            for bi in changed or []:
                if active[available[bi - 1]] and state["not_subset_count"][ai][bi - 1] == 1:
                    counts["enabled_batch_pending_one_observations"] += 1


def main():
    current = "--current-r" in sys.argv
    started = time.monotonic()
    source_sha256 = hashlib.sha256((ROOT / "R/CnfFormula_simplify.R").read_bytes()).hexdigest()
    assert source_sha256 == "7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc"
    bridge = Bridge(current)
    counts = Counter()
    reports = []
    for case in cases():
        counts["positive_bases"] += 1
        trace_counts = Counter()
        for order in itertools.permutations(range(3)):
            for style in (0, 1):
                clauses = arrange(case["clauses"], order, style)
                trace = order == (0, 1, 2) and style == 0
                result = bridge.request(dict(domains=case["domains"], clauses=clauses, trace=trace))
                counts["valuation_rows"] += check_outputs(case["domains"], clauses, result["first"], result["second"])
                counts["positive_arrangements"] += 1
                if trace:
                    observe(result["events"], trace_counts)
                    counts["observed_calls_identical_to_plain"] += 1
        counts.update(trace_counts)
        reports.append(dict(name=case["name"], initial_widths=[len(c) for c in case["clauses"]],
                            clauses=case["clauses"], domains=case["domains"], observations=dict(trace_counts)))
    negative = json.loads((HERE.parent / "independent_solver/first_order_sse1_production_replay.json").read_text())
    result = bridge.request(dict(domains=negative["domains"], clauses=negative["clauses"], trace=False))
    remaining = residuals(negative["domains"], decode(result["first"]))
    assert any(item[0] == "sse1" for item in remaining)
    assert normalized(result["first"]) != normalized(result["second"])
    counts["outside_scope_known_gap_rejected"] += 1
    bridge.close()
    assert counts["enabled_conservative_true_observations"]
    assert counts["enabled_batch_pending_one_observations"]
    report = dict(version=bridge.version, source_sha256=source_sha256, counts=dict(counts), bases=reports,
                  negative_residuals=remaining, elapsed_seconds=round(time.monotonic() - started, 3))
    path = HERE / ("checks_r46.json" if current else "checks_r36.json")
    path.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({k: v for k, v in report.items() if k not in ("bases", "negative_residuals")}))


if __name__ == "__main__":
    main()
