"""Replay discovered enabled-phase boundaries on both R versions."""
from collections import Counter
import json
from pathlib import Path

from checks import Bridge, formula, normal, opportunities, seq, snapshot_observations, truth

HERE = Path(__file__).resolve().parent


def callback_debt(events):
    found = []
    for event in events:
        if event["label"] not in ("ordinary_enter", "twoend_enter", "sse2_try", "range_request"):
            continue
        state = event["state"]
        if not state["second_order_enabled"]:
            continue
        entries = formula(state["entries"])
        active = [not a and not b for a, b in zip(seq(state["eliminated"]), seq(state["is_unit"]))]
        available = [i - 1 for i in seq(state["available"])]
        for ai, a in enumerate(available):
            if not active[a]:
                continue
            for bi, b in enumerate(available):
                if a == b or not active[b]:
                    continue
                for s, bit in zip(seq(state["columns"][ai]), state["is_not_subset_of"][ai][bi]):
                    if bit and entries[a].get(s, set()) <= entries[b].get(s, set()):
                        found.append(dict(source=a, target=b, symbol=s, event=event))
    return found


def main():
    input_report = json.loads((HERE / "results_r36.json").read_text())
    controls = [input_report["examples"][key] for key in ("debt_enabled", "enabled_batch_pending_one")]
    report = dict(source_sha256=input_report["source_sha256"], versions=[])
    previous_outputs = None
    for current in (False, True):
        bridge = Bridge(current)
        outputs = []
        counts = Counter()
        examples = []
        for case in controls:
            domains, clauses = case["domains"], case["clauses"]
            result = bridge.request(dict(domains=domains, clauses=clauses, trace=True))
            original, first, second = map(formula, (clauses, result["first"], result["second"]))
            assert truth(domains, original) == truth(domains, first) == truth(domains, second)
            assert not opportunities(domains, first)
            assert normal(result["first"]) == normal(result["second"])
            observed, _ = snapshot_observations(result["events"], original)
            counts.update(observed)
            debt = callback_debt(result["events"])
            counts["enabled_debt_at_callback_entry"] += len(debt)
            outputs.append(result["first"])
            examples.append(dict(domains=domains, clauses=clauses, first=result["first"],
                                 callback_debt=debt, events=result["events"]))
        negative = json.loads((HERE.parent / "independent_solver/first_order_sse1_production_replay.json").read_text())
        result = bridge.request(dict(domains=negative["domains"], clauses=negative["clauses"], trace=False))
        original, first, second = map(formula, (negative["clauses"], result["first"], result["second"]))
        assert truth(negative["domains"], original) == truth(negative["domains"], first) == truth(negative["domains"], second)
        residual = opportunities(negative["domains"], first)
        assert any(rule["kind"] == "sse1" for rule in residual)
        assert normal(result["first"]) != normal(result["second"])
        counts["known_sse1_gap_controls"] += 1
        outputs.append(result["first"])
        bridge.close()
        if previous_outputs is not None:
            assert outputs == previous_outputs
        previous_outputs = outputs
        assert counts["debt_enabled"] and counts["enabled_batch_pending_one"]
        report["versions"].append(dict(version=bridge.version, ordinary_production_calls=6,
            instrumentation_identity_calibrations=2, counts=dict(counts), examples=examples,
            sse1_negative_control=dict(domains=negative["domains"], clauses=negative["clauses"],
                first=result["first"], second=result["second"], residual=residual)))
    (HERE / "boundary_replays.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps([dict(version=x["version"], counts=x["counts"]) for x in report["versions"]]))


if __name__ == "__main__":
    main()
