"""Read-only consistency audit of the active or finished exhaustive report."""
import argparse
import hashlib
import json
from pathlib import Path

from check_masks import independently_generate_profiles


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--require-complete", action="store_true")
    args = parser.parse_args()
    source = HERE.parent / "root/full_three_symbol_results.json"
    report = json.loads(source.read_text())
    profiles, _ = independently_generate_profiles()
    ordered = sorted(profiles, key=lambda values: sum(1 << value for value in values))
    n_profiles = len(ordered)
    assert n_profiles == 193
    parameters = report["parameters"]
    assert (parameters["x_start"], parameters["x_stop"], parameters["y_limit"], parameters["z_limit"]) == (0, 193, 193, 193)
    completed = report["completed"]
    assert len(completed) == len(set(completed))
    assert set(completed) <= set(range(n_profiles))
    assert report["expected_inputs"] == n_profiles ** 3
    assert report["mask_shape_calibrations"] == 7 ** 3
    assert report["errors"] == []
    counts = {int(k): v for k, v in report["productive_pass_counts"].items()}
    assert all(k >= 0 and isinstance(v, int) and v > 0 for k, v in counts.items())
    stats = report["stats"]
    assert stats["inputs"] == len(completed) * n_profiles ** 2 == sum(counts.values())
    assert stats["executions"] == sum((k + 1) * v for k, v in counts.items())
    assert stats["independent_calibrations"] == len(completed) * 10
    initial_valuations = sum(len(ordered[i]) for i in completed) * sum(map(len, ordered)) ** 2
    assert initial_valuations <= stats["valuations"] <= (1 + max(counts, default=0)) * initial_valuations
    for line in report["source_manifest"].splitlines():
        digest, filename = line.split()
        assert hashlib.sha256((REPO / filename).read_bytes()).hexdigest() == digest
    complete = bool(report.get("finished_utc"))
    if complete:
        assert set(completed) == set(range(n_profiles))
        assert stats["inputs"] == n_profiles ** 3
    if args.require_complete:
        assert complete, "The live report is unfinished; no complete enumeration claim is certified."
    result = dict(complete=complete, completed_profiles=len(completed), total_profiles=n_profiles,
                  covered_inputs=stats["inputs"], expected_inputs=n_profiles ** 3,
                  productive_pass_counts=counts, executions=stats["executions"],
                  initial_valuation_checks=initial_valuations,
                  all_pass_valuation_checks=stats["valuations"],
                  finished_utc=report.get("finished_utc"), consistency_checks_passed=True)
    (HERE / "full_report_audit.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
