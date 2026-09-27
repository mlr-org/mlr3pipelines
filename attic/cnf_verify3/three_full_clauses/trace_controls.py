"""Save exact production traces for the directed structural controls."""

import json
from pathlib import Path
import sys

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "independent_solver"))
from oracle import RBridge  # noqa: E402
from ordered_controls import models, normalize  # noqa: E402


def main():
    controls = json.loads((HERE / "ordered_controls_results.json").read_text())["directed"]
    controls.append(dict(
        name="known_two_symbol_equality_gap",
        domains=dict(X=list("1245"), Y=list("235")),
        clauses=[dict(X=list("24"), Y=["3"]),
                 dict(Y=["5"], X=list("21")),
                 dict(Y=["2"], X=list("25"))]))
    bridge = RBridge()
    report = []
    for case in controls:
        domains, clauses = case["domains"], case["clauses"]
        first = bridge.simplify(domains, clauses, variant="unit_birth", detailed=True)
        assert first["ok"], first
        second = bridge.simplify(domains, first["result"], audit=True, detailed=True)
        assert second["ok"], second
        plain = bridge.simplify(domains, clauses)
        assert normalize(plain["result"]) == normalize(first["result"])
        expected = models(domains, clauses)
        assert models(domains, first["result"]) == expected
        assert models(domains, second["result"]) == expected
        report.append(dict(name=case["name"], domains=domains, clauses=clauses,
                           first=first, second=second,
                           second_productive=normalize(first["result"]) != normalize(second["result"])))
    bridge.close()
    (HERE / "directed_traces.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps([dict(name=r["name"], second_productive=r["second_productive"],
                           events=len(r["first"].get("events", []))) for r in report]))
    assert [r["second_productive"] for r in report] == [False, False, False, True]


if __name__ == "__main__":
    main()
