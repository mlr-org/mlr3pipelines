"""Exhaust every selected-donor set pattern on domains of size one to eight."""

import json
import pathlib

HERE = pathlib.Path(__file__).resolve().parent
records = []
for size in range(1, 9):
    universe = set(range(size))
    subsets = [{v for v in universe if mask & (1 << v)} for mask in range(1 << size)]
    selected = 0
    for current in subsets:
        for donor in subsets:
            missing = donor - current
            if not missing:
                continue
            updated = current | (universe - (current | donor))
            assert missing <= universe - updated
            assert len(updated) < len(universe)
            assert updated == current | (universe - donor)
            selected += 1
    assert selected == 4 ** size - 3 ** size
    records.append(dict(domain_size=size, selected_donor_pairs=selected))
result = dict(records=records, total=sum(r["selected_donor_pairs"] for r in records))
(HERE / "hla_complement_results.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(result))
