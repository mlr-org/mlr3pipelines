"""Enumerate small preorders and check their finite-set realization directly."""
import itertools
import json
import math
from pathlib import Path

counts = {}
realizations = 0
for k in range(1, 5):
    pairs = [(i, j) for i in range(k) for j in range(k) if i != j]
    total = 0
    for bits in range(1 << len(pairs)):
        relation = {(i, i) for i in range(k)}
        relation.update(pair for bit, pair in enumerate(pairs) if bits & (1 << bit))
        if any((i, j) in relation and (j, h) in relation and (i, h) not in relation
               for i, j, h in itertools.product(range(k), repeat=3)):
            continue
        ranges = [{h for h in range(k) if (h, i) in relation} for i in range(k)]
        domain = set(range(k + 1))
        assert all(r and r < domain for r in ranges)
        assert all((ranges[i] <= ranges[j]) == ((i, j) in relation)
                   for i, j in itertools.product(range(k), repeat=2))
        total += 1
        realizations += 1
    counts[k] = total

records = []
for m in range(1, 5):
    signatures = sum(math.comb(m, k) * counts[k] for k in range(1, m + 1))
    sharper_bound = m + sum(math.comb(m, k) * counts[k] * k * (1 << k)
                            for k in range(1, m + 1))
    coarse_bound = m + m * (1 << (m * m + m))
    assert sharper_bound <= coarse_bound
    records.append(dict(clauses=m, signatures=signatures, sharper_bound=sharper_bound,
                        coarse_bound=coarse_bound))
result = dict(preorder_counts=counts, checked_set_realizations=realizations, bounds=records)
Path(__file__).with_name("signature_counts.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(result, indent=2))
