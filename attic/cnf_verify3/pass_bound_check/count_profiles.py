"""Independently count signatures by range profiles, not binary relations.

For m <= 4, enumerate every subset of nonzero m-bit membership patterns.
An additional unused value 000...0 makes all present ranges proper. This
enumerates all possible support/inclusion signatures without invoking the
preorder realization construction used in the proposed proof.
"""

import json
import math
from collections import Counter
from pathlib import Path


def count_signatures(m):
    patterns = list(range(1, 1 << m))
    coordinates = [
        sum(1 << position for position, pattern in enumerate(patterns) if pattern >> i & 1)
        for i in range(m)
    ]
    signatures = set()
    full_support = (1 << m) - 1
    for chosen in range(1, 1 << len(patterns)):
        support = sum(1 << i for i, coordinate in enumerate(coordinates) if chosen & coordinate)
        exceptions = tuple(
            bool(chosen & coordinates[i] & ~coordinates[j])
            for i in range(m)
            for j in range(m)
            if i != j
        )
        signatures.add((support, exceptions))
    by_size = Counter(bin(support).count("1") for support, _ in signatures)
    return {
        "m": m,
        "enumerated_pattern_domains": (1 << len(patterns)) - 1,
        "signatures": len(signatures),
        "signatures_by_support_size": dict(sorted(by_size.items())),
        "full_support_signatures": sum(support == full_support for support, _ in signatures),
        "coarse_bound": m + m * 2 ** (m * m + m),
        "sharp_bound": m + sum(number * size * 2**size for size, number in by_size.items()),
    }


results = [count_signatures(m) for m in range(1, 5)]
assert [row["signatures"] for row in results] == [1, 6, 44, 499]
preorders = {row["m"]: row["full_support_signatures"] for row in results}
for row in results:
    m = row["m"]
    assert row["signatures"] == sum(math.comb(m, k) * preorders[k] for k in range(1, m + 1))
    assert row["sharp_bound"] == m + sum(
        math.comb(m, k) * preorders[k] * k * 2**k for k in range(1, m + 1)
    )
output = Path(__file__).with_name("profile_counts.json")
output.write_text(json.dumps(results, indent=2) + "\n")
print(json.dumps(results, indent=2))
