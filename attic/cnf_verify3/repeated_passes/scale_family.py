"""Larger constructed chains and a forward-order scheduling control."""

import json
from repeated import HERE, Bridge, chain_family, iterate, canonical


bridge = Bridge()
records = []
try:
    for n in (3, 8, 16, 24, 32):
        domains, clauses = chain_family(n)
        reverse = iterate(bridge, domains, clauses, limit=n + 3, verify=True)
        forward_clauses = clauses[:n + 1] + list(reversed(clauses[n + 1:]))
        forward = iterate(bridge, domains, forward_clauses, limit=n + 3, verify=True)
        assert reverse["productive"] == n
        assert forward["productive"] == 1
        assert canonical(reverse["trace"][-1]) == canonical(forward["trace"][-1])
        record = dict(n=n, domains=domains, clauses=clauses,
                      reverse=reverse, forward=forward)
        records.append(record)
        print("n=%d: reverse %d useful passes, forward %d, reverse seconds %.3f" %
              (n, reverse["productive"], forward["productive"],
               sum(reverse["seconds"])), flush=True)
        (HERE / "family_scale.json").write_text(json.dumps(records, indent=2) + "\n")
finally:
    bridge.close()
