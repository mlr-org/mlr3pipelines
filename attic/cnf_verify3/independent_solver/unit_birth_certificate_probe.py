"""Check skipped propagation using only units older than its source unit.

Each observed unit itself is independently checked as an input consequence.
The certificate check deliberately does not assume the registering unit.
"""
import collections
import json
import random
import time

from fixed_point import random_dense
from oracle import HERE, MDDOracle, OneHotOracle, RBridge, evaluate, normalize, values


def fixed_zero_cycle(rng):
    n, size = rng.randint(3, 7), rng.randint(4, 8)
    domain = list(map(str, range(size)))
    ds = {"Q%d" % j: domain for j in range(n)}
    names, cs = list(ds), []
    for j, s in enumerate(names):
        transition = ["0"] + rng.sample(domain[1:], size - 1)
        t = names[(j + 1) % n]
        for a, b in zip(domain, transition):
            terms = [(s, [v for v in domain if v != a]), (t, [b])]
            if rng.random() < 0.5:
                terms.reverse()
            cs.append(dict(terms))
    rng.shuffle(cs)
    assert evaluate(cs, {s: "0" for s in names})
    return ds, cs


def check_certificates(ds, cs, answer, stats):
    sat, mdd = OneHotOracle(ds), MDDOracle(ds)
    assert sat.difference(cs, answer["result"]) is None
    assert mdd.equivalent(cs, answer["result"])
    births, historical_domains = {}, {}
    for event in answer["events"]:
        if event["kind"] == "unit_birth":
            source_id = event["source_id"]
            assert source_id not in births, ("repeated unit index", event)
            source = normalize([event["source"]])[0]
            assert len(source) == 1
            observed = {s:set(values(v)) for s,v in (event["units"] or {}).items()}
            assert observed == historical_domains, ("unit registry differs from prior birth constraints", event)
            assert sat.implies(cs, [source])
            assert mdd.equivalent(cs, cs + [source])
            births[source_id] = event
            s = next(iter(source))
            historical_domains[s] = historical_domains.get(s, set(ds[s])) & set(source[s])
            stats["unit_births"] += 1
        elif event["kind"] == "unit_skip":
            birth = births[event["source_id"]]
            observed = {s:set(values(v)) for s,v in (event["units"] or {}).items()}
            assert observed == historical_domains, ("skip sees an unexpected effective unit", event)
            symbol = event["symbol"]
            source_domain = set(values(birth["source"][symbol]))
            older = birth["units"] or {}
            prior_domain = set(values(older[symbol])) if symbol in older else set(ds[symbol])
            current = set(values(event["target"].get(symbol)))
            assert current & prior_domain <= source_domain, dict(birth=birth, skip=event)
            assert set(values(event["source"][symbol])) == source_domain
            stats["skip_certificates"] += 1
            if not current <= source_domain:
                stats["requires_older_unit"] += 1
            allowed = set(values(event["allowed"]))
            if not current <= allowed:
                stats["outside_current_unit"] += 1
    return births


def main():
    rng, bridge = random.Random(7748630), RBridge()
    stats, start, saved = collections.Counter(), time.time(), False
    for i in range(3000):
        ds, cs = fixed_zero_cycle(rng) if i % 8 == 0 else random_dense(rng, i)
        answer = bridge.simplify(ds, cs, audit=True, variant="unit_birth")
        assert answer["ok"], dict(trial=i, domains=ds, clauses=cs, answer=answer)
        try:
            previous_deferred = stats["requires_older_unit"]
            check_certificates(ds, cs, answer, stats)
            if not saved and i % 8 == 0 and stats["requires_older_unit"] > previous_deferred:
                record = dict(trial=i, domains=ds, clauses=cs, answer=answer)
                (HERE / "unit_birth_satisfiable_deferred_certificate.json").write_text(json.dumps(record, indent=2) + "\n")
                saved = True
        except AssertionError as error:
            record = dict(trial=i, domains=ds, clauses=cs, answer=answer, message=str(error))
            (HERE / "unit_birth_certificate_failure.json").write_text(json.dumps(record, indent=2) + "\n")
            raise
        stats["formulas"] += 1
        if (i + 1) % 250 == 0:
            print(json.dumps(dict(stats=stats, elapsed=time.time() - start)), flush=True)
    bridge.close()
    report = dict(stats=stats, elapsed=time.time() - start)
    (HERE / "unit_birth_certificate_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report), flush=True)


if __name__ == "__main__":
    main()
