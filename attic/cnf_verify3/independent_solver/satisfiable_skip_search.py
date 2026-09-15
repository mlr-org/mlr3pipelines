"""Force a known satisfying state while searching temporary out-of-unit skips."""
import collections
import json
import random
import time

from oracle import HERE, MDDOracle, OneHotOracle, RBridge, audit_events, evaluate


def main():
    rng, bridge = random.Random(7251904), RBridge()
    stats = collections.Counter()
    start = time.time()
    found = None
    for i in range(3000):
        n, size = rng.randint(3, 6), rng.randint(4, 9)
        domain = [str(j) for j in range(size)]
        domains = {"Q%d" % j: domain for j in range(n)}
        names = list(domains)
        clauses = []
        for j, source in enumerate(names):
            target = names[(j + 1) % n]
            transition = ["0"] + rng.sample(domain[1:], size - 1)
            for a, b in zip(domain, transition):
                pairs = [(source, [v for v in domain if v != a]), (target, [b])]
                if rng.random() < 0.5: pairs.reverse()
                clauses.append(dict(pairs))
        rng.shuffle(clauses)
        assert evaluate(clauses, {s: "0" for s in names})
        answer = bridge.simplify(domains, clauses, audit=True, variant="lifecycle")
        assert answer["ok"], answer
        assert evaluate(answer["result"], {s: "0" for s in names}), (domains, clauses, answer)
        counts = audit_events(answer["events"], domains)
        stats.update(counts)
        stats["formulas"] += 1
        if counts.get("unit_skip_outside"):
            answer = bridge.simplify(domains, clauses, audit=True, detailed=True, variant="lifecycle")
            assert OneHotOracle(domains).difference(clauses, answer["result"]) is None
            assert MDDOracle(domains).equivalent(clauses, answer["result"])
            found = dict(trial=i, domains=domains, clauses=clauses, answer=answer)
            (HERE / "outside_skip_satisfiable_cycle.json").write_text(json.dumps(found, indent=2) + "\n")
            break
        if (i + 1) % 100 == 0:
            print(json.dumps(dict(elapsed=time.time() - start, stats=stats)), flush=True)
    bridge.close()
    report = dict(elapsed=time.time() - start, stats=stats, found_trial=None if found is None else found["trial"])
    (HERE / "satisfiable_skip_search_results.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report), flush=True)


if __name__ == "__main__":
    main()
