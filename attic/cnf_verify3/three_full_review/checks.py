"""Independent set-rule observer and small order-sensitive production controls.

Uses only Python's standard library. The checker never imports the proposal's
algebraic SMT assertions, its oracle, or the completed profile enumeration.
"""
import argparse
from collections import Counter
import hashlib
import itertools
import json
from pathlib import Path
import random
import subprocess
import time

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]


def seq(x):
    return [x] if isinstance(x, (str, int, bool)) else x or []


def formula(x):
    if isinstance(x, bool):
        return x
    return [{s: frozenset(seq(v)) for s, v in c.items()} for c in x]


def normal(x):
    x = formula(x)
    if isinstance(x, bool):
        return x
    return sorted(tuple(sorted((s, tuple(sorted(v))) for s, v in c.items())) for c in x)


def truth(domains, clauses):
    if isinstance(clauses, bool):
        return tuple(itertools.repeat(clauses, product_size(domains)))
    symbols = tuple(domains)
    return tuple(all(any(a[symbols.index(s)] in v for s, v in c.items()) for c in clauses)
                 for a in itertools.product(*(domains[s] for s in symbols)))


def product_size(domains):
    n = 1
    for values in domains.values():
        n *= len(values)
    return n


def propagate(domains, clauses, target):
    """Finite domain propagation with T false; repeat all donors to closure."""
    remaining = {s: set(v) - clauses[target].get(s, set()) for s, v in domains.items()}
    if any(not v for v in remaining.values()):
        return True
    donors = [c for i, c in enumerate(clauses) if i != target]
    while True:
        changed = False
        for donor in donors:
            possible = [(s, v & remaining[s]) for s, v in donor.items() if v & remaining[s]]
            if not possible:
                return True
            if len(possible) == 1:
                s, values = possible[0]
                if values != remaining[s]:
                    remaining[s] = values
                    changed = True
        if not changed:
            return False


def virtual_closure(domains, clauses, target):
    """Separate calibration: grow a virtual clause by donor complements."""
    current = {s: set(clauses[target].get(s, set())) for s in domains}
    donors = [c for i, c in enumerate(clauses) if i != target]
    if any(current[s] == set(domains[s]) for s in domains):
        return True
    while True:
        old = {s: set(v) for s, v in current.items()}
        for donor in donors:
            exceptions = [s for s, v in donor.items() if not v <= current[s]]
            if not exceptions:
                return True
            if len(exceptions) == 1:
                s = exceptions[0]
                current[s].update(set(domains[s]) - donor[s])
        if old == current:
            return False


def opportunities(domains, clauses):
    """Enumerate raw set premises, independently of production cached roles."""
    if isinstance(clauses, bool):
        return []
    symbols = tuple(domains)
    found = []
    for t, target in enumerate(clauses):
        for d, donor in enumerate(clauses):
            if t == d:
                continue
            if all(v <= target.get(s, set()) for s, v in donor.items()):
                found.append(dict(kind="direct", target=t, donor=d))
            for s in target:
                if (target[s] - donor.get(s, set()) and
                        all(v <= target.get(q, set()) for q, v in donor.items() if q != s)):
                    found.append(dict(kind="sse1", target=t, donor=d, symbol=s))
        for a, b in itertools.combinations([i for i in range(len(clauses)) if i != t], 2):
            left, right = clauses[a], clauses[b]
            for s, q in itertools.permutations(symbols, 2):
                if not target.get(q, set()) - (left.get(q, set()) | right.get(q, set())):
                    continue
                if not left.get(s, set()) & right.get(s, set()) <= target.get(s, set()):
                    continue
                if all(v <= target.get(k, set()) for donor in (left, right)
                       for k, v in donor.items() if k not in (s, q)):
                    found.append(dict(kind="sse2", target=t, donors=[a, b], pivot=s, symbol=q))
        if propagate(domains, clauses, t):
            found.append(dict(kind="hla", target=t))
    return found


def apply_rule(clauses, rule):
    out = [{s: set(v) for s, v in c.items()} for c in clauses]
    target = rule["target"]
    if rule["kind"] in ("direct", "hla"):
        out.pop(target)
        return out
    s = rule["symbol"]
    donors = [rule["donor"]] if rule["kind"] == "sse1" else rule["donors"]
    bound = set().union(*(clauses[d].get(s, set()) for d in donors))
    out[target][s].intersection_update(bound)
    if not out[target][s]:
        del out[target][s]
    return out


def calibrate():
    domains = {s: ("0", "1") for s in "XY"}
    literals = [frozenset(), frozenset(("0",)), frozenset(("1",))]
    clauses = [{s: v for s, v in zip("XY", pair) if v}
               for pair in itertools.product(literals, repeat=2) if any(pair)]
    counts = Counter()
    for selected in itertools.product(clauses, repeat=3):
        base = truth(domains, selected)
        for target in range(3):
            assert propagate(domains, selected, target) == virtual_closure(domains, selected, target)
            counts["propagation_calibrations"] += 1
        for rule in opportunities(domains, selected):
            assert truth(domains, apply_rule(selected, rule)) == base, rule
            counts["rule_soundness_" + rule["kind"]] += 1
        counts["calibration_formulas"] += 1
    assert all(counts["rule_soundness_" + k] for k in ("direct", "sse1", "sse2", "hla"))
    return counts


class Bridge:
    def __init__(self, current_r=False):
        if current_r:
            subprocess.run(["bash", "attic/cnf_verify3/review_semantics/run_r46.sh", "-e", "invisible(NULL)"],
                cwd=str(ROOT), check=True)
        # The launcher omits podman -i, so its stdin cannot carry this protocol.
        command = (["podman", "exec", "-i", "cnf-review-r46", "Rscript"] if current_r else ["Rscript"])
        self.process = subprocess.Popen(command + [str(HERE.relative_to(ROOT) / "bridge.R")],
            cwd=str(ROOT), stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, bufsize=1)
        self.version = self.request(dict(version=True))["version"]

    def request(self, request):
        self.process.stdin.write(json.dumps(request) + "\n")
        self.process.stdin.flush()
        line = self.process.stdout.readline()
        if not line:
            raise RuntimeError("R bridge exited: %s" % self.process.poll())
        result = json.loads(line)
        assert result["ok"], result
        return result

    def close(self):
        self.process.stdin.close()
        self.process.wait(timeout=10)


def snapshot_observations(events, original):
    counts = Counter()
    saved = {}
    for event in events:
        state = event["state"]
        counts["event_" + event["label"]] += 1
        entries = formula(state["entries"])
        if isinstance(entries, bool) or not state["available"] or not state["not_subset_count"]:
            continue
        active = [not a and not b for a, b in zip(seq(state["eliminated"]), seq(state["is_unit"]))]
        if len(original) == 3 and all(active):
            for target in range(3):
                a, b = [i for i in range(3) if i != target]
                for t in original[target]:
                    private = original[target][t] - (original[a][t] | original[b][t])
                    if not private & entries[target].get(t, set()):
                        continue
                    assert entries[target][t] == original[target][t]
                    counts["private_target_range_checks"] += 1
                    for s in original[target]:
                        if s != t:
                            assert (entries[a].get(s, set()) & entries[b].get(s, set()) ==
                                    original[a][s] & original[b][s])
                            counts["constant_donor_intersection_checks"] += 1
        if event["label"] == "range_request" and event["local"]["is_unit_propagation"]:
            local = event["local"]
            assert entries[local["clause_idx"] - 1].get(local["symbol"], set()) <= set(seq(local["restringent"]))
            counts["unit_no_narrowing_checks"] += 1
        # jsonlite encodes matrices as lists of rows, including dimension one.
        available = [i - 1 for i in seq(state["available"])]
        matrices = state["is_not_subset_of"]
        for ai, source in enumerate(available):
            if not active[source] or not matrices[ai]:
                continue
            columns = seq(state["columns"][ai])
            for bi, target in enumerate(available):
                if ai == bi or not active[target] or state["not_subset_count"][ai][bi] is None:
                    continue
                bits = matrices[ai][bi]
                assert sum(bits) == state["not_subset_count"][ai][bi], (event["label"], ai, bi,
                    columns, bits, state["not_subset_count"], event)
                for s, bit in zip(columns, bits):
                    contained = entries[source].get(s, set()) <= entries[target].get(s, set())
                    # At all chosen checkpoints reverse flags have been repaired,
                    # except range_request/symbol_remove which precede any write.
                    if not bit:
                        assert contained, (event["label"], source, target, s, event)
                    if bit and contained:
                        key = "debt_enabled" if state["second_order_enabled"] else "debt_initialization"
                        counts[key] += 1
                        saved.setdefault(key, dict(source=source, target=target, symbol=s, event=event))
        if event["label"] == "symbol_batch" and state["second_order_enabled"]:
            local = event["local"]
            ai = local["meta_idx"] - 1
            for bi in [i - 1 for i in seq(local["rows_changed"])]:
                target = available[bi]
                if active[target] and state["not_subset_count"][ai][bi] == 1:
                    counts["enabled_batch_pending_one"] += 1
                    saved.setdefault("enabled_batch_pending_one", event)
    return counts, saved


def jsonable(value):
    if isinstance(value, (set, frozenset, tuple)):
        return list(value)
    raise TypeError(type(value).__name__)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--current-r", action="store_true")
    parser.add_argument("--random", type=int, default=1200)
    parser.add_argument("--trace", type=int, default=1200)
    parser.add_argument("--boolean", action="store_true")
    args = parser.parse_args()
    started = time.time()
    counts = calibrate()
    report = dict(source_sha256=hashlib.sha256((ROOT / "R/CnfFormula_simplify.R").read_bytes()).hexdigest(),
        seed=609202631, arguments=vars(args), examples={}, controls=[])
    bridge = Bridge(args.current_r)
    report["r_version"] = bridge.version

    def check(domains, clauses, trace=False, negative=False, label=None):
        result = bridge.request(dict(domains=domains, clauses=clauses, trace=trace))
        initial, first, second = map(formula, (clauses, result["first"], result["second"]))
        expected = truth(domains, initial)
        assert truth(domains, first) == expected
        assert truth(domains, second) == expected
        residual = opportunities(domains, first)
        counts["formula_arrangements"] += 1
        counts["ordinary_production_calls"] += 2
        counts["truth_comparisons"] += 2
        counts["truth_valuation_rows"] += 2 * product_size(domains)
        if negative:
            assert residual and normal(result["first"]) != normal(result["second"])
            report["controls"].append(dict(label=label, first=result["first"], residual=residual))
            counts["known_gap_controls"] += 1
        else:
            if residual or normal(result["first"]) != normal(result["second"]):
                failure = dict(domains=domains, clauses=clauses, result=result, residual=residual)
                (HERE / "COUNTEREXAMPLE.json").write_text(json.dumps(failure, indent=2, default=jsonable) + "\n")
                raise AssertionError("Counterexample saved")
            counts["saturated_arrangements"] += 1
        if trace:
            observed, examples = snapshot_observations(result["events"], initial)
            counts.update(observed)
            counts["instrumentation_identity_calibrations"] += 1
            for name, example in examples.items():
                report["examples"].setdefault(name, dict(domains=domains, clauses=clauses, witness=example))

    # Falsification controls deliberately lie outside the theorem's input shape.
    check(dict(X=list("1245"), Y=list("235")),
        [dict(X=list("24"), Y=["3"]), dict(Y=["5"], X=list("21")), dict(Y=["2"], X=list("25"))],
        negative=True, label="three_binary_clauses_unit_equality")
    raw = json.loads((HERE.parent / "independent_solver/minimized_oneend_symbol_removal.json").read_text())
    check(raw["domains"], raw["clauses"], negative=True, label="five_clauses_missing_sse2")

    perms = list(itertools.permutations("XYZ"))
    orders = [(tuple("XYZ"), p, q) for p in perms for q in perms]
    directed = [
        (dict(X=list("01234"), Y=list("0123"), Z=list("01")),
         [dict(X=list("01"), Y=list("01"), Z=["0"]),
          dict(X=list("02"), Y=list("012"), Z=["0"]),
          dict(X=list("13"), Y=["0"], Z=["0"])]),
        (dict(X=list("01234"), Y=list("0123"), Z=list("01")),
         [dict(X=list("01"), Y=list("01"), Z=["0"]),
          dict(X=list("02"), Y=list("012"), Z=["0"]),
          dict(X=["3"], Y=["0"], Z=["0"])]),
        ({s: list("0123") for s in "XYZ"},
         [dict(X=list("012"), Y=["0"], Z=["0"]),
          dict(X=list("01"), Y=["1"], Z=["1"]),
          dict(X=["2"], Y=["2"], Z=["2"])])
    ]
    for domains, clauses in directed:
        for order in itertools.product(perms, repeat=3):
            check(domains, [{s: c[s] for s in o} for c, o in zip(clauses, order)], trace=True)
            counts["directed_arrangements"] += 1
    if args.boolean:
        domain = {s: ["0", "1"] for s in "XYZ"}
        literals = list(itertools.product("01", repeat=3))
        for base in itertools.product(literals, repeat=3):
            clauses = [{s: [v] for s, v in zip("XYZ", c)} for c in base]
            for oi, order in enumerate(orders):
                check(domain, [{s: c[s] for s in o} for c, o in zip(clauses, order)], trace=oi == 0)
            counts["boolean_base_inputs"] += 1

    rng = random.Random(report["seed"])
    for i in range(args.random):
        # Independently sampled membership profiles, including absent 000 cells.
        domains = {}
        clauses = [dict() for _ in range(3)]
        for symbol in "XYZ":
            while True:
                maximum = 5 if i % 2 else 9
                profiles = sorted(rng.sample(range(8), rng.randrange(2, maximum)))
                values = [[str(p) for p in profiles if p & (1 << j)] for j in range(3)]
                if all(0 < len(v) < len(profiles) for v in values):
                    break
            domains[symbol] = list(map(str, profiles))
            for j, v in enumerate(values):
                clauses[j][symbol] = v
        # All 216 independent within-clause orders on the first eight cases.
        selected_orders = list(itertools.product(perms, repeat=3)) if i < 8 else [rng.choice(orders)]
        for order in selected_orders:
            check(domains, [{s: c[s] for s in o} for c, o in zip(clauses, order)], trace=i < args.trace)
        counts["random_base_inputs"] += 1
        if (i + 1) % 200 == 0:
            print(json.dumps(dict(progress=i + 1, arrangements=counts["formula_arrangements"],
                                  elapsed=time.time() - started)), flush=True)
    bridge.close()
    report["counts"] = dict(counts)
    report["elapsed_seconds"] = time.time() - started
    suffix = "r46" if args.current_r else "r36"
    (HERE / ("results_" + suffix + ".json")).write_text(json.dumps(report, indent=2, default=jsonable) + "\n")
    print(json.dumps(dict(r_version=report["r_version"], counts=counts, elapsed=report["elapsed_seconds"])), flush=True)


if __name__ == "__main__":
    main()
