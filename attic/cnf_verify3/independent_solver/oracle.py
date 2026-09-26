"""Independent semantics and persistent R adapter. Python 3.8 compatible."""
import functools
import itertools
import json
import pathlib
import subprocess

import z3

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[2]


def values(value):
    return (value,) if isinstance(value, str) else tuple(value or ())


def normalize(formula):
    if isinstance(formula, bool):
        return formula
    return [{symbol: values(v) for symbol, v in clause.items()} for clause in formula]


def evaluate(formula, assignment):
    """Direct definition, used only for calibration and extracted witnesses."""
    if isinstance(formula, bool):
        return formula
    return all(any(assignment[symbol] in values(v) for symbol, v in clause.items())
               for clause in formula)


class RBridge:
    def __init__(self):
        self.process = subprocess.Popen(
            ["Rscript", str(HERE / "r_bridge.R")], cwd=str(ROOT),
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, bufsize=1)

    def simplify(self, domains, clauses, audit=False, direct=False, detailed=False, variant=None):
        request = dict(domains=domains, clauses=clauses, audit=audit, direct=direct, detailed=detailed, variant=variant)
        self.process.stdin.write(json.dumps(request, separators=(",", ":")) + "\n")
        self.process.stdin.flush()
        answer = self.process.stdout.readline()
        if not answer:
            raise RuntimeError("R bridge exited: %s" % self.process.poll())
        return json.loads(answer)

    def close(self):
        self.process.stdin.close()
        self.process.wait(timeout=10)


class OneHotOracle:
    """Ordinary Boolean SAT: one atom per concrete symbol/value, exactly one true."""
    def __init__(self, domains, timeout_ms=30000):
        self.domains = {s: tuple(dict.fromkeys(v)) for s, v in domains.items()}
        self.atoms = {(s, v): z3.Bool("v%d_%d" % (si, vi))
                      for si, (s, vals) in enumerate(self.domains.items())
                      for vi, v in enumerate(vals)}
        self.solver = z3.Solver()
        self.solver.set(timeout=timeout_ms)
        for symbol, domain in self.domains.items():
            atoms = [self.atoms[symbol, value] for value in domain]
            self.solver.add(z3.Or(atoms))
            # Explicit ordinary clauses; no multivalued propagation in the oracle.
            self.solver.add([z3.Or(z3.Not(a), z3.Not(b))
                             for a, b in itertools.combinations(atoms, 2)])

    def encode(self, formula):
        if isinstance(formula, bool):
            return z3.BoolVal(formula)
        return z3.And([z3.Or([self.atoms[s, v] for s, vals in clause.items()
                             for v in values(vals)]) for clause in formula])

    def difference(self, left, right):
        """Return None for equivalent, or a checked concrete countervaluation."""
        self.solver.push()
        self.solver.add(z3.Xor(self.encode(left), self.encode(right)))
        verdict = self.solver.check()
        if verdict == z3.unknown:
            reason = self.solver.reason_unknown()
            self.solver.pop()
            raise TimeoutError(reason)
        witness = None
        if verdict == z3.sat:
            model = self.solver.model()
            witness = {s: next(v for v in domain if z3.is_true(model.eval(self.atoms[s, v])))
                       for s, domain in self.domains.items()}
            if evaluate(left, witness) == evaluate(right, witness):
                raise AssertionError("Boolean encoding returned an invalid countervaluation")
        self.solver.pop()
        return witness

    def implies(self, left, right):
        # Reuse equivalence: L implies R iff L equals L and R.
        if isinstance(left, bool) or isinstance(right, bool):
            self.solver.push()
            self.solver.add(self.encode(left), z3.Not(self.encode(right)))
            verdict = self.solver.check()
            self.solver.pop()
            if verdict == z3.unknown:
                raise TimeoutError("implication unknown")
            return verdict == z3.unsat
        return self.difference(left, list(left) + list(right)) is None


class MDDOracle:
    """Reduced ordered multivalued decision diagrams with canonical DAG ids.

    Clause construction is a chain of value tests. Formula conjunction uses
    the Shannon apply algorithm. There is no clause restriction, resolution,
    subset matrix, SAT encoding, or production simplification rule here.
    """
    def __init__(self, domains, node_limit=1000000):
        self.domains = [tuple(dict.fromkeys(v)) for v in domains.values()]
        self.symbols = {s: i for i, s in enumerate(domains)}
        self.nodes = [None, None]
        self.unique = {}
        self.node_limit = node_limit
        self.apply = functools.lru_cache(None)(self._apply)

    def node(self, variable, children):
        children = tuple(children)
        if len(set(children)) == 1:
            return children[0]
        key = variable, children
        if key not in self.unique:
            if len(self.nodes) >= self.node_limit:
                raise MemoryError("MDD node budget exceeded")
            self.unique[key] = len(self.nodes)
            self.nodes.append(key)
        return self.unique[key]

    def _apply(self, left, right):
        if left == 0 or right == 0:
            return 0
        if left == 1:
            return right
        if right == 1 or left == right:
            return left
        li, lc = self.nodes[left]
        ri, rc = self.nodes[right]
        variable = min(li, ri)
        return self.node(variable, [self.apply(lc[i] if li == variable else left,
                                                rc[i] if ri == variable else right)
                                   for i in range(len(self.domains[variable]))])

    def clause(self, clause):
        result = 0
        for symbol in sorted(clause, key=self.symbols.__getitem__, reverse=True):
            variable = self.symbols[symbol]
            accepted = set(values(clause[symbol]))
            result = self.node(variable, [1 if v in accepted else result
                                          for v in self.domains[variable]])
        return result

    def encode(self, formula):
        if isinstance(formula, bool):
            return int(formula)
        result = 1
        for clause in formula:
            result = self.apply(result, self.clause(clause))
        return result

    def equivalent(self, left, right):
        return self.encode(left) == self.encode(right)


def audit_events(events, domains):
    """Check semantic sufficient premises with Python set semantics.

    The implementation supplies witnesses only. The audit does not trust its
    subset matrices, donor counts, clause registries, or truth values.
    """
    counts = {}
    for event in events:
        kind = event["kind"]
        counts[kind] = counts.get(kind, 0) + 1
        if kind == "unit_skip":
            current = set(values(event["target"].get(event["symbol"])))
            allowed = set(values(event["allowed"]))
            relation = "absent" if not current else "equal" if current == allowed else "proper" if current < allowed else "outside"
            key = "unit_skip_" + relation
            counts[key] = counts.get(key, 0) + 1
            continue
        units = {s: set(values(v)) for s, v in (event.get("units") or {}).items()}
        def ranges(clause, contextual=False):
            result = {s: set(values(v)) for s, v in clause.items()}
            if contextual:
                result = {s: (v & units[s]) if s in units else v for s, v in result.items()}
            return result
        target = ranges(event["target"])
        if kind == "sse2":
            assert all(event.get("active_donors", [True, True])), event
            assert event.get("active_target", True), event
            s, t = event["intersect_symbol"], event["restrict_symbol"]
            assert s != t, event
            def sse2_premises(contextual):
                a, b = ranges(event["donor_a"], contextual), ranges(event["donor_b"], contextual)
                target = ranges(event["target"], contextual)
                return (all(all(v <= target.get(k, set()) for k, v in donor.items() if k not in (s, t))
                            for donor in (a, b)) and
                        a.get(s, set()) & b.get(s, set()) <= target.get(s, set()))
            if not sse2_premises(False):
                assert sse2_premises(True), event
                counts["contextual_sse2"] = counts.get("contextual_sse2", 0) + 1
        else:
            assert event.get("active_donor", True), event
            assert event.get("active_target", True), event
            donor = ranges(event["donor"])
            exempt = event.get("symbol")
            if not all(v <= target.get(k, set()) for k, v in donor.items() if k != exempt):
                # Using the target unit itself as context while proving its
                # redundancy would be circular. Unit-HLA must start with actual
                # physical containment, and preserve raw inclusion thereafter.
                assert kind != "unit_hla", event
                contextual_target, contextual_donor = ranges(event["target"], True), ranges(event["donor"], True)
                assert all(v <= contextual_target.get(k, set()) for k, v in contextual_donor.items() if k != exempt), event
                counts["contextual_" + kind] = counts.get("contextual_" + kind, 0) + 1
            if kind in ("hla", "unit_hla"):
                assert event["target_id"] != event["donor_id"], event
                expected = target.get(exempt, set()) | (set(domains[exempt]) - donor[exempt])
                assert expected == set(values(event["extended"])), event
                # The selected donor must remain a non-subset at the exceptional
                # symbol. This stronger premise proves the HTE branch unreachable.
                assert donor[exempt] - target.get(exempt, set()), event
                assert expected < set(domains[exempt]), event
    return counts
