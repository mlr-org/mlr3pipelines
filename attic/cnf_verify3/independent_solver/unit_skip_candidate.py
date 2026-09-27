"""Causal probe of the equality-skip finding using a source-copy variant."""
import itertools
import json

from fixed_point import opportunities
from oracle import HERE, MDDOracle, OneHotOracle, RBridge


def main():
    bridge = RBridge()
    report = []
    for file in ("minimized_subsumption.json", "minimized_sse1.json", "minimized_sse2.json", "directed_oneend_shrink_min.json"):
        record = json.loads((HERE / file).read_text())
        domains, clauses = record["domains"], record["clauses"]
        original = bridge.simplify(domains, clauses)
        candidate = bridge.simplify(domains, clauses, variant="unit_skip_length")
        assert original["ok"] and candidate["ok"]
        assert OneHotOracle(domains).difference(clauses, candidate["result"]) is None
        assert MDDOracle(domains).equivalent(clauses, candidate["result"])
        report.append(dict(file=file, original=original["result"], candidate=candidate["result"],
                           remaining=opportunities(candidate["result"])))
    # All order variants of both quotient forms of the minimized selector:
    # common + 3 private values, with/without one value in no input range.
    extra_cases = 0
    for outside in (False, True):
        domains = {"X": ["c", "a", "b", "d"] + (["z"] if outside else []), "Y": ["a", "b", "c"]}
        for permutation in itertools.permutations(range(3)):
            for orientation in itertools.product((0, 1), repeat=3):
                clauses = []
                for i in permutation:
                    pairs = [("X", ["c", ["a", "b", "d"][i]]), ("Y", [domains["Y"][i]])]
                    if orientation[i]: pairs.reverse()
                    clauses.append(dict(pairs))
                candidate = bridge.simplify(domains, clauses, variant="unit_skip_length")
                assert candidate["ok"]
                assert not opportunities(candidate["result"]), (domains, clauses, candidate)
                assert OneHotOracle(domains).difference(clauses, candidate["result"]) is None
                assert MDDOracle(domains).equivalent(clauses, candidate["result"])
                extra_cases += 1
    bridge.close()
    output = dict(regressions=report, all_order_selector_cases=extra_cases)
    (HERE / "unit_skip_candidate_results.json").write_text(json.dumps(output, indent=2) + "\n")
    print(json.dumps(output, indent=2))


if __name__ == "__main__":
    main()
