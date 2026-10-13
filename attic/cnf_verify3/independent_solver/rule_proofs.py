"""Exact pointwise Boolean abstraction of all local algebraic rule schemas.

Every arbitrary-domain valuation induces one of these Boolean patterns, so
these finite exhaustive checks prove the schemas for all domain cardinalities.
They do not alone prove the implementation establishes the schema premises.
"""
import itertools
import json


def prove(names, premise, before, after):
    names = names.split()
    total = admitted = 0
    for row in itertools.product((False, True), repeat=len(names)):
        valuation = dict(zip(names, row))
        total += 1
        if premise(**valuation):
            admitted += 1
            assert before(**valuation) == after(**valuation), valuation
    return dict(total_patterns=total, satisfying_premises=admitted, counterexamples=0)


def main():
    proofs = {}
    proofs["subsumption"] = prove("a b", lambda a, b: not a or b,
        lambda a, b: a and b, lambda a, b: a)
    proofs["unit_restriction"] = prove("u ts to", lambda **v: True,
        lambda u, ts, to: u and (ts or to),
        lambda u, ts, to: u and ((ts and u) or to))
    proofs["sse1"] = prove("ds do ts to", lambda ds, do, ts, to: not do or to,
        lambda ds, do, ts, to: (ds or do) and (ts or to),
        lambda ds, do, ts, to: (ds or do) and ((ts and ds) or to))
    proofs["hla"] = prove("ds do ts to", lambda ds, do, ts, to: not do or to,
        lambda ds, do, ts, to: (ds or do) and (ts or to),
        lambda ds, do, ts, to: (ds or do) and (ts or to or not ds))
    names = "a_s a_t a_o b_s b_t b_o t_s t_t t_o"
    def premise(a_s, a_t, a_o, b_s, b_t, b_o, t_s, t_t, t_o):
        return (not a_o or t_o) and (not b_o or t_o) and (not (a_s and b_s) or t_s)
    def before(a_s, a_t, a_o, b_s, b_t, b_o, t_s, t_t, t_o):
        return (a_s or a_t or a_o) and (b_s or b_t or b_o) and (t_s or t_t or t_o)
    def after(a_s, a_t, a_o, b_s, b_t, b_o, t_s, t_t, t_o):
        return (a_s or a_t or a_o) and (b_s or b_t or b_o) and (t_s or (t_t and (a_t or b_t)) or t_o)
    proofs["sse2"] = prove(names, premise, before, after)
    print(json.dumps(proofs, indent=2))


if __name__ == "__main__":
    main()
