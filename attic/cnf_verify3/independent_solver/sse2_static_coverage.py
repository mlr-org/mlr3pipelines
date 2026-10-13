"""Exhaustive semantic case split for static SSE2 candidate coverage."""
import collections
import itertools
import json

from oracle import HERE

PATTERNS = list(itertools.product((0, 1), repeat=3))


def ranges(mask):
    return [set(j for j, p in enumerate(PATTERNS) if mask & (1 << j) and p[i])
            for i in range(3)]


def main():
    stats = collections.Counter()
    for mask_s in range(1, 256):
        sa, sb, st = ranges(mask_s)
        ds = set(j for j in range(8) if mask_s & (1 << j))
        for mask_t in range(1, 256):
            ta, tb, tt = ranges(mask_t)
            stats["configurations"] += 1
            if not (sa & sb <= st and tt - (ta | tb)):
                continue
            stats["useful_rules"] += 1
            ea = ({"s"} if not sa <= st else set()) | ({"t"} if not ta <= tt else set())
            eb = ({"s"} if not sb <= st else set()) | ({"t"} if not tb <= tt else set())
            assert not tt <= ta and not tt <= tb
            if not ea or not eb:
                stats["direct_subsumption"] += 1
            elif ea == {"t"} or eb == {"t"}:
                stats["stronger_sse1"] += 1
                chosen = ta if ea == {"t"} else tb
                assert tt - chosen
                assert tt & chosen <= tt & (ta | tb)
            elif ea == eb == {"s"}:
                stats["hla_deletion"] += 1
                extended = st | (ds - sa)
                assert sb <= extended and tb <= tt
            else:
                stats["twoend_enumerated"] += 1
                assert "s" in ea and "s" in eb and sa and sb
                assert ea == {"s", "t"} or eb == {"s", "t"}
                assert ea in ({"s"}, {"s", "t"}) and eb in ({"s"}, {"s", "t"})
                assert tt
    assert stats["useful_rules"] == sum(stats[k] for k in
        ("direct_subsumption", "stronger_sse1", "hla_deletion", "twoend_enumerated"))
    (HERE / "sse2_static_coverage_results.json").write_text(json.dumps(stats, indent=2) + "\n")
    print(json.dumps(stats, indent=2))


if __name__ == "__main__":
    main()
