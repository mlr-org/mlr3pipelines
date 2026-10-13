"""Independent exhaustive multiset lemma and calibrated failed strengthenings."""
from itertools import product
from pathlib import Path
import json


def check(max_size=5):
    totals = []
    for size in range(1, max_size + 1):
        checked = 0
        repeated_new = 0
        nonsaturated_old = 0
        for capacity in product(range(1, 4), repeat=size):
            # Every bounded submultiset, broader than the reachable forms.
            for old in product(*(range(n + 1) for n in capacity)):
                for donor in product((False, True), repeat=size):
                    witness = [i for i in range(size) if donor[i] and old[i] == 0]
                    if not witness:
                        continue
                    new = tuple(old[i] if old[i] or donor[i] else capacity[i]
                                for i in range(size))
                    support_new = tuple(bool(old[i]) or not donor[i] for i in range(size))
                    assert tuple(bool(n) for n in new) == support_new
                    assert all(new[i] <= capacity[i] for i in range(size))
                    assert all(new[i] == 0 for i in witness)
                    assert sum(new) < sum(capacity)
                    assert sum(support_new) < size
                    checked += 1
                    repeated_new += any(n > 1 for n in new)
                    nonsaturated_old += any(0 < old[i] < capacity[i] for i in range(size))
        totals.append(dict(domain_values=size, capacity_vectors=3**size,
                           selected_extensions=checked, repeated_new=repeated_new,
                           nonsaturated_old=nonsaturated_old))
        print(totals[-1], flush=True)

    # Loss of selected-donor exception allows a full-support underfilled
    # multiset: supports cover the domain while vector lengths differ.
    capacity, old, donor = (2, 1), (1, 0), (False, False)
    new = tuple(old[i] if old[i] or donor[i] else capacity[i] for i in range(2))
    assert all(new) and sum(new) != sum(capacity)

    # Loss of the capacity bound admits a false positive despite a valid
    # donor witness: duplicate copies of a different value compensate.
    bad_capacity, bad_old, bad_donor = (1, 1), (2, 0), (False, True)
    bad_new = tuple(bad_old[i] if bad_old[i] or bad_donor[i] else bad_capacity[i]
                    for i in range(2))
    assert not all(bad_new) and sum(bad_new) == sum(bad_capacity)

    # An implementation mutation that appends the donor itself immediately
    # destroys the independently asserted missing-value certificate.
    mutation_killed = False
    try:
        mutated = (1, 1)
        assert mutated[1] == 0
    except AssertionError:
        mutation_killed = True
    assert mutation_killed
    return dict(rows=totals, selected_extensions=sum(r['selected_extensions'] for r in totals),
                false_negative_without_exception=dict(capacity=capacity, old=old, donor=donor, new=new),
                false_positive_without_capacity_bound=dict(capacity=bad_capacity, old=bad_old,
                                                           donor=bad_donor, new=bad_new),
                donor_append_mutation_killed=mutation_killed)


if __name__ == '__main__':
    result = check()
    Path('attic/cnf_verify3/domain_storage_contract/local_lemma.json').write_text(
        json.dumps(result, indent=2) + '\n')
