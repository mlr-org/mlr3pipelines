"""Exhaust all pointwise Boolean states of the subset-cache transitions.

A set inclusion is equivalent to its implication at each value. These checks
therefore cover arbitrary finite domains for the listed local transitions.
They do not verify that production R chooses the intended transition at every
call site; that separate source-level argument is in LIFECYCLE_PROOF.md.

Run: python3 attic/cnf_verify3/proof_state/certificate_obligations.py
"""

from itertools import product


def subset_at_value(source, target, unit=True):
    return not (source and unit) or (target and unit)


checks = {}


def require(name, condition):
    assert condition, name
    checks[name] = checks.get(name, 0) + 1


for source, target, unit, restriction in product((False, True), repeat=4):
    if not subset_at_value(source, target, unit):
        continue

    # A previous positive subset certificate survives shrinking its source.
    require("source_shrink", subset_at_value(source and restriction, target, unit))

    # A target shrunk by a non-unit rule refreshes a FALSE bit to TRUE
    # whenever raw source inclusion ceased to hold. Retained FALSE bits are
    # therefore contextually sound (possibly more conservative than needed).
    target_new = target and restriction
    remains_false = subset_at_value(source, target_new)
    require("nonunit_target_shrink_with_reverse_refresh",
            not remains_false or subset_at_value(source, target_new, unit))

    # For a unit target restriction, inverse cache updates may be omitted:
    # the new unit intersects both interpretations, including pending sources.
    unit_new = unit and restriction
    require("unit_target_shrink_without_reverse_refresh",
            subset_at_value(source, target_new, unit_new))

    # Source-symbol deletion clears the entire comparison column FALSE.
    require("source_symbol_deletion", subset_at_value(False, target, unit))

    # A target-symbol deletion flips all old FALSEs with a nonempty source
    # range. Pointwise source absence is the only case retaining FALSE here.
    remains_false = not source
    require("target_symbol_deletion_with_reverse_refresh",
            not remains_false or subset_at_value(source, False, unit))


for source, target, unit in product((False, True), repeat=3):
    # A direct raw comparison may introduce a new positive certificate.
    require("raw_comparison_introduction",
            not subset_at_value(source, target) or subset_at_value(source, target, unit))

    # This is the pointwise induction step for a deferred birth certificate:
    # if C is physically inside all earlier units, C ∩ earlier_units ⊆ R
    # becomes ordinary C ⊆ R after those earlier obligations are discharged.
    certificate = subset_at_value(source and unit, target)
    prior_obligations = subset_at_value(source, unit)
    require("birth_induction_step",
            not (certificate and prior_obligations) or subset_at_value(source, target))


# The stronger raw invariant really is false during a unit restriction.
source, old_target, old_unit, new_restriction = True, True, True, False
assert subset_at_value(source, old_target, old_unit)
assert not subset_at_value(source, old_target and new_restriction)
assert subset_at_value(source, old_target and new_restriction,
                       old_unit and new_restriction)

for name, count in checks.items():
    print(f"{name}: {count} pointwise states verified")
print(f"TOTAL: {sum(checks.values())} obligations; all passed")
