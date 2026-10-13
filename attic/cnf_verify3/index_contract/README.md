# Index and value-shape contract audit

Completed 2026-09-06 against the unchanged kernel source SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.

**Result:** no canonical-input indexing/shape counterexample found. The
source-level argument discharges valid numerical/name coordinates, matrix
allocation and rank, scalar exceptional-name selection, definition before
use, callback snapshots, and the late change from a count matrix to a vector.
It is formulated as a first-failing-operation/prefix induction; whole-function
successful normal return is not an assumption.

The result is specifically about ordinary R argument validity under the
canonical finite-set contract. Primitive totality, absence of custom input
evaluation, representable arithmetic and enough stack/allocation resources
are separate assumptions. Finite termination and semantic preservation are
separate proofs; this directory does not silently combine those obligations.

**A literal claim that no intermediate NA indices ever occur would be false.**
Lines 382/424 deliberately use NA-producing logical vector `[` selectors for
unfinished pairs. Lines 383/425 filter the resulting candidate vectors before
any scalar `[[` or matrix-coordinate use. The safety result concerns unsafe
consumption, not these defined intermediate vector operations.

## Source argument

* `PROOF.md` gives the full obligations and prefix induction. It distinguishes
  allocated matrices from initialized pairs, states which invariants hold at
  inference boundaries, and explains the finite between-write transitions.
* `INVENTORY.md` groups every source indexing site by its guard/invariant.
* `sites_r36_seed19073.csv` and `sites_r46_seed19073.csv` are independently
  generated exact-line inventories of all **340 indexing sites**, **108 `if`
  sites**, and **34 short-circuit operator sites**.

The important discharges are:

1. A callback cannot advance `meta_idx_outer`. A saved registry entry at or
   below that bound had an allocated matrix while active at snapshot creation,
   even if it subsequently becomes a unit on another symbol. Future slots
   remain protected by the bound checks.
2. Actual clauses only shorten, while matrix column names never change.
   Current and just-deleted symbols therefore have valid frozen matrix columns;
   looking in the shortened clause instead would be wrong.
3. Count equals TRUE-bit count at every selection/inference boundary, with
   synchronized bit/count repair before callbacks. This supplies scalar
   count-one pivots and two-element count-two pivots without needing semantic
   exactness of a temporarily stale comparison.
4. A non-unit HLA target cannot decrement a missing self-donor counter: its
   self row is FALSE. Every other registry index has a defined local inverse.
5. Unit-HLA rows cannot be decremented before allocation. An unallocated row
   consequently retains its initial count, and a selected count-one row has
   exactly one TRUE when initialized. This shape-only lazy-row lemma needs
   fewer premises than the separate semantic lazy-row lemma.
6. `remaining_nonunit_entries` and its registry domain are immutable when the
   delayed inverse is created/forced. No pair helper is called after
   `not_subset_count` becomes the unit-HLA vector.

## Independent exact-source instrumentation

`check_source_shapes.R` reads and parses the actual production file, annotates
every original access/condition with its source line, and creates an in-memory
observed function. Production files are never changed. It uses no earlier
harness or random generator and requires only base R.

Read wrappers evaluate each original object/index once, validate its extent,
name, scalarity and rank, then apply the ordinary primitive. Nested assignment
ancestors and final write coordinates are checked before the original
assignment, without changing the assignment itself. All original assignment
indices are pure; prechecking them cannot change the location the subsequent
assignment writes. The wrappers preserve missing matrix arguments and literal
NULL arguments in the parsed source. Additional observations check unique
registration, stale other-symbol units, future-slot guards, fixed lazy-inverse
domain and the count/row relationship of allocated/unallocated unit-HLA rows.

The detector deliberately enforces the original kernel's intended scalar
`[[` forms; it is not an implementation of every feature of arbitrary R
indexing, such as recursive multi-element `[[` or negative selectors that this
kernel never uses. It permits ordinary missing named-list/environment lookup,
legal zero `[` coordinates, and the deliberate NA-producing candidate slices.

For each case, returned storage is compared with the unmodified function and
an independent direct truth-table evaluator checks input/output equivalence.
The input portfolio consists of:

* 12 directed cases, including constants, empty conjunction, duplicate and
  contradictory units, zero/one available non-units, a deferred-registration
  example, and the newly constructed stale other-symbol-unit snapshot below;
* all 584 ordered lists of one through three clauses over the eight canonical
  clauses on two Boolean symbols (`8 + 8^2 + 8^3`), including duplicate clauses;
* 3,000 independently generated inputs with seed 19073, 2–6 symbols, 2–5 values
  per domain, and 1–18 clauses, with nonempty proper literal ranges.

Completed runs used **R 3.6.3** and **R 4.6.1**. Both passed the same **3,596
inputs**. Their count objects, site inventories and saved boundary fixtures
were checked to be identical. This is one input portfolio replayed in two
R versions, not 7,192 distinct generated formulas.

| Observation | Each R version |
| --- | ---: |
| Input/output storage comparisons and independent truth tables | 3,596 |
| Prechecked ordinary reads | 1,769,855 |
| Prechecked final writes | 221,459 |
| Separately checked nested write ancestors | 49,291 |
| Scalar conditions / short-circuit operands checked | 1,192,136 |
| Unit registrations checked | 9,954 |
| Future unit registrations | 63 |
| Future matrix guard taken | 171 |
| Defined NA-producing vector selectors | 5,154 |
| Preprocessing left zero available non-units | 684 |
| Preprocessing left one available non-unit | 279 |
| Stale snapshot now holding a different-symbol unit | 1 |
| Still-unallocated unit-HLA donor rows checked | 6,880 |
| Allocated unit-HLA donor rows checked | 3,495 |
| Direct negative shape controls rejected | 8 |

Every one of the 108 `if` sites and 34 short-circuit sites was evaluated; this
does **not** assert all outcomes or paths were covered. **339 of 340 indexing
sites** were exercised. The only unexecuted indexing site is the current-unit
elimination write at line 759 inside the hidden-tautology branch. Its scalar
index is already discharged by the source invariant; no unreachability claim
is needed to justify that write's shape. Earlier semantic reviews separately
explain why that branch is unreachable on canonical inputs.

The detailed runs are `run_r36_seed19073.log` and `run_r46_seed19073.log`.
The corresponding `results_*.rds` files retain counts, exact source MD5,
R version, generator parameters, per-site counts and first boundary examples.

## Directed stale-snapshot witness

With `X={0,1,2}` and Boolean `Y`, `Z`, `W`, use:

```
(X=2 OR Y=0), (Y=1 OR X=0), (X=2 OR Z=0),
(X in {0,1} OR W=0), (X in {0,1} OR W=1).
```

The last pair creates `X in {0,1}`. Its propagation produces nested X → Y → X
registrations. The third clause becomes the retained unit `Z=0` before the
original X loop reaches its saved entry. That loop's optional matrix reads
still refer to an allocated old X column, then the restriction's missing-X
guard exits. The checker observes exactly one such event. Direct evaluation
gives `X=0 AND Y=0 AND Z=0`, with W free, agreeing with the actual output.

This is a boundary witness, not a production defect. Root independently
derived its truth table and added public-API regression coverage in the
simplification test file, including alternate clause orders; that root-owned
change is outside this directory.

## Source-mutant controls

`check_source_controls.R` reuses this stream's instrumentation definitions,
accepts each canonical fixture with the original source, and then checks a
deliberately corrupted in-memory source copy. All **seven originals passed**
and all **seven modified copies were rejected**, on both R versions:

| Deliberate corruption | Rejected boundary |
| --- | --- |
| Remove the non-unit HLA match-NA guard | Missing donor index, line 688 |
| Give lazy unit-HLA donors the wrong initial count | Row/count invariant before selection |
| Remove future-matrix guards | Unallocated matrix consumed at line 205 |
| Find a removed symbol in the shortened current clause | Missing column position at line 264 |
| Drop the ordinary count matrix dimensions | Wrong rank when indexing the derived enable object at line 635 |
| Give the delayed inverse an empty/wrong domain | Missing unit-HLA row index at line 765 |
| Change the self row from FALSE to TRUE | Missing self-donor count index at line 711 |

The logs and exact canonical fixtures are saved in `source_controls_r36.*`
and `source_controls_r46.*`. These controls test detector sensitivity to the
specific safety obligations. They are not counterexamples to the original
source, and the earlier unit-HLA row/count detector fires before the eventual
vector-name consumption in its corrupted copy.

## Reproduction

From the repository root:

```sh
CNF_SHAPE_TRIALS=3000 CNF_SHAPE_TAG=r36_seed19073 Rscript attic/cnf_verify3/index_contract/check_source_shapes.R
CNF_SHAPE_TAG=r36 Rscript attic/cnf_verify3/index_contract/check_source_controls.R

bash attic/cnf_verify3/review_semantics/run_r46.sh -e 'Sys.setenv(CNF_SHAPE_TRIALS="3000", CNF_SHAPE_TAG="r46_seed19073"); source("attic/cnf_verify3/index_contract/check_source_shapes.R")'
bash attic/cnf_verify3/review_semantics/run_r46.sh -e 'Sys.setenv(CNF_SHAPE_TAG="r46"); source("attic/cnf_verify3/index_contract/check_source_controls.R")'
```

The current-R wrapper uses the already documented environment in
`../review_semantics/R46_ENVIRONMENT.md`. No dependencies, production edits
or commits were made by this stream. The finite checks support the source
argument but do not supply its general induction premise.
