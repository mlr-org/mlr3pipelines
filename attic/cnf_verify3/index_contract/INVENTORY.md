# Dynamic lookup and shape inventory

The generated `sites_r36_seed19073.csv` / `sites_r46_seed19073.csv` inventories
every original indexing and conditional expression with its exact source line:
105 `[` calls, 235 `[[` calls, 108 `if` sites, 8 `&&` sites and 26 `||` sites.
The source hash and contract are in `PROOF.md`. This table groups that complete
inventory by the invariant that discharges it; it does not rely on the dynamic
execution counts to establish safety.

| Source sites | Dynamic operation / required shape | Guard or invariant |
| --- | --- | --- |
| 30–32 | Character-vector masks in intersection/difference/union | `%in%` returns a defined logical vector of its left argument's length; a NULL absent range is a permitted empty set. |
| 48–51 | Entry permutation and Boolean flag lengths | `order(lengths(entries))` is a permutation of actual indices; flag vectors retain that fixed length. |
| 88–104 | Unit entry, scalar environment name, prior representative, first literal | Registration only receives a singleton; representative/domain bindings are installed together and never removed before HLA. Empty merged intersections exit before later use. |
| 115–133 | Registering-unit inverse, saved matrix column, registry-snapshot inverse, old matrix named column | Initial/preprocessing registrations have NULL matrices. Later births come from `available`. The registering and snapshot `<= meta_idx_outer` guards plus activity at snapshot creation establish allocated slots; frozen columns preserve names after conversion. |
| 124–135 | Propagation snapshot flags and current domain | Snapshot indices remain valid after deletion; eliminated entries are skipped. A retained other-symbol unit has lost the pivot, so restriction exits on missing match. |
| 147–167 | Scalar match into a changing clause, local indexed replacement, actual entry replacement | Scalar name supplied by every caller; NA match returns before use. Local indexed work precedes callbacks. Empty ranges take literal deletion before actual storage. |
| 172–188 | Restriction inverse, initialized reverse count column, named source matrix column | Matrix-NULL/future guards; registry membership plus non-NA pair mask. Reverse loop has no callbacks. |
| 194–222 | Frozen-column match, matrix vector mask, saved row list, current named ranges and count decrement | Current symbol belongs to birth columns. `which` removes NA candidates. Targets and source are rechecked around callbacks; a TRUE-bit recheck guards each decrement. Current absent list ranges are defined NULL. |
| 231 | Range-update callback's matrix index and symbol | Reached only after matrix guards and with active source; saved symbol remains a frozen column name. |
| 237–251 | Literal removal by name, registry deletion, singleton name lookup | Restriction checked actual presence without an intervening callback. Unit branch has exactly one remaining name and removes both registrations before registration recurses. |
| 257–272 | Deleted-symbol matrix column, changed-row selection, inverse of removed-symbol registry snapshot | Matrix/future guards; deleted column remains physically present. Unit-only `sr` reassignment returns before this branch. Non-NA mask excludes future and diagonal pairs. |
| 276–288 | Changed-row actual flags and count threshold | Fixed index maps; initial vector filter plus a fresh activity guard on each iteration. Source rechecked after each callback. |
| 298–314 | Scalar count and exceptional-name selection | Every caller supplies an initialized distinct live pair. Count equals logical row sum at this inference boundary. Zero/two/large cases branch away, leaving exactly one TRUE column. |
| 322–333 | Target/donor entry lookup, scalar pivot, enable matrix | Fixed initialized indices; saved name belongs to the matrix's frozen columns. Actual absent target name is handled by restriction's match guard. Enabled flag and source checks precede second-order work. |
| 348–365 | Range-change column, candidate vector masks, enable matrix snapshots | Frozen column exists. `which` uses activity and non-NA masks. A handler rechecks each target and an inactive source stops the outer loop. |
| 374–383 | Oneend frozen column, symbol registry map, `twoends` vector slice | Symbol is scalar and in the source matrix's frozen names. Registry inverse is defined. The NA-producing logical slice is an intentional ordinary `[` operation; `!is.na(twoends)` filters before consumption. |
| 386–402 | Count-one bit, twoend candidate flags/count, target-name selection | Existing scalar positions; count/bit guard rechecked after recursive trials. Count two supplies two TRUE names, and explicit `length(symbol_target) == 1` plus target membership gates the trial. |
| 408–427 | Twoend count, two saved exceptional columns, both orientations, candidate slice | Entry count equals two. Saved column/name vectors have exactly two elements. NA-producing candidate slices are filtered at 425. Source's count and both exceptional bits are checked again. |
| 430–444 | Candidate entries and `match(..., nomatch = 0L)` column selector | Candidate is in the fixed index space and rechecked active. Match returns two in-range positions/zeros; zero is legal in `[`. `sum` of a zero/one/two-element slice is scalar. Target membership is rechecked. |
| 453–467 | Target matrix's two donor rows and scalar pivot column; named literal ranges | Both rows are valid fixed indices and target pivot is present. Named absent donor/intersection ranges are defined NULL; `all`/`any` are scalar. |
| 473–477 | Elimination flags, clause names, registry removal | Call graph admits only live non-units; no recursive callback inside the helper. Every actual index and name is already valid; empty registry bindings are permitted. |
| 485–491 | Initial unit queue and final unit-only selector | `which` produces unique indices; all-unit equality also covers the empty conjunction. No nonempty sequence is constructed in that case. |
| 499–520 | Preprocessing interval, symbol snapshot, current range, registry append | Non-all-unit guard establishes non-descending `seq.int`. Call return flags break on elimination/conversion. Current names are reread before insertion. The only numeric append is exactly `length(sr_entry) + 1`. |
| 530–548 | Fixed inverse, list length, count/enable matrix dimensions | `available` is a unique actual-index vector; dimensions are explicitly `n` by `n`, including `n=0`. No callback between setup statements. |
| 551–575 | Outer/inner positions, matrix allocation and diagonal row | `seq_along`/`seq_len` avoid descending sequences. Each allocated matrix is `n` by at least two. Activity guards exclude previously skipped slots. |
| 580–610 | Common-symbol mapping and both matrix/count directions | `nomatch=0` map is filtered before positional use. Saved clauses do not change during the direct comparison loop. Frozen matrix columns contain their current names. Both sums are initialized before callbacks. |
| 612–646 | Pair callback coordinates, disabled-row/column slices, `which(..., arr.ind=TRUE)` queue | Initialized distinct pair invariants and fresh activity checks. Elementwise comparison preserves the count matrix dimensions. The queue has exactly two columns even when empty. |
| 657–659 | Descending clause-length ordering and non-unit/unit split | Exactly one live unit per domain binding before HLA. Thus split length is nonnegative, non-units precede singletons, and the unit sequence is guarded by positive unit count. |
| 663–673 | HLA target/inverse, other-donor slice, local count vector | Every live non-unit belongs to `available`; current target removed only from the local donor vector. The count slice has exactly that donor length, including zero/one. |
| 686–693 | Non-unit HLA donor match and pivot, target/donor/universe range lookups | Explicit scalar-NA guard; local count/row sum equality gives one pivot name. Universe contains every frozen column name. Absent named target literal is defined NULL. |
| 705–712 | Registry meta-indices, matrix update, unguarded local donor match | Registry lists current non-units. The current target's self row is FALSE and blocks its missing self-donor match. Every other registry index occurs once in `remaining_other_entries`. |
| 714–722 | Current-target elimination, used-donor flag and virtual assignment | Only the current target is deleted and the loop immediately stops; donor positions and names remain valid otherwise. |
| 730–741 | Delayed inverse, scalar unit target, unit-HLA count/list/vector allocations | Non-unit survivor list and registry are fixed from now on. Actual unit is a singleton; each new vector/list has one slot per surviving non-unit. |
| 745–755 | Unit-HLA donor match, lazy row, exceptional name and virtual ranges | Explicit match-NA guard. An unallocated row has its unchanged initial count; every decrement allocates first. Allocated row sums track counts, so a selected count-one row supplies one name. |
| 763–774 | Registry-to-lazy inverse, lazy row's named pivot, local count decrement | Every registry index occurs once in the immutable survivor list. Named row comes from unchanged donor names, including the registry pivot. Guarded TRUE-to-FALSE transition decrements one count. |
| 775–788 | Unit elimination flags, used-donor flags, return selector | Only current unit is changed. No subsequent pair helper consumes the overwritten vector-valued `not_subset_count`. All actual flags remain defined scalars. |

For nested replacement expressions, the harness checks each ancestor lookup
and the final write separately. Ordinary absent named-list/environment lookup
is permitted; recursive multi-element `[[` is rejected because no original
kernel operation intends that R feature. Matrix coordinate/rank checks are
performed before access. Numeric source selectors are nonnegative; zero is
permitted for `[` and scalar `[[` requires a positive in-range position.
