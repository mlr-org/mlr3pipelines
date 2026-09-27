# CNF fixes and remaining open bugs

Status: **2026-09-07**, implementation commits **`7f4ffb71`** and **`9fb17923`**. The numbers below preserve the user's 19-item open-bug
list. They are not the older experiment numbers or section numbers in the
investigation diaries.

This follow-up implements the requested fixes for **1, 2, 7, 8, 11, 12, 13,
14, 15, and 16**, followed by **19** through shared operator registrations.
The remaining open bugs are **3, 4, 5, 6, 9, 10, 17, and 18**.
The historical investigation used production baseline `09770eaa`
and ended at `d5ccd1be`; its deliberately failing reproductions describe that
baseline. Use the package regression tests for the repaired behavior.

## Fixes implemented

| Number | Defect | Repair |
| --- | --- | --- |
| 1 | Unicode symbol names can lose their identity when R converts environment binding names to the native character locale; simplification can then change truth. | Normalize names to UTF-8 and require an exact native-locale round trip before insertion or `$` lookup. Unsupported names fail before insertion. Keep `LC_CTYPE` unchanged while a universe is in use. |
| 2 | Matrix/array clause selectors can retain duplicate symbols and cause incorrect simplification or a crash. | Reject dimensional selectors before deduplication; ordinary repeated vector indices still select each symbol once. |
| 7 | Universe-free constants make Clause/Formula constructor composition depend on their position. | Determine and validate the common nonconstant universe before processing constants. Constants adopt that universe. |
| 8 | FALSE encountered after nested formulas is concatenated with previously collected clauses and sent to the kernel in malformed form. | Return a FALSE `CnfFormula` immediately after constructor validation. |
| 11 | `TRUE | clause` can return a bare logical. | Coerce the early-return operand with `as.CnfClause()`. |
| 12 | Missing selectors can create an NA symbol and NULL range. | Reject missing indices of every accepted atomic type before the alternative selector validators run. |
| 13 | Byte-marked or invalid text can fail ordinary character operations or be silently replaced during conversion. | Reject bytes and invalid native text before UTF-8 conversion, then validate Unicode scalar encodings. Apply the same text contract to names, domains, atom values, and `$` lookup names. |
| 14 | Equivalent UTF-8/Latin-1 text produces different serialized clause-order keys and false comparison differences. | Normalize constructor text and comparison names/ranges to UTF-8 before hashing. Comparisons also handle proper older stored objects that retain Latin-1 marks. |
| 15 | Locale collation ties make ordering depend on input order, including delegated universe comparison. | Use radix ordering for normalized values, symbol names, and clause keys. Add `all.equal.CnfUniverse()` to compare binding maps in the same deterministic name order. |
| 16 | Dimensional atom values retain repeated scalar values and compare unequal to the same ordinary set. | Reject matrices/arrays and classed character inputs at creation. Proper ordinary vectors retain scalar deduplication. |
| 19 | Mixed CNF classes select different binary operator methods and fail on R < 4.3. | Register one shared handler per operator for all three classes, using roxygen2 `@rawNamespace` tags. The generated registrations point directly to `cnf_and`/`cnf_or`; class-specific wrappers and `chooseOpsMethod()` hooks are removed. R >= 3.3 is retained. |

The changes are confined to the five CNF representation files, their namespace
registration and documentation, and focused regression tests. The simplifier
kernel `R/CnfFormula_simplify.R` is unchanged.

## Contract decisions

- Unicode is supported where symbol names can be represented faithfully in
  native environment bindings. This is not an ASCII-only restriction. Domain
  and atom values can still contain marked Unicode text under `LC_CTYPE=C`,
  because these values are not environment binding names.
- UTF-8 conversion preserves distinct code-point sequences. In particular,
  precomposed and decomposed Unicode values remain distinct; no NFC/NFD
  normalization is performed.
- Names and nonempty domain/value inputs are ordinary character vectors
  without dimensions or custom classes. Named ordinary vectors remain
  accepted. Domain repetitions and domain order remain unchanged. Atom ranges
  still remove repeated scalar values. Existing unclassed empty atom inputs,
  including `NULL`, still represent FALSE.
- All nonconstant constructor inputs must share one identical universe, even
  if a constant would otherwise short-circuit the result. Constant-only lists
  retain the first available universe, or NULL. Constants with a different
  owner do not override the universe of the nonconstant inputs.
- `all.equal()` remains structural comparison, with order-insensitive proper
  clause/range content. It is not a decision procedure for arbitrary logical
  equivalence. Domain-vector order remains significant. Existing logical/type
  guards, `check.attributes`, `all.names`, and runtime-specific
  `evaluate=FALSE` behavior are preserved.

## Remaining open bugs

### Incomplete scheduling of implemented simplification rules

These four defects preserve the formula's truth function in the saved
examples, but leave a specific implemented rule applicable on return.

| Number | Trigger and consequence | Reproduction and next useful reference |
| --- | --- | --- |
| 3 | An SSE1 donor range shrinks while its exceptional comparison remains TRUE. The target is not revisited, leaving an available first-order restriction. | [`minimized_first_order_phase_sse1.json`](../cnf_verify3/independent_solver/minimized_first_order_phase_sse1.json); [arbitrarily wide four-clause family](../cnf_verify3/wide_four_boundary/PROOF.md). |
| 4 | A target shrink raises a reverse donor count from one to two after the SSE2 queue has been built. The newly eligible pair is not queued. | [`minimized_sse2.json`](../cnf_verify3/independent_solver/minimized_sse2.json). |
| 5 | An already-contained donor range shrinks or its literal disappears without a comparison flip. SSE2 does not reconsider the smaller union. | [`directed_oneend_shrink_min.json`](../cnf_verify3/independent_solver/directed_oneend_shrink_min.json), [`minimized_oneend_symbol_removal.json`](../cnf_verify3/independent_solver/minimized_oneend_symbol_removal.json). |
| 6 | During nested unit registration, a stale outgoing TRUE comparison is mistaken for strict containment. Equality is skipped and a clause directly subsumed by a retained unit survives. | [`minimized_subsumption.json`](../cnf_verify3/independent_solver/minimized_subsumption.json). This is distinct from the unit-merge defect fixed before campaign 3. |

Run `Rscript attic/cnf_verify3/root/replay_findings.R` from the repository root
for small public-constructor replays and independent valuation checks. The
post-fix run still shows the pending restrictions and preserves every input
truth table; see [open_scheduling_r36.log](open_scheduling_r36.log).
The [candidate scheduling changes](../cnf_verify3/independent_solver/CANDIDATE_REPAIRS.md)
and [independent saturation review](../cnf_verify3/scheduler_review/REVIEW.md)
are the next starting points. A fixed number of repeated passes is not a
general repair: the [saved family](../cnf_verify3/repeated_passes/PROOF.md)
requires arbitrarily many productive passes as the input grows.

### Clause-to-list conversion

| Number | Remaining behavior | Evidence |
| --- | --- | --- |
| 9 | `as.list(as.CnfClause(TRUE))` calls an unsupported `as.CnfAtom()` conversion and errors. The return should also consistently have list shape. | [Original reproduction and cause](../cnf/CLAUDE.md#bug-1-aslistcnfclause-crashes-on-true-clauses). |
| 10 | `as.list()` on a proper clause drops the documented symbol names, preventing the documented named-list access/commutation. | [Independent earlier review](../cnf/review_5_2_pro.md), [representation review](../cnf_verify3/representation/NOTES.md). |

### Runtime depth

| Number | Remaining behavior | Evidence |
| --- | --- | --- |
| 17 | Reversed implication chains create linearly nested unit propagation and exhaust the R/C stack. | [Unit-chain source recurrence and queue experiment](../cnf_verify3/unit_queue/README.md). Recorded failures were around 256 symbols on R 3.6 and 1,024 on R 4.6; thresholds depend on the environment. |
| 18 | Guarded implication chains whose clauses remain non-unit exhaust the stack through restriction/subset-update callbacks. Fixing only unit propagation does not address this path. | [Separate non-unit family](../cnf_verify3/unit_queue/README.md#6-independent-nonunit-recursion-family). |

The kernel has not changed in this follow-up, so the recorded recursion
limitations remain.
The two clause-to-list methods are likewise unchanged.
The R 3.6 public-call recheck of bugs 9, 10 and 19 before the operator fix is
recorded in [open_api_r36.log](open_api_r36.log). Bug 19 is now closed; see the
[shared-method implementation and compiled-package checks](ops_dispatch/README.md).

## Regression and review record

New package tests:

- [`test_CnfInputs.R`](../../tests/testthat/test_CnfInputs.R): valid/invalid
  character encodings, native binding identity, lookup, shape restrictions,
  empty selections, and Unicode semantic controls.
- [`test_CnfSelectors.R`](../../tests/testthat/test_CnfSelectors.R): dimensional
  and missing selectors, ordinary subsetting, and the repaired contradictory
  four-clause example with ordinary repeated indices.
- [`test_CnfConstructors.R`](../../tests/testthat/test_CnfConstructors.R): constant
  positions and owners, nested FALSE, universe validation, and result coercion.
- [`test_CnfComparisons.R`](../../tests/testthat/test_CnfComparisons.R): encoding
  aliases, stored legacy names/ranges, collation ties, unequal controls,
  universe association, and comparison options.

The root and three independent agent streams divided constructor composition,
input contracts, comparison design, and final cross-review. Initial regression
runs reproduced the failures before the corresponding fixes. Records include
[constructor runs](../cnf_verify3/constructor_fixes/),
[input runs](../cnf_verify3/input_contract_fixes/),
[selector baseline](selectors_before.log), and
[comparison baseline](comparisons_before.log).

Two extra input-validation mistakes were caught during review:

1. In the C character locale, `validEnc()` can accept unmarked invalid bytes,
   which `enc2utf8()` replaces with printable ASCII escapes. Native text is
   now checked with strict `iconv(..., sub=NA)` before conversion.
2. R 3.6's `validEnc()` accepts surrogate/out-of-range marked UTF-8 sequences.
   Public construction reproduced this for U+D800, U+DFFF and U+110000.
   The final decoder check rejects errors or missing code points, and also
   checks the Unicode upper bound for older decoders. Valid scalar-boundary
   controls remain accepted. See [the pre-fix record](scalar_encoding_before.log)
   and the source review in the input-fix directory.

Final package validation used R 4.6.1:

```sh
bash attic/cnf_verify3/review_semantics/run_r46.sh -e 'devtools::test(filter = "^Cnf", stop_on_failure = TRUE)'
```

All **4,255 expectations passed**, with **zero failures, warnings or skips**.
This includes all existing CNF tests and the four new regression files; it is
not a whole-package test result. The [final console record](all_cnf_r46.log)
omits transient progress-spinner lines. The first integration run passed
4,218 expectations; subsequent review added 11 comparison controls and 26
scalar-encoding controls, followed by the complete final CNF rerun above.

The final input test file also passed **128 assertions in nine tests** on
R 3.6.3, with zero warnings/skips, using real production CNF functions and
checkmate with a small explicitly labelled assertion shim. The host lacks
testthat/devtools. The [runner](../cnf_verify3/input_contract_fixes/run_inputs_r36.R)
and [result with source/test hashes](../cnf_verify3/input_contract_fixes/final_inputs_r36.log)
make this separate compatibility check reproducible.

The old-decoder source findings and their limits are recorded in
[SCALAR_VALIDATION.md](../cnf_verify3/input_contract_fixes/SCALAR_VALIDATION.md).
Documentation was regenerated with roxygen2 8.1.0; it reported pre-existing
missing-Suggests cross-reference warnings. Only the CNF documentation and new
S3 registration were retained; unrelated generated changes were discarded.

The later operator repair has its own [validation record](ops_dispatch/README.md),
including the new `test_CnfOperators.R` file and actual installed, byte-compiled
CNF namespace checks on both R 3.6.3 and R 4.6.1. The initial 4,255-expectation
record above predates this separate fix.

## Evidence boundaries

The earlier source proofs retain their stated kernel assumptions because the
kernel did not change. Rejecting unsafe public representations closes the
identified routes into invalid kernel inputs; it does not turn the heuristic
into a complete SAT solver or a canonical logical-equivalence checker.
The unchanged kernel's SHA-256 is
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.

Missing `universe[[name]]` returning NULL, ordinary domain repetitions, domain
order sensitivity, alternative equivalent fixed points, and hypothetical hash
collisions are not additional confirmed open bugs. Older diaries sometimes
listed these more broadly; the current eight-item list above is the supported
open-bug inventory.
