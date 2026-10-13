# Ordinary character identity review

The unrestricted accepted-input claim is false: a valid marked non-ASCII
symbol name under C CTYPE can make the unchanged simplifier turn an
unsatisfiable public formula into a satisfiable one. Start with
[`FINDING.md`](FINDING.md) and the minimal standalone
[`reproduce_public.R`](reproduce_public.R). All objects are public-built;
all domain values in the failure are ASCII. No universe mutation, selector
malformation, invalid string, byte mark, or CNF comparison helper is needed.

[`PROOF.md`](PROOF.md) gives the positive direct source-prefix simulation
with the necessary native-key identity contract. Equivalent valid
UTF-8/Latin-1/native spellings and COLLATE changes preserve the kernel's
ordered payload and source decisions when native name translation remains
faithful. Precomposed and decomposed Unicode spellings are kept distinct.

## Evidence and counts

All checks used real checkmate 2.3.4, native R 3.6.3, and the existing
`cnf-review-r46` R 4.6.1 environment. Production source hashes are saved in
`production_hashes.json`; no production file was edited and no commit was
made by this stream. `all.equal.Cnf*` is never an oracle or test predicate.

| Check | Per R version | Result |
| --- | --- | --- |
| Minimal public semantic failure | UTF-8/C CTYPE controls | 0 input models becomes 2 output models only in C |
| Detailed failure trace | UTF-8 and Latin-1 name marks × two CTYPE locales | Both marks fail in C; both controls return FALSE |
| Base primitive alias checks | 66 encoding/locale pairs | All 54 UTF-8-native pairs pass; 12 C-native pairs violate name identity |
| Public alias construction | C and C.UTF-8 | C accepts duplicate ordinary names and passes them into the kernel |
| Main positive corpus | 20 cases × 96 configurations = 1,920 formulas | Exact normalized inputs, outputs, truth and source traces agree |
| Main semantic evaluation | 122,880 assignments | All input/output truth agrees |
| Main source observation | 1,992,576 events, 287/290 sites | All 13 local helpers observed; encoding-sensitive negative control detected |
| Values-only locale control | 160 formulas, 10,240 assignments, 166,048 source events | Non-ASCII values with ASCII names agree in C and C.UTF-8 |

The main 96 configurations are two UTF-8 native locales
(`C.UTF-8`, `en_US.UTF-8`), three collation locales (`C`, `C.UTF-8`,
`en_US.UTF-8`), four domain encoding profiles, and four literal/name encoding
profiles. Profiles are marked UTF-8, Latin-1 wherever representable,
unmarked correctly encoded native UTF-8, and an occurrence-varying mixture.
The decomposed string cannot be encoded in Latin-1 and remains marked
UTF-8 in that profile; no substitution is used. The corpus contains 12
directed integer-ID specifications and eight independently generated
seven-clause specifications using seed `260906`.

Universes are created once per domain profile and remain unchanged across
all literal profiles, all corpus cases, and every collation locale. The
literal/registry spellings can vary independently. A positional code-point
normalizer preserves all clause, symbol and range order; it does not sort
or deduplicate. The independent truth oracle assigns integer domain IDs
and evaluates source specifications without CNF methods. Semantic negative
controls distinguish different ranges and swapped symbol associations.

`harness.R` builds a private observed closure by walking the production AST.
It logs helper entry, every `if`, evaluated short-circuit operands, all/any
and comparison results, and complete loop sequences/iterations. The
observer's output must be identical to the unmodified public output before
its trace is used. This design is adapted from the campaign's existing
source-event technique; the corpus and string/semantic oracles are local.
It preserves callback order and does not project away branch differences.

The three unobserved source sites are retained in the saved site table;
coverage is supporting evidence, not an unbounded proof. `checks_*.rds`
contains all cases, 20 full baseline traces, site metadata/counts, per-run
records, and the observer negative control. All corresponding R 3.6.3 and
R 4.6.1 baseline payloads/traces, case specifications, per-run records and
site counts match exactly (`cross_version.json`).

## Reproduction commands

Run from repository root:

```sh
Rscript attic/cnf_verify3/character_identity_review/reproduce_public.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/character_identity_review/reproduce_public.R
```

The detailed scripts use the same invocation pattern on both runtimes:

```sh
Rscript attic/cnf_verify3/character_identity_review/primitive_checks.R
Rscript attic/cnf_verify3/character_identity_review/alias_construction.R
Rscript attic/cnf_verify3/character_identity_review/ctype_counterexample.R
Rscript attic/cnf_verify3/character_identity_review/checks.R
Rscript attic/cnf_verify3/character_identity_review/value_locale_checks.R
Rscript attic/cnf_verify3/character_identity_review/compare_versions.R
```

`value_locale_checks.R` reads the main run's saved corpus; run `checks.R`
first. Run both runtimes before `compare_versions.R`. The corresponding
`*_r36.log`/`*_r46.log`, JSON summaries and RDS records are saved here.

`source_audit.py` independently fetches exact official R source tags and
records hashes/source locations supporting the native-key analysis:

```sh
python3 attic/cnf_verify3/character_identity_review/source_audit.py
```

No package-wide test suite is run. These are standalone review experiments,
not production changes or new package unit-test infrastructure.
