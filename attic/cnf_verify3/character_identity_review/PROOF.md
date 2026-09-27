# Encoding identity and collation independence: the exact positive contract

Source review dated 2026-09-06. Production `R/CnfFormula_simplify.R` SHA-256:
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
The authoritative complete hash is also recorded in `production_hashes.json`.
This is a direct source-prefix simulation argument, not a formalization of
the R interpreter or a theorem inferred from successful tests.

**The unrestricted accepted-input claim is false.** See `FINDING.md` for
the canonical-shape public counterexample with a valid non-ASCII symbol
name and C CTYPE. The proof below states the additional identity premise
precisely and distinguishes it from R's ordinary value equality.

## 1. Strings, names, and two relevant identity maps

For a valid ordinary nonmissing string `x`, let `Q(x)` be its finite Unicode
code-point sequence after interpreting its declared UTF-8/Latin-1 mark or
its actual native encoding. No Unicode normalization is applied.

Encoding-equivalent strings have equal `Q`. UTF-8 bytes `C3 A9`, Latin-1
byte `E9`, and a correctly encoded native representation of precomposed
e acute are examples. Precomposed `U+00E9` and decomposed `U+0065 U+0301`
have **different Q**, even if a collation locale gives them equal weights.

For a symbol name at a fixed native-encoding configuration, let `N(x)`
be the native byte string installed in R's symbol table by `installTrChar`.
Unlike membership, R environment indexing and ordinary list `[[` exact
name indexing use this native-translation route in both reviewed R versions.

The required **name-identity premise** is:

1. Every symbol name is nonempty and translates faithfully to native
   encoding without substitution. Every name remains a valid string.
2. `N(x)=N(y)` iff `Q(x)=Q(y)` for all participating symbol spellings.
3. A registry binding created for `x` can be read by every participating
   spelling of that same Q-name, and its enumerated name has Q equal to
   `Q(x)`. The universe was created with a compatible native configuration;
   native encoding does not change incompatibly while it remains in use.

Ordinary valid UTF-8 names in a UTF-8 native locale satisfy these conditions,
including genuinely distinct precomposed/decomposed spellings. ASCII names
also satisfy them in C. Native-translatability is needed for **names**;
ordinary marked values need only the Q-equality behavior of the value
primitives. Non-ASCII marked values can therefore be used with ASCII names
in C CTYPE, as the separate value controls demonstrate.

R's [match documentation](https://stat.ethz.ch/R-manual/R-patched/library/base/html/match.html)
describes agreement after UTF-8 translation for ordinary marked strings,
and the shared equality definition of `match`, `unique`, and `duplicated`.
The [encoding documentation](https://stat.ethz.ch/R-manual/R-patched/library/base/html/Encoding.html)
distinguishes declared encodings from native strings and warns about
changing encodings during a running session. The implementation-level
native-identity distinction above is from the exact-version source audit
in Section 6, not an assumption that every character primitive uses `match`.

## 2. The paired-execution contract

Take two executions of the unchanged constructors and simplifier in one
fixed R implementation, each with its own single unchanged universe.
They may share the same universe when native configuration permits it.
Keep these features fixed:

* The universe's ordered scalar domain contents, the input clause order,
  each clause's ordered symbol sequence, and each literal's value sequence,
  all interpreted through Q. Every actual clause has distinct nonmissing
  registered names; every actual literal is a unique nonempty proper
  character subset of its domain. The canonical domain vectors are finite,
  nonempty, ordinary, unnamed, and duplicate-free.
* The public construction operation sequence, including constant cases,
  repeated atom occurrences, and list nesting. The compared calls refer
  only to their corresponding unchanged universe environments.
* Every scalar string occurrence may independently choose an equivalent
  valid UTF-8, Latin-1, or actual native representation. Non-Latin-1 strings
  must retain a representation that actually expresses their Q sequence.
* `%in%`, `match`, `unique`, and `==`/`!=` agree with Q equality on all
  participating strings; their ordinary vector operations have no custom
  dispatch. The name-identity premise in Section 1 holds throughout.

The two collation settings may differ arbitrarily; they do not enter the
kernel's value/name decisions. Changing CTYPE is covered only when the
stated value and native-key premises continue to hold. Keeping CTYPE in
C.UTF-8/en_US.UTF-8 while changing COLLATE between C and the tested UTF-8
locales is a measured instance. Equivalence is not asserted for changing
an existing environment's native encoding incompatibly.

Byte-marked strings, invalid encodings, missing names/values, zero-length
symbol names, native substitutions, malformed selector-produced clauses,
active bindings, external mutation, modified primitive bindings, and custom
methods that alter the stated operations are excluded. Sufficient memory,
stack, vector/index capacity, and finite supported arithmetic are assumed
in both runs. This compares source decisions, not resource usage or timing.

**Theorem.** Every prefix of the two simplifier executions corresponds:
the same helpers enter, the same scalar and short-circuit decisions occur,
the same clause/symbol indices are traversed, and the same recursive
continuations resume. All actual and virtual ranges have exactly the same
Q-valued sequence, with the same length and order. The returned ordered
payloads are exactly equal after elementwise Q interpretation, preserving
clause order, symbol order, range order and multiplicities. Constants agree.
Each output keeps its own unchanged input universe reference.

This is stronger than agreement of unordered literal sets. It deliberately
does not claim equal encoding marks or serialized hashes, identical fresh
universe environments, a unique normal form, idempotence, completeness,
or correctness of any `all.equal.Cnf*` method.

## 3. Public constructor prefix

`CnfSymbol` validates a scalar name and a nonempty character domain, then
uses `exists` and `assign`. Under the native-key premise it accepts/rejects
the same registration and installs the corresponding Q-domain. Universe
symbol retrieval uses the same identity. A repeated spelling with an
equivalent encoding consequently cannot create a second ordinary name.

`CnfAtom` retrieves that domain, checks subset membership, tests complete
coverage using `%in%`, and otherwise calls `unique(values)`. Its acceptance,
constant classification, and first-occurrence Q-valued sequence are the
same. Real checkmate is used in all executable checks; no assertion shim
replaces its decisions.

`CnfClause` iterates the same atom/inner-clause sequence. Its `[[name]]`
lookup and replacement locate the same position by Section 1. `c` preserves
the same combined Q sequence, `unique` retains corresponding first
occurrences, and full-domain `%in%` has the same truth value. Thus repeated
atoms merge identically and proper constructor output satisfies the same
canonical clause boundary. Universe checks compare references, which are
the same within each construction; no cross-universe identity is assumed.

For ordinary flat character selectors, the selector's `unique` and subset
check have the same result and exact named indexing selects the same
positions. This does not extend to the known matrix/NA selector defects.
`as.list` and reconstruction use the corresponding names and ranges.
Atom complement uses ordinary `setdiff`, so it also respects Q.

`CnfFormula` sees the same constant/proper classifications, appends the
same ordered raw clauses, and calls `simplify_cnf` on related inputs. Its
constructor and simplifier do not call any CNF comparison helper. This
proof couples existing constructor behavior, including any unrelated
constant/universe limitation, rather than claiming all accepted operations
have already been proved semantically correct.

## 4. Simplifier prefix induction

At corresponding source positions relate the state as follows:

* Actual entries, unit-domain ranges, local snapshots, restringents, donor
  unions, virtual HLA ranges and saved string variables have identical
  Q-valued sequences and corresponding attributes.
* Clause positions, symbol positions, lengths, indices, flags, Boolean
  matrices, counters, queues, available-index maps, and saved loop bounds
  are identical. Matrix column names correspond by Q in their original order.
* The three private registries contain the same Q-keys and corresponding
  values. Registry enumeration order need not agree.
* The call stack, paused callbacks, saved snapshots, and source promises
  correspond. Stale cached state, if present, is stale in both runs.

The relation holds at entry by Section 3. Each source operation preserves it:

1. `char_intersect`, `char_setdiff`, `char_union`, direct unit intersection,
   and every membership-based inclusion/disjointness expression choose
   exactly corresponding positions. Since this is an encoding change, no
   scalar cardinality changes. Every range-length comparison, including
   both HLA full-domain tests, has exactly equal integer operands. Unlike
   a many-to-one domain lift, no weighted-cardinality argument is needed.
2. Exact list indexing, matrix name indexing, `match` on column names,
   environment get/set, and string `==`/`!=` resolve the same Q-symbols.
   Environment assignment updates the corresponding binding. No collision
   or split is permitted by the name-identity premise.
3. The only environment-name enumeration affecting propagation is line
   502: the left operand of `char_intersect` is the ordered clause names;
   environment names are only a membership table. Its result therefore
   follows the same clause positions regardless of hash/enumeration order.
   `length(unit_domains)` counts corresponding bindings.
4. The only sorts are numeric `order(lengths(entries))` at line 48 and
   numeric descending clause-width ordering at line 657. Their complete
   numeric input vectors are identical. There is no value or symbol-name
   lexical sort, digest key, or collation-based comparison in the kernel.
5. Every other condition, integer `match`, `which`, row sum, and index
   update has identical arguments. Thus branches, evaluated `&&`/`||`
   operands, first selected HLA donor, loop snapshots, and delayed index
   matches agree. All explicit loops traverse clauses, symbol names,
   structural indices, or the same two second-order orientations.

Consequently each helper call, mutation and return preserves the relation.
This works for every finite execution prefix, including nested unit births
and callbacks that reenter unfinished work. It does not assume that every
stored subset bit is currently mathematically exact: coupling the same
bits and snapshots is enough. Existing canonical scheduling gaps are
preserved, rather than disproved, by the simulation.

The final eliminated mask selects identical ordered clause positions.
The range relation yields the exact normalized output claim. Assignments
identified by Q have corresponding input/output truth in the two runs.
The symmetry argument transfers a separate semantic proof where its
premises hold; it does not establish semantic correctness of arbitrary
malformed input by itself.

## 5. Relation to set symmetry and domain storage

`../set_symmetry/PROOF.md` already audits every scalar length, set operation,
sort, and callback for arbitrary finite full-preimage lifts. This review
rechecks the simpler one-to-one, order-preserving encoding specialization
and establishes when the previously assumed name identities are compatible
in real R. The source induction remains independent of the canonical
semantic/correctness proof stack.

`../domain_storage_contract/PROOF.md` concerns a different transformation:
domain attributes and repetitions can change virtual multiplicities while
actual ranges remain unique. An encoding-only change does not change
those multiplicities. Its native-key/equality premise must still hold; the
C CTYPE example identifies an accepted case where it does not. The
storage theorem's selected-donor/physical-containment reasoning cannot
repair missing unit propagation caused by a different key identity.

Distinct precomposed/decomposed strings remain separate symbols and values
throughout this argument. A collation tie never supplies Q equality, and
the kernel contains no operation that turns that tie into an identity.
The observed comparison-helper failures are therefore not evidence against
this conditional kernel theorem. The separate native-key counterexample is.

## 6. Exact-version R source audit

`source_audit.py` fetches official R source tags, records complete SHA-256
hashes and the source locations of relevant implementation points in
`r_source_manifest.json`, and records current production CNF hashes.
It retains no full copies of unrelated R source files.

* [R 3.6.3 memory.c](https://svn.r-project.org/R/tags/R-3-6-3/src/main/memory.c)
  line 4366 and [R 4.6.1 memory.c](https://svn.r-project.org/R/tags/R-4-6-1/src/main/memory.c)
  line 5006 implement `Seql`; the nontrivial encoding path compares
  UTF-8-translated strings. `unique.c` uses `Seql` for character equality,
  and `relop.c` uses it for `==`/`!=`; ordering is a different path.
* [R 3.6.3 sysutils.c](https://svn.r-project.org/R/tags/R-3-6-3/src/main/sysutils.c)
  lines 941/960 and [R 4.6.1 sysutils.c](https://svn.r-project.org/R/tags/R-4-6-1/src/main/sysutils.c)
  lines 1825/1869 implement `translateChar`/`installTrChar`. The latter
  installs a native key. Their conversion paths generate the diagnostic
  ASCII escapes when a character cannot be represented. R 4.6.1 preserves
  the historical installation behavior but warns on such substitution.
* [R 3.6.3 subscript.c](https://svn.r-project.org/R/tags/R-3-6-3/src/main/subscript.c)
  lines 225–228 and [R 4.6.1 subscript.c](https://svn.r-project.org/R/tags/R-4-6-1/src/main/subscript.c)
  lines 279–282 compare native translations for ordinary list exact
  `[[` name indexing. It would be incorrect to silently model this as
  `%in%` for arbitrary accepted marked names.
* `subset.c`/`subassign.c` route environment `[[` through `installTrChar`;
  `envir.c` does likewise for assignment and retrieval. `FrameNames` returns
  the installed symbol's print name. `names.c` installs native byte keys.
  Their exact source URLs, hashes and locations are in the manifest.

The executable probes and public corpus validate these premises for the
specific measured platforms/locales. The proof's unbounded claim is
conditional on the stated ordinary R operation contract, not on assuming
that this finite locale/character sample covers every possible platform.
