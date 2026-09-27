# Unicode scalar validation across R versions

The initial text-normalization proposal used `validEnc()`. Independent review
found two concrete gaps and the final helper closes both.

1. Under the C character locale, unmarked byte `FF` can pass `validEnc()` and
   be converted to literal ASCII `<ff>` by `enc2utf8()`. Strict native
   `iconv(..., sub=NA)` validation must precede conversion.
2. On R 3.6.3, marked UTF-8 sequences `ED A0 80`, `ED BF BF`, and `F4 90 80 80`
   pass `validEnc()` even though they encode the surrogate endpoints U+D800
   and U+DFFF, and out-of-range U+110000. Before the decoder check, public
   `CnfSymbol()` and `CnfAtom()` construction accepted all three as domain/range
   values. See [the recorded reproduction](../../cnf_fixes/scalar_encoding_before.log).

`utf8ToInt()` rejects those three sequences by throwing on R 3.6.3 and by
returning NA on R 4.6.1. Either behavior rejects the constructor input. The
final helper checks decoded missing values and an explicit Unicode upper
bound after conversion, while retaining the decoder's own errors. Ordinary
values immediately below/above the surrogate interval and at U+10FFFF remain
accepted on both tested runtimes.

The upper-bound comparison also matters for the package's R >= 3.3 support
declaration. Independent review of the official
[R 3.3.0 decoder source](https://svn.r-project.org/R/tags/R-3-3-0/src/main/raw.c)
and [UTF-8 validator](https://svn.r-project.org/R/tags/R-3-3-0/src/main/valid_utf8.h)
found that the four-byte decoder lacks an upper-bound check. On a signed-char
build, the validator's second-byte comparison can also miss `F4 90 80 80`.
Checking only for NA would therefore not establish the range contract on that
version. The [R 3.6.3 decoder](https://svn.r-project.org/R/tags/R-3-6-3/src/main/raw.c)
contains the later bound. This R 3.3 conclusion is a source review, not a
claim of an executed R 3.3 test run.

One compatibility limit of using the runtime decoder is explicit: the R 3.3
decoder rejects the valid Unicode noncharacters U+FFFE and U+FFFF. Those values
are consequently rejected by the new input contract on that runtime. They
are accepted by the tested R 3.6/R 4.6 decoders. The implementation uses the
runtime's Unicode conversion facilities and does not add a custom UTF-8 parser.

The complete final input regression file passes on R 3.6.3 under the
[recorded assertion shim](run_inputs_r36.R), and in the R 4.6.1 devtools run.
See [final_inputs_r36.log](final_inputs_r36.log) and the
[full CNF result](../../cnf_fixes/all_cnf_r46.log). The shim executes the test
file verbatim and checks its source hashes before and after execution; it is
not a testthat installation.
