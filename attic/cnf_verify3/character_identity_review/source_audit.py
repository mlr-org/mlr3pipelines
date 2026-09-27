"""Record hashes and relevant source locations from exact official R tags.

The source text is fetched only for inspection; no full source copies are kept.
"""
import hashlib
import json
from pathlib import Path
from urllib.request import urlopen

audit = Path(__file__).parent
needles = {
    "sysutils.c": ["SEXP installTrChar(", "const char *translateChar(", "<U+%04X>", "<%02x>", "unable to translate"],
    "memory.c": ["int Seql("],
    "subscript.c": ["ss = translateChar(", "streql(translateChar(STRING_ELT(names, i)), ss)"],
    "subset.c": ["findVarInFrame(x, installTrChar"],
    "subassign.c": ["defineVar(installTrChar"],
    "envir.c": ["name = installTrChar(STRING_ELT(CAR(args), 0))", "t1 = installTrChar(STRING_ELT(CAR(args), 0))", "static void FrameNames("],
    "unique.c": ["return Seql(xi, yj)", "translateCharUTF8(STRING_ELT(x, indx))"],
    "relop.c": ["Seql(c1, c2)"],
    "names.c": ["SEXP install(const char *name)", "strcmp(name, CHAR(PRINTNAME(CAR(sym))))"],
}
records = []
for tag in ("R-3-6-3", "R-4-6-1"):
    for file, patterns in needles.items():
        url = f"https://svn.r-project.org/R/tags/{tag}/src/main/{file}"
        source = urlopen(url, timeout=30).read()
        lines = source.decode().splitlines()
        records.append({"tag": tag, "file": file, "url": url,
                        "sha256": hashlib.sha256(source).hexdigest(),
                        "locations": {pattern: [i + 1 for i, line in enumerate(lines) if pattern in line]
                                      for pattern in patterns}})
(audit / "r_source_manifest.json").write_text(json.dumps(records, indent=2) + "\n")
cnf_sources = {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(Path("R").glob("Cnf*.R"))}
(audit / "production_hashes.json").write_text(json.dumps(cnf_sources, indent=2) + "\n")
print(f"Recorded {len(records)} official R source hashes and {len(cnf_sources)} unchanged CNF source hashes.")
