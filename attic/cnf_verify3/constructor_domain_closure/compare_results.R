source("attic/cnf_verify3/constructor_domain_closure/harness.R")
read_result = function(stem, version) readRDS(file.path(closure_here, paste0(stem, "_", version, ".rds")))
plain36 = read_result("composition", "r36")
plain46 = read_result("composition", "r46")
gate36 = read_result("composition_gate", "r36")
gate46 = read_result("composition_gate", "r46")
stopifnot(identical(plain36, gate36), identical(plain46, gate46))
different = which(!vapply(seq_along(plain36), function(i) identical(plain36[[i]], plain46[[i]]), logical(1L)))
# R 3.6 reports the implicit class of a raw matrix as "matrix"; R 4.6 reports
# c("matrix", "array"). Check this precise base-R metadata difference instead
# of weakening the comparison with all.equal or ignoring actual range values.
stopifnot(length(different) == 182L)
for (i in different) {
  stopifnot(identical(plain36[[i]]$payload, plain46[[i]]$payload),
    identical(plain36[[i]]$class, "matrix"),
    identical(plain46[[i]]$class, c("matrix", "array")))
}
primitive36 = read_json(file.path(closure_here, "primitives_r36.json"))
primitive46 = read_json(file.path(closure_here, "primitives_r46.json"))
stopifnot(identical(primitive36$counts, primitive46$counts),
  identical(primitive36$source_md5, primitive46$source_md5))
cat("Exact cross-version payload agreement:", length(plain36), "saved composition results.\n")
cat("There are", length(different), "precisely checked implicit raw-matrix class metadata differences.\n")
cat("The diagnostic kernel gate leaves every saved result exactly unchanged on each runtime.\n")
cat("Primitive counts and source hashes agree across runtimes.\n")
