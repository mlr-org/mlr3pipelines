# Identity checks, independent of CNF comparison helpers.
suppressMessages(library(checkmate))
suppressMessages(library(jsonlite))

capture_error = function(expr) tryCatch(list(value = expr), error = function(e) list(error = conditionMessage(e)))
utf8_id = function(x) unname(vapply(enc2utf8(x), function(s) paste(utf8ToInt(s), collapse = ":"), ""))
native_unmarked = function(x) {
  x = enc2native(x)
  Encoding(x) = "unknown"
  x
}
describe = function(x) list(encoding = Encoding(x), bytes = lapply(x, charToRaw), id = utf8_id(x))

original = Sys.getlocale()
original_ctype = Sys.getlocale("LC_CTYPE")
original_collate = Sys.getlocale("LC_COLLATE")
records = list()
distinct_records = list()
for (ctype in c("C.UTF-8", "en_US.UTF-8", "C")) {
  if (!nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", ctype)))) next
  for (collate in c("C", "C.UTF-8", "en_US.UTF-8")) {
    if (!nzchar(suppressWarnings(Sys.setlocale("LC_COLLATE", collate)))) next
    a = "\u00e9"
    b = iconv(a, from = "UTF-8", to = "latin1")
    variants = list(utf8 = a, latin1 = b)
    if (l10n_info()[["UTF-8"]]) variants$native = native_unmarked(a)
    # NFC and NFD are deliberately distinct keys, irrespective of collation.
    # Include ASCII lookalikes of possible translateChar() diagnostic escapes.
    distinct = c(a, "e\u0301", "<U+00E9>", "<e9>", "\u00f6", "", " ", ".", "...")
    equality = outer(distinct, distinct, `==`)
    eq_id = outer(utf8_id(distinct), utf8_id(distinct), `==`)
    stopifnot(identical(equality, eq_id), !anyDuplicated(distinct))
    for (left in names(variants)) for (right in names(variants)) {
      x = variants[[left]]
      y = variants[[right]]
      env = new.env(parent = emptyenv())
      env[[x]] = 17L
      ll = setNames(list(17L), x)
      mt = matrix(17L, 1L, 1L, dimnames = list(NULL, x))
      record = list(ctype = Sys.getlocale("LC_CTYPE"), collate = Sys.getlocale("LC_COLLATE"),
        left = left, right = right, left_string = describe(x), right_string = describe(y),
        identical = identical(x, y), equal = x == y,
        unique = length(unique(c(x, y))) == 1L, match = identical(match(y, x), 1L),
        member = y %in% x, checkmate_subset = isTRUE(checkmate::check_subset(y, x)),
        checkmate_string = isTRUE(checkmate::check_string(y)),
        environment_exists = exists(y, envir = env, inherits = FALSE),
        environment_get = identical(capture_error(get(y, envir = env, inherits = FALSE))$value, 17L),
        environment_index = identical(env[[y]], 17L),
        list_index = identical(ll[[y]], 17L), matrix_index = identical(unname(mt[1L, y]), 17L))
      ll[[y]] = 23L
      env[[y]] = 23L
      record$list_assignment = length(ll) == 1L && identical(ll[[x]], 23L)
      record$environment_assignment = length(env) == 1L && identical(env[[x]], 23L)
      record$environment_names_member = y %in% names(env)
      records[[length(records) + 1L]] = record
    }
    # Distinct strings must remain distinct in all symbol-addressing primitives.
    env = new.env(parent = emptyenv())
    keys = distinct[nzchar(distinct)]
    for (i in seq_along(keys)) env[[keys[[i]]]] = i
    ll = setNames(as.list(seq_along(keys)), keys)
    distinct_checks = list(ctype = ctype, collate = collate, env_size = length(env) == length(keys),
      env_values = identical(vapply(keys, function(k) env[[k]], 1L), setNames(seq_along(keys), keys)),
      list_values = identical(vapply(keys, function(k) ll[[k]], 1L), setNames(seq_along(keys), keys)))
    cat("DISTINCT", ctype, collate, toJSON(distinct_checks, auto_unbox = TRUE), "\n")
    distinct_records[[length(distinct_records) + 1L]] = distinct_checks
    if (!all(unlist(distinct_checks[c("env_size", "env_values", "list_values")]))) print(as.list(env))
  }
}
invisible(Sys.setlocale("LC_CTYPE", original_ctype))
invisible(Sys.setlocale("LC_COLLATE", original_collate))
flags = c("identical", "equal", "unique", "match", "member", "checkmate_subset", "checkmate_string",
  "environment_exists", "environment_get", "environment_index", "list_index", "matrix_index",
  "list_assignment", "environment_assignment", "environment_names_member")
failed = lapply(records, function(record) {
  bad = flags[!vapply(record[flags], isTRUE, FALSE)]
  if (length(bad)) list(ctype = record$ctype, collate = record$collate,
    left = record$left, right = record$right, failed = bad) else NULL
})
failed = Filter(Negate(is.null), failed)
stopifnot(length(records) == 66L, length(failed) == 12L,
  all(vapply(failed, function(record) identical(record$ctype, "C"), FALSE)))
for (record in distinct_records) {
  if (record$ctype != "C") stopifnot(record$env_size, record$env_values, record$list_values)
}
result = list(runtime = as.character(getRversion()), checkmate = as.character(packageVersion("checkmate")),
  original_locale = original, comparisons = length(records), failed = failed,
  distinct_records = distinct_records, records = records)
suffix = if (getRversion() < "4.0") "r36" else "r46"
write_json(result, file.path("attic/cnf_verify3/character_identity_review", paste0("primitives_", suffix, ".json")),
  pretty = TRUE, auto_unbox = TRUE)
cat("COMPARISONS", length(records), "FAILED", length(failed), "\n")
if (length(failed)) print(failed)
