# Independent truth/class oracle applied only to the installed package.
classes = c("CnfAtom", "CnfClause", "CnfFormula")
methods = lapply(c("&", "|"), function(op) setNames(lapply(classes,
  function(class) getS3method(op, class, envir = ns)), classes))
names(methods) = c("&", "|")
compiled = function(method) {
  result = tryCatch({capture.output(compiler::disassemble(method)); TRUE}, error = function(e) FALSE)
  isTRUE(result)
}
stopifnot(all(vapply(unlist(methods, recursive = FALSE), compiled, logical(1L))))
stopifnot(all(vapply(unlist(methods, recursive = FALSE), function(method) identical(environment(method), ns), logical(1L))))
shared = vapply(methods, function(group) all(vapply(group[-1L], identical,
  logical(1L), y = group[[1L]], ignore.bytecode = FALSE)), logical(1L))
cat("Resolved binary methods are byte-compiled namespace closures.\n")
cat("Shared resolved methods:", paste(names(shared), shared, collapse = "; "), "\n")
if (stage == "current") {
  stopifnot(all(shared))
  for (op in names(methods)) {
    handler = get(if (op == "&") "cnf_and" else "cnf_or", envir = ns)
    stopifnot(all(vapply(methods[[op]], identical, logical(1L), y = handler, ignore.bytecode = FALSE)))
  }
}
registered = getNamespaceInfo(ns, "S3methods")
capture.output(print(registered[registered[, 1L] %in% c("&", "|", "chooseOpsMethod"), , drop = FALSE]),
  file = file.path(audit_dir, "results", paste0(label, "_registrations.txt")))

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("0", "1"))
Z = CnfSymbol(u, "Z", c("p", "q"))
worlds = expand.grid(X = c("a", "b", "c"), Y = c("0", "1"), Z = c("p", "q"), stringsAsFactors = FALSE)
eval_clause = function(clause) {
  answer = rep(FALSE, nrow(worlds))
  for (i in seq_along(clause)) answer = answer | worlds[[names(clause)[[i]]]] %in% clause[[i]]
  answer
}
truth = function(object) {
  if (is.logical(object)) return(rep(as.vector(object), nrow(worlds)))
  if (inherits(object, "CnfAtom")) return(worlds[[object$symbol]] %in% object$values)
  payload = c(object)
  if (inherits(object, "CnfClause")) return(eval_clause(payload))
  stopifnot(inherits(object, "CnfFormula"))
  answer = rep(TRUE, nrow(worlds))
  for (i in seq_along(payload)) answer = answer & eval_clause(payload[[i]])
  answer
}
atom = CnfAtom(X, c("a", "b"))
clause = CnfClause(list(CnfAtom(X, "a"), CnfAtom(Y, "1")))
formula = CnfFormula(list(CnfClause(list(CnfAtom(X, "b"), CnfAtom(Y, "0"))),
  CnfClause(list(CnfAtom(Y, "1"), CnfAtom(Z, "p")))))
operands = list(atom = atom, clause = clause, formula = formula, raw_true = TRUE, raw_false = FALSE)
for (class in classes) {
  convert = get(paste0("as.", class), envir = ns)
  for (value in c(FALSE, TRUE)) {
    operands[[paste(class, value, "free", sep = "_")]] = convert(value)
    operands[[paste(class, value, "owned", sep = "_")]] = convert(CnfAtom(X, if (value) c("a", "b", "c") else character()))
  }
}
stopifnot(length(operands) == 17L)
capture_call = function(code) {
  warnings = character()
  error = NULL
  answer = withCallingHandlers(tryCatch(force(code), error = function(e) {
    error <<- conditionMessage(e)
    NULL
  }), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(answer = answer, error = error, warnings = warnings)
}
old_baseline = stage == "baseline" && getRversion() < "4.3.0"
rows = list()
for (left_name in names(operands)) {
  for (right_name in names(operands)) {
    left = operands[[left_name]]
    right = operands[[right_name]]
    left_class = class(left)
    right_class = class(right)
    mixed = left_class %in% classes && right_class %in% classes && left_class != right_class
    for (op in c("&", "|")) {
      expected = if (op == "&") truth(left) & truth(right) else truth(left) | truth(right)
      expected_class = if (!is.object(left) && !is.object(right)) "logical" else if (op == "&" ||
        inherits(left, "CnfFormula") || inherits(right, "CnfFormula")) "CnfFormula" else "CnfClause"
      result = capture_call(do.call(op, list(left, right)))
      truth_ok = is.null(result$error) && identical(truth(result$answer), expected)
      class_ok = is.null(result$error) && identical(class(result$answer), expected_class)
      if (old_baseline && mixed) {
        stopifnot(any(grepl("Incompatible methods", result$warnings, fixed = TRUE)))
        if (is.null(result$error)) stopifnot(truth_ok)
      } else {
        stopifnot(is.null(result$error), !length(result$warnings), truth_ok, class_ok)
      }
      rows[[length(rows) + 1L]] = data.frame(left = left_name, op = op, right = right_name,
        expected_class = expected_class, actual_class = if (is.null(result$error)) class(result$answer) else NA_character_,
        truth_ok = truth_ok, class_ok = class_ok,
        warnings = paste(result$warnings, collapse = "; "), error = if (is.null(result$error)) "" else result$error,
        stringsAsFactors = FALSE)
    }
  }
}
cases = do.call(rbind, rows)
stopifnot(nrow(cases) == 578L)
if (old_baseline) {
  example = cases[cases$left == "atom" & cases$op == "&" & cases$right == "clause", ]
  stopifnot(nrow(example) == 1L, nzchar(example$error), nzchar(example$warnings))
  cat("Baseline proper mixed-class failure:\n")
  print(example, row.names = FALSE)
}
write.table(cases, file.path(audit_dir, "results", paste0(label, "_binary_cases.tsv")),
  sep = "\t", row.names = FALSE, quote = TRUE)

# Different universes must still fail through the dispatched implementation.
v = CnfUniverse()
VX = CnfSymbol(v, "X", c("a", "b", "c"))
VY = CnfSymbol(v, "Y", c("0", "1"))
foreign = list(atom = CnfAtom(VX, "a"), clause = as.CnfClause(CnfAtom(VX, "a")),
  formula = CnfFormula(list(as.CnfClause(CnfAtom(VX, "a")), as.CnfClause(CnfAtom(VY, "0")))))
universe_cases = 0L
for (left in operands[c("atom", "clause", "formula")]) {
  for (right in foreign) {
    for (reverse in c(FALSE, TRUE)) {
      pair = if (reverse) list(right, left) else list(left, right)
      for (op in c("&", "|")) {
        result = capture_call(do.call(op, pair))
        stopifnot(!is.null(result$error))
        if (old_baseline && class(left) != class(right)) {
          stopifnot(any(grepl("Incompatible methods", result$warnings, fixed = TRUE)))
        } else {
          stopifnot(!length(result$warnings), grepl("same universe", result$error, fixed = TRUE))
        }
        universe_cases = universe_cases + 1L
      }
    }
  }
}

# Negation is tested on every operand, then in expressions involving mixed types.
negation_cases = 0L
for (operand in operands) {
  once = capture_call(!operand)
  twice = capture_call(!!operand)
  expected_class = if (inherits(operand, "CnfClause")) "CnfFormula" else class(operand)
  stopifnot(is.null(once$error), is.null(twice$error), !length(once$warnings), !length(twice$warnings),
    identical(truth(once$answer), !truth(operand)), identical(truth(twice$answer), truth(operand)),
    identical(class(once$answer), expected_class), identical(class(twice$answer), expected_class))
  negation_cases = negation_cases + 2L
}
expressions = list(quote(!(atom & clause)), quote(!(atom | formula)), quote(!clause | formula),
  quote((!atom | clause) & !formula), quote((!atom & !clause) | !formula), quote(!(!formula | clause)))
expected_truth = list(!(truth(atom) & truth(clause)), !(truth(atom) | truth(formula)),
  !truth(clause) | truth(formula), (!truth(atom) | truth(clause)) & !truth(formula),
  (!truth(atom) & !truth(clause)) | !truth(formula), !(!truth(formula) | truth(clause)))
composition_errors = 0L
for (i in seq_along(expressions)) {
  result = capture_call(eval(expressions[[i]]))
  if (old_baseline && i %in% c(1L, 2L, 4L, 5L, 6L)) {
    stopifnot(!is.null(result$error), any(grepl("Incompatible methods", result$warnings, fixed = TRUE)))
    composition_errors = composition_errors + 1L
  } else {
    stopifnot(is.null(result$error), !length(result$warnings), inherits(result$answer, "CnfFormula"),
      identical(truth(result$answer), expected_truth[[i]]))
  }
}
summary = list(scope = "Isolated six-file production CNF package, installed and byte-compiled",
  stage = stage, runtime = R.version.string, shared_resolved_methods = shared,
  dependency_versions = setNames(vapply(c("checkmate", "mlr3misc", "digest"),
    function(package) as.character(packageVersion(package)), character(1L)), c("checkmate", "mlr3misc", "digest")),
  binary_cases = nrow(cases), binary_errors = sum(nzchar(cases$error)),
  binary_warning_cases = sum(nzchar(cases$warnings)),
  binary_class_failures_without_error = sum(!cases$class_ok & !nzchar(cases$error)),
  binary_truth_failures_without_error = sum(!cases$truth_ok & !nzchar(cases$error)),
  universe_error_cases = universe_cases, negation_cases = negation_cases,
  composition_cases = length(expressions), expected_old_R_composition_errors = composition_errors)
dput(summary, file = file.path(audit_dir, "results", paste0(label, "_summary.R")))
print(summary)
cat("PASS: all installed-namespace checks matched their expected baseline/current outcomes.\n")
