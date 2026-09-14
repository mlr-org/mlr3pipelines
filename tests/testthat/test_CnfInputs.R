with_cnf_input_locale = function(locales, code) {
  old = Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old))
  for (locale in locales) {
    changed = suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (nzchar(changed)) return(force(code))
  }
  skip("The requested character locale is unavailable")
}

test_that("CNF constructors reject dimensional and classed character inputs", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  for (name in list(matrix("Y"), array("Y", c(1L, 1L, 1L)), structure("Y", class = "AsIs"))) {
    expect_error(CnfSymbol(CnfUniverse(), name, c("a", "b")))
  }
  for (values in list(matrix(c("a", "a"), 1L), array(c("a", "b"), c(1L, 1L, 2L)),
    structure(c("a", "b"), class = "AsIs"))) {
    expect_error(CnfSymbol(CnfUniverse(), "Y", values))
    expect_error(CnfAtom(X, values))
  }
  expect_error(CnfAtom(X, matrix(character(0), 0L, 1L)))
  expect_error(CnfAtom(X, structure(character(0), class = "AsIs")))
})

test_that("CNF constructors reject missing, byte-marked and invalid text", {
  with_cnf_input_locale(c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8"), {
    bytes = rawToChar(as.raw(255L))
    Encoding(bytes) = "bytes"
    invalid_utf8 = rawToChar(as.raw(255L))
    Encoding(invalid_utf8) = "UTF-8"
    invalid_native = rawToChar(as.raw(255L))
    X = CnfSymbol(CnfUniverse(), "X", c("a", "b"))
    for (bad in list(NA_character_, bytes, invalid_utf8, invalid_native)) {
      u = CnfUniverse()
      expect_error(suppressWarnings(CnfSymbol(u, bad, c("a", "b"))))
      expect_length(u, 0L)
      expect_error(CnfSymbol(CnfUniverse(), "Y", c(bad, "a")))
      expect_error(CnfAtom(X, bad))
    }
    expect_error(CnfSymbol(CnfUniverse(), "", "a"))
    expect_error(CnfSymbol(CnfUniverse(), "Y", c("a", NA_character_)))
    for (values in list(1, TRUE, factor("a"), list("a"), as.raw(1L))) {
      expect_error(CnfSymbol(CnfUniverse(), "Y", values))
      expect_error(CnfAtom(X, values))
    }
  })
})

test_that("CNF text is normalized to UTF-8 without merging distinct Unicode values", {
  with_cnf_input_locale(c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8"), {
    utf8 = "\u00e9"
    latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
    native = iconv(utf8, from = "UTF-8", to = "", mark = FALSE)
    decomposed = "e\u0301"
    u = CnfUniverse()
    X = CnfSymbol(u, latin1, c(latin1, utf8, decomposed, "other"))
    expect_identical(Encoding(c(X)), "UTF-8")
    expect_identical(Encoding(u[[utf8]]), c("UTF-8", "UTF-8", "UTF-8", "unknown"))
    atom = CnfAtom(X, c(latin1, native, utf8, decomposed))
    expect_identical(atom$values, c(utf8, decomposed))
    expect_identical(Encoding(atom$values), c("UTF-8", "UTF-8"))
    expect_true(utf8 != decomposed)
    expect_false("other" %in% atom$values)
    expect_error(CnfSymbol(u, utf8, c("a", "b")), "already exists")
    distinct = CnfSymbol(u, decomposed, c("a", "b"))
    expect_identical(c(distinct), decomposed)
    expect_length(u, 2L)
  })
})

test_that("CNF rejects UTF-8 encodings outside the Unicode scalar range", {
  for (locales in list("C", c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8"))) {
    with_cnf_input_locale(locales, {
      X = CnfSymbol(CnfUniverse(), "X", c("a", "b"))
      for (bytes in list(c(0xed, 0xa0, 0x80), c(0xed, 0xbf, 0xbf), c(0xf4, 0x90, 0x80, 0x80))) {
        invalid = rawToChar(as.raw(bytes))
        Encoding(invalid) = "UTF-8"
        u = CnfUniverse()
        expect_error(suppressWarnings(CnfSymbol(u, invalid, c("a", "b"))))
        expect_length(u, 0L)
        expect_error(CnfSymbol(CnfUniverse(), "Y", c(invalid, "a")))
        expect_error(CnfAtom(X, invalid))
      }
      # Values on either side of the surrogate range and at the Unicode limit are valid.
      values = intToUtf8(c(0xd7ff, 0xe000, 0x10ffff), multiple = TRUE)
      Y = CnfSymbol(CnfUniverse(), "Y", c(values, "other"))
      expect_identical(CnfAtom(Y, values)$values, values)
    })
  }
})

test_that("CNF constructors preserve ordinary metadata, repetitions and empty selections", {
  u = CnfUniverse()
  X = CnfSymbol(u, c(label = "X"), c(first = "a", second = "a", third = "b", fourth = ""))
  expect_identical(unname(u[["X"]]), c("a", "a", "b", ""))
  expect_identical(CnfAtom(X, c(first = "a", second = "a"))$values, "a")
  expect_identical(CnfAtom(X, "")$values, "")
  expect_true(as.logical(CnfAtom(X, c("a", "b", ""))))
  for (empty in list(character(0), logical(0), integer(0), numeric(0), complex(0), raw(0), list(), NULL)) {
    atom = CnfAtom(X, empty)
    expect_s3_class(atom, "CnfAtom")
    expect_false(as.logical(atom))
  }
})

test_that("CNF names must preserve native binding identity before insertion", {
  with_cnf_input_locale("C", {
    utf8 = "\u00e9"
    latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
    for (name in list(utf8, latin1)) {
      u = CnfUniverse()
      expect_error(suppressWarnings(CnfSymbol(u, name, c("a", "b", "c"))), "native|locale")
      expect_length(u, 0L)
      CnfSymbol(u, "<U+00E9>", c("a", "b"))
      expect_error(suppressWarnings(CnfSymbol(u, name, c("a", "b", "c"))), "native|locale")
      expect_length(u, 1L)
    }
    X = CnfSymbol(CnfUniverse(), "ordinary name", c(latin1, "a", "b"))
    expect_identical(Encoding(CnfAtom(X, latin1)$values), "UTF-8")
  })
})

test_that("invalid native text cannot be replaced by printable escapes", {
  with_cnf_input_locale("C", {
    invalid_native = rawToChar(as.raw(255L))
    u = CnfUniverse()
    expect_error(CnfSymbol(u, invalid_native, c("a", "b")), "valid text")
    expect_length(u, 0L)
    expect_error(CnfSymbol(CnfUniverse(), "X", c(invalid_native, "a")), "valid text")
    X = CnfSymbol(CnfUniverse(), "X", c("<ff>", "a"))
    expect_error(CnfAtom(X, invalid_native), "valid text")
    expect_identical(CnfAtom(X, "<ff>")$values, "<ff>")
  })
})

test_that("compatible locales retain Unicode symbols and correct formula truth", {
  with_cnf_input_locale(c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8"), {
    for (name in c("\u00e9", "\u4e2d", "\U0001f642")) {
      u = CnfUniverse()
      X = CnfSymbol(u, name, c("a", "b", "c"))
      Y = CnfSymbol(u, "Y", c("a", "b"))
      expect_true(name %in% names(u))
      expect_identical(u[[name]], c("a", "b", "c"))
      clauses = list(
        CnfClause(list(X %among% "a")),
        CnfClause(list(X %among% "b", Y %among% "a")),
        CnfClause(list(X %among% "c", Y %among% "b"))
      )
      expect_false(as.logical(CnfFormula(clauses)))
    }
  })
})

test_that("universe lookup validates and normalizes symbol names", {
  with_cnf_input_locale(c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8"), {
    utf8 = "\u00e9"
    latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
    native = iconv(utf8, from = "UTF-8", to = "", mark = FALSE)
    u = CnfUniverse()
    X = CnfSymbol(u, utf8, c("a", "b"))
    for (alias in list(latin1, native)) {
      symbol = `$.CnfUniverse`(u, alias)
      expect_identical(Encoding(c(symbol)), "UTF-8")
      expect_identical(CnfAtom(symbol, "a")$symbol, c(X))
    }
    expect_error(`$.CnfUniverse`(u, matrix(utf8)))
    expect_error(`$.CnfUniverse`(u, structure(utf8, class = "AsIs")))
  })
  with_cnf_input_locale("C", {
    u = CnfUniverse()
    for (name in c("<U+00E9>", "<e9>", "<ff>")) CnfSymbol(u, name, c("a", "b"))
    utf8 = "\u00e9"
    latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
    for (alias in list(utf8, latin1)) {
      expect_error(suppressWarnings(`$.CnfUniverse`(u, alias)), "native|locale")
    }
    invalid_native = rawToChar(as.raw(255L))
    expect_error(`$.CnfUniverse`(u, invalid_native), "valid text")
    expect_identical(c(`$.CnfUniverse`(u, "<U+00E9>")), "<U+00E9>")
    expect_length(u, 3L)
  })
})
