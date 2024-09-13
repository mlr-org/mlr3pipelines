

simplify_cnf2 = function(entries, universe) {
  # faster intersect that relies on x, y being characters with no duplicates
  char_intersect = function(x, y) x[x %in% y]

  # construct the CnfFormula return object
  return_entries = function(entries) {
    structure(
      if (!length(entries)) TRUE else entries,
      universe = universe,
      class = "CnfFormula"
    )
  }

  if (is.logical(entries)) return(return_entries(entries))

  entries = entries[order(lengths(entries))]

  is_unit = lengths(entries) == 1L
  unit_queue = which(is_unit)
  eliminated = logical(length(entries))

  # maps symbol name -> the index of unit clauses
  unit_registry = new.env(parent = emptyenv())

  # maps symbol name -> their current domain
  unit_domains = new.env(parent = emptyenv())

  # maps symbol name -> the index of all non-unit clauses that refer to it
  symbol_registry = new.env(parent = emptyenv())

  is_not_subset_of = NULL  # see further down
  available_inverse = NULL  # ditto

  register_unit <- function(unit_idx) {
    unit = entries[[unit_idx]]
    nu = names(unit)
    ur = unit_registry[[nu]]
    if (is.null(ur)) {
      unit_registry[[nu]] = unit_idx  # environment assignment
      unit_domains[[nu]] = unit[[1L]]  # environment assignment
      is_unit[[unit_idx]] <<- TRUE
    } else {
      # two units refer to the same symbol
      # -> we keep the one we saw before but update it to the intersection.
      # the current one is eliminated
      prev_unit = unit_domains[[nu]]
      unit_isct = prev_unit[prev_unit %in% unit[[1L]]]
      if (!length(unit_isct)) return(TRUE)  # signal that we have a contradiction and can exit
      entries[[ur]][[1L]] <<- (unit_domains[[nu]] = unit_isct)
      eliminated[[unit_idx]] <<- TRUE
    }
    for (s in symbol_registry[[nu]]) {
      if (apply_unit_propagation(sr, nu)) return(TRUE)  # forward contradiction signal
    }
    FALSE  # no contradiction
  }

  apply_unit_propagation = function(clause_idx, symbol_s) {
    for (symbol in symbol_s) {
      clause = entries[[clause_idx]]
      symbol_idx = match(symbol, names(clause))
      clause_symbol_length_before = length(clause[[symbol_idx]])
      clause[[symbol_idx]] = char_intersect(clause[[symbol_idx]], unit_domains[[symbol]])
      if (length(clause[[symbol_idx]]) == length(unit_domains[[symbol]])) {
        # If the lengths match, then the clause is a superset of the unit (since they are equal)
        # the clause is therefore subsumed by the unit
        eliminated[[clause_idx]] <<- TRUE
        for (s in names(clause)) {
          sr = symbol_registry[[s]]
          symbol_registry[[s]] = sr[sr != clause_idx]
        }
        break
      }
      if (!length(clause[[symbol_idx]])) {
        esfc = eliminate_symbol_from_clause(clause_idx, symbol)
        if (is.null(esfc)) {
          break  # don't iterate over other symbols for this clause
        } else if (esfc) {
          return(TRUE)  # forward contradiction signal
        }
      } else if (length(clause[[symbol_idx]]) != clause_symbol_length_before) {
        # clause was changed. We need to update the subset relations matrix
        if (!is.null(is_not_subset_of)) {
          meta_idx = available_inverse[[clause_idx]]
          # meta_idx_outer is the index up to which we have built is_not_subset_of; if this is not TRUE, then we haven't seen the clause yet.
          # we are not relying on the fact that as soon as is_not_subset_of is non-null, we will only get here for clauses for which
          # this was built, because it could be cascading from a unit elimination.
          if (meta_idx <= meta_idx_outer) {
            available_inverse[symbol_registry[[symbol]]]
            # we could now be a subset of things that we were not a subset of before, so we only need to check the TRUE entries and may be setting them to FALSE.
            # We do not need to check other entries, since apply_unit_propagation happens to all clauses in a row, so the counter-side is updated in time.
            # In particular, we do not need to worry that some other clause that was formerly our subset ceases to be our subset, since that other clause
            # will also be restricted by the unit.
            # We can only ever set those entries to FALSE for which the other clause is in the symbol registry for the current symbol
            # Note: we cannot address is_not_subset_of by symbol_idx, since clauses may get shorter but is_not_subset_of stays the same!
            rows_to_check = which(is_not_subset_of[[meta_idx]][, symbol] & available %in% symbol_registry[[symbol]] & available <= meta_idx_outer)
            for (other_meta_idx in rows_to_check) {
              # when we get here, that means that the symbol exists both in clause_idx and in other_ref_this symbol, *and* we have seen both clauses
              # in the is_not_subset_of building process.
              # If this were not the case, then we would not need to change anything.
              other_clause_range = entries[[available[[other_meta_idx]]]][[symbol]]
              if (all(clause[[symbol_idx]] %in% other_clause_range)) {
                is_not_subset_of[[meta_idx]][other_meta_idx, symbol] <<- FALSE
              }
              rowsum = sum(is_not_subset_of[[meta_idx]][other_meta_idx, ])
              if (rowsum == 0) {
                # subsumption elimination
                eliminate_subsumption(available[[other_meta_idx]])
                next
              } else if (rowsum == 1) {
                # self-subsumption elimination
                # TODO: self-subsumption elimination
                #   - here we don't need to limit ourselves to two symbols being complements, we can do unit-eliminiation-style restriction here.
                #   - also apply this further down in the meta_idx loop
                # TODO: think harder about whether we can run in circles and end up eliminating something on the basis of something else that was also eliminated
              }
            }
          }
        }
        entries[[clause_idx]] <<- clause
      }
    }
    FALSE  # no contradiction
  }

  # returns 'NULL' for eliminated, 'TRUE' for contradiction, 'FALSE' for nothing happened
  eliminate_symbol_from_clause = function(clause_idx, symbol) {
    clause = entries[[clause_idx]]
    clause[[symbol]] = NULL
    if (!length(clause)) return(TRUE)  # signal that we have a contradiction and can exit
    # remove from symbol registry of the symbol that went to 0
    sr = symbol_registry[[symbol]]
    symbol_registry[[symbol]] = sr[sr != clause_idx]

    if (length(clause) == 1) {
      # new unit ahoy
      # remove from symbol registry of the symbol that remains in the clause
      # (since it is now a unit)
      sr = symbol_registry[[names(clause)]]
      symbol_registry[[names(clause)]] = sr[sr != clause_idx]
      if (register_unit(clause_idx)) return(TRUE)  # forward signal from register_unit: contradiction
      entries[[clause_idx]] <<- clause
      return(NULL)  # clause was turned into a unit
    }
    # clause was not turned into a unit, so we need to fill in is_not_subset_of
    if (!is.null(is_not_subset_of)) {  # we started filling this one in
      meta_idx = available_inverse[[clause_idx]]
      # meta_idx_outer is the index up to which we have built is_not_subset_of; if this is not TRUE, then we haven't seen the clause yet.
      # we are not relying on the fact that as soon as is_not_subset_of is non-null, we will only get here for clauses for which
      # this was built, because it could be cascading from a unit elimination.
      if (meta_idx <= meta_idx_outer) {
        is_not_subset_of[[meta_idx]][, symbol] <<- FALSE
        for (others_ref_this_symbol in available_inverse[sr]) {
          if (others_ref_this_symbol <= meta_idx_outer) {  # meta_idx_outer is the index up to which we have built is_not_subset_of
            is_not_subset_of[[others_ref_this_symbol]][meta_idx, symbol] <<- TRUE
          }
        }
      }
    }
    entries[[clause_idx]] <<- clause
    FALSE
  }

  eliminate_subsumption = function(clause_idx) {
    eliminated[[clause_idx]] <<- TRUE
    for (s in names(entries[[clause_idx]])) {
      sr = symbol_registry[[s]]
      symbol_registry[[s]] = sr[sr != clause_idx]
    }
  }

  # process units:
  # - populate unit_domains
  # - start populating symbol_registry; it gets finished later down with non-units
  # - also reduce duplicate units and replace them with their intersection
  for (unit_idx in unit_queue) {
    if (register_unit(unit_idx)) return(return_entries(FALSE))
  }

  # if there were only units, we are done
  if (length(unit_queue) == length(entries)) return(return_entries(entries[!eliminated]))

  # process non-units:
  # - eliminate entries subsumed by units
  # - populate symbol_registry
  # we sorted clauses by length, so we can skip ahead by length(unit_queue)
  for (clause_idx in seq.int(length(unit_queue) + 1L, length(entries))) {
    clause = entries[[clause_idx]]

    # intersect with units and eliminate subsumed clauses
    clause_symbol_isct = char_intersect(names(clause), names(unit_domains))
    # apply unit-propagation early, since we otherwise run the risk of adding the clause
    # to lots of symbol registry entries, only to remove it again right away
    if (apply_unit_propagation(clause_idx, clause_symbol_isct)) return(return_entries(FALSE))

    if (eliminated[[clause_idx]]) next  # could happen from unit propagation

    for (symbol in names(clause)) {
      sr_entry = symbol_registry[[symbol]]
      if (is.null(sr_entry)) {
        symbol_registry[[symbol]] = clause_idx
      } else {
        symbol_registry[[symbol]][[length(sr_entry) + 1L]] = clause_idx
      }
    }

  }


  # let's start with (self)-subsumption.
  # this can also create new units etc.

  # we don't need to check for units, upon registration they are automatically removed, and they cannot help with self-subsumption or HLA
  available = which(!(eliminated | is_unit))
  available_inverse = match(seq_along(entries), available)

  # record for each clause whether it is a subset of another clause.
  # we record this for each symbol separately, so when a symbol gets self-subsumed, we can re-check others more quickly.
  # Default is FALSE (not not a subset, i.e. it is a subset), this aligns with absent columns (which are subsets of everything)
  is_not_subset_of = vector("list", length(available))

  for (meta_idx_outer in seq_along(available)) {
    clause_idx = available[[meta_idx_outer]]
    clause = entries[[clause_idx]]
    sc = names(clause)
    # is_not_subset_of[[i]][j, k] records whether for symbol k, clause i is not a subset of clause j
    # if k is not in clause i, then var(i, k) is trivially a subset of var(j, k), so this is interpreted as FALSE.
    # if k is not in clause j but is in clause i, then var(i, k) is not a subset of var(j, k), so this is interpreted as TRUE.
    # we initialize with TRUE and then search for instances where a symbol k is in both clauses i and j, and where var(i, k) *is* a subset of var(j, k).
    is_not_subset_of[[meta_idx_outer]] = matrix(
      TRUE,
      ncol = length(available),  # indexed by meta_idx!
      nrow = length(clause),
      dimnames = list(NULL, names(clause))
    )

    for (meta_idx_inner in seq_len(meta_idx_outer - 1L)) {
      clause_idx_inner = available[[meta_idx_inner]]
      if (eliminated[[clause_idx_inner]] || is_unit[[clause_idx_inner]]) next
      clause_inner = entries[[clause_idx_inner]]
      # note we can *not* rely on clause_inner to have smaller or equal length than clause,
      # since unit elimination can change the length of clauses


      sci = names(clause_inner)
      sci_sc_map = match(sci, sc, nomatch = 0L)
      sci_names_in_common = which(sci != 0L)
      all_symbols = unique(c(sci, sc))

      # symbols that are not in common trivially get the matrix entry TRUE
      # (they are subsets of their counterpart in the other clause, since the other ones are empty)

      for (symbol_idx_inner in sci_names_in_common) {
        symbol_idx_outer = sci_sc_map[[symbol_idx_inner]]
        symbol_inner = sci[[symbol_idx_inner]]
        symbol_outer = sc[[symbol_idx_outer]]
        range_inner = clause_inner[[symbol_inner]]
        range_outer = clause[[symbol_outer]]

        inner_subset_of_outer = all(range_inner %in% range_outer)
        outer_subset_of_inner = (inner_subset_of_outer && length(sc) == length(sci)) || all(range_outer %in% range_inner)
        if (inner_subset_of_outer) {
          # avoid assigning TRUE to matrix that is initialized with TRUE
          is_not_subset_of[[meta_idx_inner]][meta_idx_outer, symbol_inner] = FALSE
        }
        if (outer_subset_of_inner) {
          is_not_subset_of[[meta_idx_outer]][meta_idx_inner, symbol_outer] = FALSE
        }
      }
      not_subset_sum_outer = sum(is_not_subset_of[[meta_idx_outer]][meta_idx_inner, ])
      if (not_subset_sum_outer == 0) {  # subsumption elimination of outer
        eliminate_subsumption(clause_idx_inner)
        next
      }
      if (not_subset_sum_outer == 1) {  # self-subsumption elimination
        symbol_to_eliminate = which(is_not_subset_of[[meta_idx_outer]][meta_idx_inner, ])
        # update clause and sc!
      }
      not_subset_sum_inner = sum(is_not_subset_of[[meta_idx_inner]][meta_idx_outer, ])


    }
    if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) next  # could happen if we eliminate or self-subsume in the inner loop

  }



  # Now for the big one: Asymmetric Hidden Literal Addition

  # copy of entries where we collect the clauses with hidden literals added
  # We keep the original entries: these are the ones we will return, and
  # we only ever need to compare each entries-clause against all entry_hla clauses,
  # not against each other.
  entry_hla = entries


  for (clause_idx in seq.int(length(unit_queue) + 1L, length(entries))) {
    clause = entries[[clause_idx]]
    if (eliminated[[clause_idx]]) next

    # for each symbol in the clause, check if it is a hidden literal
    # if it is, we add a new clause with the hidden literal removed
    # and add it to entry_hla
    for (symbol in names(clause)) {
      if (length(symbol_registry[[symbol]]) == 1) {
        # hidden literal found
        hla_clause = clause
        hla_clause[[symbol]] = NULL
        if (!length(hla_clause)) return(return_entries(FALSE))  # signal that we have a contradiction and can exit
        entry_hla[[length(entry_hla) + 1L]] = hla_clause
      }
    }
  }







}
