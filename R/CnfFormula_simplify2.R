

simplify_cnf2 = function(entries, universe) {
  #################################
  # Some vocabulary:
  # - `entries` is a list of `clauses`, which are named lists.
  # - `clauses`' names are `symbols`, the values are the `ranges` of the symbols within that clause
  # - `unit`: clause with single symbol
  # Glossary of operations:
  # - `unit propagation`: ranges of a symbol within a unit constitute the entire possible range of that symbol,
  #   so all clauses with that symbol are restricted to those values
  # - `subsumption elimination`: if all ranges of all symbols within one clause are subsets of those ranges in another clause,
  #   then the other clause can be removed
  # - `self-subsumption elimination`: if the ranges of all except one symbol within one clause are a subset of those ranges
  #   in another clause, then the remaining symbol in the latter clause can be intersected with the remaining symbol.
  # - `hidden literal addition` (HLA): if the ranges of all except one symbol within one clause are a subset of those ranges
  #   in another clause, then the *complement* of the remaining symbol in the former clause can be added to the latter clause.
  #   This is basically the reverse of self-subsumption elimination. We do not use this to change clauses directly;
  #   the point here is that the resulting clause, even if applied repeatedly, is logically equivalent to the original clause within the formula:
  #   - `hidden subsumption elimination`: if applying HLA results in a clause that can be subsumed, the original clause can be removed.
  #   - `hidden tautology elimination`: if applying HLA results in a clause that is a tautology, the original clause can be removed.
  #   Note: 'Clause Elimination Procedures for CNF Formulas' by Marijn Heule et al. calls this "Asymmetric Hidden Literal Addition".
  #################################

  ########################
  ## Helper functions   ##
  ########################

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

  ########################
  ## Variable setup     ##
  ########################

  entries = entries[order(lengths(entries))]

  is_unit = lengths(entries) == 1L
  eliminated = logical(length(entries))

  # maps symbol name -> the index of unit clauses
  unit_registry = new.env(parent = emptyenv())

  # maps symbol name -> their current domain
  unit_domains = new.env(parent = emptyenv())

  # maps symbol name -> the index of all non-unit clauses that refer to it
  symbol_registry = new.env(parent = emptyenv())

  is_not_subset_of = NULL  # see further down
  available_inverse = NULL  # ditto

  ##################################
  ## Event-driven clause handling ##
  ##################################
  # The following functions process the CNF clauses in various ways,
  # depending on the current state of the entries.
  # The state of the entries is updated in-place.
  # The functions are then called in a loop, but they may also call each other.
  # It is therefore important that they make the global state consistent before
  # calling other functions, and that they are aware of how the global state *can*
  # have changed upon their return.
  #  - entries[[clause_idx]] should be updated if necessary before other fns are called
  #  - symbol_registry[[symbol]] could have been updated
  #  - eliminated[[clause_idx]] could have been set to TRUE
  # Since functions can call each other in different ways, `eliminated` could have changed even for clauses
  # for which the other functions were not called.

  # Register a clause as a unit:
  # - check if there is already a unit for this symbol
  #   - if not, add it to the unit registry
  #   - otherwise, intersect unit domains (and exit on contradiction)
  # - apply unit propagation
  # - We do *not* update the sybmol registry here; if the clause is registered for a given symbol,
  #   it has to be removed from there *before* calling this function.
  register_unit = function(unit_idx) {
    unit = entries[[unit_idx]]
    nu = names(unit)
    ur = unit_registry[[nu]]
    if (is.null(ur)) {
      unit_registry[[nu]] = unit_idx  # environment assignment
      unit_domains[[nu]] = unit[[1L]]  # environment assignment
      # this is init'd TRUE for those w/ length 1 at the start, we set it to TRUE for clauses where symbols were eliminated
      is_unit[[unit_idx]] <<- TRUE
    } else {
      # two units refer to the same symbol
      # -> we keep the one we saw before but update it to the intersection.
      # the current one is eliminated
      prev_unit = unit_domains[[nu]]
      unit_isct = prev_unit[prev_unit %in% unit[[1L]]]
      if (!length(unit_isct)) return(TRUE)  # signal that we have a contradiction and can exit
      entries[[ur]][[1L]] <<- (unit_domains[[nu]] = unit_isct)  # update the *old* unit and unit_domains in one go. unit_domains is updated by-reference (environment)
      eliminated[[unit_idx]] <<- TRUE
    }
    # The symbol registry is empty at the start, this happens when new units are added later
    for (s_clause_idx in symbol_registry[[nu]]) {
      if (eliminated[[s_clause_idx]]) next  # could have been eliminated by subsumption elimination during unit propagation of another clause
      adr = apply_domain_restriction(s_clause_idx, nu, unit_domains[[nu]])
      if (identical(adr, TRUE)) return(TRUE)  # forward contradiction signal
    }
    FALSE  # no contradiction
  }

  # restrict the symbol in the clause to a given domain, e.g. a unit
  # - clause_idx: index in entries
  # - symbol: name of symbol to restrict
  # - restringent: domain to restrict to
  # returns TRUE if a contradiction was detected, NULL if the clause was eliminated
  apply_domain_restriction = function(clause_idx, symbol, restringent) {
    clause = entries[[clause_idx]]
    symbol_idx = match(symbol, names(clause))
    if (is.na(symbol_idx)) return(FALSE)

    clause_symbol_length_before = length(clause[[symbol_idx]])
    clause[[symbol_idx]] = char_intersect(clause[[symbol_idx]], restringent)
    if (length(clause[[symbol_idx]]) == length(restringent)) {
      # If the lengths match, then the clause is a superset of the unit (since the intersections are equal)
      # the clause is therefore subsumed by the unit
      eliminate_subsumption(clause_idx)
      return(NULL)
    }
    # clause was not changed
    if (length(clause[[symbol_idx]]) == clause_symbol_length_before) next
    # clause was changed.
    entries[[clause_idx]] <<- clause
    if (!length(clause[[symbol_idx]])) {
      # the symbol is not in the clause anymore
      return(eliminate_symbol_from_clause(clause_idx, symbol))
    }
    # We need to update the subset relations matrix, if it exists
    if (is.null(is_not_subset_of)) return(FALSE)

    meta_idx = available_inverse[[clause_idx]]
    # meta_idx_outer is the index up to which we have built is_not_subset_of; if meta_idx > meta_idx_outer, then we haven't seen the clause yet.
    # we are not relying on the fact that as soon as is_not_subset_of is non-null, we will only get here for clauses for which
    # is_not_subset_of was built, because unit-propagation could be cascading from a unit elimination.
    if (meta_idx > meta_idx_outer) return(FALSE)

    is_not_subset_of_col = match(symbol, colnames(is_not_subset_of[[meta_idx]]))
    # * We could now be a subset of things that we were not a subset of before, so we only need to check the TRUE entries and may be setting them to FALSE.
    #  --> hence `is_not_subset_of[meta_idx][is_not_subset_of[meta_idx]`
    #   Note: we cannot address is_not_subset_of by symbol_idx, since clauses may get shorter but is_not_subset_of stays the same!
    # * We do not need to check other entries, since apply_domain_restriction happens to all clauses in a row, so the counter-side is updated in time.
    #   In particular, we do not need to worry that some other clause that was formerly our subset ceases to be our subset, since that other clause
    #   will also be restricted by the unit.
    # * We can only ever set those entries to FALSE for which the other clause is in the symbol registry for the current symbol
    #  --> hence `available %in% symbol_registry[[symbol]]`
    # * We only need to check up to the point where we have built is_not_subset_of
    #  --> hence `available <= meta_idx_outer`
    rows_to_check = which(is_not_subset_of[[meta_idx]][, is_not_subset_of_col] & available %in% symbol_registry[[symbol]] & available <= meta_idx_outer)
    for (other_meta_idx in rows_to_check) {
      # when we get here, that means that the symbol exists both in clause_idx and in other_ref_this symbol, *and* we have seen both clauses
      # in the is_not_subset_of building process.
      # If this were not the case, then we would not need to change anything.
      other_clause_range = entries[[available[[other_meta_idx]]]][[symbol]]
      if (!all(clause[[symbol_idx]] %in% other_clause_range)) next

      # we are now a subset (and most likely were not before)
      # ("most likely" because it could probably happen that we re-enter this part through some cascading elimination / propagation)
      is_not_subset_of[[meta_idx]][other_meta_idx, is_not_subset_of_col] <<- FALSE
      ousr = on_updated_subset_relations(meta_idx, other_meta_idx)
      if (identical(ousr, TRUE)) return(TRUE)  # forward contradiction signal. We don't care if other_meta_idx was eliminated.
      if (eliminated[[clause_idx]]) return(NULL)  # need to check more directly if things escalated somehow and clause_idx was eliminated indirectly
    }
    FALSE  # no contradiction
  }




  # eliminate `symbol` from `clause_idx`
  # returns 'NULL' for turned into a unit or eliminated, 'TRUE' for contradiction, 'FALSE' for nothing happened
  eliminate_symbol_from_clause = function(clause_idx, symbol) {
    clause = entries[[clause_idx]]
    clause[[symbol]] = NULL
    if (!length(clause)) return(TRUE)  # signal that we have a contradiction and can exit
    # remove from symbol registry of the symbol that went to 0
    sr = symbol_registry[[symbol]]
    symbol_registry[[symbol]] = sr[sr != clause_idx]

    entries[[clause_idx]] <<- clause
    if (length(clause) == 1) {
      # new unit ahoy
      # remove from symbol registry of the symbol that remains in the clause
      # (since it is now a unit)
      sr = symbol_registry[[names(clause)]]
      symbol_registry[[names(clause)]] = sr[sr != clause_idx]
      if (register_unit(clause_idx)) return(TRUE)  # forward signal from register_unit: contradiction
      return(NULL)  # clause was turned into a unit
    }
    # clause was not turned into a unit, so we need to fill in is_not_subset_of
    if (is.null(is_not_subset_of)) return(FALSE)  # we have not started filling this one in

    meta_idx = available_inverse[[clause_idx]]
    # meta_idx_outer is the index up to which we have built is_not_subset_of; if meta_idx > meta_idx_outer, then we haven't seen the clause yet.
    # we are not relying on the fact that as soon as is_not_subset_of is non-null, we will only get here for clauses for which
    # this was built, because it could be cascading from a unit elimination.
    if (meta_idx > meta_idx_outer) return(FALSE)

    is_not_subset_of_col = match(symbol, colnames(is_not_subset_of[[meta_idx]]))
    rows_changed = which(is_not_subset_of[[meta_idx]][, is_not_subset_of_col])
    is_not_subset_of[[meta_idx]][, is_not_subset_of_col] <<- FALSE
    for (others_ref_this_symbol in available_inverse[sr]) {
      if (others_ref_this_symbol <= meta_idx_outer) {  # meta_idx_outer is the index up to which we have built is_not_subset_of
        is_not_subset_of[[others_ref_this_symbol]][meta_idx, is_not_subset_of_col] <<- TRUE
      }
    }
    for (meta_idx_other in rows_changed) {
      # We have to do this *after* we set the corresponding values to TRUE for others_ref_this_symbol,
      # since calling this could realistically change the symbol registry (e.g. if it leads to a symbol
      # being eliminated from other clauses).
      ousr = on_updated_subset_relations(meta_idx, meta_idx_other)
      if (identical(ousr, TRUE)) return(TRUE)
      # on_updated_subset_relations could cascade down to eliminating meta_idx (i.e. clause_idx)
      if (eliminated[[clause_idx]]) return(NULL)
    }
    FALSE
  }

  # returns 'NULL' when meta_idx_other was eliminated (or possibly turned into a unit),
  # 'TRUE' for contradiction, 'FALSE' for nothing changed
  on_updated_subset_relations = function(meta_idx, meta_idx_other) {
    # the row `meta_idx_other` of `is_not_subset_of[[meta_idx]]` has been updated.
    # we now check whether we can apply subsumption elimination, or at least self-subsumption elimination, on `meta_idx`.
    rowsum = sum(is_not_subset_of[[meta_idx]][meta_idx_other, ])
    if (rowsum > 1) return(FALSE)  # nothing to do
    if (rowsum == 0) {
      eliminate_subsumption(available[[meta_idx]])
      return(NULL)
    }
    # rowsum == 1
    symbol_to_restrict = colnames(is_not_subset_of[[meta_idx]])[is_not_subset_of[[meta_idx]][meta_idx_other, ]]
    # update clause and sc!

    # note we update meta_idx_other: we are currently subset of that other clause w/r/t all except one symbol.
    # we can therefore intersect that last symbol in the other clause with the range of that symbol in the current clause.
    # apply_domain_restriction will take care of eliminating the symbol if the range becomes empty.
    # It could in theory even do subsumption, but we have already taken care of that above.
    apply_domain_restriction(available[[meta_idx_other]], symbol_to_restrict, entries[[available[[meta_idx]]]][[symbol_to_restrict]])

    # TODO: think harder about whether we can run in circles and end up eliminating something on the basis of something else that was also eliminated

  }

  # mark a clause as eliminated and update symbol registry
  # This is relatively safe to call, since it does not modify any other clauses and does not create new units or subset relationships.
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
  unit_queue = which(is_unit)
  for (unit_idx in unit_queue) {
    if (register_unit(unit_idx)) return(return_entries(FALSE))
  }

  # if there were only units, we are done
  if (length(unit_queue) == length(entries)) return(return_entries(entries[!eliminated]))

  # process non-units:
  # - eliminate entries subsumed by units
  # - populate symbol_registry
  # we sorted clauses by length, so we can skip ahead by length(unit_queue)
  # At this point, everything after the unit_queue is not (yet) a unit.
  # The seq.int here is "dangerous" and only allowed because we checked for length(unit_queue) == length(entries) above.
  for (clause_idx in seq.int(length(unit_queue) + 1L, length(entries))) {
    clause = entries[[clause_idx]]

    # intersect with units and eliminate subsumed clauses
    clause_symbol_isct = char_intersect(names(clause), names(unit_domains))
    # apply unit-propagation early, since we otherwise run the risk of adding the clause
    # to lots of symbol registry entries, only to remove it again right away
    for (symbol in clause_symbol_isct) {
      adr = apply_domain_restriction(clause_idx, symbol, unit_domains[[symbol]])
      if (is.null(adr)) break
      if (adr) return(return_entries(FALSE))
    }

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
  #
  # note that as soon as is_not_subset_of is non-null, some other functions assume that meta_idx_outer is also defined.
  is_not_subset_of = vector("list", length(available))

  for (meta_idx_outer in seq_along(available)) {
    clause_idx = available[[meta_idx_outer]]
    clause = entries[[clause_idx]]
    sc = names(clause)
    # is_not_subset_of[[i]][j, k] records whether for symbol k, clause i is not a subset of clause j
    # If k is not in clause i, then var(i, k) is trivially a subset of var(j, k), so this is interpreted as FALSE.
    # If k is not in clause j but is in clause i, then var(i, k) is not a subset of var(j, k), so this is interpreted as TRUE.
    # We initialize with TRUE and then search for instances where a symbol k is in both clauses i and j, and where var(i, k) *is* a subset of var(j, k).
    # In that case, we set the entry to FALSE.
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
      # note that even though we sort entries at the beginning, we can *not* rely on clause_inner to have smaller or equal length than clause,
      # since unit elimination can change the length of clauses

      sci = names(clause_inner)
      sci_sc_map = match(sci, sc, nomatch = 0L)
      sci_names_in_common = which(sci_sc_map != 0L)

      # symbols that are not in common trivially get the matrix entry TRUE
      # (they are not subsets of their counterpart in the other clause, since the other ones are empty)

      for (symbol_idx_inner in sci_names_in_common) {
        symbol_idx_outer = sci_sc_map[[symbol_idx_inner]]
        symbol_inner = sci[[symbol_idx_inner]]
        symbol_outer = sc[[symbol_idx_outer]]
        range_inner = clause_inner[[symbol_idx_inner]]
        range_outer = clause[[symbol_idx_outer]]

        inner_subset_of_outer = all(range_inner %in% range_outer)
        outer_subset_of_inner = (inner_subset_of_outer && length(sc) == length(sci)) || all(range_outer %in% range_inner)
        if (inner_subset_of_outer) {
          # Avoid assigning TRUE to matrix that is initialized with TRUE.
          # We index by column name here, since it is possible that clauses were shortened by
          # unit propagation or self subsumption elimination somewhere on the way here
          is_not_subset_of[[meta_idx_inner]][meta_idx_outer, symbol_inner] = FALSE
        }
        if (outer_subset_of_inner) {
          is_not_subset_of[[meta_idx_outer]][meta_idx_inner, symbol_outer] = FALSE
        }
      }
      # prefer to eliminate the outer loopo clause first, since we have already
      # done more work for the inner loop clause (which comes earlier in 'entries')
      ousr = on_updated_subset_relations(meta_idx_inner, meta_idx_outer)
      if (is.null(ousr)) break
      if (!ousr) return(return_entries(FALSE))

      ousr = on_updated_subset_relations(meta_idx_outer, meta_idx_inner)
      if (is.null(ousr)) next
      if (!ousr) return(return_entries(FALSE))

    }
    # not necessary, since the only place where this could happen is on_update_subset_relations,
    # and we do check the return value of that one:
    #  > if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) next

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
