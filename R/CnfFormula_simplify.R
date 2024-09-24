

simplify_cnf = function(entries, universe) {
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

  # faster intersect, setdiff etc. that rely on x, y being characters with no duplicates
  char_intersect = function(x, y) x[x %in% y]
  char_setdiff = function(x, y) x[!x %in% y]
  char_union = function(x, y) c(x, y[!y %in% x])
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
    # The symbol registry is empty at the start; the following happens when new units are added later
    use_inso = FALSE
    if (!is.null(is_not_subset_of)) {
      # if we have the is_not_subset_of matrix, we can use it to skip some checks
      unit_idx_meta = available_inverse[[unit_idx]]
      use_inso = unit_idx_meta <= meta_idx_outer  # .. but only if we have built the matrix up to the current index
      if (use_inso) {
        # if the unit is a subset of the other clause, we will never skip.
        inso_column = is_not_subset_of[[unit_idx_meta]][, nu]
      }
    }
    for (s_clause_idx in symbol_registry[[nu]]) {
      # could have been eliminated by subsumption elimination during unit propagation of another clause
      # (we are not afraid of is_unit here, since we are the unit for the symbol here -- everything else that has the
      # symbol gets eliminated)
      if (eliminated[[s_clause_idx]]) next
      if (use_inso) {
        # if we already know that s_clause_idx[[nu]] is a subset of unit_idx, we can skip this
        # exception: unit_idx is *also* a subset, implying equality, in which case we will do subsumption elimination.
        s_clause_idx_meta = available_inverse[[s_clause_idx]]
        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next
      }
      adr = apply_domain_restriction(s_clause_idx, nu, unit_domains[[nu]], TRUE)
      if (identical(adr, TRUE)) return(TRUE)  # forward contradiction signal
    }
    FALSE  # no contradiction
  }

  # restrict the symbol in the clause to a given domain, e.g. a unit
  # - clause_idx: index in entries
  # - symbol: name of symbol to restrict
  # - restringent: domain to restrict to
  # returns TRUE if a contradiction was detected, NULL if the clause was eliminated
  apply_domain_restriction = function(clause_idx, symbol, restringent, is_unit_propagation) {
    clause = entries[[clause_idx]]
    symbol_idx = match(symbol, names(clause))
    if (is.na(symbol_idx)) return(FALSE)

    clause_symbol_length_before = length(clause[[symbol_idx]])
    clause[[symbol_idx]] = char_intersect(clause[[symbol_idx]], restringent)
    if (length(clause[[symbol_idx]]) == length(restringent)) {
      # If the lengths match, then the clause is a superset of the unit (since the intersections are equal)
      # the clause is therefore subsumed by the unit
      eliminate_clause_update_sr(clause_idx)
      return(NULL)
    }
    # clause was not changed
    if (length(clause[[symbol_idx]]) == clause_symbol_length_before) return(FALSE)
    if (!length(clause[[symbol_idx]])) {
      # the symbol is not in the clause anymore
      return(eliminate_symbol_from_clause(clause_idx, symbol))
    }
    # clause was changed.
    # We don't need to do this when we go the eliminate_symbol_from_clause route, since that will update the clause in any case.
    entries[[clause_idx]] <<- clause

    # We need to update the subset relations matrix, if it exists
    if (is.null(is_not_subset_of)) return(FALSE)

    meta_idx = available_inverse[[clause_idx]]
    # meta_idx_outer is the index up to which we have built is_not_subset_of; if meta_idx > meta_idx_outer, then we haven't seen the clause yet.
    # we are not relying on the fact that as soon as is_not_subset_of is non-null, we will only get here for clauses for which
    # is_not_subset_of was built, because unit-propagation could be cascading from a unit elimination.
    if (meta_idx > meta_idx_outer) return(FALSE)

    if (!is_unit_propagation) {
      # we are not propagating units, so we need to update the subset relations in the other direction.
      # we do this first, in case on_updated_subset_relations gets too eager later
      # No need to compare against meta_idx_outer, since not_subset_count will be NA for larger indices..
      idx_to_check = which(!is.na(not_subset_count[, meta_idx]) & available %in% symbol_registry[[symbol]])
      for (other_meta_idx in idx_to_check) {
        # was the other a subset of us before? that could have changed now.
        if (!is_not_subset_of[[other_meta_idx]][meta_idx, symbol] && !all(entries[[available[[other_meta_idx]]]][[symbol]] %in% entries[[clause_idx]][[symbol]])) {
          is_not_subset_of[[other_meta_idx]][meta_idx, symbol] <<- TRUE
          # no need to call on_updated_subset_relations, since we are increasing rowsums, not decreasing them
          not_subset_count[other_meta_idx, meta_idx] <<- not_subset_count[other_meta_idx, meta_idx] + 1L
          # no need to call the higher order self subsumption elimination handler here, since we need to do this anyways further down.
        }
      }
    }

    is_not_subset_of_col = match(symbol, colnames(is_not_subset_of[[meta_idx]]))
    # * We could now be a subset of things that we were not a subset of before, so we only need to check the TRUE entries and may be setting them to FALSE.
    #  --> hence `is_not_subset_of[meta_idx][is_not_subset_of[meta_idx]`
    #   Note: we cannot address is_not_subset_of by symbol_idx, since clauses may get shorter but is_not_subset_of stays the same!
    # * If this is unit propagation, we do not need to check other entries, since apply_domain_restriction happens to all clauses in a row, so the counter-side is updated in time.
    #   In particular, we do not need to worry that some other clause that was formerly our subset ceases to be our subset, since that other clause
    #   will also be restricted by the unit.
    # * We can only ever set those entries to FALSE for which the other clause is in the symbol registry for the current symbol
    #  --> hence `available %in% symbol_registry[[symbol]]`
    # * We only need to check up to the point where we have built is_not_subset_of
    #  --> hence the is.na()
    rows_to_check = which(!is.na(not_subset_count[meta_idx, ]) & is_not_subset_of[[meta_idx]][, is_not_subset_of_col] & available %in% symbol_registry[[symbol]])
    for (other_meta_idx in rows_to_check) {
      clause_idx_other = available[[other_meta_idx]]
      # while we only get here for rows that are in the symbol_registry and therefore not eliminated / units, we need to check again since on_updated_subset_relations()
      # can change this during the loop.
      if (eliminated[[clause_idx_other]] || is_unit[[clause_idx_other]]) next

      # when we get here, that means that the symbol exists both in clause_idx and in other_ref_this symbol, *and* we have seen both clauses
      # in the is_not_subset_of building process.
      # If this were not the case, then we would not need to change anything.
      other_clause_range = entries[[available[[other_meta_idx]]]][[symbol]]
      if (!all(entries[[clause_idx]][[symbol]] %in% other_clause_range)) next

      # we are now a subset (and most likely were not before)
      # ("most likely" because it could probably happen that we re-enter this part through some cascading elimination / propagation)
      if (!is_not_subset_of[[meta_idx]][other_meta_idx, is_not_subset_of_col]) next
      is_not_subset_of[[meta_idx]][other_meta_idx, is_not_subset_of_col] <<- FALSE
      not_subset_count[meta_idx, other_meta_idx] <<- (rowsum = not_subset_count[meta_idx, other_meta_idx] - 1L)
      if (rowsum > 1L + second_order_enabled) next
      ousr = on_updated_subset_relations(meta_idx, other_meta_idx, FALSE)
      if (identical(ousr, TRUE)) return(TRUE)  # forward contradiction signal. We don't care if other_meta_idx was eliminated.
      if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) return(NULL)  # need to check more directly if things escalated somehow and clause_idx was eliminated indirectly
    }
    # call on_domain_changed_handle_2nd_order_sse
    # it is not a waste w/r/t the 'on_updated_subset_relations' call, since that one only deals with clauses for which the symbol is set to FALSE now,
    # i.e. precisely the other clauses.
    if (second_order_enabled) on_update_range(meta_idx, symbol) else FALSE
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
    rows_changed = which(is_not_subset_of[[meta_idx]][, is_not_subset_of_col] & !is.na(not_subset_count[meta_idx, ]))
    is_not_subset_of[[meta_idx]][, is_not_subset_of_col] <<- FALSE
    not_subset_count[meta_idx, rows_changed] <<- not_subset_count[meta_idx, rows_changed] - 1L
    others_ref_this_symbol = available_inverse[sr]
    # only where we have already set up is_not_subset_of
    for (meta_idx_other in others_ref_this_symbol[!is.na(not_subset_count[others_ref_this_symbol, meta_idx])]) {
      if (!is_not_subset_of[[meta_idx_other]][meta_idx, symbol]) {
        is_not_subset_of[[meta_idx_other]][meta_idx, symbol] <<- TRUE
        not_subset_count[meta_idx_other, meta_idx] <<- not_subset_count[meta_idx_other, meta_idx] + 1L
      }
    }
    # we could have some leftover TRUEs from eliminated or unit-ed clauses
    rows_changed_ids = available[rows_changed]
    for (meta_idx_other in rows_changed[!eliminated[rows_changed_ids] & !is_unit[rows_changed_ids] & rows_changed <= meta_idx_outer]) {
      # We have to do this *after* we set the corresponding values to TRUE for others_ref_this_symbol,
      # since calling this could realistically change the symbol registry (e.g. if it leads to a symbol
      # being eliminated from other clauses).
      clause_idx_other = available[[meta_idx_other]]
      # while we *do* restrict meta_idx_other in the for loop already, eliminated and is_unit can still change during the loop, so check here again
      if (eliminated[[clause_idx_other]] || is_unit[[clause_idx_other]]) next
      if (not_subset_count[meta_idx, meta_idx_other] > 1L + second_order_enabled) next
      ousr = on_updated_subset_relations(meta_idx, meta_idx_other, FALSE)
      if (identical(ousr, TRUE)) return(TRUE)
      # on_updated_subset_relations could cascade down to eliminating meta_idx (i.e. clause_idx)
      if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) return(NULL)
    }
    FALSE
  }

  # returns 'NULL' when meta_idx_other was eliminated (or possibly turned into a unit),
  # 'TRUE' for contradiction, 'FALSE' for nothing changed
  on_updated_subset_relations = function(meta_idx, meta_idx_other, second_order_only) {
    # the row `meta_idx_other` of `is_not_subset_of[[meta_idx]]` has been updated.
    # we now check whether we can apply subsumption elimination, or at least self-subsumption elimination, on `meta_idx`.
    rowsum = not_subset_count[meta_idx, meta_idx_other]
    if (rowsum > 1L + second_order_enabled) return(FALSE)  # nothing to do
    if (rowsum == 2L) {
      # we usually don't get here when we are second_order_only; however, it may be possible that a oneend clause
      # gets turned into a twoend clause during 2nd order triggering before it gets triggered itself.
      hs2oo = handle_sse_2nd_order_twoend(meta_idx, meta_idx_other, NULL)
      if (identical(hs2oo, TRUE)) return(TRUE)
      clause_other = available[[meta_idx_other]]
      if (eliminated[[clause_other]] || is_unit[[clause_other]]) return(NULL)
      return(FALSE)
    }
    if (rowsum == 0) {
      eliminate_clause_update_sr(available[[meta_idx_other]])
      return(NULL)
    }
    # rowsum == 1
    symbol_to_restrict = colnames(is_not_subset_of[[meta_idx]])[is_not_subset_of[[meta_idx]][meta_idx_other, ]]
    # update clause and sc!

    if (!second_order_only) {
      # note we update meta_idx_other: we are currently subset of that other clause w/r/t all except one symbol.
      # we can therefore intersect that last symbol in the other clause with the range of that symbol in the current clause.
      # apply_domain_restriction will take care of eliminating the symbol if the range becomes empty.
      # It could in theory even do subsumption, but we have already taken care of that above.
      adr = apply_domain_restriction(available[[meta_idx_other]], symbol_to_restrict, entries[[available[[meta_idx]]]][[symbol_to_restrict]], FALSE)
      if (is.null(adr)) return(NULL)
      if (adr) return(TRUE)
      if (!second_order_enabled) return(FALSE)
    }
    if (!second_order_enabled_matrix[meta_idx, meta_idx_other]) return(FALSE)
    clause_idx = available[[meta_idx]]
    if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) return(FALSE)
    hs2oo = handle_sse_2nd_order_oneend(meta_idx, meta_idx_other, symbol_to_restrict)
    if (identical(hs2oo, TRUE)) return(TRUE)
    clause_other = available[[meta_idx_other]]
    if (eliminated[[clause_other]] || is_unit[[clause_other]]) return(NULL)
    FALSE
  }

  # second order self-subsumption elimination
  # this is called whenever the range of symbol in meta_idx shrinks, opening up the possibility for 2nd order self-subsumption.
  # meta_idx is the clause that will remove symbol range from other clauses.
  # returns NULL if meta_idx was eliminated, FALSE if no contradiction, TRUE if contradiction
  on_update_range = function(meta_idx, symbol) {
    ## assuming the symbol exists in meta_idx
    # availability of symbol in the other clause is not necessary here!
    # units don't need to be treated separately:
    # - since they only work with 2 length-2-clauses that have both the same symbol: the one of the unit, and another one; in the other one they must be disjoint.
    # - because of this, they will HS-eliminate the unit.

    inso_symbol_col = match(symbol, colnames(is_not_subset_of[[meta_idx]]))
    potential_targets = which(is_not_subset_of[[meta_idx]][, inso_symbol_col] & !eliminated[available] & !is_unit[available] & !is.na(not_subset_count[meta_idx, ]))

    targets_while_oneend = potential_targets[not_subset_count[meta_idx, potential_targets] == 1L]

    for (meta_idx_target in targets_while_oneend) {
      if (!second_order_enabled_matrix[meta_idx, meta_idx_target]) next
      hs2oo = handle_sse_2nd_order_oneend(meta_idx, meta_idx_target, symbol)
      if (is.null(hs2oo)) return(NULL)  # meta_idx eliminated
      if (hs2oo) return(TRUE)
    }
    targets_while_twoend = potential_targets[not_subset_count[meta_idx, potential_targets] == 2L]
    for (meta_idx_target in targets_while_twoend) {
      if (!second_order_enabled_matrix[meta_idx, meta_idx_target]) next
      hs2oo = handle_sse_2nd_order_twoend(meta_idx, meta_idx_target, symbol)
      if (is.null(hs2oo)) return(NULL)  # meta_idx eliminated
      if (hs2oo) return(TRUE)
    }
    FALSE
  }

  # meta_idx is not subset of meta_idx_target over 'symbol'.
  # check if there are other clauses that are non-subsets of meta_idx_target for two symbols, one of which is 'symbol' as well.
  # return NULL if clause_idx is eliminated.
  handle_sse_2nd_order_oneend = function(meta_idx, meta_idx_target, symbol) {
    # we don't need to check whether symbol is in meta_idx, since then it will automatically not be a non-subset of target
    inso_symbol_col = match(symbol, colnames(is_not_subset_of[[meta_idx]]))
    clause_idx = available[[meta_idx]]
    symbol_clauses = symbol_registry[[symbol]]
    symbol_clauses_meta = available_inverse[symbol_clauses]

    clause_idx_target = available[[meta_idx_target]]
    # intersect on symbol with some other clause that has not_subset_count == 2, and where the target symbol is present in the garget
    if (eliminated[[clause_idx_target]] || is_unit[[clause_idx_target]]) return(FALSE)
    twoends = symbol_clauses_meta[not_subset_count[symbol_clauses_meta, meta_idx_target] == 2L]
    for (meta_idx_twoend in twoends[!is.na(twoends)]) {  # twoends is NA for clauses that have not been processed yet
      # in every loop iteration, check our conditions again, since anything can happen when we do sse:
      # target exists, is not a unit (these are checked outside the for loop as well as with `ts2o`), and exactly one symbol ("symbol") of meta_idx does not cover it.
      if (not_subset_count[meta_idx, meta_idx_target] != 1L || !is_not_subset_of[[meta_idx]][meta_idx_target, inso_symbol_col]) break
      clause_idx_twoend = available[[meta_idx_twoend]]
      # now check that the meta_idx_twoend is still valid:
      # exists, not a unit, it does not cover the target with exactly two symbols, one of which is `symbol`
      if (eliminated[[clause_idx_twoend]] || is_unit[[clause_idx_twoend]] ||
          not_subset_count[meta_idx_twoend, meta_idx_target] != 2L) {
        next
      }
      symbol_target = colnames(is_not_subset_of[[meta_idx_twoend]])[is_not_subset_of[[meta_idx_twoend]][meta_idx_target, ]]
      symbol_target = symbol_target[symbol_target != symbol]
      if (length(symbol_target) != 1L) next  # this can happen if a previous loop changed the clause in some way
      if (!symbol_target %in% names(entries[[clause_idx_target]])) next
      # `symbol_target` is two symbols here: target and intersect
      ts2o = try_sse_2nd_order(meta_idx, meta_idx_twoend, meta_idx_target, symbol, symbol_target)
      if (identical(ts2o, TRUE)) return(TRUE)
      if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) return(NULL)
      if (is.null(ts2o)) return(FALSE)  # target was eliminated
    }
    FALSE
  }

  handle_sse_2nd_order_twoend = function(meta_idx, meta_idx_target, symbol) {
    clause_idx = available[[meta_idx]]

    clause_idx_target = available[[meta_idx_target]]
    # check that meta_idx_target is still valid: not eliminated, not a unit, not_subset_count 2 w/r/t the target, one of which is still the symbol itself.
    if (eliminated[[clause_idx_target]] || is_unit[[clause_idx_target]] || not_subset_count[meta_idx, meta_idx_target] != 2) return(FALSE)
    inso_symbols_cols = which(is_not_subset_of[[meta_idx]][meta_idx_target, ])
    symbols_twoend = colnames(is_not_subset_of[[meta_idx]])[inso_symbols_cols]
    if (!is.null(symbol) && !symbol %in% symbols_twoend) return(FALSE)
    for (symbol_inner_idx in 1:2) {
      symbol_inner = symbols_twoend[[symbol_inner_idx]]  # the intersection symbol
      symbol_other = symbols_twoend[[3 - symbol_inner_idx]]
      # symbol_other must be present in the target clause, otherwise we can skip.
      if (!symbol_other %in% names(entries[[clause_idx_target]])) next
      symbol_inner_clauses_meta = available_inverse[symbol_registry[[symbol_inner]]]
      # 'oneends' may either have 1 or 2 non-subset symbols, with both symbols equal in the latter case.
      # We do this in both inner loops, since we switch on which one of them is the target.
      oneends = symbol_inner_clauses_meta[not_subset_count[symbol_inner_clauses_meta, meta_idx_target] <= 2L]
      for (meta_idx_oneend in oneends[!is.na(oneends)]) {  # oneends is NA for clauses that have not been processed yet
        # check if the twoend-bit broke
        if (not_subset_count[meta_idx, meta_idx_target] != 2 || !all(is_not_subset_of[[meta_idx]][meta_idx_target, inso_symbols_cols])) break

        # check if theoneend-bit broke
        clause_idx_oneend = available[[meta_idx_oneend]]
        if (eliminated[[clause_idx_oneend]] || is_unit[[clause_idx_oneend]]) next
        # so the 'oneend' thing may also overlap with 'twoend' on the other symbol.
        # The trouble is that we cannot rely on both symbols being in is_not_subset_of[[meta_idx_oneend]].
        # We therefore get the columns, with '0' if one of the symbols is not there, and then check whether the two columns are the only
        # two that are TRUE (where the sum may be either 1 or 2).
        symbols_twoend_idx = match(symbols_twoend, colnames(is_not_subset_of[[meta_idx_oneend]]), nomatch = 0L)
        if (not_subset_count[meta_idx_oneend, meta_idx_target] != sum(is_not_subset_of[[meta_idx_oneend]][meta_idx_target, symbols_twoend_idx])) {
          next
        }
        if (!symbol_other %in% names(entries[[clause_idx_target]])) next
        ts2o = try_sse_2nd_order(meta_idx_oneend, meta_idx, meta_idx_target, symbol_inner, symbol_other)
        if (identical(ts2o, TRUE)) return(TRUE)
        if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) return(NULL)
        if (is.null(ts2o)) return(FALSE)  # target was eliminated
      }
    }
    FALSE
  }

  try_sse_2nd_order = function(meta_idx_oneend, meta_idx_twoends, meta_idx_target, symbol_intersect, symbol_target) {
    ## assuming that symbol_target exists in meta_idx_target
    ## assuming that meta_idx_oneend and meta_idx_twoends both have symbol_intersect, and that at least meta_idx_twoends has symbol_target
    if (!all(is_not_subset_of[[meta_idx_target]][c(meta_idx_oneend, meta_idx_twoends), symbol_target])) return(FALSE)
    idx_oneend = available[[meta_idx_oneend]]
    idx_twoends = available[[meta_idx_twoends]]
    idx_target = available[[meta_idx_target]]
    # if (any(eliminated[c(idx_oneend, idx_twoends, idx_target)]) || any(is_unit[c(idx_oneend, idx_twoends, idx_target)])) return(FALSE)
    clause_oneend = entries[[idx_oneend]]
    clause_twoends = entries[[idx_twoends]]

    # only proceed if clause_oneend and clause_twoends are disjoint outside of clause_target, with respect to symbol_intersect
    clause_oneend_symbol_intersect = clause_oneend[[symbol_intersect]]
    if (any(clause_oneend_symbol_intersect %in% clause_twoends[[symbol_intersect]] &
        !clause_oneend_symbol_intersect %in% entries[[idx_target]][[symbol_intersect]])) {
      return(FALSE)
    }
    apply_domain_restriction(idx_target, symbol_target, char_union(clause_oneend[[symbol_target]], clause_twoends[[symbol_target]]), FALSE)
  }

  # mark a clause as eliminated and update symbol registry
  # This is relatively safe to call, since it does not modify any other clauses and does not create new units or subset relationships.
  eliminate_clause_update_sr = function(clause_idx) {
    if (is_unit[[clause_idx]]) stop("Bug in the code, this should never happen.")
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

    # intersect with units and eliminate subsumed clauses
    clause_symbol_isct = char_intersect(names(entries[[clause_idx]]), names(unit_domains))
    # apply unit-propagation early, since we otherwise run the risk of adding the clause
    # to lots of symbol registry entries, only to remove it again right away
    for (symbol in clause_symbol_isct) {
      adr = apply_domain_restriction(clause_idx, symbol, unit_domains[[symbol]], TRUE)
      if (is.null(adr)) break
      if (adr) return(return_entries(FALSE))
    }

    if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) next  # could happen from unit propagation

    # do cache `clause = entries[[clause_idx]]` and refer to names(clause) here,
    # since apply_domain_restriction might have removed symbols from the clause
    for (symbol in names(entries[[clause_idx]])) {
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
  # not_subset_count[i, j]: how many symbols of clause i are not subset of clause j?
  # if this is 1, we can do self-subsumption elimination and HLA.
  # if this is 2, we can do self-subsumption with resolution.
  # This is the same as sum(is_not_subset_of[[i]][j, ])
  not_subset_count = matrix(NA_integer_, nrow = length(available), ncol = length(available))

  # whether second-order subsumption is enabled for the pair
  # We only enable it after the initial first-order checks are done, to reduce 2nd order calls as much as possible.
  second_order_enabled = FALSE
  second_order_enabled_matrix = matrix(FALSE, nrow = length(available), ncol = length(available))


  for (meta_idx_outer in seq_along(available)) {
    clause_idx = available[[meta_idx_outer]]
    if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) next  # may happen if we somehow turn something into a unit that eliminates a later clause
    clause = entries[[clause_idx]]

    # is_not_subset_of[[i]][j, k] records whether for symbol k, clause i is not a subset of clause j
    # If k is not in clause i, then var(i, k) is trivially a subset of var(j, k), so this is interpreted as FALSE.
    # If k is not in clause j but is in clause i, then var(i, k) is not a subset of var(j, k), so this is interpreted as TRUE.
    # We initialize with TRUE and then search for instances where a symbol k is in both clauses i and j, and where var(i, k) *is* a subset of var(j, k).
    # In that case, we set the entry to FALSE.
    is_not_subset_of[[meta_idx_outer]] = matrix(
      TRUE,
      nrow = length(available),  # indexed by meta_idx!
      ncol = length(clause),
      dimnames = list(NULL, names(clause))
    )
    # we are not a subset of ourselves
    # (this prevents us from checking this later in on_updated_subset_relations; if we do check it there, we would eliminate the clause from itself)
    is_not_subset_of[[meta_idx_outer]][meta_idx_outer, ] = FALSE

    for (meta_idx_inner in seq_len(meta_idx_outer - 1L)) {
      ## need to refresh clause here, since it might have been shortened by unit propagation in an earlier iteration
      clause = entries[[clause_idx]]
      clause_idx_inner = available[[meta_idx_inner]]
      if (eliminated[[clause_idx_inner]] || is_unit[[clause_idx_inner]]) next
      clause_inner = entries[[clause_idx_inner]]
      # note that even though we sort entries at the beginning, we can *not* rely on clause_inner to have smaller or equal length than clause,
      # since unit elimination can change the length of clauses

      sci = names(clause_inner)
      sci_sc_map = match(sci, names(clause), nomatch = 0L)
      sci_names_in_common = which(sci_sc_map != 0L)

      # symbols that are not in common trivially get the matrix entry TRUE
      # (they are not subsets of their counterpart in the other clause, since the other ones are empty)

      for (symbol_idx_inner in sci_names_in_common) {
        symbol_idx_outer = sci_sc_map[[symbol_idx_inner]]
        symbol = sci[[symbol_idx_inner]]

        range_inner = clause_inner[[symbol_idx_inner]]
        range_outer = clause[[symbol_idx_outer]]

        inner_subset_of_outer = all(range_inner %in% range_outer)
        outer_subset_of_inner = (inner_subset_of_outer && length(range_outer) == length(range_inner)) || all(range_outer %in% range_inner)
        if (inner_subset_of_outer) {
          # Avoid assigning TRUE to matrix that is initialized with TRUE.
          # We index by column name here, since it is possible that clauses were shortened by
          # unit propagation or self subsumption elimination somewhere on the way here
          is_not_subset_of[[meta_idx_inner]][meta_idx_outer, symbol] = FALSE
        }
        if (outer_subset_of_inner) {
          is_not_subset_of[[meta_idx_outer]][meta_idx_inner, symbol] = FALSE
        }
      }
      # prefer to eliminate the outer loopo clause first, since we have already
      # done more work for the inner loop clause (which comes earlier in 'entries')
      not_subset_count[meta_idx_outer, meta_idx_inner] = sum(is_not_subset_of[[meta_idx_outer]][meta_idx_inner, ])
      rowsum = sum(is_not_subset_of[[meta_idx_inner]][meta_idx_outer, ])
      not_subset_count[meta_idx_inner, meta_idx_outer] = rowsum
      if (rowsum <= 1) {
        ousr = on_updated_subset_relations(meta_idx_inner, meta_idx_outer, FALSE)
        if (is.null(ousr)) break
        if (ousr) return(return_entries(FALSE))
        if (eliminated[[clause_idx_inner]] || is_unit[[clause_idx_inner]]) next  # yes this can happen.
      }
      # need to get rowsum back again, since the call above could have changed something!
      rowsum = not_subset_count[meta_idx_outer, meta_idx_inner]
      if (rowsum <= 1) {
        ousr = on_updated_subset_relations(meta_idx_outer, meta_idx_inner, FALSE)
        if (identical(ousr, TRUE)) return(return_entries(FALSE))
        if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) break  # probably this can happen if things cascade onwards when updating clause_inner
        if (is.null(ousr)) next
      }
    }
  }

  second_order_enabled = TRUE
  second_order_enabled_matrix = not_subset_count != 2L  # allow cascading of indirect SSE, except for the combinations where we trigger it manually (otherwise we'd be running them twice)
  # We only trigger twoend-clauses, because every twoend clauses looks for all available oneend-clauses by itself.
  # We can not do oneend-clauses-only, since twoend-clauses also look for other twoend-clauses that have common overlap.
  # we need to make sure we don't trigger for eliminated / unit clauses, so we set them to TRUE here.
  # This means all remaining entries of the second_order_enabled_matrix are exactly the ones for which we trigger on_updated_subset_relations() manually.
  meta_disabled = eliminated[available] | is_unit[available]
  second_order_enabled_matrix[meta_disabled, ] = TRUE
  second_order_enabled_matrix[, meta_disabled] = TRUE
  # alternatively, we could also enable none of the cascading in the beginning and have a nested loop in the following, where we 'repeat' until there are no more rowsum <= 2L
  # That would probably give uns less cascading, but we'd have to run 'which()' on a N^2 matrix potentially many times.

  sse_to_trigger = which(!second_order_enabled_matrix, arr.ind = TRUE)
  for (sse_idx in seq_len(nrow(sse_to_trigger))) {
    meta_idx = sse_to_trigger[sse_idx, 1L]
    meta_idx_other = sse_to_trigger[sse_idx, 2L]
    if (eliminated[[available[[meta_idx]]]] || is_unit[[available[[meta_idx]]]] || eliminated[[available[[meta_idx_other]]]] || is_unit[[available[[meta_idx_other]]]]) next
    second_order_enabled_matrix[meta_idx, meta_idx_other] = TRUE
    if (identical(on_updated_subset_relations(meta_idx, meta_idx_other, TRUE), TRUE)) return(return_entries(FALSE))
  }


  # Now for the big one: Asymmetric Hidden Literal Addition (Marijn et al.)

  # First, we do non-units, then we do units separately.
  # We go big to small, since eliminating one clause could prevent us from eliminating other clauses, and all things being equal, we prefer eliminating long ones.

  # We only loop over the entries that have not been eliminated yet.
  # We update `remaining_nonunit_entries` inside the loop whenever a clause is eliminated.
  remaining_entries = which(!eliminated)[order(lengths(entries[!eliminated]), decreasing = TRUE)]
  remaining_nonunit_entries = remaining_entries[seq_len(length(remaining_entries) - length(unit_domains))]
  remaining_unit_entries = if (length(unit_domains)) remaining_entries[seq.int(length(remaining_entries) - length(unit_domains) + 1L, length(remaining_entries))]

  # iterating from large to small entries
  for (clause_idx in remaining_nonunit_entries) {
    clause = entries[[clause_idx]]

    # index inside `is_not_subset_of`
    meta_idx = available_inverse[[clause_idx]]
    remaining_other_entries = remaining_nonunit_entries[remaining_nonunit_entries != clause_idx]

    # we keep track of which clauses were used for HLA, since every other clause can only do that once.
    was_used = logical(length(remaining_other_entries))

    # for each of the other clauses, how many of their symbols are not a subset of the current clause?
    not_subset_count_current = not_subset_count[available_inverse[remaining_other_entries], meta_idx]
    # for (roe_idx in seq_along(remaining_other_entries)) {
    #   # if we had set up is_not_subset_of as a 3-dim array, then the following could just have been a colSums over is_not_subset_of[, meta_idx, ]
    #   # However, that array would take a lot of memory: N_clauses^2 x N_all_symbols; The current setup has N_clauses^2 x max(N_symbols_per_clause)
    #   other_clause_idx = remaining_other_entries[[roe_idx]]
    #   meta_idx_other = available_inverse[[other_clause_idx]]
    #   not_subset_count[[roe_idx]] = sum(is_not_subset_of[[meta_idx_other]][meta_idx, ])
    # }
    repeat {
      # we do HLA by looking for a clause that is a subset of the current clause w/r/t all except one symbol.
      # We also only consider every clause at most once, since it won't have anything else to add a second time.
      # No need to check for == 0 here: if it was the case before, then we eliminated the clause already. If it becomes the case during the loop, we check
      # for it further down.
      hla_clause_idx = match(TRUE, not_subset_count_current == 1L & !was_used)
      if (is.na(hla_clause_idx)) break  # no more clauses to consider: all are either used or have no_subset_count > 1
      clause_idx_other = remaining_other_entries[[hla_clause_idx]]
      meta_idx_other = available_inverse[[clause_idx_other]]
      symbol = colnames(is_not_subset_of[[meta_idx_other]])[is_not_subset_of[[meta_idx_other]][meta_idx, ]]
      range_old = clause[[symbol]]
      # new range of the symbol inside the clause: Union of old range and complement of that symbol's range in the other clause.
      range_new = c(range_old, char_setdiff(universe[[symbol]], c(range_old, entries[[clause_idx_other]][[symbol]])))
      if (length(range_new) == length(universe[[symbol]])) {
        # hidden tautology elimination

        # We still need the symbol registry (see below), so this can not be replaced by just `eliminated[[clause_idx]] = TRUE`
        eliminate_clause_update_sr(clause_idx)
        remaining_nonunit_entries = remaining_other_entries
        break
      }
      # Now loop over clauses that also use the symbol, checking if they have one less `not_subset_count_current` because of that
      # Note we do not need to check subsumption w/r/t units any more: All symbols are proper subsets w/r/t units at this point,
      # so if adding the complement of one symbol to another made it a superset of a unit, the result was a tautology already.
      for (updating_clause_meta_idx in available_inverse[symbol_registry[[symbol]]]) {
        # we don't want to skip 'was_used' here, since we might still do hidden subsumption elimination with them.
        if (is_not_subset_of[[updating_clause_meta_idx]][meta_idx, symbol] &&
              all(entries[[available[[updating_clause_meta_idx]]]][[symbol]] %in% range_new)) {
          is_not_subset_of[[updating_clause_meta_idx]][meta_idx, symbol] = FALSE
          roe_idx = match(available[[updating_clause_meta_idx]], remaining_other_entries)  # I wished I didn't have to do this
          not_subset_count_current[[roe_idx]] = not_subset_count_current[[roe_idx]] - 1L
          if (not_subset_count_current[[roe_idx]] == 0L) {
            # hidden subsumption elimination
            eliminate_clause_update_sr(clause_idx)
            remaining_nonunit_entries = remaining_other_entries
            break
          }
        }
      }
      if (eliminated[[clause_idx]]) break  # the break inside the for-loop above will not break the repeat loop
      was_used[[hla_clause_idx]] = TRUE
      clause[[symbol]] = range_new
    }
  }

  # We now do the same thing as above, only for units.
  # Units are different, because they don't have the `is_not_subset_of` matrices. These are trivially constructed from
  # The symbol registry: "is_not_subset_of" for unit relating to symbol `s` is TRUE for all symbols in a clause that are not `s`.
  # We deliberately make use of `remaining_nonunit_entries` here, which has been updated in the loop above as nonunit-clauses were eliminated
  delayedAssign("roe_inverse", match(seq_along(entries), remaining_nonunit_entries))  # only do this match()-call if it is actually needed
  for (clause_idx in remaining_unit_entries) {
    clause = entries[[clause_idx]]
    unitsymbol = names(clause)
    was_used = logical(length(remaining_nonunit_entries))
    # every clause that is not yet eliminated is a proper subset of the current unit.
    # If it is length N and contains the unit symbol, then N - 1 of its symbols are not subsets;
    # otherwise (if it does not contain the unit symbol), all N symbols are not subsets.
    not_subset_count = lengths(entries[remaining_nonunit_entries]) - (remaining_nonunit_entries %in% symbol_registry[[unitsymbol]])

    # corresponding to `is_not_subset_of` matrix; only construct this for entries where it is needed.
    is_not_subset_of_unit = vector("list", length(remaining_nonunit_entries))

    # The following loop is mostly identical to the loop above, only for units.
    repeat {
      hla_clause_idx = match(TRUE, not_subset_count == 1L & !was_used)
      if (is.na(hla_clause_idx)) break  # no more clauses to consider: all are either used or have no_subset_count > 1
      clause_idx_other = remaining_nonunit_entries[[hla_clause_idx]]
      clause_other = entries[[clause_idx_other]]
      if (is.null(is_not_subset_of_unit[[hla_clause_idx]])) {
        is_not_subset_of_unit[[hla_clause_idx]] = structure(names(clause_other) != unitsymbol, names = names(clause_other))
      }
      is_not_subset_entry = is_not_subset_of_unit[[hla_clause_idx]]
      symbol = names(is_not_subset_entry)[is_not_subset_entry]
      range_old = clause[[symbol]]
      range_new = c(range_old, char_setdiff(universe[[symbol]], c(range_old, clause_other[[symbol]])))
      if (length(range_new) == length(universe[[symbol]])) {
        # hidden tautology elimination
        # no more need to update the unit registry
        eliminated[[clause_idx]] = TRUE
        break
      }
      # Now loop over clauses that also use the symbol, checking if they have one less `not_subset_count` because of that
      for (updating_clause_idx in symbol_registry[[symbol]]) {
        updating_hla_clause_idx = roe_inverse[[updating_clause_idx]]
        if (is.null(is_not_subset_of_unit[[updating_hla_clause_idx]])) {
          # Dynamically construct this entry if it is not set yet.
          # We could delay this further, but that would make things even more complicated
          updating_clause = entries[[updating_clause_idx]]
          is_not_subset_of_unit[[updating_hla_clause_idx]] = structure(names(updating_clause) != unitsymbol, names = names(updating_clause))
        }
        if (is_not_subset_of_unit[[updating_hla_clause_idx]][[symbol]] &&
              all(entries[[updating_clause_idx]][[symbol]] %in% range_new)) {
          is_not_subset_of_unit[[updating_hla_clause_idx]][[symbol]] = FALSE
          not_subset_count[[updating_hla_clause_idx]] = not_subset_count[[updating_hla_clause_idx]] - 1L
          if (not_subset_count[[updating_hla_clause_idx]] == 0) {
            # hidden subsumption elimination
            eliminated[[clause_idx]] = TRUE
            break
          }
        }
      }
      if (eliminated[[clause_idx]]) break  # the break inside the for-loop above will not break the repeat loop
      was_used[[hla_clause_idx]] = TRUE
      clause[[symbol]] = range_new
    }
  }

  return_entries(entries[!eliminated])
}
