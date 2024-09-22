

simplify_cnf_broken = function(entries, universe) {
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
  symbol_registry_ext = NULL  # see further down

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
    # it may be possible that we hidden subsumption eliminate a unit but get a new unit; for this case, we check whether ur is eliminated.
    # In the eliminated-and-new-unit case, we do not need to worry about intersecting ranges, since the new unit will have been made a subset
    # through unit propagation at this point.
    if (is.null(ur) || eliminated[[ur]]) {
      unit_registry[[nu]] = unit_idx  # environment assignment
      unit_domains[[nu]] = unit[[1L]]  # environment assignment
      # this is init'd TRUE for those w/ length 1 at the start, we set it to TRUE for clauses where symbols were eliminated
      is_unit[[unit_idx]] <<- TRUE
    } else {
      # two units refer to the same symbol
      # -> we keep the one we saw before but update it to the intersection.
      # the current one is eliminated
      prev_unit = unit_domains[[nu]]
      # if we reached the is_not_subset_of part, we have the symbol registry set up and do unit propagation already, so
      # we know the new unit is a subset of the old one.
      unit_isct = if (is.null(is_not_subset_of)) prev_unit[prev_unit %in% unit[[1L]]] else unit[[1L]]
      if (!length(unit_isct)) return(TRUE)  # signal that we have a contradiction and can exit
      entries[[ur]][[1L]] <<- (unit_domains[[nu]] = unit_isct)  # update the *old* unit and unit_domains in one go. unit_domains is updated by-reference (environment)
      eliminate_clause_update_sr(unit_idx)
      unit_idx = ur
    }
    update_unit_range(unit_idx, nu)
  }

  update_unit_range = function(unit_idx, unit_name) {
    # The symbol registry is empty at the start; the following happens when new units are added later
    use_inso = FALSE
    if (!is.null(is_not_subset_of)) {
      # if we have the is_not_subset_of matrix, we can use it to skip some checks
      unit_idx_meta = available_inverse[[unit_idx]]
      use_inso = unit_idx_meta <= meta_idx_outer  # .. but only if we have built the matrix up to the current index
      if (use_inso) {
        # if the unit is a subset of the other clause, we will never skip.
        inso_column = is_not_subset_of[[unit_idx_meta]][, unit_name]
      }
    }
    for (s_clause_idx in symbol_registry[[unit_name]]) {
      # could have been eliminated by subsumption elimination during unit propagation of another clause
      # (we are not afraid of is_unit here, since we are the unit for the symbol here -- everything else that has the
      # symbol gets eliminated)
      if (eliminated[[s_clause_idx]]) next
      if (use_inso) {
        # if we already know that s_clause_idx[[nu]] is a subset of unit_idx, we can skip this
        # exception: unit_idx is *also* a subset, implying equality, in which case we will do subsumption elimination.
        s_clause_idx_meta = available_inverse[[s_clause_idx]]
        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, unit_name]) next
      }
      adr = apply_domain_restriction(s_clause_idx, unit_name, unit_domains[[unit_name]], TRUE)
      if (identical(adr, TRUE)) return(TRUE)  # forward contradiction signal
    }
    if (!is.null(symbol_registry_ext)) {
      ## we do not need to do unit propagation for the plus clauses, but we *do* need to check for unit subsumption.
      # We do this here since eliminate_symbol_from_clause will not do this when a unit was generated.
      unit_meta_idx = available_inverse[[unit_idx]]
      for (meta_idx_plus in symbol_registry_ext[[unit_name]]) {
        if (meta_idx_plus == unit_meta_idx) next
        if (all(unit_domains[[unit_name]] %in% entries_ext[[meta_idx_plus]][[unit_name]])) {
          # we are a subset of the other clause
          eliminate_clause_update_sr(available_ext[[meta_idx_plus]])
        }
      }
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
      # If the lengths match, then the clause is a superset of the restringent (since the intersections are equal)
      # the clause is therefore subsumed by the restringent -- unit elimination or (hidden) subsumption elimination
      eliminate_clause_update_sr(clause_idx)
      return(NULL)
    }
    # clause was not changed
    if (length(clause[[symbol_idx]]) == clause_symbol_length_before) return(FALSE)
    if (!length(clause[[symbol_idx]])) {
      # the symbol is not in the clause anymore
      if (is_unit[[clause_idx]]) return(TRUE)  # unit contradiction
      return(eliminate_symbol_from_clause(clause_idx, symbol))
    }
    # clause was changed.
    # We don't need to do this when we go the eliminate_symbol_from_clause route, since that will update the clause in any case.
    entries[[clause_idx]] <<- clause

    # We need to update the subset relations matrix, if it exists
    if (is.null(is_not_subset_of)) return(FALSE)

    if (is_unit[[clause_idx]]) {
      # restricting units only happens if the unit has a clause+, so we
      # only worry about this once is_not_subset_of exists
      # update_unit_range will check updated subset relations for unit subsumption elimination
      return(update_unit_range(clause_idx, symbol))
    }

    meta_idx = available_inverse[[clause_idx]]
    # meta_idx_outer is the index up to which we have built is_not_subset_of; if meta_idx > meta_idx_outer, then we haven't seen the clause yet.
    # we are not relying on the fact that as soon as is_not_subset_of is non-null, we will only get here for clauses for which
    # is_not_subset_of was built, because unit-propagation could be cascading from a unit elimination.
    if (meta_idx > meta_idx_outer) return(FALSE)

    ### TODO: I *THINK* this is not needed, since we only care about subset relations between clause and clause+, which only ever updates towards FALSE.
    # if (!is_unit_propagation) {
    #   # we are not propagating units, so we need to update the subset relations in the other direction.
    #   # we do this first, in case on_updated_subset_relations gets too eager later
    #   idx_to_check = which(available[seq_len(meta_idx_outer)] %in% symbol_registry[[symbol]])
    #   for (other_meta_idx in idx_to_check) {
    #     # was the other a subset of us before? that could have changed now.
    #     if (!is_not_subset_of[[other_meta_idx]][meta_idx, symbol] && !all(entries[[available[[other_meta_idx]]]][[symbol]] %in% entries[[clause_idx]][[symbol]])) {
    #       is_not_subset_of[[other_meta_idx]][meta_idx, symbol] <<- TRUE
    #       # no need to call on_updated_subset_relations, since we are increasing rowsums, not decreasing them
    #     }
    #   }
    # }

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
    #  --> hence `available <= meta_idx_outer`
    # * We do not need to check that meta_idx != other_meta_plus, since that row is initialized as FALSE
    rows_to_check = which(is_not_subset_of[[meta_idx]][, is_not_subset_of_col] & available_ext %in% symbol_registry_ext[[symbol]] & !is.na(not_subset_count[meta_idx, ]))
    for (meta_idx_plus in rows_to_check) {
      clause_idx_other = available_ext[[meta_idx_plus]]
      # while we only get here for rows that are in the symbol_registry and therefore not eliminated / units, we need to check again since on_updated_subset_relations()
      # can change this during the loop.
      if (eliminated[[clause_idx_other]]) next
      if (!is_not_subset_of[[meta_idx]][meta_idx_plus, is_not_subset_of_col]) next  # check again, since on_updated_subset_relations() can change this

      # when we get here, that means that the symbol exists both in clause_idx and in other_ref_this symbol, *and* we have seen both clauses
      # in the is_not_subset_of building process.
      # If this were not the case, then we would not need to change anything.
      other_clause_range = entries_ext[[meta_idx_plus]][[symbol]]
      if (!all(entries[[clause_idx]][[symbol]] %in% other_clause_range)) next

      # we are now a subset and were not before. We know this, since we check that is TRUE *inside the loop*
      is_not_subset_of[[meta_idx]][meta_idx_plus, is_not_subset_of_col] <<- FALSE
      not_subset_count[meta_idx, meta_idx_plus] <<- (rowsum = not_subset_count[meta_idx, meta_idx_plus] - 1L)
      if (rowsum > 2L) next
      ousr = on_updated_subset_relations(meta_idx, meta_idx_plus, rowsum)
      if (identical(ousr, TRUE)) return(TRUE)  # forward contradiction signal. We don't care if meta_idx_plus was eliminated.
      if (eliminated[[clause_idx]]) return(NULL)  # need to check if things escalated somehow and clause_idx was eliminated indirectly
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
    rows_changed = which(is_not_subset_of[[meta_idx]][, is_not_subset_of_col] & available_ext %in% symbol_registry_ext[[symbol]] & !is.na(not_subset_count[meta_idx, ]))
    not_subset_count[meta_idx, rows_changed] <<- not_subset_count[meta_idx, rows_changed] - 1L
    is_not_subset_of[[meta_idx]][, is_not_subset_of_col] <<- FALSE
    ## TODO: I think this is not necessary, since we only care about subset relations between clause and clause+, which only ever updates towards FALSE.
    # for (others_ref_this_symbol in available_inverse[sr]) {
    #   if (others_ref_this_symbol <= meta_idx_outer) {  # meta_idx_outer is the index up to which we have built is_not_subset_of
    #     is_not_subset_of[[others_ref_this_symbol]][meta_idx, symbol] <<- TRUE
    #   }
    # }
    # we could have some leftover TRUEs from eliminated or unit-ed clauses

    for (meta_idx_plus in rows_changed) {
      ## TODO: maybe the following comment is obsolete, see above
      # We have to do this *after* we set the corresponding values to TRUE for others_ref_this_symbol,
      # since calling this could realistically change the symbol registry (e.g. if it leads to a symbol
      # being eliminated from other clauses).
      clause_idx_other = available_ext[[meta_idx_plus]]
      # while we *do* restrict meta_idx_other in the for loop already, eliminated and is_unit can still change during the loop, so check here again
      if (eliminated[[clause_idx_other]]) next
      rowsum = not_subset_count[meta_idx, meta_idx_plus]
      if (rowsum > 2L) next
      ousr = on_updated_subset_relations(meta_idx, meta_idx_plus, rowsum)
      if (identical(ousr, TRUE)) return(TRUE)
      # on_updated_subset_relations could cascade down to eliminating meta_idx (i.e. clause_idx)
      if (eliminated[[clause_idx]]) return(NULL)
    }
    FALSE
  }

  apply_hidden_literal_addition = function(meta_idx_plus, symbol, other_range) {
    fullrange = universe[[symbol]]
    # The following is not necessary; the plus-clauses are only imaginary, and everything
    # for which we add the complement will be a proper subset of the unit.
    #> fullrange = unit_domains[[symbol]]
    #> if (is.null(fullrange)) fullrange = universe[[symbol]]
    range_old = entries_ext[[meta_idx_plus]][[symbol]]

    # hidden literal addition: we add the complement of the restricting symbol to the other clause.
    if (is.null(range_old)) {
      range_new = char_setdiff(fullrange, other_range)
      # add to symbol_registry_ext
      if (is.null(symbol_registry_ext[[symbol]])) {
        symbol_registry_ext[[symbol]] = meta_idx_plus
      } else {
        symbol_registry_ext[[symbol]][[length(symbol_registry_ext[[symbol]]) + 1L]] = meta_idx_plus
      }
    } else {
      range_new = c(range_old, char_setdiff(fullrange, c(range_old, other_range)))
      if (length(range_new) == length(fullrange)) {
        # hidden tautology elimination
        eliminate_clause_update_sr(available_ext[[meta_idx_plus]])
        return(NULL)
      }
    }
    if (length(range_new) == length(range_old)) return(FALSE)

    entries_ext[[meta_idx_plus]][[symbol]] <<- range_new

    # update subset relations matrix. HLA only starts when the matrix exists, so no need to check for is.null(is_not_subset_of)
    other_ref_symbol = available_inverse[symbol_registry[[symbol]]]

    # don't bother updating the ones we have not seen yet
    for (meta_idx_other in other_ref_symbol[!is.na(not_subset_count[other_ref_symbol, meta_idx_plus]) & other_ref_symbol != meta_idx_plus]) {
      idx_other = available[[meta_idx_other]]
      if (eliminated[[idx_other]] || is_unit[[idx_other]]) next
      if (!is_not_subset_of[[meta_idx_other]][meta_idx_plus, symbol]) next

      if (all(entries[[idx_other]][[symbol]] %in% entries_ext[[meta_idx_plus]][[symbol]])) {
        # we are a new subset of the other clause
        is_not_subset_of[[meta_idx_other]][meta_idx_plus, symbol] <<- FALSE
        not_subset_count[meta_idx_other, meta_idx_plus] <<- (rowsum = not_subset_count[meta_idx_other, meta_idx_plus] - 1L)
        if (rowsum > 2L) next
        ousr = on_updated_subset_relations(meta_idx_other, meta_idx_plus, rowsum)
        if (is.null(ousr)) return(NULL)
        if (ousr) return(TRUE)
      }
    }

    FALSE
  }

  # returns 'NULL' when meta_idx_other was eliminated (or possibly turned into a unit),
  # 'TRUE' for contradiction, 'FALSE' for nothing changed
  on_updated_subset_relations = function(meta_idx, meta_idx_plus, rowsum) {
    # the row `meta_idx_plus` of `is_not_subset_of[[meta_idx]]` has been updated.
    # we now check whether we can apply subsumption elimination, or at least self-subsumption elimination, on `meta_idx`.
    # rowsum = sum(is_not_subset_of[[meta_idx]][meta_idx_plus, ])
    if (rowsum > 2L) return(FALSE)  # nothing to do
    if (rowsum == 0L) {
      eliminate_clause_update_sr(available_ext[[meta_idx_plus]])
      return(NULL)
    }
    if (rowsum == 1L) {
      symbol_to_restrict = colnames(is_not_subset_of[[meta_idx]])[is_not_subset_of[[meta_idx]][meta_idx_plus, ]]
      range_restricting = entries[[available[[meta_idx]]]][[symbol_to_restrict]]

      ahla = apply_hidden_literal_addition(meta_idx_plus, symbol_to_restrict, range_restricting)
      if (is.null(ahla)) return(NULL)
      if (ahla) return(TRUE)
      return(FALSE)
      # update clause and sc!
      # note we update meta_idx_plus: we are currently subset of that other clause w/r/t all except one symbol.
      # we can therefore intersect that last symbol in the other clause with the range of that symbol in the current clause.
      # apply_domain_restriction will take care of eliminating the symbol if the range becomes empty.
      # It could in theory even do subsumption, but we have already taken care of that above.
      return(apply_domain_restriction(available_ext[[meta_idx_plus]], symbol_to_restrict, range_restricting, FALSE))
    }
    # rowsum == 2
    # TODO
    FALSE
  }

  # mark a clause as eliminated and update symbol registry
  # This is relatively safe to call, since it does not modify any other clauses and does not create new units or subset relationships.
  eliminate_clause_update_sr = function(clause_idx) {
    eliminated[[clause_idx]] <<- TRUE
    # we could actually be a unit, after hidden literal addition. In that case,
    # the unit is not entered in the symbol_registry, but it is in the symbol_registry_ext.
    if (!is_unit[[clause_idx]]) {
      for (s in names(entries[[clause_idx]])) {
        sr = symbol_registry[[s]]
        symbol_registry[[s]] = sr[sr != clause_idx]
      }
    }
    if (!is.null(symbol_registry_ext)) {
      meta_idx = available_inverse[[clause_idx]]
      for (s in names(entries[[clause_idx]])) {
        sr = symbol_registry_ext[[s]]
        symbol_registry_ext[[s]] = sr[sr != meta_idx]
      }
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
  available_ext = c(available, which(is_unit))
  available_inverse = match(seq_along(entries), available_ext)

  # record for each clause whether it is a subset of another clause.
  # we record this for each symbol separately, so when a symbol gets self-subsumed, we can re-check others more quickly.
  # Default is FALSE (not not a subset, i.e. it is a subset), this aligns with absent columns (which are subsets of everything)
  #
  # note that as soon as is_not_subset_of is non-null, some other functions assume that meta_idx_outer is also defined.
  is_not_subset_of = vector("list", length(available))

  # not_subset_count[i, j]: how many symbols of clause i are not subset of clause_ext j?
  # if this is 1, we can do self-subsumption elimination and HLA.
  # if this is 2, we can do self-subsumption with a resolution.
  not_subset_count = matrix(NA_integer_, nrow = length(available), ncol = length(available_ext))

  # symbol registry for 'extended' clauses
  symbol_registry_ext = new.env(hash = TRUE, parent = emptyenv(), size = length(length(symbol_registry) + 10))
  for (n in names(symbol_registry)) {
    symbol_registry_ext[[n]] = available_inverse[symbol_registry[[n]]]
  }

  for (su in names(unit_registry)) {
    unit_idx = available_inverse[[unit_registry[[su]]]]
    if (is.null(symbol_registry_ext[[su]])) {
      symbol_registry_ext[[su]] = unit_idx
    } else {
      symbol_registry_ext[[su]][[length(symbol_registry_ext[[su]]) + 1L]] = unit_idx
    }
  }

  entries_ext = entries[available_ext]

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
      NA,
      nrow = length(available_ext),  # indexed by meta_idx!
      ncol = length(clause),
      dimnames = list(NULL, names(clause))
    )
    # we are not a subset of ourselves
    # (this prevents us from checking this later in on_updated_subset_relations; if we do check it there, we would eliminate the clause from itself)
    is_not_subset_of[[meta_idx_outer]][meta_idx_outer, ] = FALSE

    not_subset_count[meta_idx_outer, meta_idx_outer] = 0L
  }

  for (meta_idx_outer in seq_along(available)) {
    clause_idx = available[[meta_idx_outer]]
    if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) next  # may happen if we somehow turn something into a unit that eliminates a later clause

    clause = entries[[clause_idx]]
    colnames_inso = colnames(is_not_subset_of[[meta_idx_outer]])
    for (inso_idx in seq_along(colnames_inso)) {
      symbol = colnames_inso[[inso_idx]]
      unit_idx = unit_registry[[symbol]]
      if (!is.null(unit_idx)) {
        meta_idx_plus = available_inverse[[unit_idx]]
        is_not_subset_of[[meta_idx_outer]][meta_idx_plus, ] = TRUE
        if (symbol %in% names(clause)) {
          is_not_subset_of[[meta_idx_outer]][meta_idx_plus, symbol] = FALSE
          not_subset_count[meta_idx_outer, meta_idx_plus] = rowsum = length(colnames_inso) - 1L
        } else {
          not_subset_count[meta_idx_outer, meta_idx_plus] = rowsum = length(colnames_inso)
        }
        if (rowsum > 2L) next
        ousr = on_updated_subset_relations(meta_idx_outer, meta_idx_plus, rowsum)
        if (identical(ousr, TRUE)) return(return_entries(FALSE))
        if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) break
      }
    }
    if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) break

    for (meta_idx_plus in seq_along(available)) {
      if (meta_idx_plus == meta_idx_outer) next
      clause_idx_inner = available[[meta_idx_plus]]

      if (eliminated[[clause_idx_inner]] || !is.na(not_subset_count[meta_idx_outer, meta_idx_plus])) next
      clause_ext_inner = entries_ext[[meta_idx_plus]]
      # note that even though we sort entries at the beginning, we can *not* rely on clause_inner to have smaller or equal length than clause,
      # since unit elimination can change the length of clauses

      ## need to refresh clause here, since it might have been shortened by unit propagation in an earlier iteration
      clause = entries[[clause_idx]]

      sci = names(clause_ext_inner)
      sci_sc_map = match(sci, names(clause), nomatch = 0L)
      sci_names_in_common = which(sci_sc_map != 0L)
      is_not_subset_of[[meta_idx_outer]][meta_idx_plus, !colnames(is_not_subset_of[[meta_idx_outer]]) %in% sci[sci_names_in_common]] = TRUE

      # symbols that are not in common trivially get the matrix entry TRUE
      # (they are not subsets of their counterpart in the other clause, since the other ones are empty)
      for (symbol_idx_inner in sci_names_in_common) {
        symbol_idx_outer = sci_sc_map[[symbol_idx_inner]]
        symbol = sci[[symbol_idx_inner]]
        if (!is.na(is_not_subset_of[[meta_idx_outer]][meta_idx_plus, symbol])) next

        range_inner = clause_ext_inner[[symbol_idx_inner]]
        range_outer = clause[[symbol_idx_outer]]

        outer_subset_of_inner = all(range_outer %in% range_inner)
        inner_subset_of_outer = outer_subset_of_inner && meta_idx_plus > meta_idx_outer && !is_unit[[clause_idx_inner]] && length(range_outer) == length(range_inner)
        if (inner_subset_of_outer) {
          symbol_idx_inner = match(symbol, colnames(is_not_subset_of[[meta_idx_plus]]))
          if (!is.na(symbol_idx_inner) && is.na(is_not_subset_of[[meta_idx_plus]][meta_idx_outer, symbol_idx_inner])) {
            # We only assign FALSE here if we find equality (which we get for basically free from range_outer %in% range_inner && lengths are equal).
            # Otherwise we leave the entry NA and set this value when meta_idx_plus is processed in the outer loop.
            # We index by column name here, since it is possible that clauses were shortened by
            # unit propagation or self subsumption elimination somewhere on the way here
            is_not_subset_of[[meta_idx_plus]][meta_idx_outer, symbol_idx_inner] = FALSE
          }
        }
        is_not_subset_of[[meta_idx_outer]][meta_idx_plus, symbol] = !outer_subset_of_inner
      }
      # prefer to eliminate the outer loopo clause first, since we have already
      # done more work for the inner loop clause (which comes earlier in 'entries')
      rowsum = sum(is_not_subset_of[[meta_idx_outer]][meta_idx_plus, ])
      not_subset_count[meta_idx_outer, meta_idx_plus] = rowsum
      if (rowsum > 2L) next
      ousr = on_updated_subset_relations(meta_idx_outer, meta_idx_plus, rowsum)
      if (identical(ousr, TRUE)) return(return_entries(FALSE))
      if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) break  # yes this can happen.
    }
  }

  return_entries(entries[!eliminated])
}
