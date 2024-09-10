




simplify_cnf = function(entries, universe) {
  return_false = structure(
    FALSE,
    universe = universe,
    class = "CnfFormula"
  )

  # can we do unit elimination?
  # if we are already TRUE or FALSE no simplification is necessary
  # this only works if there actually are units.
  can_simplify = !is.logical(entries)
  # likewise, if there is only one clause left, no simplification is necessary.

  units = list()

  while (can_simplify && length(entries) > 1) {
    # we do the following until we are sure there are no more simplifications to be made.
    # this is the case if we have not meaningfully simplified anything in the last iteration.
    can_simplify = FALSE
    # sort clauses by length, since (1) length-1-clauses are special, and (2) short clauses can only ever subsume longer ones
    entries = entries[order(lengths(entries))]
    # Let sum(A) be the symbols in clause A, and val(A, s) be the values of symbol s admitted in clause A.
    if (any(lengths(entries) == 1)) {
      for (i in seq_along(entries)) {
        ei = entries[[i]]
        # If |sym(A)| == 1, sym(A) == {s} and s is in sym(B), then val(B, s) <- intersect(val(A, s), val(B, s)) ("unit propagation")
        # units is a named list of symbols in size-one-clauses, together with their values.
        # We iterate over all symbols in ei that are also in units, and intersect their values.
        for (unit_symbol in intersect(names(ei), names(units))) {
          length_before_ei = length(ei[[unit_symbol]])
          length_before_unit = length(units[[unit_symbol]])
          intersection = intersect(units[[unit_symbol]], ei[[unit_symbol]])
          ei[[unit_symbol]] = intersection
          if (length(ei) == 1 && length(intersection) != length_before_unit) {
            # we made a unit shorter, this means we need to simplify the entry from which units[[unit_symbol]] came
            can_simplify = TRUE
          }
          if (!length(ei[[unit_symbol]])) {
            ei[[unit_symbol]] = NULL
          }
          if (!length(ei)) {
            return(return_false)
          }
          if (length(intersection) != length_before_ei) {
            # need to store changed ei entry only if its length changed; otherwise we know the intersection did not do anything.
            entries[[i]] = ei
          }
        }
        if (length(ei) == 1) {
          # even if names(ei) is already in units, at this point ei[[1]] is the intersection of the values
          units[[names(ei)]] = ei[[1]]
        }
      }
      if (can_simplify) next
    }

    entries = entries[order(lengths(entries))]  # removing units may have changed the order
    eliminated = logical(length(entries))
    for (i in seq_along(entries)) {
      if (eliminated[[i]]) next  # can only happen if we do the hidden subsumption elimination, which searches forward
      ei = entries[[i]]
      # If sym(A) is a subset of sym(B) and for each s in sym(A), val(A, s) is a subset of val(B, s), then A implies B, so B can be removed ("subsumption elimination").
      for (j in seq_len(i - 1)) {
        if (eliminated[[j]]) next
        ej = entries[[j]]
        name_overlap = names(ej) %in% names(ei)
        if (all(name_overlap) && all(sapply(names(ej), function(s) all(ej[[s]] %in% ei[[s]])))) {
          # can't do entries[[i]] = NULL, since we are iterating over entries; the entries[[i]] would break.
          eliminated[[i]] = TRUE
          break
        }
        # simple self subsumption and hidden subsumption elimination
        # HSE:
        # if s is in sym(A) and sym(B), and val(A, s) and val(B, s) are disjoint, then (A - s | B - s) is implied,
        # and all superset clauses of (A - s | B - s) can be removed.
        # we could also do this for higher order terms, intersection over X of val(X, s) == 0 etc., but this gets more complex.
        # tbh, I am not sure if this is actually worth it
        # SSE: If val(A, s) and val(B, s) are disjoint, and A - s is a subset of B - s, then A & B <=> A & (B - s)

        which_name_overlap = which(name_overlap)
        # - build the union of values of overlapping symbols
        # - in the innerloop we will check that most of this is a subset of any other clause
        # - "most of" here means: all but the one symbol s where the values are disjoint
        # - use delayedAssign to avoid computation if there is no overlap with empty intersect
        delayedAssign("cnames", union(names(ei), names(ej)))

        # Putting ei[[s2]] first on purpose, since union() preserves the order of the first argument.
        # If and only if ej[-s] is a subset of ei[-s], then cunion[-s] will be the same as ei[-s].
        delayedAssign("cunion", sapply(cnames, function(s2) union(ei[[s2]], ej[[s2]]), simplify = FALSE))

        nameshift = 0  # in case we eliminate names from ej, we need to shift the indices for later loops iterations
        for (no in which_name_overlap) {
          s = names(ej)[[no - nameshift]]
          # intersection is not 0 --> try next one
          if (length(intersect(ej[[s]], ei[[s]]))) next
          cnames_s = setdiff(cnames, s)

          # Let's try self subsumption elimination first
          if (length(cnames) == length(ei)) {
            # cnames is the union of symbols in ei and ej.
            # If length(cnames) is different from max(length(ei), length(ej)), then ei and ej can not be subsets of one another.
            # Since j < i, length(ej) <= length(ei).

            # ej is usually smaller than ei, so often only ej can be a subset of ei.
            # They may, however, have the same number of terms, in which case the reverse is also possible.
            # Use 'c()' to drop attributes from ei here.
            ei_without_s = ei[cnames_s]
            do_sse = identical(cnames[cnames_s], c(ei_without_s))
            # if they have the same number of terms, ei could be a subset of ej.
            # However, then we can not rely on the order of values in the union being correct.
            do_sse_reverse = (!do_sse && (length(ei) == length(ej)) &&
                              all(sapply(cnames_s, function(s2) all(ei[[s2]] %in% ej[[s2]]))))
            if (do_sse_reverse) {
              # ei is a subset of ej
              # --> eliminate s from ej
              ej[[s]] = NULL
              new_entry = entries[[j]] = ej
              dont_eliminate = j
              nameshift = nameshift + 1
            } else if (do_sse) {
              # ej is a subset of ei
              # --> eliminate s from ei
              new_entry = entries[[i]] = ei_without_s
              dont_eliminate = i
            }

            if (do_sse || do_sse_reverse) {

              if (length(new_entry) == 1) {
                # we have a unit clause now

                # It could happen that we eliminate the entry further, in which
                # case units[[]] would get stale, but then we return_false anyway.
                units[[names(new_entry)]] = new_entry[[1]]
                can_simplify = TRUE  # do unit elimination all over again
                ## TODO: we could probably make this more efficient by only unit-eliminating the terms that are new
              }

              if (!length(new_entry)) {
                # we have a contradiction now
                # Maybe this could happen if we SS-Eliminate multiple symbols from a clause in a row.
                return(return_false)
              }

              # What can have happened now is that the newly created element can eliminate other elements.
              # If it can eliminate the current element i, that means it is a subset of ei, which only happens if ej\s is a subset of ei\s. However, in that case, we always want to keep element i.
              # We therefore only need to loop up to i - 1.
              # The loop is similar to the subsumption elimination loop; only in this case, we check if we can eliminate the elements that come before i, not like above where we check if we can eliminate i.

              # loop down from large to small, since then we can break once we find a clause with insufficient entries
              for (k in rev(seq_len(i - 1))) {
                if (k == dont_eliminate || eliminated[[k]]) next
                ek = entries[[k]]
                if (length(ek) < length(new_entry)) break  # once we reached entries that are shorter than the new_entry, we won't find anything longer any more and we can break
                if (all(cnames_s %in% names(ek)) && all(sapply(cnames_s, function(s2) all(new_entry[[s2]] %in% ek[[s2]])))) {
                  eliminated[[k]] = TRUE
                }
              }
              next
            }
          }
          # Hidden Subsumption Elimination
          #
          # We only do this if self subsumption elimination did not occur.
          #
          # We eliminate based on an implied clause (A - s | B - s)
          # Since we will forget about this clause after this iteration, we have to eliminate all clauses with size larger or equal to ||A | B|| - 1
          # loop down from large to small, since then we can break once we find a clause with insufficient entries
          for (k in rev(seq_along(entries))) {
            if (k == i || k == j || eliminated[[k]]) next
            ek = entries[[k]]
            # all of cnames_s must be in ek, otherwise we can't eliminate
            # since we are looping down from large to small ek, we can break once this is not the case
            if (length(ek) < length(cnames) - 1) break
            if (all(cnames_s %in% names(ek)) && all(sapply(cnames_s, function(s2) all(cunion[[s2]] %in% ek[[s2]])))) {
              eliminated[[k]] = TRUE
            }
          }
        }

      }

    }
    entries = entries[!eliminated]
  }
  structure(
    if (!length(entries)) TRUE else entries,
    universe = universe,
    class = "CnfFormula"
  )
}


## to test:
# subsumption
# unit elimination
# unit elimination makes large clause smaller, leading to additional u.e.
# unit elimination makes large clause smaller, leading to subsumption

# branch with totune is swallowed by single unbranch
# branch with totune is swallowed by multiple unbranch
# branches with totune can possibly lead to conflicts, but not always (multiple active inputs to unbranch, or mixed inputs to normal pipeops)
# branches with totune always lead to conflicts (multiple active inputs to unbranch, or mixed inputs to normal pipeops, or choice between both)
# can we recognize that a certain choice would lead to a conflict, making only other choices possible?

# why does this blow up?
#

# profvis::profvis(replicate(3000, { !!(((u$A %among% "T" | u$B %among% "F") & (u$A %among% "F" | u$C %among% "T") & (u$B %among% "T" | u$C %among% "F"))) ; NULL }) -> ., simplify = FALSE)

# This is because we don't eliminate (C | A) & (!C | B) & (B | A) by removing (B | A)
# so, if  clause X and clause Y have a symbol in common where the values are disjoint, the disjunction of the rest is implied?
