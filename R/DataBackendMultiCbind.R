

#' @export
DataBackendMultiCbind = R6Class("DataBackendMultiCbind", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(bs) {
      assert_list(bs, min.len = 1)
      lapply(bs, assert_backend)

      formats = Reduce(intersect, map(bs, "data_formats"))

      private$.colnames = unique(unlist(map(bs, "colnames")))

      # primary key: if all backends have the same pk, just use that one.
      otherpk = unique(unlist(map(bs, "primary_key")))
      if (length(otherpk) == 1) {
        pk = otherpk
      } else {
        # otherwise: introduce a new primary key that is completely different from the previous ones.
        pk = "..row_id"
        index = 0
        while (pk %in% private$.colnames) {
          index = index + 1
          pk = paste0("..row_id.", index)
        }
        private$.colnames = c(private$.colnames, pk)
      }

      super$initialize(list(bs = rev(bs)), pk, formats)
    },
    data = function(rows, cols, data_format = "data.table") {
      bs = private$.data$bs

      urows = unique(rows)

      datas = list()
      pks = character(length(bs))
      include_pk = logical(length(bs))
      cols_remaining = cols
      allrows = list()
      for (i in seq_along(bs)) {
        ## Not doing 'if (length(cols_remaining)) break' because there could still be tables remaining that add rows
        pk = bs[[i]]$primary_key
        pks[[i]] = pk
        include_pk[[i]] = pk %in% cols_remaining
        if (include_pk[[i]]) {
          datas[[i]] = bs[[i]]$data(urows, cols_remaining, data_format = data_format)
          cols_remaining = setdiff(cols_remaining, colnames(datas[[i]]))
        } else {
          datas[[i]] = bs[[i]]$data(urows, c(pk, cols_remaining), data_format = data_format)
          cols_remaining = setdiff(cols_remaining, colnames(datas[[i]])[-1])
        }
        allrows[[i]] = datas[[i]][[pk]]
      }
      presentrows = unique(unlist(allrows))
      join = list(presentrows)
      result = do.call(cbind, pmap(list(datas, pks, include_pk), function(data, pk, include) {
        if (include) {
          result = data[join, on = pk, nomatch = NA]
          set(result, result[[pk]] %nin% data[[pk]], pk, NA)
        } else {
          data[join, -pk, on = pk, with = FALSE, nomatch = NA]
        }
      }))
      sbk = self$primary_key

      set(result, , sbk, presentrows)
      join = list(rows)
      result[join, intersect(cols, colnames(result)), with = FALSE, on = sbk, nomatch = NULL]
    },
    head = function(n = 6L) {
      rows = head(self$rownames, n)
      self$data(rows = rows, cols = self$colnames)
    },
    distinct = function(rows, cols, na_rm = TRUE) {
      bs = private$.data$bs
      getpk = self$primary_key %in% cols
      reslist = list()
      remaining_cols = cols
      if (!na_rm || getpk) {
        rows = intersect(rows, self$rownames)
      }
      for (i in seq_along(bs)) {
        if (!length(remaining_cols)) break
        reslist[[i]] = bs[[i]]$distinct(rows = rows, cols = cols, na_rm = na_rm)
        remaining_cols = setdiff(remaining_cols, names(reslist[[i]]))
        if (!na_rm && !all(rows %in% bs[[i]]$rownames)) {
          reslist[[i]] = map(reslist[[i]], function(x) if (any(is.na(x))) x else c(x, NA))
        }
      }
      result = unlist(reslist, recursive = FALSE)
      if (getpk) {
        result[[self$primary_key]] = rows
      }
      result[match(cols, names(result), nomatch = 0)]
    },
    missings = function(rows, cols) {
      rows = rows[rows %in% self$rownames]
      bs = private$.data$bs
      getpk = self$primary_key %in% cols
      reslist = list()
      remaining_cols = cols
      for (i in seq_along(bs)) {
        if (!length(remaining_cols)) break
        missingrows = sum(rows %nin% bs[[i]]$rownames)
        reslist[[i]] = bs[[i]]$missings(rows, remaining_cols) + missingrows
        remaining_cols = setdiff(remaining_cols, names(reslist[[i]]))
      }
      result = unlist(reslist)
      if (self$primary_key %in% cols) {
        result[[self$primary_key]] = 0L
      }
      result[match(cols, names(result), nomatch = 0)]
    }
  ),
  active = list(
    rownames = function() {
      if (is.null(private$.rownames_cache)) private$.rownames_cache = unique(unlist(rev(map(private$.data$bs, "rownames"))))
      private$.rownames_cache
    },
    colnames = function() {
      private$.colnames
    },
    nrow = function() length(self$rownames),
    ncol = function() length(self$colnames)
  ),
  private = list(
    .rownames_cache = NULL,
    .colnames = NULL,
    .calculate_hash = function() {
      do.call(calculate_hash, private$.data$bs)
    }
  )
)
