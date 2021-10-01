

#' @export
DataBackendJoin = R6Class("DataBackendJoin", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(bs) {
      assert_list(bs, min.len = 1)
      lapply(bs, assert_backend)

      formats = Reduce(intersect, map(bs, "data_formats"))

      super$initialize(list(bs = rev(bs)), bs[[1]]$primary_key, formats)
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
        include_pk = pk %in% cols_remaining
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
      result = do.call(rbind, pmap(list(datas, pks, include_pk), function(data, pk, include) {
        if (include) data[presentrows] else data[presentrows, -pk, with = FALSE, nomatch = NA]
      }))
      sbk = self$primary_key
      if (sbk %in% cols) result[, (sbk) := presentrows]
      data[J(presentrows), on = sbk, nomatch = NULL]
    },
    head = function(n = 6L) {
      rows = head(self$rownames, n)
      self$data(rows = rows, cols = self$colnames)
    },
    distinct = function(rows, cols, na_rm = TRUE) {
      bs = private$data$bs
      getpk = self$primary_key %in% cols
      results = list()
      remaining_cols = cols
      if (na_rm || getpk) {
        rows = intersect(rows, self$rownames)
      }
      for (i in seq_along(bs)) {
        if (!length(remaining_cols)) break
        results[[i]] = bs[[i]]$distinct(rows = rows, cols = cols, na_rm = na_rm)
        remaining_cols = setdiff(remaining_cols, names(results[[i]]))
        if (na_rm && !all(rows %in% bs[[i]]$rownames)) {
          results[[i]] = c(results[[i]], NA)
        }
      }
      result = unlist(result, recursive = FALSE)
      if (getpk) {
        result[[self$primary_key]] = rows
      }
      result[match(cols, names(result), nomatch = 0)]
    },
    missings = function(rows, cols) {
      rows = rows[rows %in% self$rownames]
      bs = private$data$bs
      getpk = self$primary_key %in% cols
      results = list()
      remaining_cols = cols
      for (i in seq_along(bs)) {
        if (!length(remaining_cols)) break
        missingrows = sum(rows %nin% bs[[i]]$rownames)
        results[[i]] = bs[[i]]$missing(rows, remaining_cols) + missingrows
        remaining_cols = setdiff(remaining_cols, names(results[[i]]))
      }
      result = unlist(result)
      if (self$primary_key %in% cols) {
        result[[self$primary_key]] = 0L
      }
      result[match(cols, names(result), nomatch = 0)]
    }
  ),
  active = list(
    rownames = function() {
      if (is.null(private$.rownames_cache)) private$.rownames_cache = unique(unlist(map(bs, "rownames")))
      private$.rownames_cache
    },
    colnames = function() {
      if (is.null(private$.colnames_cache)) private$.colnames_cache = unique(unlist(map(bs, "colnames")))
      private$.colnames_cache
    },
    nrow = function() length(self$rownames),
    ncol = function() length(self$colnames)
  ),
  private = list(
    .rownames_cache = NULL,
    .colnames_cache = NULL,
    .calculate_hash = function() {
      do.call(calculate_hash, private$.data$bs)
    }
  )
)
