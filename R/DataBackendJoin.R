

#' @export
DataBackendJoin = R6Class("DataBackendJoin", inherit = DataBackend, cloneable = FALSE,
  public = list(
    initialize = function(b1, b2, by_b1 = NULL, by_b2 = NULL, type = "outer", b1_index_colname = NULL, b2_index_colname = NULL) {
      assert_backend(b1)
      assert_backend(b2)

      if ("data.table" %nin% intersect(b1$data_formats, b2$data_formats)) {
        stop("DataBackendJoin currently only supports DataBackends that support 'data.table' format.")
      }

      assert_choice(type, c("left", "right", "outer", "inner"))

      colnames_b1 = b1$colnames
      colnames_b2 = b2$colnames
      allcolnames = union(colnames_b1, colnames_b2)

      assert_choice(by_b1, colnames_b1, null.ok = TRUE)
      assert_choice(by_b2, colnames_b2, null.ok = TRUE)

      assert_string(b1_index_colname, null.ok = TRUE)
      assert_string(b2_index_colname, null.ok = TRUE)

      if (!is.null(b1_index_colname) && b1_index_colname %in% setdiff(allcolnames, b1$primary_key)) stopf("b1_index_colname '%s' already a non-primary-key column in b1 or b2.", b1_index_colname)
      if (!is.null(b2_index_colname) && b2_index_colname %in% setdiff(allcolnames, b2$primary_key)) stopf("b2_index_colname '%s' already a non-primary-key column in b2 or b2.", b2_index_colname)
      if (!is.null(b1_index_colname) && !is.null(b2_index_colname) && b1_index_colname == b2_index_colname) stop("b1_index_colname and b2_index_colname must be different, but are both '%s'.", b1_index_colname)

      rownames_b1 = b1$rownames
      rownames_b2 = b2$rownames

      joinby_b1 = if (is.null(by_b1)) rownames_b1 else b1$data(rownames_b1, by_b1, data_format = "data.table")[[1]]
      joinby_b2 = if (is.null(by_b2)) rownames_b2 else b2$data(rownames_b2, by_b2, data_format = "data.table")[[1]]

      index_table = merge(data.table(rownames_b1, joinby_b1), data.table(rownames_b2, joinby_b2), by.x = "joinby_b1", by.y = "joinby_b2",
        all.x = type %in% c("left", "outer"), all.y = type %in% c("right", "outer"), sort = FALSE, allow.cartesian = TRUE)

      index_table[, "joinby_b1" := NULL]

      pk = "..row_id"
      index = 0
      while (pk %in% allcolnames) {
        index = index + 1
        pk = paste0("..row_id.", index)
      }

      super$initialize(list(
        b1 = b1, b2 = b2,
        colnames_b1 = setdiff(colnames_b1, colnames_b2),
        allcolnames = unique(c(colnames_b1, colnames_b2, b1_index_colname, b2_index_colname, pk)),
        index_table = index_table,
        b1_index_colname = b1_index_colname,
        b2_index_colname = b2_index_colname,
        pk = pk,
        aux_hash = calculate_hash(by_b1, by_b2, type, b1_index_colname, b2_index_colname)
      ), primary_key = pk, data_formats = "data.table")
    },

    data = function(rows, cols, data_format = "data.table") {
      d = private$.data
      rows = rows[inrange(rows, 1, nrow(d$index_table))]
      indices = d$index_table[rows]
      b1_rows = indices[!is.na(rownames_b1), rownames_b1]
      b2_rows = indices[!is.na(rownames_b2), rownames_b2]
      indices[!is.na(rownames_b1), b1_index := seq_len(nrow(b1_rows))]
      indices[!is.na(rownames_b2), b2_index := seq_len(nrow(b2_rows))]
      b1_index = indices[, b1_index]
      b2_index = indices[, b2_index]

      data = d$b2$data(b2_rows, cols, data_format = "data.table")[b2_index]
      remainingcols = intersect(cols, d$colnames_b1)
      if (length(remainingcols)) {
        data = cbind(data, d$b1$data(b1_rows, cols, data_format = "data.table")[b1_index])
      }
      if (d$pk %in% cols) {
        data[, (d$pk) := rows]
      }
      if (!is.null(d$b2_index_colname) && d$b2_index_colname %in% cols) {
        rownames_b2 = b2_rows$rownames_b2
        data[, (d$b2_index_colname) := rownames_b2]
      }
      if (!is.null(d$b1_index_colname) && d$b1_index_colname %in% cols) {
        rownames_b1 = b1_rows$rownames_b1
        data[, (d$b1_index_colname) := rownames_b1]
      }
      data[, intersect(cols, names(data)), with = FALSE]
    },

    head = function(n = 6L) {
      rows = head(self$rownames, n)
      self$data(rows = rows, cols = self$colnames)
    },
    distinct = function(rows, cols, na_rm = TRUE) {
      indices = private$.data$index_table[rows]
      b1_rows = indices[!is.na(rownames_b1), rownames_b1]
      b2_rows = indices[!is.na(rownames_b2), rownames_b2]
      d2 = private$.data$b2$distinct(rows = b2_rows, cols = cols, na_rm = na_rm)
      d1 = private$.data$b1$distinct(rows = b1_rows, cols = setdiff(cols, names(d2)), na_rm = na_rm)
      if (!na_rm && nrow(b1_rows) < length(rows)) {
        d1 = map(d1, function(x) if (any(is.na(x))) x else c(x, NA))
      }
      if (!na_rm && nrow(b2_rows) < length(rows)) {
        d2 = map(d2, function(x) if (any(is.na(x))) x else c(x, NA))
      }
      res = c(d1, d2)
      res[match(cols, names(res), nomatch = 0)]
    },
    missings = function(rows, cols) {
      indices = private$.data$index_table[rows]
      b1_rows = indices[!is.na(rownames_b1), rownames_b1]
      b2_rows = indices[!is.na(rownames_b2), rownames_b2]
      m2 = private$.data$b2$missings(b2_rows, cols)
      m1 = private$.data$b1$missings(b1_rows, setdiff(cols, names(m2)))
      m1 = m1 + length(rows) - nrow(b1_rows)
      m2 = m2 + length(rows) - nrow(b2_rows)
      res = c(m1, m2)
      res[match(cols, names(res), nomatch = 0)]
    }
  ),
  active = list(
    rownames = function() seq_len(nrow(private$.data$index_table)),
    colnames = function() private$.data$allcolnames,
    nrow = function() nrow(private$.data$index_table),
    ncol = function() length(private$.data$allcolnames)
  ),
  private = list(
    .calculate_hash = function() {
      d = private$.data
      calculate_hash(d$b1$hash, d$b2$hash,d$aux_hash)
    }
  )
)
