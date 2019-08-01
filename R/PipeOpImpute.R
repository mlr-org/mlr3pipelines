#' @title PipeOpImpute
#'
#' @name mlr_pipeop_impute
#'
#' @description
#' Impute missing values with varying methods.
#'
#' `numeric` or `integer` features are imputed by `method_num`.
#'
#' `factor`, `ordered`, and `character` features are imported by `method_fct`.
#'
#' `logical` features are always imputed by sampling from the training column.
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpImpute = R6Class("PipeOpImpute",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "impute", param_vals = list()) {
      ps = ParamSet$new(list(
        ParamFct$new("method_num", levels = c("median", "mean", "sample", "hist"), default = "median"),
        ParamFct$new("method_fct", levels = c("newlvl", "sample"), default = "newlvl"),
        ParamFct$new("add_dummy", levels = c("none", "missing_train", "all"), default = "missing_train")
      ))
      ps$values = list(method_num = "median", method_fct = "newlvl", add_dummy = "missing_train")
      super$initialize(id, ps, param_vals = param_vals)
    },

    get_state = function(task) {
      num_feats = task$feature_types[get("type") %in% c("numeric", "integer"), get("id")]
      fct_feats = task$feature_types[get("type") %nin% c("numeric", "integer"), get("id")]
      lgl_feats = task$feature_types[get("type") == "logical", get("id")]
      num_model = map(task$data(cols = num_feats), function(col) {
        if (all(is.na(col))) {
          col = 0L
        }
        switch(self$param_set$values$method_num,
          median = median(col, na.rm = TRUE),
          mean = mean(col, na.rm = TRUE),
          sample = col[!is.na(col)],
          hist = hist(col, plot = FALSE)[c("counts", "breaks")])
      })
      fct_model = imap(task$data(cols = fct_feats), function(col, colname) {
        method = self$param_set$values$method_fct
        if (colname %in% lgl_feats) {
          method = "sample"
        }
        switch(method,
          newlvl = NULL,
          sample = col[!is.na(col)])
      })
      feats_with_missings = task$feature_names[map_lgl(task$data(cols = task$feature_names),
        function(x) any(is.na(x)))]

      list(num_model = num_model, fct_model = fct_model,
        lgl_feats = lgl_feats, feats_with_missings = feats_with_missings)
    },

    transform = function(task) {

      num_model = self$state$num_model
      fct_model = self$state$fct_model
      lgl_feats = self$state$lgl_feats
      data = task$data(cols = task$feature_names)
      data_dummy = as.data.table(is.na(data))
      predict_missings = task$feature_names[map_lgl(data_dummy, any)]
      # even if nothing is missing we still go through with everything,
      # because we may need to add dummies
      data = data[, predict_missings, with = FALSE]


      ..col = NULL
      imap(data, function(col, colname) {
        col[is.na(col)] = if (colname %in% names(num_model)) {
          num = switch(self$param_set$values$method_num,
            median = num_model[[colname]],
            mean = num_model[[colname]],
            sample = {
              choices = num_model[[colname]]
              if (length(choices == 1)) {
                rep_len(choices, sum(is.na(col)))
              } else {
                sample(choices, sum(is.na(col)), replace = TRUE)
              }
            },
            hist = {
              counts = num_model[[colname]]$counts
              breaks = num_model[[colname]]$breaks
              which.bins = sample.int(length(counts), sum(is.na(col)), replace = TRUE, prob = counts)
              runif(length(which.bins), breaks[which.bins], breaks[which.bins + 1L])
          })
          if (task$feature_types[colname, get("type")] == "integer") {
            num = as.integer(round(num))
          }
          num
        } else {
          method = self$param_set$values$method_fct
          if (colname %in% lgl_feats) {
            method = "sample"
          }
          switch(method,
            newlvl = {
              if (is.factor(col)) {
                levels(col) = c(levels(col), ".MISSING")
              }
              ".MISSING"
            },
            sample = sample(fct_model[[colname]], sum(is.na(col)), replace = TRUE)
          )
        }
        data[, (colname) := ..col]
      })
      if (self$param_set$values$add_dummy == "missing_train") {
        data_dummy = data_dummy[, self$state$feats_with_missings, with = FALSE]
      }
      # don't add dummy cols for factors or ordereds if they get the '.MISSING' level
      if (self$param_set$values$method_fct == "newlvl") {
        data_dummy = data_dummy[,
          setdiff(colnames(data_dummy), setdiff(names(fct_model), lgl_feats)),
          with = FALSE]
      }
      if (self$param_set$values$add_dummy != "none" && ncol(data_dummy)) {
        colnames(data_dummy) = paste0("missing_", colnames(data_dummy))
        if (ncol(data)) {
          data = cbind(data, data_dummy)
        } else {
          data = data_dummy
        }
      }
      if (!ncol(data)) {
        return(task)
      }
      task$select(setdiff(task$feature_names, colnames(data)))$cbind(data)
    }
  )
)

mlr_pipeops$add("impute", PipeOpImpute)
