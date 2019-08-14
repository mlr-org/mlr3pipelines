#' @title PipeOpModelMatrix
#'
#' @name mlr_pipeop_modelmatrix
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`].
#'
#' @description
#' Transforms columns using a given `formula`.
#' It therefore uses the `stats::model.matrix()` function.
#'
#' Use the `$affect_columns` functionality to only encode a subset of columns,
#' or only encode columns of a certain type.
#'
#' @section Parameter Set:
#' * `formula`  :: `formula` \cr Formula to use. Higher order interactions
#' can be created using constructs like `~. ^ 2`.
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpModelMatrix = R6Class("PipeOpModelMatrix",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "modelmatrix", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("formula")
        # ParamFct$new("affect.type", levels = c("numeric",
        #   "factor", "ordered", "other", NULL), default = NULL),
        # ParamInt$new("affect.index", default = integer(0)),
        # ParamFct$new("affect.names", default = character(0)),
        # ParamFct$new("affect.pattern", default = NULL),
        # ParamLgl$new("affect.invert", default = TRUE),
        # ParamLgl$new("affect.pattern.ignore.case", default = FALSE),
        # ParamLgl$new("affect.pattern.perl", default = FALSE),
        # ParamLgl$new("affect.pattern.fixed", default = FALSE)
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "stats")
    }

    # select_cols = function(task) {
    #   task$feature_types[get("type") %in%  c("numeric",
    #        "factor", "ordered"), get("id")]
    # }

    # get_state_dt = function(dt, levels) {
    #   contrasts = switch(self$param_set$values$method,
    #     "one-hot" = function(x) stats::contr.treatment(x, contrasts = FALSE),
    #     treatment = stats::contr.treatment,
    #     helmert = stats::contr.helmert,
    #     poly = function(x) {
    #       cont = stats::contr.poly(x)
    #       rownames(cont) = x
    #       colnames(cont) = NULL
    #       cont
    #     },
    #     sum = stats::contr.sum,
    #     stop("Unknown 'method' parameter value.")
    #   )
    #   list(contrasts = lapply(levels, function(x) {
    #     con = contrasts(x)
    #     if (is.null(colnames(con))) {
    #       colnames(con) = as.character(seq_len(ncol(con)))
    #     }
    #     con
    #   }))
    # },
    #
    # transform_dt = function(dt, levels) {
    #   cols = imap(self$state$contrasts, function(contrasts, id) {
    #     x = dt[[id]]
    #     contrasts[as.character(x), , drop = FALSE]
    #   })
    #   cols = as.data.table(cols)
    #   setnames(cols, names(cols), make.names(names(cols), unique = TRUE))
    #   cols
    # }
  )
)

mlr_pipeops$add("modelmatrix", PipeOpModelMatrix)
