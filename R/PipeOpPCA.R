# Rotate data
# [dt] -> [dt]
PipeOpPCA = R6Class("PipeOpPCA",

  inherit = PipeOpDT,

  public = list(
    initialize = function(id = "pca") {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale.", default = FALSE),
        ParamInt$new("rank.", default = NULL, lower = 1, upper = Inf, special_vals = list(NULL))
      ))
      super$initialize(id, ps)
    },

    train_dt = function(dt) {
      pcr = prcomp(as.matrix(dt),
        center = self$param_vals$center,
        scale. = self$param_vals$scale.,
        rank.  = self$param_vals$rank.)
      self$state = pcr
      return(pcr$x)
    },

    predict_dt = function(newdt) {
      rotated = predict(self$state, as.matrix(newdt))
      return(rotated)
    }
  )
)