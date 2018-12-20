# Scale Data
# [dt] -> [dt]
PipeOpScale = R6Class("PipeOpScale",

  inherit = PipeOpDT,

  public = list(
    initialize = function(id = "scale") {
      ps = ParamSet$new(params = list(
        ParamLgl$new("center", default = TRUE),
        ParamLgl$new("scale", default = TRUE)
      ))
      super$initialize(id, ps)
    },

    train_dt = function(dt) {
      sc = scale(as.matrix(dt),
        center = self$param_vals$center,
        scale = self$param_vals$scale)

      private$state = list(
        center = attr(sc, "scaled:center") %??% 0,
        scale = attr(sc, "scaled:scale") %??% 1
      )
      return(sc)
    },

    predict_dt = function(newdt) {
      scaled = (newdt - private$state$center) / private$state$center
      return(scaled)
    }
  )
)
