# simple feature transform, no hyperpars
PipeOpDownsample = R6Class("PipeOpDownsample",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "downsample") {
      ps = ParamSet$new(params = list(
        ParamNum$new("perc", default = 0.7, lower = 0, upper = 1),
        ParamLgl$new("stratify", default = FALSE)
      ))
      super$initialize(id, ps)
      private$.intype = list("any")
      private$.outtype = list("any")

    },

    train = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      # FIXME: this is really bad code how i change the data of the task
      # ich muss hier das backend austauschen
      task = self$inputs[[1L]]
      fn = task$feature_names
      # FIXME: Discuss whether we want to use the current mlr implementation
      list(TaskClassif$new(id = task$id, data = d, target = task$target_names))
    },

    predict = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      # FIXME: Make sure dimensions fit (if input did not have full rank)
      list(as.data.frame(as.matrix(input) %*% self$params))
    }
  )
)