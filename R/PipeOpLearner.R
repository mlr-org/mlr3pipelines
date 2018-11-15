# FIXME: war gibt das in train zurück? modell, task oder beidies?

PipeOpLearner = R6Class("PipeOpLearner",

  inherit = PipeOp,

  public = list(

    learner = NULL,

    initialize = function(learner) {
      self$learner = learner
      super$initialize(learner$id)
    },


    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      task = inputs[[1L]]

      experiment <- Experiment$new(task = task, learner = self$learner)
      experiment$train()
      private$.params = experiment

      experiment$predict()
      d <- experiment$prediction[,-1]
      colnames(d)[seq_along(task$target_names)] <- task$target_names

      db <- as_data_backend(d)
      private$.result <- TaskClassif$new(id = task$id, backend = db, target = task$target_names)
      private$.result
    },

    predict2 = function() {
      assert_list(inputs, len = 1L, type = "Task")
      predict(self$params)
    }
  ),

  active = list(
    param_set = function() self$learner$param_set,

    param_vals = function(value) {
      if (missing(value)) return(self$learner$param_vals)
      else self$learner$param_set = value
    }
  )
)


