# FIXME: war gibt das in train zurück? modell, task oder beidies?

PipeOpLearner = R6Class("PipeOpLearner", 
 
  inherit = PipeOp,
  
  public = list(

    learner = NULL,

    initialize = function(learner) {
      self$learner = learner 
      super$initialize(learner$id)
    },


    train2 = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      task = self$inputs[[1L]]
      
      experiment <- Experiment$new(task = task, learner = self$learner)
      experiment$train()
      private$.params = experiment
      
      experiment$predict()
      d <- experiment$prediction[,-1]
      colnames(d)[seq_along(task$target_names)] <- task$target_names
      
      db <- DataBackendDataTable$new(d)
      TaskClassif$new(id = task$id, backend = db, target = task$target_names)
    },

    predict2 = function() {
      assert_list(inputs, len = 1L, type = "Task")
      predict(self$params)
    }
  ),
  
  active = list(
    par_set = function() self$learner$par_set,
    
    par_vals = function(value) {
      if (missing(value)) return(self$learner$par_vals)
      else self$learner$par_set = value
    }
  )
)


