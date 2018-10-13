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
      assert_list(inputs, len = 1L, type = "Task")
      task = inputs[[1L]]
      private$.params = train(task, self$learner)
      return(self$params)
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


