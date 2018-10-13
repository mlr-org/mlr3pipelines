PipeOpTune = R6Class("PipeOpTune", 
 
  inherit = PipeOp,
  
  public = list(
    
    task = NULL,
    learner = NULL,
    resampling = NULL,
    tune_par_set = NULL,
    control = NULL,

    initialize = function(task, learner, resampling, par_set, control) {
      self$task = task
      self$learner = learner 
      self$resampling = resampling
      self$tune_par_set = par_set
      self$control = control
      super$initialize("tuner", ParamSet$new())
    },

    train2 = function(input) {
      # FIXME reall call into tuneParams here
      # tuned_pars = tuneParams(input, lrn, resa, par_set, ctrl)
      self$params = self$tune_par_set$sample()
      return(self$params)
    },

    predict2 = function(input) {
      return(input)
    }
  )
)


