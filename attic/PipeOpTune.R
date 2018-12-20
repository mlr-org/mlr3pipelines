PipeOpTune = R6Class("PipeOpTune",

  inherit = PipeOp,

  public = list(

    task = NULL,
    learner = NULL,
    resampling = NULL,
    tune_param_set = NULL,
    control = NULL,

    initialize = function(task, learner, resampling, param_set, control) {
      self$task = task
      self$learner = learner
      self$resampling = resampling
      self$tune_param_set = param_set
      self$control = control
      super$initialize("tuner", ParamSet$new())
    },

    train2 = function(input) {
      # FIXME reall call into tuneParams here
      # tuned_pars = tuneParams(input, lrn, resa, param_set, ctrl)
      self$params = self$tune_param_set$sample()
      return(self$params)
    },

    predict2 = function(input) {
      return(input)
    }
  )
)


