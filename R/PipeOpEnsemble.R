# Basic Ensemble Constructor
# Can other ensembles inherit from that?
PipeOpEnsemble = R6Class("PipeOpEnsemble",
  inherit = PipeOp,

  public = list(
    initialize = function(id, param_set = ParamSet$new()) {
      super$initialize(id, param_set)
    },

    train2 = function() {
      apply_fun_to_rows()
    },

    predict2 = function() {
      apply_fun_to_rows()
    }
  ),

  private = list(
    apply_fun_to_rows = function() {
      assert_list(self$inputs, len = 1L, type = "Task")
      task = self$inputs[[1L]]
      # FIXME: this is really bad code how i change the data of the task
      fn = task$feature_names
      d = task$data()
      # FIXME: Use propper dt style
      avg = dd[, ..fn][, private$fun(), by = ..I]
      return(avg)
    }
  )
)

PipeOpEnsembleAverage = R6Class("PipeOpEnsembleAverage",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function() {
      super$initialize("PipeOpEnsembleAverage")
      private$fun = function(x) mean(x, na.rm = TRUE)
    }
  )
)

PipeOpEnsembleMajorityVote = R6Class("PipeOpEnsembleMajorityVote",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function() {
      super$initialize("PipeOpEnsembleMajorityVote")
      private$fun = function(x) mean(x, na.rm = TRUE)
    }
  ),

  private = list(
    mode = function(x) {
      tab = tabulate(x)
      modecol = which.max(tab)
      if(sum(tab == max(tab)) > 1) modecol = NA
      return(modecol)
    }
  )
)
