# simple feature transform, but hyperpars
Pipeline = R6Class("Pipeline", 
 
  public = list(
   

    ops = list(),
    
    # FIXME: this is all bad and here so we can call mlr resample
    packages = character(0L),
    par_vals = list(),
    task_type = "classif",
    id = "foobar",

    # kopieren wir ops hier? ansontsen ändert sich der zustand beim training auch außen
    initialize = function(ops) {
      self$ops = ops
      names(self$ops) = extractSubList(ops, "id")
    },

    train = function(task) {
      input = task
      for (i in 1:length(self$ops)) {
        print(i)
        op = self$ops[[i]]
        input = op$train(input)
      }
      return(input)
    },

    # FIXME: the "state" of the coded pipeline is now in self and model. that seems weird?
    # can we remove "ops" from pipeline
    predict = function(task, model) {
      print("pred")
      input = task
      for (i in 1:length(self$ops)) {
        op = self$ops[[i]]
        input = op$predict(input)
      }
      print(class(input))
      return(input)
    },

    print = function(...) {
      s = extractSubList(self$ops, "id")
      s = BBmisc::collapse(s, "->")
      catf("Pipeline: %s", s)
    }
  )
)

length.Pipeline = function(x) {
  length(x$ops)
}

`[[.Pipeline` = function(x, i, j, ...) {
  x$ops[[i]]  
}
