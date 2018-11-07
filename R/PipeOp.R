
PipeOp = R6Class("PipeOp", 
  public = list(
  
    initialize = function(id, par_set = ParamSet$new()) {
      private$.id = id
      private$.par_set = par_set
      #FIXME: we really need a function in paradox now to get defaults
      private$.par_vals = BBmisc::extractSubList(par_set$params, "default", simplify = FALSE)
    },


    train = function() {
      self$acquire_inputs()
      BBmisc::messagef("Train op='%s'", self$id)
      result = self$train2() 
      private$.result = result
      return(result)
    }, 
    
    predict = function() {
      self$acquire_input()
      BBmisc::messagef("Predict op='%s'", self$id)
      result = self$predict2() 
      private$.result = result
      return(result)
    }, 

    reset = function() {
      self$params = NULL
      self$result.train = NULL
      self$result.predict = NULL
      invisible(self)
    },

    print = function(...) {
      BBmisc::catf("PipeOp: <%s>", self$id)
      BBmisc::catf("parvals: <%s>", BBmisc::listToShortString(self$par_vals))
      BBmisc::catf("is_learnt=%s", self$is_learnt)
      BBmisc::catf("Input: %s", BBmisc::listToShortString(self$inputs))
      BBmisc::catf("Result: %s", BBmisc::listToShortString(self$result))
      BBmisc::catf("Prev ops: %s", self$prev_ops$print_str)
      BBmisc::catf("Next ops: %s", self$next_ops$print_str)
    },
    
    #FIXME: AB machen
    set_par_vals = function(vals) {
      private$.par_vals = insert(private$.par_vals, vals)
      invisible(self)
    },
    
    

    set_prev = function(ops) {
      self$prev_ops = OpList$new(ops)
      for (op in ops) {
        op$next_ops = OpList$new(list(self))
      }
    }


  ),

  active = list(
    id = function() private$.id,
    par_set = function() private$.par_set,
    par_vals = function() private$.par_vals,
    params = function() private$.params,
    result = function() private$.result,
    is_learnt = function() !is.null(self$params),
    has_result = function() !is.null(self$result)
  ),

  private = list(
    .id = NULL,
    .par_set = NULL,
    .par_vals = NULL,
    .params = NULL,
    .result = NULL
  )
)
