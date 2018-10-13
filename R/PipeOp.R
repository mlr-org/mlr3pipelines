
PipeOp = R6Class("PipeOp", 
  public = list(
    

    inputs = NULL,
    next_ops = NULL,
    prev_ops = NULL,

    initialize = function(id, par_set = ParamSet$new()) {
      private$.id = id
      private$.par_set = par_set
      #FIXME: we really need a funtion in ph2 now to get defaults
      private$.par_vals = extractSubList(par_set$params, "default", simplify = FALSE)
    },


    train = function() {
      self$acquire_inputs()
      messagef("Train op='%s'", self$id)
      result = self$train2() 
      private$.result = result
      return(result)
    }, 
    
    predict = function() {
      self$acquire_input()
      messagef("Predict op='%s'", self$id)
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


    acquire_inputs = function() {
      if (!self$has_no_prevs)
        self$inputs = self$prev_ops$map(function(x) x$result) 
    },

    # trainRecursive = function(input) {
      # input = train(input)
      # lapply(trainRecursive())
    # }


    print = function(...) {
      catf("PipeOp: <%s>", self$id)
      catf("parvals: <%s>", listToShortString(self$par_vals))
      catf("is_learnt=%s", self$is_learnt)
      catf("Input: %s", listToShortString(self$inputs))
      catf("Result: %s", listToShortString(self$result))
      catf("Prev ops: %s", self$prev_ops$print_str)
      catf("Next ops: %s", self$next_ops$print_str)
    },
    
    #FIXME: AB machen
    set_par_vals = function(vals) {
      private$.par_vals = insert(private$.par_vals, vals)
      invisible(self)
    },
    
    set_next = function(ops) {
      self$next_ops = OpList$new(ops)
      for (op in ops) {
        op$prev_ops = OpList$new(list(self))
      }
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
    has_result = function() !is.null(self$result),
    has_no_prevs = function() length(self$prev_ops) == 0L,
    
    can_fire = function() {
      if (self$has_no_prevs)
        !is.null(self$inputs)
      else
        all(self$prev_ops$map_s(function(x) x$has_result))
    }
  ),

  private = list(
    .id = NULL,
    .par_set = NULL,
    .par_vals = NULL,
    .params = NULL,
    .result = NULL
  )
)
