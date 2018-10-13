PipeOpFeatureUnion = R6Class("PipeOpFeatureUnion", 
 
  inherit = PipeOp,
  
  public = list(
    initialize = function(nout) {
      super$initialize("featureunion")
    },
      

    train2 = function(input) {
      do.call(cbind, input)
    },

    predict2 = function(input) {
      do.call(cbind, input)
    }
  )
)





