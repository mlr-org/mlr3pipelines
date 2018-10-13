library(R6)

MeasureAbstract = R6Class("MeasureAbstract", 
 
  public = list(
    
    id = NULL,

    initialize = function(id) {
      assert_string(id)
    },

    calcMeasureVal = function(y, yhat) {
      stop("abstract!")
    }
  )
)

MeasureMMCE = R6Class("MeasureMMCE", 
 
  inherit = MeasureAbstract,

  public = list(
    
    initialize = function() {
      super$initialize("mmce")
    },

    # calcMeasureVal = function(y, yhat) {
      # sum(y != yhat)
    # }
    calcMeasureVal = iris
  )
)

mm = MeasureMMCE$new()
# print(mm$calcMeasureVal(1:10, 10:1))




