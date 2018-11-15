PipeOpFeatureUnion = R6Class("PipeOpFeatureUnion",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "featureunion") {
      super$initialize(id)
    },


    train = function(inputs) {

      ## check if all target_names are equal
      is_target_equal <- length(unique(vapply(
        inputs,
        function(x) digest::digest(x$target_names),
        FUN.VALUE = ""
      ))) == 1

      all_data = lapply(inputs, function(x) {
        x$data(cols = x$feature_names)
      })

      targets = inputs[[1]]$data(cols = task$target_names)
      browser()
      Reduce(mlr3:::DataBackendCbind$new(), BBmisc::extractSubList(inputs, "backend"))

      data = do.call(cbind, c(all_data, list(targets)))
      db <- DataBackendDataTable$new(data)
      private$.result <- TaskClassif$new(id = task$id, backend = db, target = task$target_names)
    },

    predict2 = function(input) {
      do.call(cbind, input)
    }
  )
)

mlr3:::DataBackendCbind$new(z[[1]], z[[2]])
