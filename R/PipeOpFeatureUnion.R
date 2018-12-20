PipeOpFeatureUnion = R6Class("PipeOpFeatureUnion",

  inherit = PipeOp,

  public = list(
    initialize = function(innum, id = "featureunion") {
      private$.intype = rep(list("any"), innum)
      private$.outtype = list("any")
      super$initialize(id)
    },

    train = function(inputs) {

      ## check if all target_names are equal
      is_target_equal = length(unique(vapply(
        inputs,
        function(x) digest::digest(x$target_names),
        FUN.VALUE = ""
      ))) == 1

      all_data = lapply(inputs, function(x) {
        x$data(cols = x$feature_names)
      })

      input1 = inputs[[1]]
      targets = input1$data(cols = input1$target_names)

      data = do.call(cbind, c(all_data, list(targets)))
      db = as_data_backend(data)

      list(TaskClassif$new(
        id = input1$id,
        backend = db,
        target = input1$target_names
      ))
    },

    predict = function(input) {
      do.call(cbind, input)
    }
  )
)
