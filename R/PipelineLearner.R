pipeline_gather_params <- function(graph) {
  
  all_params <- traverseGraph(
    graph,
    function(x) x$par_set$clone(deep = TRUE)$params
  )
  
  all_params_named <- mapply(function(params_list, id) {
    
    lapply(params_list, function(x) {
      
      x <- x$clone(deep = TRUE)
      x$id <- paste(id, x$id, sep =":")
      x
    })
    
  }, all_params, names(all_params), SIMPLIFY = FALSE)
  
  paradox::ParamSet$new(params = unlist(all_params_named))
}
