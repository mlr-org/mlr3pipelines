#' Traverse graphs and apply the function `fnc`
traverseGraph <- function(root, fnc) {
  #FIXME: check visited nodes
  
  front = OpList$new(list(root))
  result_list <- list()
  
  while(length(front) > 0L) {
    new_front = OpList$new()
    for (i in seq_along(front)) {
      op = front[[i]]
      result_list[[op$id]] <- fnc(op)
      
      if(is.null(op$next_ops)) break
      new_front$join_new(op$next_ops)
    }
    front = new_front
  }
  result_list
}


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
