

trainGraph = function(root, task) {
  root$inputs = list(task)
  front = OpList$new(list(root))

  while(length(front) > 0L) {
    BBmisc::messagef("front step, front=%s", front$print_str)
    new_front = OpList$new()
    to_remove = integer(0L)
    for (i in seq_along(front)) {
      op = front[[i]]
      op$acquire_inputs()
      BBmisc::messagef("checking front node %s, can_fire=%s", op$id, op$can_fire)
      if (op$can_fire) {
        op$train()
        
        if(is.null(op$next_ops)) {
          # there's no next operations
          # so the loop can be stopped
          break
        }
        new_front$join_new(op$next_ops)
      } else {
        new_front$add(op) 
      }
    }
    front = new_front
    BBmisc::messagef("front step done, front=%s", front$print_str)
  }
}



