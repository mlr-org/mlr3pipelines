

trainGraph = function(root, task) {
  root$inputs = list(task)
  front = OpList$new(list(root))

  while(length(front) > 0L) {
    messagef("front step, front=%s", front$print_str)
    new_front = OpList$new()
    to_remove = integer(0L)
    for (i in seq_along(front)) {
      op = front[[i]]
      op$acquire_inputs()
      messagef("checking front node %s, can_fire=%s", op$id, op$can_fire)
      if (op$can_fire) {
        op$train()
        new_front$join_new(op$next_ops)
      } else {
        new_front$add(op) 
      }
    }
    front = new_front
    messagef("front step done, front=%s", front$print_str)
  }
}



