# TODO: move to mlr3misc
map_values = function(x, old, new) {
  i = match(x, old, nomatch = 0L)
  x[i != 0L] = new[i]
  x
}


# x = letters[1:5]
# old = c("b", "d", "z")
# new = c("x", "y", "1")
# map_values(x, old, new)

# old = c("m")
# new = c("n")
# map_values(x, old, new)
