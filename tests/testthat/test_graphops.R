

test_that("gunion", {
  g1 = PipeOpPCA$new() %>>% PipeOpScale$new()
  g2 = PipeOpPCA$new(id = "pca2") %>>% PipeOpPCA$new(id = "xx")
  g3 = PipeOpScale$new(id = "blub") %>>% PipeOpScale$new(id = "foo")
  g4 = gunion(g1, g2, g3)


  g5 = PipeOpPCA$new() %>>% PipeOpScale$new()
  g6 = PipeOpPCA$new(id = "pca2") %>>% PipeOpScale$new(id = "xx")
  g7 = PipeOpScale$new(id = "blub") %>>% PipeOpScale$new(id = "foo")
  g8 = gunion(g5, g6)
  g9 = gunion(g8, g7)


  g10 = PipeOpPCA$new() %>>% PipeOpScale$new()
  g11 = PipeOpPCA$new("asdf")
  g12 = PipeOpPCA$new("asdf2")
  g13 = gunion(g10, g11, g12)

  g14 = gunion(PipeOpPCA$new(), PipeOpScale$new())
  g15 = gunion(PipeOpPCA$new(), x = PipeOpPCA$new())
  g16 = gunion(g4, second = g4)
})

test_that("greplicate", {
  graph = PipeOpPCA$new() %>>% PipeOpScale$new()
  g2 = greplicate(graph, 1)
  greplicate(PipeOpPCA$new(), 5)
  greplicate(graph, letters[1:3])
})
