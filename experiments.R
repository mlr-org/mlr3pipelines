library("paradox")
library("mlr3")

options(error=recover)

devtools::document("mlr3pipelines")

devtools::load_all("mlr3pipelines")
testthat::test("mlr3pipelines")
testthat::test_package("mlr3pipelines")

BasicPO = R6::R6Class("BasicPO",
  inherit = PipeOp,
  public = list(
      train = function(...) print("hi"),
      predict = function(...) print("yo"),
      initialize = function(...) {
        super$initialize(...)
        private$.intype = list("data.frame")
        private$.outtype = list("data.frame")
      }
  )
)
BasicPOAny = R6::R6Class("BasicPOAny",
  inherit = PipeOp,
  public = list(
      nin = NULL,
      nout = NULL,
      train = function(input) {
        catf("Training %s with input %s", self$id, deparse(input))
        self$state = input
        iin = input[[1]]
        as.list(iin + seq_len(self$nout))
      },
      predict = function(input) {
        catf("Predicting %s with input %s and state %s", self$id, deparse(input), deparse(self$state))
        iin = input[[1]]
        as.list(iin + seq_len(self$nout))
      },
      initialize = function(nin, nout, id, ...) {
        p = ParamInt$new(id = "par", lower = 0, upper = 10, default = 0)
        self$nin = nin
        self$nout = nout
        super$initialize(id, ParamSet$new(list(p)), list(...))
        private$.intype = rep(list("data.frame"), nin)
        private$.outtype = rep(list("data.table"), nout)
      }
  )
)
BasicPOAnyNamed = R6::R6Class("BasicPOAnyNamed",
  inherit = PipeOp,
  public = list(
      train = function(...) print("hi"),
      predict = function(...) print("yo"),
      initialize = function(nin, nout, ...) {
        super$initialize(...)
        private$.intype = rep(list("data.frame"), nin)
        names(private$.intype) = letters[seq_along(private$.intype)]
        private$.outtype = rep(list("data.table"), nout)
        names(private$.outtype) = letters[seq_along(private$.outtype)]
      }
  )
)
gr = Graph$new()
gr2 = Graph$new()
gr3 = Graph$new()


pipeop = BasicPO$new("testa")

gr = Graph$new()
gr$add_node(BasicPO$new("testa"))
gr$add_node(BasicPO$new("testb"))
gr$add_node(BasicPO$new("testc"))
gr[["testa"]]$next_node_channels[[1]] = gr[["testb"]]$in_channels[[1]]
gr$plot()



gr$add_node(BasicPO$new("testa2"))
gr$add_node(BasicPO$new("testb2"))
gr$add_node(BasicPO$new("testc2"))


gr2$add_node(BasicPOAny$new(1, 2, "testad"))
gr2$add_node(BasicPOAny$new(1, 2, "testbd"))
gr2$add_node(BasicPOAny$new(1, 2, "testcd"))
gr2[["testad"]]$next_node_channels[[1]] = gr2[["testbd"]]$in_channels[[1]]
gr2[["testcd"]]$next_node_channels[[2]] = gr2[["testbd"]]$in_channels[[1]]
gr2[["testcd"]]$next_node_channels[[1]] = gr2[["testad"]]$in_channels[[1]]
gr2$add_node(BasicPOAny$new(2, 1, "tested"))
gr2[["tested"]]$prev_node_channels[[1]] = gr2[["testbd"]]$out_channels[[2]]
gr2[["tested"]]$prev_node_channels[[2]] = gr2[["testbd"]]$out_channels[[1]]

gr2$out_channels
gr3 = gr2 %>>% BasicPOAny$new(3, 1, "testxx") %>>% BasicPOAnyNamed$new(1, 2, "testyy")
gr3$plot()

gr3 = gr2 %>>% BasicPOAny$new(3, 1, "testxx") %>>% BasicPOAny$new(1, 1, "testyy")


BasicPOAny$new(1, 2, "teest")$train(list(1))
BasicPOAny$new(2, 3, "teest2")$train(list(10, 20))


gr3[["testad"]]$pipeop$param_set

gr3$param_set
gr3$param_vals

gr3[["testad"]]$pipeop$param_vals$par = 1
gr3$param_vals
gr3$param_vals$testcd.par = 1
gr3$param_vals




gr3$train(1)

gr3$predict(2)



gr3[["testyy"]]$prev_node_channels['a'] = list(NULL)
gr3[["testxx"]]$next_node_channels[[1]] = gr3[["testyy"]]$in_channels$a

gr3$plot()



gr2$plot()
gr3$plot()



gr$plot()

gr2[["testad"]]$graph
gr2[["testbd"]]$graph



gr$plot()
gr2$plot()



gr[["testb"]]$out_channels$pca

gr[["testb"]]$prev_node_channels[[1]] = gr[["testa"]]$out_channels$pca

gr[["testb"]]$prev_node_channels[[1]] = gr[["testa"]]$out_channels[[1]]
gr[["testb"]]$prev_node_channels[[2]] = gr[["testc"]]$out_channels[[1]]  # add connection

gr[["testb"]]$prev_node_channels[[1]] = gr[["testa"]]$out_channels[[1]]
gr[["testb"]]$prev_node_channels[[1]] = gr[["testc"]]$out_channels[[1]]  # replace connection


gr$plot()

gr[["pca"]]$prev_node_channels[[1]] = gr[["mplx"]]$out_channels[["pca"]]
gr[["ica"]]$prev_node_channels[[1]] = gr[["mplx"]]$out_channels[["ica"]]




bpo = BasicPO$new("testid")
bpo2 = BasicPO$new("testid2")
bpo3 = BasicPO$new("testid3")

(bpo %>>% bpo2 %>>% bpo3)$plot()

pon = PipeOpNULL$new()

pon$train(list("test"))

pon$predict(1)

graph = bpo %>>% bpo2
graph$plot()
graph2 = graph %>>% bpo3

graph$plot()
graph2$plot()

node = GraphNode$new(bpo, gr)
node2 = GraphNode$new(bpo2, gr)
node3 = GraphNode$new(bpo3, gr)




sort_nodes(gr$node_list)
gr$plot()

gr[["testid"]]$prev_node_channels[[1]] =gr[["testid2"]]$out_channels[[1]]


gr$out_channels
gr$in_channels


gr$plot()
gr2 = gr$clone(deep = TRUE)

gr2$plot()

(gr2[["testid"]], gr[["testid"]]


gr[["testid"]]$prev_node_channels[[1]] = gr[["testid2"]]$out_channels[[1]]
# gr[["testid"]]$next_node_edges[[1]] = gr[["testid2"]]$in_edges[[1]]

graph_plot(gr)

gr$node_list
gr$sorted_node_list

debugonce(sort_nodes)
attr(sort_nodes(gr$node_list, TRUE), "layer")

node$graph
node$pipeop
node$prev_nodes
node$next_nodes
node$in_edges
node$out_edges
node$intype
node$outtype
node$input_complete
node$output_complete

gr$node_list[[1]]$in_edges
gr$node_list[[1]]$intype
gr$node_list[[1]]$next_nodes
gr$sorted_node_list

gr$update_connections()
gr$intype
gr$outtype
gr$in_edges
gr$out_edges
gr$source_nodes
gr$sink_nodes

li = gr[["testid"]]$next_node_edges
li[[1]] = gr[["testid2"]]$in_edges[[1]]

el = gr[["testid"]]
el$next_node_edges = li


gr[["testid"]]$next_node_edges= li








gr[["testid"]]$next_node_edges[[1]]

gr[["testid"]]$next_nodes




gr$is_learnt
gr$param_set

gr




#------------------------------------




library("R6")

clx <- R6Class("test",
  public = list(
      test = "test",
      xyz = 111,
      initialize = function(param) {
        private$xy = list(a = param, b = "b")
      },
      get = function(xx) {
        private$xy = xx$private$xy
      }
  ),
  active = list(
      t2 = function(x) {
        if (missing(x)) {
          private$xy
        } else {
          print(x)
        }
      }
  ),
  private = list(
      xy = NULL
  )
)


cl <- clx$new(123)
cl2 <- clx$new(99)

cl2$get(cl)

cl$t2
cl2$t2


cl$t2$a <- 1000


clx2 <- R6Class("test2",
  inherit = clx,
  public = list(
      test2 = "t",
      xy = 9000,
      initialize = function(param) {
        super$initialize(param)
      }
  ),
  active = list(
      t2 = function() 112
  )
)

cl2 <- clx2$new(1222)

cl2$test
cl2$test2
cl2$xyz
cl2$xy
cl2$t2

# TODO: auto cycle
#

task = mlr_tasks$get("iris")
lrn_rp = mlr_learners$get("classif.rpart")
g = PipeOpPCA() %>>% PipeOpLearner(lrn_rp)

g1 = tune(g)
g1 >> g

