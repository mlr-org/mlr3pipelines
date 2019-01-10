library("paradox")
library("mlr3")

options(error=recover)

devtools::document("mlr3pipelines")

devtools::load_all("mlr3pipelines")
testthat::test_package("mlr3pipelines")



testthat::test_package("mlr3pipelines", filter = "Graph")
testthat::test_package("mlr3pipelines", filter = "greplicate")


gr$param_vals$scale.scale = FALSE

gr$add_edge("scale", "1", "pca", "1")
gr$add_edge("scale", "1", "pca", "1")

task = mlr_tasks$get("iris")

lrn = mlr_learners$get("classif.rpart")
gr = PipeOpLearner$new(lrn)

glrn = GraphLearner$new(gr)
expect_learner_fits(glrn, task)

glrn = GraphLearner$new(gr)
glrn$train(task)
expect_prediction_classif({graphpred = glrn$predict(task)})
expect_equal(graphpred,
  lrn$train(task)$predict(task))

set.seed(1)
resgraphlrn = resample(task, lrn, mlr_resamplings$get("cv"))
set.seed(1)
resjustlrn = resample(task, lrn, mlr_resamplings$get("cv"))
expect_equal(resgraphlrn$data$prediction, resjustlrn$data$prediction)

gr2 = PipeOpScale$new() %>>% PipeOpLearner$new(lrn)
glrn2 = GraphLearner$new(gr2)
expect_learner_fits(glrn, task)
glrn2$train(task)
expect_prediction_classif({graphpred2 = glrn2$predict(task)})

scidf = cbind(scale(iris[1:4]), iris[5])
scalediris = TaskClassif$new("scalediris", as_data_backend(scidf), "Species")
expect_equal(graphpred, lrn$train(scalediris)$predict(scalediris))
expect_equal(graphpred2, lrn$train(scalediris)$predict(scalediris))

dblrn = mlr_learners$get("classif.debug")






MeasureClassifACC$new()$calculate(Experiment$new(task, lrn)$train()$predict())
MeasureClassifACC$new()$calculate(Experiment$new(task, lrn)$train()$predict())

MeasureClassifACC$new()$aggregate
C


# ---------------------- multiplex

task = mlr_tasks$get("iris")
opchoice = PipeOpChoice$new(3)
opchoicenamed = PipeOpChoice$new(c("opscale", "oppca", "opnop"))
opscale = PipeOpScale$new()
oppca = PipeOpPCA$new()
opnop = PipeOpNULL$new()
opunchoice = PipeOpUnchoice$new(3)

graph1 = opchoice %>>% gunion(opscale, oppca, opnop) %>>% opunchoice
graph2 = opchoicenamed %>>% gunion(opscale, oppca, opnop) %>>% opunchoice

graph1$param_vals$choice.selection = 1
assert(all.equal(graph1$train(task), opscale$train(list(task))[[1]]))

graph1$param_vals$choice.selection = 2
assert(all.equal(graph1$train(task), oppca$train(list(task))[[1]]))
assert(all.equal(graph1$predict(task), oppca$predict(list(task))[[1]]))

graph2$param_vals$choice.selection = "opscale"
assert(all.equal(graph2$train(task), opscale$train(list(task))[[1]]))

graph2$param_vals$choice.selection = "oppca"
assert(all.equal(graph2$train(task), oppca$train(list(task))[[1]]))
assert(all.equal(graph2$predict(task), oppca$predict(list(task))[[1]]))

# ----------------------  Learner

task = mlr_tasks$get("iris")
lrn = mlr_learners$get("classif.rpart")
graph = PipeOpScale$new() %>>% PipeOpPCA$new() %>>% PipeOpLearner$new(lrn)
assert(is.null(graph$train(task)))
graph$predict(task)  # problem is in mlr3

# ---------------------- Complex graph structure

gr = BasicPOAny$new(1, 2, "beginning") %>>%
  greplicate(BasicPOAny$new(1, 2, "middle"), 2) %>>%
  gunion(BasicPOAny$new(1, 2, "left"), BasicPOAny$new(2, 2, "secondmiddle"), BasicPOAny$new(1, 2, "right")) %>>%
  greplicate(BasicPOAny$new(3, 2, "lower"), 2) %>>%
  BasicPOAny$new(4, 1, "end")

gr$plot()

assert(gr$train(0) == 8)
assert(gr$predict(0) == 8)


# --------------------- PipeOpLearnerCV







gr = Graph$new()
gr2 = Graph$new()
gr3 = Graph$new()




pipeop = BasicPO$new("testa")

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

gr2[["testad"]]$graph
gr2[["testbd"]]$graph

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

