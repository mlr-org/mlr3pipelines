


library("paradox")
library("mlr3")

options(error=recover)
options(error=dump.frames)
data.table::setDTthreads(0)
data.table::setDTthreads(1)
Sys.setenv(NOT_CRAN = "true")

devtools::document()

# devtools::load_all("paradox")

devtools::load_all()

devtools::test(filter = "textvectorizer")

Sys.setenv(TESTTHAT_CPUS = 20)
testthat::test_package("mlr3pipelines")


tools::buildVignettes(dir = "mlr3pipelines")


system.time(testthat::test_package("mlr3pipelines", filter = "pipeop_impute"), gcFirst = FALSE)
devtools::run_examples("mlr3pipelines")

profvis::profvis(testthat::test_package("mlr3pipelines"))


testthat::test_package("mlr3pipelines", filter = "textvectorizer")


testthat::test_package("mlr3pipelines", filter = "Graph")


ntree <- 10
mtry <- 2

gr <- po("replicate", reps = ntree) %>>%
  po("select", selector = function(task) sample(task$feature_names, mtry)) %>>%
  po("subsample") %>>% lrn("classif.rpart") %>>%
  po("classifavg", collect = TRUE)

gr <- po("ovrsplit") %>>% lrn("classif.rpart") %>>% po("ovrunite")
gr <- po("ovrsplit") %>>%
  po("replicate", reps = 3) %>>%
  po("subsample") %>>% lrn("classif.rpart") %>>%
  po("classifavg", collect = TRUE) %>>%
  po("ovrunite")

gr <- po("replicate", reps = 3) %>>%
  po("ovrsplit") %>>%
  po("subsample") %>>% lrn("classif.rpart") %>>%
  po("ovrunite") %>>%
  po("classifavg", collect = TRUE)

library("mlr3learners")

gr <- list(
    po("nop"),
    po("learner_cv", lrn("regr.lm")),
    po("replicate", reps = 3) %>>% po("learner_cv", lrn("regr.ranger"))
  ) %>>%
  po("featureunion") %>>% lrn("regr.rpart") %>>% po("regravg", collect = TRUE)



testthat::test_package("mlr3pipelines", filter = "scale")

testthat::test_package("mlr3pipelines", filter = "removeconstants")
testthat::test_package("mlr3pipelines", filter = "conversion")

devtools::test("mlr3pipelines", stop_on_warning = TRUE, filter = "imputelearner")

devtools::test("mlr3pipelines", stop_on_warning = TRUE, filter = "impute")


testthat::test_package("mlr3pipelines", filter = "renamecolumns")


testthat::test_package("mlr3pipelines", filter = "^_[a-d].*")

testthat::test_package("mlr3pipelines", filter = "^_[a-m].*")
testthat::test_package("mlr3pipelines", filter = "^_[n-s].*")
testthat::test_package("mlr3pipelines", filter = "^_[^a-s].*")


pofi = PipeOpFixFactors$new()


poe = PipeOpEncodeImpact$new()

t2 = po("histbin")$train(list(tsk("iris")))[[1]]

poe$get_state(tsk("boston_housing"))
poe$get_state(t2)
poe$param_set$values$impute_zero = FALSE


poe$train(list(tsk("boston_housing")))[[1]]$data()

poe$predict(list(tsk("boston_housing")$clone()$filter(1)))[[1]]$data()

poe$train(list(tsk("boston_housing")))[[1]]$data()
poe$train(list(t2))[[1]]$data()
poe$predict(list(t2$clone()$filter(1)))[[1]]$data()

poe$param_set$values$smoothing = 1e10

pom = PipeOpMutate$new()

pom$train(list(task))

pom$param_set$values$delete_originals = TRUE

pom$train(list(task))

pom$param_set$values$delete_originals = FALSE

pom$param_set$values$mutation = alist(x = Sepal.Width + Petal.Length)

pom$train(list(task))[[1]]$data()

pom$param_set$values$delete_originals = TRUE

pom$train(list(task))[[1]]$data()

pom$param_set$values$delete_originals = FALSE

pom$param_set$values$mutation = alist(Sepal.Width = Sepal.Width + Petal.Length)

pom$train(list(task))[[1]]$data()

g = PipeOpScale$new() %>>% PipeOpApply$new()
g2 = g$clone()
g2$train(task)


g$model = g2$model

g$param_set$values$apply.applicator = as.factor

task = mlr_tasks$get("iris")
str(g$train(task)[[1]]$data())

str(task$data())

g = PipeOpScale$new() %>>% PipeOpSelect$new()


g$train(task)[[1]]$data()
g$param_set$values$select.invert = TRUE
g$train(task)[[1]]$data()
g$param_set$values$select.invert = FALSE
g$param_set$values$select.selector = selector_grep("^Petal\\.")
g$train(task)[[1]]$data()
g$param_set$values$select.invert = TRUE
g$train(task)[[1]]$data()


g = PipeOpScale$new() %>>% PipeOpPCA$new()

profvis::profvis(Reduce(`%>>%`, lapply(c(letters, paste0(letters, "1"), paste0(letters, "2")), PipeOpScale$new)))

  paste0(letters, as.character(0:9))

system.time(for (i in seq_len(100)) {gr <- PipeOpScale$new() %>>% PipeOpPCA$new() }) / 100
system.time(for (i in seq_len(100)) {gr <- PipeOpScale$new() %>>% PipeOpPCA$new() %>>% PipeOpScale$new("scale2") %>>% PipeOpPCA$new("pca2")}) / 100

system.time(for (i in seq_len(100)) {g1 <- ensure_graph(PipeOpScale$new())}) / 100
system.time(for (i in seq_len(100)) {g1 <- ensure_graph(PipeOpScale$new()) ; g2 <- ensure_graph(PipeOpPCA$new())}) / 100
system.time(for (i in seq_len(100)) {g1 <- ensure_graph(PipeOpScale$new()) ; g2 <- ensure_graph(PipeOpPCA$new()) ; gu <- gunion(list(g1, g2)) ; g1out <- g1$output}) / 100

system.time(for (i in seq_len(100)) {g1 <- ensure_graph(PipeOpScale$new()) ; g2 <- ensure_graph(PipeOpPCA$new()) ; gu <- gunion(list(g1, g2)) ; g1out <- g1$output}) / 100



system.time(for (i in seq_len(100)) {g1 <- ensure_graph(PipeOpScale$new()) ; g2 <- ensure_graph(PipeOpPCA$new()) ; gu <- gunion(list(g1, g2)) ; g1out <- g1$output ; g2in <- g2$input ;   newedges <- cbind(g1out[, list(src_id = op.id, src_channel = channel.name)],g2in[, list(dst_id = op.id, dst_channel = channel.name)]) ; do.call(gu$add_edge, transpose(newedges)[[1]])}) / 100


g1 <- ensure_graph(PipeOpScale$new())
g1$input
g1$output

g1 <- PipeOpSelect$new(param_vals = list(selector = selector_type("numeric"))) %>>% PipeOpCopy$new(2) %>>% gunion(list(PipeOpScale$new(), PipeOpPCA$new())) %>>% PipeOpFeatureUnion$new(2) %>>% PipeOpLearner$new(mlr_learners$get("regr.rpart"))

g1$train(mlr_tasks$get("boston_housing"))
predict(g1, mlr_tasks$get("boston_housing")$data()[, -"medv"])
predict(g1, mlr_tasks$get("boston_housing"))

(PipeOpSelect$new(param_vals = list(selector = selector_type("numeric"))) %>>% PipeOpCopy$new(2))$train(mlr_tasks$get("boston_housing"))

system.time(for (i in seq_len(1000)) {g1$input}) / 1000
system.time(for (i in seq_len(1000)) {g1$output}) / 1000

Graph$active$input
Graph$active$output

g1$input
g1$output



g

gr$add_pipeop(PipeOpUnbranch$new(2))

gr$input
gr$output
gr$lhs
gr$rhs

gr$edges


gr$param_vals$scale.scale = FALSE

gr$add_edge("scale", "1", "pca", "1")
gr$add_edge("scale", "1", "pca", "1")




scidf = cbind(scale(iris[1:4]), iris[5])
scalediris = TaskClassif$new("scalediris", as_data_backend(scidf), "Species")
expect_equal(graphpred, lrn$train(scalediris)$predict(scalediris))
expect_equal(graphpred2, lrn$train(scalediris)$predict(scalediris))

dblrn = mlr_learners$get("classif.debug")






MeasureClassifACC$new()$calculate(Experiment$new(task, lrn)$train()$predict())
MeasureClassifACC$new()$calculate(Experiment$new(task, lrn)$train()$predict())

MeasureClassifACC$new()$aggregate
C
# -----------

devtools::load_all("mlr3")
testthat::test_package("mlr3")


# ---------------------- multiplex

task = mlr_tasks$get("iris")
opchoice = PipeOpChoice$new(3)
opchoicenamed = PipeOpChoice$new(c("opscale", "oppca", "opnop"))
opscale = PipeOpScale$new()
oppca = PipeOpPCA$new()
opnop = PipeOpNOP$new()
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

pon = PipeOpNOP$new()

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




)




dfa = data.frame( a = 1:5, b = 2:6)
dfb = data.frame( x = c("a", "b", "c", "x", "y"), y = rnorm(5))
dba = as_data_backend(dfa)
dbb = as_data_backend(dfb)
dbx = DataBackendCbind$new(dba, dbb, colnames(dfa), colnames(dfb))

dbx

dbx$data(cols = c("a", "x"), rows = c(3, 4))


dbi = as_data_backend(iris)

task_regr_iris = TaskRegr$new(id = "iris.regr", backend = dbi, target = "Sepal.Length")

task_regr_iris$filter(1:10)

task_regr_iris$data()

task_regr_iris$select(c("Petal.Length", "Species"))


task_regr_iris$data()


task_regr_iris_2 = TaskRegr$new(id = "iris.regr", backend = dbi, target = "Sepal.Length")

task_regr_iris_2$select(c("Petal.Length", "Species"))

task_regr_iris$data()
task_regr_iris_2$data()

task_regr_iris$rbind(task_regr_iris_2$data())

task_regr_iris$data()

mlr_tasks$get("iris")


lrn = mlr_learners$get("classif.rpart")

lrn$train(task)

lrn$data$model


lrn$predict(task)

pred

library("mlr3learners")


lrnsvm = mlr_learners$get("classif.svm")

lrnsvm$train(task)

lrnsvm$predict(task)


pred$score(MeasureClassifACC$new())

cv = mlr_resamplings$get("cv")

resres = resample(task, lrn, cv)

resres$aggregate(MeasureClassifACC$new())


resres = resample(task, lrnsvm , cv)
resres$aggregate(MeasureClassifACC$new())

design = data.table::data.table(
  task = mlr_tasks$mget(c("iris", "sonar")),
  learner = mlr_learners$mget(c("classif.featureless", "classif.rpart")),
  resampling = mlr_resamplings$mget(c("cv3", "holdout"))
)
design$resampling = Map(
  function(task, resampling) resampling$clone()$instantiate(task),
  task = design$task, resampling = design$resampling
)


design

bmr = benchmark(design)

bmr$aggregate()

names(bmr)


mlr_resamplings$get("holdout")
mlr_measures$get("classif.acc")
mlr_tasks$get("iris")
mlr_learners$get("classif.rpart")

mlr_control

rpart = LearnerClassifRpart$new()

rpart$param_set

rpart$param_set$values$cp = 0.4

rpart$param_set$values$cp = 1.1


lrn$param_set$values$cp = 0.2
resample(task, lrn, cv)$aggregate()

scaling = PipeOpScale$new()

transformed_data = scaling$train(list(task))[[1]]

plot(transformed_data$data()$Petal.Length)



scaling$state

task2 =task$clone(deep=TRUE)


task2$filter(1)

task2$data()

scaling$predict(list(task2))[[1]]$data()

scaling$train
scaling$predict
scaling$state
scaling$param_set$values$center = FALSE

transformed_data = scaling$train(list(task))[[1]]

transformed_data$data()

scaling$state





PipeOp


Graph




graph = PipeOpScale$new() %>>% PipeOpFilter$new(mlr3filters::FilterVariance$new(), param_vals = list(filter.frac = .5))

graph$pipeops

graph$edges

graph$plot()

task_out = graph$train(task)[[1]]
task_out$data()

graph$predict(task)[[1]]$data()

graph$plot()

graph$add_pipeop(PipeOpPCA$new())

graph$plot()

graph$ids()
graph$edges

graph$add_edge("variance", "pca")

graph$plot()

graph$edges


graph$add_pipeop(PipeOpCopy$new(3))

graph$plot()

graph$add_edge("copy", "scale", src_channel = "output2")

graph$plot()

graph$add_pipeop(PipeOpApply$new())

graph$plot()


graph$add_edge("copy", src_channel = "output1", "apply")

graph$plot()

graph$add_pipeop(PipeOpFeatureUnion$new(3))

graph$plot()

graph$add_edge("copy", "featureunion", src_channel = "output3", dst_channel = "input1")

graph$plot()


graph$add_edge("apply", "featureunion", dst_channel = "input2")

graph$add_edge("pca", "featureunion", dst_channel = "input3")

graph$param_set
graph$param_set$values$apply.applicator = as.numeric

graph$train(task)

g1 = Graph$new()$add_pipeop(PipeOpCopy$new(2))
g2 = Graph$new()$add_pipeop(PipeOpPCA$new())$add_pipeop(PipeOpScale$new())


g1$output

g2$input

gsum = g1 %>>% g2 %>>% PipeOpFeatureUnion$new(2)
gsum$plot()
gsum$train(task)[[1]]$data()

gsum$predict(task)[[1]]$data()


ghalf = g1 %>>% g2

str(ghalf$train(task))

gsum = PipeOpCopy$new(2) %>>%
  gunion(list(PipeOpPCA$new(), PipeOpScale$new())) %>>%
  PipeOpFeatureUnion$new(2)
gsum$plot()

gsum$state


gsum$add_edge("featureunion", "copy")

PipeOpPCA$new()$train(list(task))

por = PipeOpLearner$new(mlr_learners$get("classif.rpart"))

glrn = gsum %>>% por

glrn$plot()

glrn$train(task)

glrn$predict(task)

llrn = GraphLearner$new(glrn)
glrn$plot()
llrn

resample(task, llrn, cv)

llrn$param_set
task$select


gr = PipeOpBranch$new(2) %>>% gunion(list(PipeOpPCA$new(), PipeOpNOP$new())) %>>% PipeOpUnbranch$new(2)

gr$plot()

gr$param_set$values$branch.selection = 1

gr$train(task)[[1]]$data()


PipeOpScale$new()$input
PipeOpScale$new()$output

PipeOpLearner$new(mlr_learners$get("classif.rpart"))$output

PipeOpLearner$new(mlr_learners$get("classif.rpart")) %>>% PipeOpModelAvg$new(1)

PipeOpScale$new() %>>% PipeOpModelAvg$new(1)



task = mlr_tasks$get("iris")$
  set_col_role("Sepal.Length", character(0))$
  cbind(data.table(x = 1:150))$
  set_col_role("Sepal.Length.xxx", "feature")


task$data()

cbind(mlr_tasks$get("iris")$data(), data.table(x = 1:150))




po("pca")$train(list(tsk("iris")))[[1]]$data()


po("pca")$train(list(Multiplicity(tsk("iris"), tsk("boston_housing"))))


gr = po("replicate", reps = 10) %>>% po("subsample") %>>%
  lrn("classif.rpart") %>>% po("classifavg", collect = TRUE)
gr$train(tsk("iris"))

gr$predict(tsk("iris"))

gr$state$classif.rpart[1:2]

gr <- po("ovrsplit") %>>% lrn("classif.rpart") %>>% po("ovrunite")

gr <- po("ovrsplit") %>>%
  po("replicate", reps = 3) %>>%
  po("subsample") %>>% lrn("classif.rpart") %>>%
  po("classifavg", collect = TRUE) %>>%
  po("ovrunite")


gr <-   po("replicate", reps = 3) %>>%
  po("ovrsplit") %>>%
  po("subsample") %>>% lrn("classif.rpart") %>>%
  po("ovrunite") %>>%
  po("classifavg", collect = TRUE)

gr <- list(
    po("nop"),
    po("learner_cv", lrn("regr.lm")),
    po("replicate", reps = 3) %>>% po("learner_cv", lrn("regr.ranger"))
  ) %>>%
  po("featureunion") %>>% lrn("regr.rpart") %>>% po("regravg", collect = TRUE)


ntree <- 10
mtry <- 2

gr <- po("replicate", reps = ntree) %>>%
  po("select", selector = function(task) sample(task$feature_names, mtry)) %>>%
  po("subsample") %>>% lrn("classif.rpart") %>>%
  po("classifavg", collect = TRUE)



`%<>>%` = function(g1, g2) {
  force(g2)
  force(g1)
  cat(sprintf("'%s' >> '%s'\n", g1, g2))
  paste0(g1, g2)
}

"one" %<>>% "two" %<>>% "three"
#> 'one' >> 'two'
#> 'onetwo' >> 'three'

#   W X Y Z
#29 q s v z

quote(
  ((Z %among% "y" & Z %among% c("y", "z")) & (Y %among% "w" & X %among% c("t", "u") | Y %among% c("v", "w") & X %among% "t")) |
  (Y %among% c("w", "v") & (Z %among% "z" | Y %among% "w") & (X %among% "s" & Y %among% "w" | X %among% c("u", "s") & W %among% "p"))
) -> expr_full ; evaluate_expression(expr_full, assignment) != evaluate_expression(formula_to_expression(eval(expr_full)), assignment)

evaluate_expression(expr_full, assignment)

clause1 <- ((Z %among% "y" & Z %among% c("y", "z")) & (Y %among% "w" & X %among% c("t", "u") | Y %among% c("v", "w") & X %among% "t"))
clause2 <- (Y %among% c("w", "v") & (Z %among% "z" | Y %among% "w") & (X %among% "s" & Y %among% "w" | X %among% c("u", "s") & W %among% "p"))


formula_to_expression(clause1) |> evaluate_expression(assignment)
formula_to_expression(clause2) |> evaluate_expression(assignment)


clause1
clause2

clause1 | clause2

formula_to_expression(clause1 | clause2) |> evaluate_expression(assignment)

# > clause1
# CnfFormula:
#      (Z ∈ {y}) !!
#    & (Y ∈ {w, v})
#    & (X ∈ {t, u}) !!
#    & (X ∈ {t} | Y ∈ {w}) !!
# > clause2
# CnfFormula:
#      (Y ∈ {w, v})
#    & (X ∈ {s, u})
#    & (Z ∈ {z} | Y ∈ {w})
#    & (W ∈ {p} | X ∈ {s})
#    & (W ∈ {p} | Y ∈ {w}) !!
# > clause1 | clause2
# CnfFormula:
#      (Y ∈ {w, v})
#    & (X ∈ {s, u} | Z ∈ {y})
#    & (W ∈ {p} | X ∈ {s} | Z ∈ {y})
#    & (Z ∈ {z} | Y ∈ {w} | X ∈ {t})


# clause 10: W ∈ {p} | X ∈ {s} | Z ∈ {y}
# clause 16: W ∈ {p} | X ∈ {t} | Y ∈ {w}
# for some reason, clasue10$X is registered as subset of clause 16

eval(expr_full)

formula_to_expression(eval(expr_full)) |> evaluate_expression(assignment)

simplified

quote(
  (Z %among% c("a", "z") | X %among% c("u", "t")) &
  (W %among% c("r", "p") | X %among% "s" | Z %among% "y") &
  (X %among% c("s", "u") | Z %among% "y" | Y %among% "v") &
  (Z %among% c("z", "y") | Y %among% "v" | X %among% "s") &
  (Z %among% "z" | X %among% c("u", "t") | W %among% c("r", "q")) &
  (Y %among% c("w", "v") | X %among% "u" | Z %among% "a") &
  (W %among% "p" | X %among% "s" | Z %among% "y" | Y %among% "v") &
  (Y %among% c("v", "w") | Z %among% "z" | W %among% "r" | X %among% "s") &
  (Z %among% c("a", "z") | X %among% "t" | Y %among% c("x", "w") | W %among% "p")
) |> evaluate_expression(assignment)


#     W X Y Z
#  28 p s v z

X %among% "s" & X %among% "u" & (Z %among% c("y", "a") | Z %among% "a") |
(Z %among% "a" & W %among% c("p", "r") | X %among% c("t", "s") & Z %among% "y")

evaluate_expression(formula_to_expression(X %among% "s" & X %among% "u" & (Z %among% c("y", "a") | Z %among% "a")), assignment)
evaluate_expression(formula_to_expression(Z %among% "a" & W %among% c("p", "r") | X %among% c("t", "s") & Z %among% "y"), assignment)

evaluate_expression(formula_to_expression(Z %among% "a" & W %among% c("p", "r")), assignment)
evaluate_expression(formula_to_expression(X %among% c("t", "s") & Z %among% "y"), assignment)

clause1 = Z %among% "a" & W %among% c("p", "r")
clause2 = X %among% c("t", "s") & Z %among% "y"

evaluate_expression(formula_to_expression(clause1), assignment)
evaluate_expression(formula_to_expression(clause2), assignment)
evaluate_expression(formula_to_expression(clause1 | clause2), assignment)

quote(
(Y %among% "w" & W %among% c("r", "q") | (X %among% "t" | Y %among% "v")) & (X %among% c("t", "s") | Z %among% "y" | W %among% "p" & Z %among% c("y", "z")) |
(W %among% c("p", "r") & Z %among% c("z", "a") | (W %among% "q" | W %among% "p")) & ((X %among% c("t", "s") | Y %among% "x") & (Z %among% "z" & X %among% c("u", "s")))
) |> evaluate_expression(assignment)

(
(Y %among% "w" & W %among% c("r", "q") | (X %among% "t" | Y %among% "v")) & (X %among% c("t", "s") | Z %among% "y" | W %among% "p" & Z %among% c("y", "z")) |
(W %among% c("p", "r") & Z %among% c("z", "a") | (W %among% "q" | W %among% "p")) & ((X %among% c("t", "s") | Y %among% "x") & (Z %among% "z" & X %among% c("u", "s")))
) |> formula_to_expression() |> evaluate_expression(assignment)


c1 = (Y %among% "w" & W %among% c("r", "q") | (X %among% "t" | Y %among% "v")) & (X %among% c("t", "s") | Z %among% "y" | W %among% "p" & Z %among% c("y", "z"))
c2 = (W %among% c("p", "r") & Z %among% c("z", "a") | (W %among% "q" | W %among% "p")) & ((X %among% c("t", "s") | Y %among% "x") & (Z %among% "z" & X %among% c("u", "s")))

evaluate_expression(formula_to_expression(c1), assignment)
evaluate_expression(formula_to_expression(c2), assignment)
evaluate_expression(formula_to_expression(c1 | c2), assignment)
#   W X Y Z
#17 q u w y
     (Z ∈ {z, y} | X ∈ {t, s})
   & (Y ∈ {v, w} | X ∈ {t} | Z ∈ {z})
   & (X ∈ {s, t} | Y ∈ {x, v}) # !!!
   & (W ∈ {r, q} | X ∈ {t} | Y ∈ {v} | Z ∈ {z})
   & (W ∈ {p} | X ∈ {s, t} | Z ∈ {y} | Y ∈ {x})


[[1]]
CnfClause:
  Z ∈ {z, y} | X ∈ {t, s}

[[2]]
CnfClause:
  Y ∈ {v, w} | X ∈ {t} | Z ∈ {z}

[[3]]
CnfClause:
  W ∈ {p} | X ∈ {t, s} | Z ∈ {z, y}

[[4]]
CnfClause:
  Z ∈ {y, z} | X ∈ {s, t} | Y ∈ {x}

[[5]]
CnfClause:
  W ∈ {r, q} | X ∈ {s, t} | Y ∈ {x, v}  # this one removes W

[[6]]
CnfClause:
  W ∈ {r, q} | X ∈ {t} | Y ∈ {v} | Z ∈ {z}

[[7]]
CnfClause:
  W ∈ {p} | X ∈ {s, t} | Z ∈ {y} | Y ∈ {x}

#   W X Y Z
# 5 q t v y
quote(
Y %among% c("w", "x") & X %among% "t" | W %among% "p" & X %among% "u" |
(Z %among% c("y", "z") | Y %among% "x") & (Z %among% "z" & W %among% "q")
) |> # |> evaluate_expression(assignment)
eval() |> formula_to_expression() |> evaluate_expression(assignment)

c1 = Y %among% c("w", "x") & X %among% "t" | W %among% "p" & X %among% "u"
c2 = (Z %among% c("y", "z") | Y %among% "x") & (Z %among% "z" & W %among% "q")

evaluate_expression(formula_to_expression(c1), assignment)
evaluate_expression(formula_to_expression(c2), assignment)
evaluate_expression(formula_to_expression(c1 | c2), assignment)


> c1
CnfFormula:
     (X ∈ {t, u})
   & (W ∈ {p} | Y ∈ {w, x})
   & (X ∈ {u} | Y ∈ {w, x})
   & (W ∈ {p} | X ∈ {t})
> c2
CnfFormula:
     (Z ∈ {z})
   & (W ∈ {q})
> c1 | c2
CnfFormula:
     (X ∈ {t, u} | Z ∈ {z})
   & (X ∈ {t, u} | W ∈ {q})
   & (W ∈ {q, p} | X ∈ {t})
   & (W ∈ {p} | X ∈ {t} | Z ∈ {z})
   & (X ∈ {u} | Y ∈ {w, x} | W ∈ {q})

   5, 6,
