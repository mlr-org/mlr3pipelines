# Use Cases

This document contains a list of use-cases we want to contain.

------
**Shortnames:**
	[PO]: PipeOperator
	[GN]: GraphNode
	[G]:  Graph
	[GL]: GraphLearner

------
## Pipe Operators:

- **Organize:**
	- `pipeOpBranch`
	- `pipeOpUnbranch`
	- `PipeOpFeatureUnion`
	- `PipeOpNULL`

- **Learner:**
	- `pipeOpLearner`
	- `PipeOpLearnerCV`
	- `pipeOpModelAverage`

- **Preprocessing:**
	- `PipeOpPCA`
	- `PipeOpScale`
	- `PipeOpDownsample`

- **Target Operators:**
	- `PipeOpThreshold`
	- `PipeOpTrafoY`


## Helper functions:

- `gunion(....)`
  Takes multiple graphs and puts them in one level (parallel to each other).
  Same as the output of a GraphOp that produces multiple outputs

- `rep(graph, k)`
  Takes a graph and copies it $k$ times and puts them in a list in
  the same way as `gunion(...)`.

## Getter functions:

- `PO$id` Get the id of a PipeOp.

Fragen: wie setzt man (schnell) die id von einem PipeOp?
wie bekommt man params vom pipeop unique. vermutlich id.param? auch doof. 
welche hyperpars exportiert ein pipeop?


## Use Cases 

------
### Usecase: Linear Pipeline

#### Usecase a): Linear Pipeline

*Scenario:* 
*We obtain a task (iris) and a learner(rpart) from mlr3.*
*Before we train the learner, we want to transform the data using PCA.*

We concatenate a `PipeOpPCA` and a `PipeOpLearner` using the `%>>%` (then) operator.
This internally does the following:
	- Wrap the [PO's] into **GraphNodes**.
	- Chains them together and returns a **Graph**.

```r
task = mlr3::mlr_tasks$get("iris")
lrn_rp = mlr3::mlr_learners$get("classif.rpart")
g = PipeOpPCA() %>>% PipeOpLearner(lrn_rp)
g$train(task)
g$predict(task)
```


##### [[pipeOpPCA]]

	- **train**: 
		- input: Task
		- does: Computes and stores rotation matrix into **.params** slot.
		- returns: Task.
	- **predict**: 
		- input: Task
		- does: rotates input data using **.params** slot.
		- returns: Task.

##### [[pipeOpLearner]]

	- **train**: 
		- input: Task
		- does: Calls the learner's`train()` method on it. Stores the model in **.params**.
		- returns: NULL
	- **predict**:
		- input: Task
		- does: Calls the `predict()` method of the stored model.
		- returns: Prediction


#### Usecase b): Resample Linear Pipeline

*Scenario:* 
*We want to resample the Graph on different folds of the data.*

We create a `GraphLearner` from the Graph and use **mlr3's** resampling.

```r
lrn_g = GraphLearner$new(graph = g)
lrn$parvals = list(pca.center = TRUE, rpart.cp = 0.1) 
resampling = mlr_resamplings$get("holdout")
rr = resample(task, lrn_g, resampling)
```

Access results: 
```r
rr[1, “models”]$learner.model[["rpart"]]$learner.model
rr[1, “models”]$learner.model[["pca"]]$params
```

##### [[GraphLearner]]

	Wraps a Graph and allows it to be used like a learner.
	- **train**: 
		- input: Task
		- does: Calls the graph's `train()` method on it.
		- returns: NULL
	- **predict**:
		- input: Task
		- does: Calls the `predict()` method on the Graph.
		- returns: NULL

#### Usecase c): Tune Linear Pipeline

*Scenario:* 
*We want to tune the Graph.*

We use the `GraphLearner` and use **mlr3's** tuning.

```r
measures = mlr3::mlr_measures$mget("mmce")
param_set = paradox::ParamSet$new(
  params = list(
   ParamLgl$new("pca.center"),
   ParamDbl$new("rpart.cp", lower = 0.001, upper = 0.1)
)
ff = FitnessFunction$new(task, lrn_g, resampling, measures, param_set)
terminator = TerminatorEvaluations$new(10)
rs = TunerRandomSearch$new(ff, terminator)
tr = rs$tune()$tune_result()
```

------
### Usecase: Feature union

```r
op1 = PipeOpScaler$new()
op2a = PipeOpPCA$new()     # was machen wir hier mit den targets?
op2b = PipeOpNULL$new()
op3 = PipeOpFeatureUnion$new()
op4 = PipeOpLearner$new(learner = "classif.rpart")

op1 >> gunion(op2a, op2b) >> op3 >> op4
```

##### [[pipeOpScale]]

	- **train**: 
		- input: Task
		- does: Scales Task to mean 0 and sd 1. Stores mean and sd into **.params**
		- returns: Task
	- **predict**: 
		- input: Task
		- does: scales input task using **.params** slot.
		- returns: Task

##### [[pipeOpNull]]

	- **train**: 
		- input: Anything
		- does: Nothing
		- returns: Input (Anything)
	- **predict**:
		- input: Anything
		- does: Nothing
		- returns: Input (Anything)

##### [[pipeOpFeatureUnion]]

	- **train**: 
		- input: List of Task's
		- does: Cbinds features from all tasks and one target col.
				(constraint: target cols and row ids all need to be the same,
			 	and same order).
		- returns: Task
	- **predict**:
		- input: List of Task's
		- does:  same as train (without targets, here we have to  do nothing).
		- returns: Task



### Usecase: Bagging (Downsampling, Modelling and Modelaveraging)

*Scenario:* 
*We want to do bagging (Train several models on subsamples of the data and 
average predictions.*

We use the `PipeOpDownSample` operator in conjunction with a `PipeOpLearner` to train a model. `rep()` let's us do the same operation multiple times.
Afterwards we average all predictions using `PipeOpModelAverage`

```r
op1 = PipeOpDownSample$new(rate = 0.6)
op2 = PipeOpLearner$new("classif.rpart")
op3 = PipeOpModelAverage$new()

rep(op1 %>>% op2, 30) %>>% op3
```

##### [[PipeOpDownsample]]

	- **train**: 
		- input: Task
		- does: Downsamples (samples rows with replacement) the input for a fraction of the original n.
		- returns: Task
	- **predict**: 
		- input: Task
		- does: Nothing
		- returns: Task

##### [[PipeOpModelAverage]]

	- **train**: 
		- input: Nothing
		- does: Nothing
		- returns: Nothing
	- **predict**: 
		- input: List of Predictions
		- does: Averages Predictions
		- returns: Prediction


### Usecase: Stacking 

*Scenario:* 
*We want to do stacking (Train several models on the data and combine predictions).*

#### Usecase a): Stacking with simple Averaging

We use various `PipeOpLearner`'s' to train models. `gunion()` let's us put the learner's parallel to each other.
Afterwards we average all predictions using `PipeOpModelAverage`.

```r
op1 = PipeOpLearner$new("regr.rpart")
op2 = PipeOpLearner$new("regr.svm")
gunion(op1, op2) %>>% PipeOpModelAverage$new()
```

#### Usecase b): Stacking with SuperLearner


Instead of using `PipeOpModelAverage`, we combine predictions to a `PipeOpLearner`.

```r
# Superlearner: We instead use PipeOpCV 
op1 = PipeOpCV("regr.rpart")
op2 = PipeOpCV("regr.svm")
gunion(op1, op2) %>>% PipeOpFeatureUnion() %>>% PipeOpLearner("regr.lm")
```

##### [[PipeOpCV]]

	- **train**: 
		- input: Task
		- does: 1. Trains models an different folds of data. Predicts on holdout splits.
				2. Trains a model on full data, saves model to **.params**.
		- returns: Prediction
	- **predict**: 
		- input: Task
		- does: Predict with model from .params
		- returns: Prediction

#### Usecase c): Stacking with SuperLearner and original data.

By adding a `pipeOpNull`, we add the original features to the SuperLearner.

```r
gunion(op1, op2, PipeOpNull) %>>% PipeOpFeatureUnion() %>>% PipeOpLearner("regr.lm")
```



### Usecase: Multiclass with Binary

```r
op1 = PipeOpMultiClass2Binary(codebook)
op2 = PipeOpLearner("classif.svm")
op1 >> rep(op2, k) >> PipeOpModelAvg$new()
```


op1 >=> rep(op2, k) >> PipeOpModelAvg$new()

PipeOpMultiClass2Binary:
	train: takes task, produces list of tasks, task-y is binarized version of orig y 
	predict: copies data k-times


Usecase: Multiplexing of different Ops (here: Learners)

op1 = PipeOpLearner$new(“regr.rpart”)
op2 = PipeOpLearner$new(“regr.svm")
g = pipeOpBranch$new(selected = 1) >> gunion(op1, op2) >> PipeOpGather(aggrFun =


 NULL)


op1 = PipeOpLearner$new(“regr.rpart”)
op2 = PipeOpLearner$new(“regr.svm")
g = PipeOpBranch$new() >> gunion(op1, op2)

PipeOpBranch
	train: takes "any" object X, returns list of [NOP, NOP, X, NOP], 
where position of X is "selected"
predict: same as train


# more complex usecase, multiplex, then something else
op1 = PipeOpLearnerPCA$new()
op2 = PipeOpNULL$new()
op3 = PipeOpLearner$new("classif.rpart")

g = PipeOpBranch$new() >> gunion(op1, op2) >> pipeOpUnbranch() >> op3







----


pipeOpBranch does nothing during train/predict. The “branched” (following) nodes then either obtain data (if selected == op$id), or NULL. We also have to specify what happens if 
a pipeOp obtains NULL as an input.

pipeOpGather collects from multiple parents and aggregates them by aggrFun. If NULL it is assumed only one parent returns something which is not NULL/PipeOpNull


Questions:
Is this not something more general like PipeOpBranch? 
Does every PipeOp have a default method when NULL is passed?





Usecase: Chunk data into k parts, train on each, then model average
chunk(10) >=> rep(PipeOpLearner("classif.rpart"), 10) >> PipeOpModelAvg()


Usecase: Thresholding

op1 = PipeOpCV$new(“”classif.rpart”)
op2 = PipeOpThresholding$new(method, measure, ...)

g = op1 >> op2 

PipeOpThreshold takes a prediction object (of a classification task) at train time and optimizes the threshold for a given perf. Metric.
At predict time it takes a prediction object and sets the threshold. 



------------------------------------ Unknown territory, Here Be Dragons

Trafo y (logarithm of y)

g = PipeOpTrafoY(fun = log) >> PipeOpLearner(“classif.svm”) >> PipeOpTrafoY(fun = exp)
g$train(task) [[trafoY(log)) >>  train(“classif.svm”, task) >> identity]]
g$predict(task) [[identity >> predict(model, task) >> trafoPreds(exp)]]

Pot = PipeOpTrafoY(traintrafo = log, predicttrafo = exp)

G = pot >> PipeOpLearner() >> pot.clone(deep = TRUE)

 
PipeOpTrafoY
	in train: takes task, returns task, transform y by fun
	in predict: takes Prediction, returns Prediction, transform yhat by fun


By Input type
Dataset (with y column) can be training or prediction, but transform in both cases, with 'traintrafo'
learner model: nop
prediction: transform y-hat with 'predicttraf'

Restrictions:
We can not tune over these transformations
User needs to ensure that trafos are correct



MultiLabel / MultiOutput (1 Task, 3 Outputs, NoCV)

Chained:
PipeOpSetTarget(“out1”) >> PipeOpLearner(“rpart”, id = “r1”) >> PipeOpCbindLearnerPrediction() >> PipeOpSetTarget(“out2”, id = “r2”) >> PipeOpLearner(“rpart”) >> PipeOpCbindLearnerPrediction() >>
PipeOpsetTarget(“final_out”) >> PipeOpLearner(“rpart”, id = “r3”) 
Parallel:
list(
PipeOpSetTarget(“out1”)       >> PipeOpLearner(“rpart”, id = “r1”),
PipeOpSetTarget(“out2”)       >> PipeOpLearner(“rpart”, id = “r2”),
PipeOpsetTarget(“final_out”) >> PipeOpLearner(“rpart”, id = “r3”)
) >> PipeOpCollectLearners()


PipeOpSetTarget:
Train:  Unset previous target (Keep hidden)
		Set new target
Predict: <No action>
Args: Should this have a hide_previous_target param?
PipeOpCbindLearnerPreds:
Train:  Get preds from previous node
	Cbind to DataBackend
Predict: Get newdata preds, cbind to data.




preproc operation: daten %>>% po --> preprozessierte Daten
trainierte preproc op auf neue daten: neuedaten %>>% (daten %>>% po) --> neuedaten insofern veraendert wie (daten%>>%po) es voreinstellt
training eines Modells: daten %>>% lrnpo --> modell trainiert auf daten
prediction: neuedaten %>>% (daten %>>% lrnpo) --> neuedaten predicted auf dem trainierten modell
splitOp: aufteilen in training und prediction data.
beide stroeme in einen knoten, und der macht dann beides?
trainierter knoten wird ausgespuckt und der predict-stream wird verwendet?
resampling mit  splitOp(k) %>>% rep(lrnpo, k) gibt direkt performances zurueck?
Alternativ splitOp(k) %>>% rep({lrnpo on training data, trained_learner on predicted data} %>>% measure performance, k)?
vll eher durch metagraphen
tuning: tunenode(<graph>), graph kriegt daten rein und gibt eine performance measure raus, und evtl auch ein predicted measure?
problem hier ist: die performance will man vll auf eine art messen (e.g. cv), die tatsaechliche Anwendung dann auf den ganzen Datensatz, aber ein grossteil der operation ist schon der gleiche.
zeilennummern will man schon irgendwie behalten
send parameter confs through the graph




# graph definition:
# op1, op2, pma exist and are supposed to be connected by
# op1 
#          > pma
# op2 
GraphDef({
	op1 %>>% pma
	op2 %>>% pma
	# all that have no output automatically output to SINK
	# all that have no input automatically input from SOURCE
	# but SOURCE and SINK can be given explicitly
	SOURCE %>>% pma
})


Fun: RotationForest
# Single Split (Takes 1 Data input, Outputs 2 Datasets)
splitter = pipeOpDownSample() >> pipeOpFeatureSubSample() >> pipeOpPCA >> PipeOpLearner(“rpart”, max.depth = 1) >> splitDataFromRpart()

splitter >> rep(2, splitter)

