# Use Cases

This document contains a list of use-cases we want to contain.

**Shortnames:**
  [PO]: PipeOperator
  [GN]: GraphNode
  [G] : Graph
  [GL]: GraphLearner


## Pipe Operators:

- **Meta:**
  - `pipeOpBranch`                          | broadcast
  - `pipeOpChunk`                           | broadcast
  - `pipeOpUnbranch`                        | aggregate
  - `pipeOpFeatureUnion`                    | aggregate
  - `pipeOpNULL`                            | linear
  - `pipeOpCopy`                            | broadcast

                                              train: input --store-params--> output        predict: input --use-params--> output
- **Learner:**
  - `pipeOpLearner`                         | linear    | task --model--> NULL           | task --model--> prediction
  - `PipeOpLearnerCV`                       | linear    | task --model--> cvtask         | task --model--> prediction
  - `pipeOpModelAverage`                    | aggregate | task --NULL--> NULL            | list-of-prediction --NULL--> prediction

- **Preprocessing:**
  - `PipeOpPCA`                             | linear    | task --params--> task          | task --params--> task
  - `PipeOpScale`                           | linear    | task --params--> task          | task --params--> task
  - `PipeOpDownsample`                      | linear    | task --NULL--> task            | task --NULL--> task

- **Target Operators:**
  - `PipeOpThreshold`                       | linear    | cvtask --threshold--> NULL     | prediction --threshold--> prediction
  - `PipeOpTrafoY`                          | linear    | task --NULL--> task            | prediction --NULL--> prediction
  - `PipeOpMultiClass2Binary`               | broadcast | task --NULL--> list-of-task    | task --NULL--> list-of-tasks
  - `PipeOpSetTarget`                       | linear    | task --NULL--> task            | task --NULL--> task

##### [[pipeOpPCA]]

  - **train**:
    - input: [[Task]]
    - does: Computes and stores rotation matrix into **.params** slot.
    - returns: [[Task]]
  - **params:**: rotation matrix
  - **predict**:
    - input: [[Task]]
    - does: rotates input data using **.params** slot.
    - returns: [[Task]]

##### [[pipeOpLearner]]

  - **train**:
    - input: [[Task]]
    - does: Calls the learner's`train()` method on it. Stores the model in **.params**.
    - returns: NULL
  - **params:**: trained model
  - **predict**:
    - input: [[Task]]
    - does: Calls the `predict()` method of the stored model.
    - returns: Prediction

##### [[GraphLearner]]

  Wraps a Graph and allows it to be used like a learner.
  - **train**:
    - input: [[Task]]
    - does: Calls the graph's `train()` method on it.
    - returns: NULL
  - **predict**:
    - input: [[Task]]
    - does: Calls the `predict()` method on the Graph.
    - returns: NULL

##### [[PipeOpSetTarget]]

  - **train**:
    - input: [[Task]]
    - does: Set previous target col to hidden. Set new target variable.
    - returns: [[Task]]
  - **params:**:
  - **predict**:
    - input: [[Task]]
    - does: Nothing
    - returns: [™Task]]

##### [[PipeOpMultiClass2Binary]]

  - **train**:
    - input: [[Task]]
    - does: Produces multiple [[Task]]s, where y is the binarized version of original y.
    - returns: List of [[Task]]s
  - **params:**: Nothing
  - **predict**:
    - input: [[Task]]
    - does: Copy [[Task]] k-times
    - returns: List of [[Task]]s

##### [[PipeOpTrafoY]]

  - **train**:
    - input: [[Task]]
    - does: Transforms target by fun.
    - returns: [[Task]]
  - **params:**:
  - **predict**:
    - input: Prediction
    - does: Transform prediction by fun
    - returns: Prediction

##### [[pipeOpScale]]

  - **train**:
    - input: [[Task]]
    - does: Scales [[Task]] to mean 0 and sd 1. Stores mean and sd into **.params**
    - returns: [[Task]]
  - **params:**: mean, sd of all training data features.
  - **predict**:
    - input: [[Task]]
    - does: scales input [[Task]] using **.params** slot.
    - returns: [[Task]]

##### [[pipeOpNull]]

  - **train**:
    - input: Anything
    - does: Nothing
    - returns: Input (Anything)
    - **params:**: Nothing
  - **predict**:
    - input: Anything
    - does: Nothing
    - returns: Input (Anything)

##### [[pipeOpFeatureUnion]]

  - **train**:
    - input: List of [[Task]]'s
    - does: Cbinds features from all [[Task]]s and one target col.
        (constraint: target cols and row ids all need to be the same,
        and same order).
    - returns: [[Task]]
  - **params:**: Nothing
  - **predict**:
    - input: List of [[Task]]'s
    - does:  same as train (without targets, here we have to  do nothing).
    - returns: [[Task]]

##### [[PipeOpBranch]]

  - **train**:
    - input: Anything
    - does: Creates list [NULL, NULL, X, NULL]; where X is at position **selected**.
    - returns: List of [[Task]] / NULLs
  - **params:**: Nothing
  - **predict**: Same as train.

##### [[PipeOpDownsample]]

  - **train**:
    - input: [[Task]]
    - does: Downsamples (samples rows with replacement) the input for a fraction of the original n.
    - returns: [[Task]]
  - **params:**: Nothing
  - **predict**:
    - input: [[Task]]
    - does: Nothing
    - returns: [[Task]]

##### [[PipeOpModelAverage]]

  - **train**:
    - input: Nothing
    - does: Nothing
    - returns: Nothing
  - **params:**: Nothing
  - **predict**:
    - input: List of Predictions
    - does: Averages Predictions
    - returns: Prediction

##### [[PipeOpLearnerCV]]

  - **train**:
    - input: [[Task]]
    - does: 1. Trains models an different folds of data. Predicts on holdout splits.
        		2. Trains a model on full data, saves model to **.params**.
    - returns: Prediction
  - **params:**: trained model
  - **predict**:
    - input: [[Task]]
    - does: Predict with model from .params
    - returns: Prediction

##### [[PipeOpThreshold]]

  - **train**:
    - input: Prediction
    - does: Optimizes the threshold for a given performance metric.
    - returns: Prediction
  - **params:**: Optimal threshold
  - **predict**:
    - input: Prediction
    - does: Binarizes the prediction using **.params**
    - returns: Prediction



## Use Cases

### Usecase: Linear Pipeline

#### Usecase a): Linear Pipeline

Scenario:
We obtain a task (iris) and a learner(rpart) from mlr3.
Before we train the learner, we want to transform the data using PCA.

We concatenate a `PipeOpPCA` and a `PipeOpLearner` using the `%>>%` (then) operator.
This internally does the following:
  - Wrap the [PO's] into **GraphNodes**.
  - Chains them together and returns a **Graph**.

```r
task = mlr_tasks$get("iris")
lrn_rp = mlr_learners$get("classif.rpart")
g = PipeOpPCA() %>>% PipeOpLearner(lrn_rp)
g$train(task)
g$predict(task)
```

#FIXME: add scaling, so we have 3 steps



#### Usecase b): Resample Linear Pipeline

*Scenario:*
*We want to resample the Graph on different folds of the data.*

We create a `GraphLearner` from the Graph and use **mlr3's** resampling.

```r
lrn_g = GraphLearner$new(graph = g)
lrn$parvals = list(pca.center = TRUE, rpart.cp = 0.1)
resampling = mlr_resamplings$get("holdout")
rr = resample([[Task]], lrn_g, resampling)
```

Access results:
```r
rr[1, "models"]$learner.model[["rpart"]]$learner.model
rr[1, "models"]$learner.model[["pca"]]$params
```


#### Usecase c): Tune Linear Pipeline

*Scenario:*
*We want to tune the Graph.*

We use the `GraphLearner` and use **mlr3's** tuning.

```r
measures = mlr_measures$mget("mmce")
param_set = paradox::ParamSet$new(
  params = list(
   ParamLgl$new("pca.center"),
   ParamDbl$new("rpart.cp", lower = 0.001, upper = 0.1)
)
ff = FitnessFunction$new([[Task]], lrn_g, resampling, measures, param_set)
terminator = TerminatorEvaluations$new(10)
rs = TunerRandomSearch$new(ff, terminator)
tr = rs$tune()$tune_result()
```

### Usecase: Feature union

```r
op1 = PipeOpScaler$new()
op2a = PipeOpPCA$new()
op2b = PipeOpNULL$new()
op3 = PipeOpFeatureUnion$new()
op4 = PipeOpLearner$new(learner = "classif.rpart")

op1 %>>% gunion(op2a, op2b) %>>% op3 %>>% op4
```



### Usecase: Bagging (Downsampling, Modelling and Modelaveraging)

*Scenario:*
*We want to do bagging (Train several models on subsamples of the data and
average predictions.*

We use the `PipeOpDownSample` operator in conjunction with a `PipeOpLearner` to train a model. `greplicate()` let's us do the same operation multiple times.
Afterwards we average all predictions using `PipeOpModelAverage`

```r
op1 = PipeOpDownSample$new(rate = 0.6)
op2 = PipeOpLearner$new("classif.rpart")
op3 = PipeOpModelAverage$new()

greplicate(op1 %>>% op2, 30) %>>% op3
```


# FIXME:
  Info: If our predictions are numeric, we simply average.
      If our predictions are binary, we majority vote (?)
      If our predictions are probabilities, we average (?)
      If our predictions are multiclass, we (?).
      Are there any other situations ?

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
Instead of a `pipeOpLearner` we use a `PipeOpLearnerCV`, in order to avoid overfitting.

```r
# Superlearner: We instead use PipeOpLearnerCV
op1 = PipeOpLearnerCV("regr.rpart")
op2 = PipeOpLearnerCV("regr.svm")
gunion(op1, op2) %>>% PipeOpFeatureUnion() %>>% PipeOpLearner("regr.lm")
```


#### Usecase c): Stacking with SuperLearner and original data.

By adding a `pipeOpNull`, we add the original features to the SuperLearner.

```r
gunion(op1, op2, PipeOpNull) %>>% PipeOpFeatureUnion() %>>% PipeOpLearner("regr.lm")
```

#### Usecase d): Multilevel Stacking

We can do the same on multiple levels by just adding the same `PipeOpLearnerCV()` again after the feature union.

```r
g = gunion(op1, op2, PipeOpNull) %>>% PipeOpFeatureUnion() %>>% 
	gunion(op1, op2) %>>% PipeOpFeatureUnion() %>>% 
	PipeOpLearner("regr.lm")
```


### Usecase: Multiclass with Binary

*Scenario:*
*We have a multiclass target, and want to predict each class in a binarized manner.*
*This occurs, for example if our model can only do binary classification.*

We use `PipeOpMultiClass2Binary` in order to split our [[Task]] up into multiple binary [[Task]]s.
Afterwards, we replicate our learner $k$ (where $k$ = number of classes - 1) times.
In order to aggregate the predictions for different classes, we use the `PipeOpModelAverage`.

```r
op1 = PipeOpMultiClass2Binary(codebook)
op2 = PipeOpLearner("classif.svm")

op1 %>>% greplicate(op2, k) %>>% PipeOpModelAverage$new()
# or:
op1 %>=>% greplicate(op2, k) %>>% PipeOpModelAverage$new()
```
	

### Usecase: Multiplexing of different Ops

*Scenario:*
*We want our pipeline to branch out, either in one direction or the other.*
*This is usefull, for example when tuning over multiple learners.*

####  Usecase: Multiplexing different learners

We use the `pipeOpBranch` in order to have our data flow only to one of the following operators.
Afterwards we collect the two streams using `PipeOpGather`.
We can now treat the pipeline like a linear pipeline.


```r
op1 = PipeOpLearner$new("regr.rpart")
op2 = PipeOpLearner$new("regr.svm")
g = pipeOpBranch$new(selected = 1) %>>% gunion(op1, op2) %>>% PipeOpGather(aggrFun = NULL)
```


####  Usecase: Multiplexing preprocessing steps

```r
op1 = PipeOpLearnerPCA$new()
op2 = PipeOpNULL$new()
op3 = PipeOpLearner$new("classif.rpart")

g = PipeOpBranch$new(selected = 1) %>>% gunion(op1, op2) %>>% PipeOpGather(aggrFun = NULL) %>>% op3
```

**FIXME:** Does every PipeOp have a default method when NULL is passed?


### Usecase: Chunk data into k parts, train on each, then model average

*Scenario:*
*We want our pipeline to branch out, either in one direction or the other.*
*This is usefull, for example when tuning over multiple learners.*


We use the `pipeOpChunk` operator to partition the [[Task]] into $k$ smaller [[Task]]s.
Afterwards we train $k$ learners on each sub[[Task]].
Afterwards the predictions are averaged in order to get a single prediction.

```r
pipeOpChunk(k) %>>% greplicate(PipeOpLearner("classif.rpart"), 10) %>>% PipeOpModelAvg()
```


### Usecase: Thresholding

*Scenario:*
*We want to obtain an optimal threshold in order to decide whether something is of class x or y.*

We use `PipeOpLearnerCV` to obtain cross-validated predictions. Afterwards we use `PipeOpThreshold()` to compute an
optimal threshold.

```r
op1 = PipeOpLearnerCV$new(""classif.rpart")
op2 = PipeOpThreshold$new(method, measure, ...)
g = op1 %>>% op2
```

------------------------------------
## Unknown territory, Here Be Dragons
------------------------------------


### Usecase: ThresholdingTrafo y (logarithm of y)

*Scenario:*
*We want to transform our target variable, for example using a log-transform.*

We use the `PipeOpTrafoY` in order to log-transform the data.
We use another `PipeOpTrafoY` after the learner in order to re-transform our data onto the original scale.

```r
top = PipeOpTrafoY(train = log, predict = identity)
retop = PipeOpTrafoY(train = identity, predict = exp)

g = top %>>% PipeOpLearner("classif.svm") %>>% retop
```

What happens:
```
  - g$train([[Task]]) [[trafoY(log)) >>  train("classif.svm", [[Task]]) %>>% identity]]
  - g$predict([[Task]]) [[identity >> predict(model, [[Task]]) >> trafoPreds(exp)]]
```

FIXME:
  - Using the same operator twice would violate the acyclic property.
  - We can not tune over **par.vals** of TrafoY, as we have a hard time storing them.
  - User needs to ensure that trafos are correct

### Usecase: MultiOutput / Multiple Targets (1 [[Task]], 3 Outputs, NoCV)


####  Usecase a): MultiOutput Parallel

*Scenario:*
*We have three possible output variables we want to predict in parallel.*

We set different targets before training each learner using `PipeOpSetTarget`.
Afterwards the different learners are collected with `PipeOpModelAverage`.

```r
g = gunion(
  PipeOpSetTarget("out1") %>>% PipeOpLearner("rpart", id = "r1"),
  PipeOpSetTarget("out2") %>>% PipeOpLearner("rpart", id = "r2"),
  PipeOpsetTarget("final_out") %>>% PipeOpLearner("rpart", id = "r3")
  ) %>>% PipeOpModelAverage()
```

####  Usecase b): MultiOutput Chained

*Scenario:*
*We have three possible output variables available during training, but they will not be availalbe during test time.*
*We want to leverage info from out1 and out2 to improve prediction on final_out*

We obtain cross-validated predictions using `PipeOpLearnerCV` sequentially for each target and use them to train the sequential models.

```r
pnop = pipeOpNull()

g = PipeOpSetTarget("out1") %>>%
  gunion(PipeOpLearnerCV("rpart", id = "r1"), pnop) %>>%
  PipeOpFeatureUnion() %>>%
  PipeOpSetTarget("out2", id = "r2") %>>%
  gunion(PipeOpLearnerCV("rpart"), pnop) %>>%
  PipeOpLearnerCV() %>>%
  PipeOpsetTarget("final_out") %>>%
  PipeOpLearner("rpart", id = "r3")
```

####  Usecase c): Hurdle Models

*Scenario:*
*We have a zero-inflated numeric target variable (e.g. amount unpaid bills).*
*We want to leverage this info.*

We obtain cross-validated predictions for whether the target variable is 0.
We the use the prediction for this intermediate target for the final prediction.


```r
pnop = pipeOpNull()
# data is our dt with a numeric "target"
data$target_is_null = data$target > 0
g = PipeOpSetTarget("target_is_null") %>>%
  gunion(PipeOpLearnerCV("rpart", id = "r1"), pnop) %>>%
  PipeOpFeatureUnion() %>>%
  PipeOpSetTarget("target") %>>%
  PipeOpLearner("rpart", id = "r3")
```