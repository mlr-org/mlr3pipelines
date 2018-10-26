# Concept

## Executive summary

This design document describes some classes that represent the current API for mlr3piplines.
In short, a **Pipeline** is a doubly connected graph, that contains several **GraphNode**s.
Each **GraphNode** contains a **PipeOP**, that contains the logic used to manipulate the inputs, i.e.
functions that transform a pipe's inputs to it's outputs.
This repository's aim is to expose a set of building blocks that can be used by users to
create and tune over arbitrarily complex pipelines.

In order to allow for more advanced Pipelines like *stacking* or *ensembling*, we require
more complicated building blocks, which is why some building blocks seem overly complex.


------------------------------------------------------------------------------------------
# Pipes
class  PipeOperator
members:


------------------------------------------------------------------------------------------
## class PipeOp

concept:
A PipeOp is a single tranformation of inputs into outputs.
During training it takes inputs, tranforms them, while doing that learns and stores its
parameters and then returns the output. It also changes its states from [unlearnerd] to [learned]

constructor() : no args

members:
- id [character(1)] : unique id of operator
- parset [ParamSet] :  hyperpar space
- parvals [list] : current settings
- params [any] : learned params from training

methods:
- train(input) : [any] --> [any] : fits params and transforms input data
- predict(input) : [any] --> [any] : uses fitted params to transform input data
- reset() : [void] --> [void] : Resets params to NULL


active bindings:
- id
- par_set
- par_vals
- params
- is_learned [logical(1)] : is PipeOp trained? checks if params is NULL


questions:
- what is public what is private?
- id, parvals can be set later per AB.
- some args can be set while OP is [unlearned]
  Might be dangerous otherwise, but can be discussed later.
- how do we markup types of input and output?
- Do we need the separation of GraphNode and PipeOp's?
  Alternative: Have all PipeOp's inherit from GraphNode
- Should Ensemble pipeOp's return [preds] or [dt]?
  We should probably write converters anyway.
- do we want 2 functions train and predict? or just on "apply" function?
  which acts dependiong on the is_learned state?


- training
  D1 ---> OP[unlearned] --> D2
  - Data enters, Output: Transformed data
  - OP learns params for prediction, and saves those.
  - OP is_learned() switches to TRUE
  - OP$params returns learned params (iff OP is [learned], else NULL)

- application  / predict
  ND1 ---> OP[learned] --> ND2
  - Transforms newdata using learned params


-----------------------------------------------------------------------------------------
## class GraphNode
concept:
A GraphNode is a (doubly connected) DAG, where each node carries as payload a single PipeOp.
A node can be traversed / executed, when results from all predecessors are available.
They are then passed to the PipeOp, the PipeOp is trained / predicted, and the result is stored in the node.

members:
- pipeop    : PipeOp      : operator in that node
- next      : list        : next nodes
- prev      : list        : previous nodes
- inputs    : list        : list of results from previous nodes
- result    : untyped     : result of current operator


methods:
- acquire_inputs     --> outlist    : fits params and transforms input data
- set_next(ops)
- set_prev(ops)


active bindings:
- result
- has_result
- has_no_prevs [logical(1)] : are no prev nodes connected?
- can_fire [logical(1)] : are all input results available?


question:
- set_next + set_prev should be ABs?


------------------------------------------------------------------------------------------
## class Graph
members:
  source_node

methods
- index operator [[id]]  --> points to GraphNode

active bindings:
- parset [ParamSet]. returns flat ParamSet, names are pipeOpid:parid, it is computed on the fly
- parvals(list). set param vals, name scheme as above, passed them down to pipeOps via id
- ids

questions:
- does index op also work with ints?
- do we want the fourth layer of topological sorting?
- how do we loops over all nodes? how do we apply something to all nodes?



------------------------------------------------------------------------------------------
FIXME: This class is new and needs to be discussed.
## class Multiplexer
concept:
A Multiplexer contains multiple GraphNodes.
It extends the corresponding paramset to allow for selection of ONE of the contained Nodes.
During training, one contained pipeOp is trained / predicted.

members:
- pipeops   : list[PipeOp]     : operator in that node
- inputs    : list              : list of results from previous nodes
- result    : untyped           : result of current operator


methods:
- extend_parset


------------------------------------------------------------------------------------------
FIXME: This function needs to be discussed.

you can concatenate PipeOP's

either using
`pipe = concat(OP1, OP2, OP3)`
or
`OP1 %>>% OP2 %>>% OP3`

questions
  - is that an OP?
  - Params stem from contained OP's
  - You can concat arbitrarily many OP's
  - What happens if some OP's are already learned?
  - How can we construct tree's?



------------------------------------------------------------------------------------------
### Use case: a linear pipeline (train, predict, resample, tuning)
------------------------------------------------------------------------------------------

op1 = PipeOpScaler$new()
op$par_vals = list(center = TRUE, scale = FALSE)

op2 = PipeOpPCA$new()

op3 = PipeOpLearner$new("classif.rpart")

# option a)
g1 = GraphNode$new(op1)
g2 = GraphNode$new(op1)
g3 = GraphNode$new(op1)

g1$set_next(g2)
g2$set_next(g3)

gg = Graph$new(g1)

# option b)
gg = PipeLine(op1, op2, op3)

# Train all OP's in the pipe
gg$train(task)
gg[["pca"]]$pipeop$params
gg[["pca"]]$result

gg$predict(task)
gg[["pca"]]$result
gg[["classif.rpart"]]$result

resample(gg, task)

ps = ParSet$new(
  ParamLogical$new("Scaler:center"),
  ParamNum$new("rpart:cp", lower = 0, upper = 1)
)
tune(gg, task, ps)


------------------------------------------------------------------------------------------
### Use case: Fork pipeOP's (at a place we use either OP A or B), eg: PCA or ICA, or PCA and no-PCA
------------------------------------------------------------------------------------------


op1 = PipeOpPCA$new()
op2 = PipeOpICA$new()
op3 = PipeOpNULL$new()

op = Multiplexer$new(op1, op2, op3)

op$parvals = list(selected = "ica")

# fragen:
was machen $parvals, $parset, ?
oder machen wir den multiplexer über die graphstruktur?

------------------------------------------------------------------------------------------
### Use case: AutoTuning of pipeop, for nested resampling
------------------------------------------------------------------------------------------
...

------------------------------------------------------------------------------------------
### Use case: Concat original data and transformed data, Feature Union
------------------------------------------------------------------------------------------

op1 = PipeOpScaler$new()
op2a = PipeOpPCA$new()     # was machen wir hier mit den targets?
op2b = PipeOpNULL$new()
op3 = PipeOpFeatureUnion$new()
op4 = PipeOpLearner$new(learner = "classif.rpart")

g1 = GraphNode$new(op1)
g2a = GraphNode$new(op2a)
g2b = GraphNode$new(op2b)
g3 = GraphNode$new(op3)
g4 = GraphNode$new(op4)

g1$set_next(list(g2a, g2b))
g3$set_prev(list(g2a, g2b))
g43$set_next(list(g3))

------------------------------------------------------------------------------------------
###Use case: Bagging
------------------------------------------------------------------------------------------

k = 100
op1 = PipeOpNULL$new()
op2 = PipeOpDownSample$new(rate = 0.6)
ops2 = repop(k, op2) # Auto-set Ids? # replicate with s3?
op3 = PipeOpLearner$new("classif.rpart")
ops3 = repop(k, op3)
op4 = PipeOpEnsembleAverage$new() # der muss halt wissen dass nur vorher learner nimmt als input

g1 = GraphNode$new(op1)
gs2 = lapply(ops2, GraphNode$new)
gs3 = lapply(ops3, GraphNode$new)

g1$set_next(gs2)
for (i in 1:k)
  gs2[[i]]$set_next(gs3[[i]])
g4$set_prev(gs3)


# can we write the above in a shorter, better way?

op1 = PipeOpNULL$new()
op2 = PipeOpDownSample$new(rate = 0.6)
op3 = PipeOpLearner$new("classif.rpart")
op4 = PipeOpEnsembleAverage$new()

Pipeline$new(list(op1, rep(k, op2), rep(k, op3), op4))
# wie wird denn hier genau verknüpft?
# 1-many: klar
# many-1: klar, muss halt RHS eine "zusammenführung" erlauben als OP
# k-k: paralleles verküpfen

# we need "connectors" as short, simple operations


gg = op1 %>>% rep(k, op2) %>>% rep(k, op3) %>>% op4

# wie kommt man an sachen ran?

gg$train(task)
# frage: welche daten hat der 3. baum gesehen, und eas ist sein modell?

gg[["rpart:3]]$result
gg[["rpart:3]]$pipeop$params
gg[["rpart"]][[3]] # das wäre cool? würde gehen wenn ops keien eindeutige id hgaben müssen?

# frage: gib mir alle rpart modelle
# wollen wir das konzept von "group of nodes"? würde über doppelpunkte gehen. achtung. es gibt jetzt rpart:3:cp. wollen wir das?
gg$get_group("rpart")[[3]]


------------------------------------------------------------------------------------------
### Use case: Stacking
------------------------------------------------------------------------------------------

op1 = PipeOpNULL$new()
op2 = PipeOpLearner$new("regr.rpart") # das hier müsste dann model UND daten im training ausgeben
op3 = PipeOpLearner$new("regr.svm") # Train: data -> list(model, data) # Predict: data -> prediction
op4a = PipeOpGetTrainPreds$new()    # Train: predict(model, data)      # Predict: identity
op4b = PipeOpGetTrainPreds$new()
op5 = PipeOpFeatureUnion$new()
op6 = PipeOpLearner$new("regr.lm")

g1 = GraphNode$new(op1)
g2 = GraphNode$new(op2)
g3 = GraphNode$new(op3)
g4a = GraphNode$new(op4a)
g4b = GraphNode$new(op4b)
g5 = GraphNode$new(op5)
g6 = GraphNode$new(op6)

PipeLine$new(op1, list(op2, op3), list(op4a, op4b), op5, op6)

------------------------------------------------------------------------------------------
### Use-case: Get out-of-bag Learner Predictions
------------------------------------------------------------------------------------------
PipeOPCrossvalLearner("rpart")
  Train: Input: data Output: oob.preds

    op1 = pipeOpLearner$new("rpart")    # Train step learner
    res = resample(Pipeline(op1), data)
    oob.preds = getOOBPreds(res)        # Get Out-Of-Bag Predictions from res
    op2 = pipeOpLearner$new("rpart")    # Predict step learner
    op1$train(data)                     # Train and store model for predict step
    params = op1

  Predict: Input: newdata Output: preds
    params$predict(newdata)





------------------------------------------------------------------------------------------
# PipeOp Defintions Template
------------------------------------------------------------------------------------------

PipeOpNULL
paramset  : <none>
behavior  : passes input without change to output
train     : [any] --> [any]
params    : -
predict   : [any] --> [any]

PipeOpScale
paramset  : center [logical(1)]; scale [logical(1)]
behavior  : centers and scales features, stores means and sds
train     : [dt] --> [dt]
params    : [list(means, sds)]
predict   : [dt] --> [dt]
- what happens with label
- what happens with cat cols?


PipeOpPCA
behavior  : learns rotation matrix and rotates feature data
paramset  : <none>
train     : [dt] --> [dt]
params    : [matrix]
predict   : [dt] --> [dt]
questions
- what happens with label
- what happens with cat cols?


PipeOpDownsample
behavior  : Downsamples the dt in train, simply passes data on in predict
paramset  : rate [numeric(1)]
train     : [dt] --> [dt]
params    : NULL
predict   : [dt] --> [dt]


PipeOpImpute
behavior  : Imputes dt with missing data
paramset  : <obtained from impute function, not copied>
train     : [dt] --> [dt]
params    : < params for  reimpute function, i.e. median, mode >
predict   : [dt] --> [dt]
questions:
- what is an "impute" function, that does not exist?

PipeOpFeatureUnion
behavior  : cbind's output of multiple Op's
paramset  : NULL
train     : [dt, dt] --> [dt]
params    : NULL
predict   : [dt, dt] --> [dt]
questions
- what happens with labels? in union we might have to remove label cols, if they appear multiple times?

PipeOpLearner
behavior  : learns a model from data; predicts response given new data
paramset  : <obtained from learner, not copied>
train     : [dt] --> [preds]
params    : [Model]
predict   : [dt] --> [preds]
questions:
- what is exactly returned here in "train"

PipeOpCrossvalLearner
behavior  : Crossval learner, return oob.preds during training; predict on data during tuning
paramset  : <obtained from learner, not copied>
train     : [dt] --> [preds]
params    : [Model]
predict   : [dt] --> [preds]


------------------------------------------------------------------------------------------
List of features from CPO we cover already:

List of features from CPO we are missing and might want:

List of features from CPO we do not want:


















Use case: Train on part of features only
-------------------------------------------------

E.g. Multi-Output: Train on Output1 while keeping Output2 out; Then train on Output2 using predicted Output1


## How do I append another step to the pipeline
# example: we add imputation
op1_1 = Imputer$new()
g1_1 = GraphNode$new(op1_1)
g1$set_next(g1_1)
g1_1$set_next(g2)




# how do we ensure that resample works for a graph, Graph has to implement interface of mlr3 Learner class
# should PipeOp and GraphNode be merged
# should params actaully be an input of PipeOp$train? and only the graphnode has a trained state? and stores params?
# graph: gibt es ein großes parvals/parset objekt? --> wäre gut für tuning...!
# params: How do we separate graph and pipeOp name and param? pipeOpId:paramId?
# we probably want a few helper functions in paradox for joining paramsets and the ":" usage



gg = Graph$new()

a)
ids = gg$ids()
for (i in ids) {
  f(gg[[i]])
}

b)
gg$apply2nodes(function(nn) nn$result)

c)



1) klasse PipeOp

members:
- id         : char       : Name
- parset     : ParamSet   : Hyperpar raum
- parvals    : list       : hyperpar settings
- params     : list       : gelernte params

methods
- train(input)     --> outlist    : fittet params und returned transformierte daten
- predict(input)   --> outlist    : nutzt gefittete params um neue daten zu transformieren
- is_learned()      --> bool

2) konstruktion:
- man kann einen OP[unlearned] bauen: op = Scaler$new()
- der konstruktor hat so gut wie keine args, wir lassen änderungen dafür später zu (mal gucken)


3) state änderungen:
- id, parvals können später gesetzt werden per AB
- vermutlich darf man einiges zu setzen wenn der OP [unlearned] ist,
  sonst ist das gefährlich? aber das ist user schutz und kommt später


4) training

D1 ---> OP[unlearned] --> D2

- daten gehen rein, kommen transformiert raus
- OP lernt params, speichert die, er hat gewechselt zu OP[learned]
- OP$params gibt die gelernten params (wenn er OP[learned] ist, sonst NULL)

5) application  / predict

ND1 ---> OP[learned] --> ND2
- transformiert newdata halt, mit gelernten params

6)
man kann OPs concatten.

pipe = concat(OP1, OP2, OP3)

ist das auch ein OP? vermutlich ja?

- der hat dann als params gelernt OPs
- man kann immer mehr hinten dran joinen (auch wenn schon gelernt?, wie dann?)
- wie kann man bäume kontruieren?


7) wichtige erweiterungen
multiplex: das ist schon wichtig als konzept um A oder B machen zu können
vermutlich wollen wir erstmal nur das martin feature-only-CPOS nennt oder?



-------------------------------------------------
Pseudo Code
-------------------------------------------------

op = Scaler$new()
op$parvals = list(center = T, scale = F)

data2 = op$train(data)
newdata2 = op2$apply(newdata)
op$params


op1 = Imputer$new()
op2 = Scaler$new()
op3 = PCA$new()

pipe = concat(op1, op2)
pipe$add(op3)

# trainiert alle ops in der pipe
data2 = pipe$train(data)

# gibt ein element, den ersten OP zurück
# hier ginge auch ein überladener index OP: pipe[[1]], pipe[["scale"]]
pipe$get(1)
pipe$get("impute")

# predict geht wie immer
nd2 = pipe$predict(data)

# frage wie kommt an azwischenergebnisse ran?
# gibt es ein großes parvals/parset objekt? --> wäre gut für tuning...!
