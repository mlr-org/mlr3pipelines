# mlr3pipelines <img src="man/figures/logo.png" align="right" />

[![Travis build status](https://travis-ci.com/mlr-org/mlr3pipelines.svg?branch=master)](https://travis-ci.com/mlr-org/mlr3pipelines)
[![Coverage](https://codecov.io/github/mlr-org/mlr3pipelines/branch/master/graphs/badge.svg)](https://codecov.io/github/mlr-org/mlr3pipelines)

## Target

This package aims to fill the gap between loading the data and fitting models.

**This entails not only:**

- Preprocessing
- Splitting / Combining Features
- Imputation
- Down/Upsampling

**but also:**

- Bagging
- Tuning over preprocessing pipelines
- Stacking
- Ensembling

A predecessor to this package is the [mlrCPO-package](https://github.com/mlr-org/mlrCPO).
We intend to replicate most of its functionality, i.e.

- simple feature transform, with hyperpars
- `pca, all scales, all filters, all imputes, cvlearner, cpoDummyEncode`
- `cpoSelect cpoCollapseFact cpoProbEncode cpoImpactEncode cpoSpatialSign`
- `cpoCbind cpoMultiplex cpoWrap`


## Design Documents

A rough draft of the design document, and some first usecases
can be found in **concept.txt**.
A series of usecases and PipeOperators can be found in usecases.md.

## PipeOperators and Status

- **Meta:**
  - [x] `PipeOpBranch`                          | broadcast
  - [x] `PipeOpChunk`                           | broadcast
  - [x] `PipeOpUnbranch`                        | aggregate
  - [x] `PipeOpFeatureUnion`                    | aggregate
  - [x] `PipeOpNULL`                            | linear
  - [x] `PipeOpCopy`                            | broadcast

                                              train: input --store-params--> output        predict: input --use-params--> output
- **Learner:**
  - [x] `PipeOpLearner`                         | linear    | task --model--> NULL           | task --model--> prediction
  - [X] `PipeOpLearnerCV`                       | linear    | task --model--> cvtask         | task --model--> prediction
  - [X] `PipeOpModelAverage`                    | aggregate | task --NULL--> NULL            | list-of-prediction --NULL--> prediction

- **Preprocessing:**
  - [x] `PipeOpPCA`                             | linear    | task --params--> task          | task --params--> task
  - [x] `PipeOpScale`                           | linear    | task --params--> task          | task --params--> task
  - [x] `PipeOpDownsample`                      | linear    | task --NULL--> task            | task --NULL--> task

- **Target Operators:**
  - [ ] `PipeOpThreshold`                       | linear    | cvtask --threshold--> NULL     | prediction --threshold--> prediction
  - [ ] `PipeOpTrafoY`                          | linear    | task --NULL--> task            | prediction --NULL--> prediction
  - [ ] `PipeOpMultiClass2Binary`               | broadcast | task --NULL--> list-of-task    | task --NULL--> list-of-tasks
  - [ ] `PipeOpSetTarget`                       | linear    | task --NULL--> task            | task --NULL--> task


### Old Specs Doc:
(https://docs.google.com/document/d/1JbzulUkLrMS0Xyk38NF5SyhpAfr2FIFsh9FJ8yiDtbE/edit?usp=sharing)
