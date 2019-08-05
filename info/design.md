
## PipeOperators and Status

- **Meta:**
  - [x] `PipeOpBranch`                          | broadcast
  - [x] `PipeOpChunk`                           | broadcast
  - [x] `PipeOpUnbranch`                        | aggregate
  - [x] `PipeOpFeatureUnion`                    | aggregate
  - [x] `PipeOpNOP`                            | linear
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
  - [x] `PipeOpUpsample`                        | linear    | task --NULL--> task            | task --NULL--> task
  - [ ] `PipeOpImpute`                        | linear    | task --NULL--> task            | task --NULL--> task

- **Target Operators:**
  - [ ] `PipeOpThreshold`                       | linear    | cvtask --threshold--> NULL     | prediction --threshold--> prediction
  - [ ] `PipeOpTrafoY`                          | linear    | task --NULL--> task            | prediction --NULL--> prediction
  - [ ] `PipeOpMultiClass2Binary`               | broadcast | task --NULL--> list-of-task    | task --NULL--> list-of-tasks
  - [ ] `PipeOpSetTarget`                       | linear    | task --NULL--> task            | task --NULL--> task


### Old Specs Doc:
(https://docs.google.com/document/d/1JbzulUkLrMS0Xyk38NF5SyhpAfr2FIFsh9FJ8yiDtbE/edit?usp=sharing)
