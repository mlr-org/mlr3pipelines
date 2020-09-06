# mlr3pipelines 0.2.1-9000

* NULL input channels accept any kind of input
* New to_dot() method added to Graphs printing a DOT representation on console
* state of PipeOps now reset to NULL if training fails
* implemented as_learner.PipeOp
* Changed PPLs:
  - fix how ppl_robustify detects whether a learner can handle factors
* Changed PipeOps:
  - PipeOpTextVectorizer can now return an "integer sequence representation".
* New PipeOps:
  - PipeOpNMF
  - PipeOpColRoles

# mlr3pipelines 0.2.1

* New feature: Multiplicities: implicit repetition of operations
* new mlr_graphs:
  - pipeline_bagging
  - pipeline_branch
  - pipeline_greplicate
  - pipeline_robustify
  - pipeline_targettrafo
  - pipeline_ovr
* New PipeOps:
  - PipeOpOVRSplit, PipeOpOVRUnite
  - PipeOpReplicate
  - PipeOpMultiplicityExply, PipeOpMultiplicityImply
  - PipeOpTargetTrafo, PipeOpTargetInvert
  - PipeOpTargetMutate
  - PipeOpTargetTrafoScaleRange
  - PipeOpProxy
  - PipeOpDateFeatures
  - PipeOpImputeConstant
  - PipeOpImputeLearner
  - PipeOpMode
  - PipeOpRandomResponse
  - PipeOpRenameColumns
  - PipeOpTextVectorizer
  - PipeOpThreshold
* Renamed PipeOps:
  - PipeOpImputeNewlvl --> PipeOpImputeOOR (with additional functionality for continuous values)
* Changed PipeOps:
  - PipeOpFeatureUnion: Bugfix: avoid silently overwriting features when names clash
  - PipeOpHistBin: Bugfix: handle test set data out of training set range
  - PipeOpLearnerCV: Allow returning trainingset prediction during train()
  - PipeOpMutate: Allow referencing newly created columns
  - PipeOpScale: Allow robust scaling
  - PipeOpLearner, PipeOpLearnerCV: learner_models for access to learner with model slot
* New Selectors:
  - selector_missing
  - selector_cardinality_greater_than
* NULL is neutral element of %>>%
* PipeOpTaskPreproc now has feature_types slot
* PipeOpTaskPreproc(Simple) internal API changed: use .train_task(), .predict_task(), .train_dt(), .predict_dt(), .select_cols(), .get_state(), .transform(), .get_state_dt(), .transform_dt() instead of the old methods without dot prefix
* PipeOp now has tags slot
* PipeOp internal API changed: use .train(), .predict() instead of train_internal(), predict_internal()
* Graph new method update_ids()
* Graph methods train(single_input = FALSE) and predict(single_input = FALSE) now handle vararg channels correctly.
* Obsoleted greplicate(); use pipeline_greplicate / ppl("greplicate") instead.
* po() now automatically converts Selector to PipeOpSelect
* po() prints available mlr_pipeops dictionary content
* mlr_graphs dictionary of useful Graphs, with short form accessor ppl()
* Work with new mlr3 version 0.4.0

# mlr3pipelines 0.1.3

* small test fix for R 4.0 (necessary for stringsAsFactors option default change in 3.6 -> 4.0)
* predict() generic for Graph
* Migrated last vignette to "mlr3 Book"

* Compact in-memory representation of R6 objects to save space when
  saving objects via saveRDS(), serialize() etc.

# mlr3pipelines 0.1.2

* Work with new mlr3 version 0.1.5 (handling of character columns changed)

# mlr3pipelines 0.1.1

* Better html graphics for linear Graphs
* New PipeOps:
  - PipeOpEncodeImpact
* Changed PipeOp Behaviour:
  - PipeOpEncode: handle NAs

# mlr3pipelines 0.1.0

* Initial upload to CRAN.
