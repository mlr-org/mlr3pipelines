# mlr3pipelines

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

Specification of syntax to construct graphs quickly
https://docs.google.com/document/d/1JbzulUkLrMS0Xyk38NF5SyhpAfr2FIFsh9FJ8yiDtbE/edit?usp=sharing



## Todo's
Open Todo's are found either in the Github Issue's or in **todos.txt**.
Additionally, some questions that need to be discussed are in the **concept.txt**
