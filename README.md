
# mlr3pipelines <img src="man/figures/logo.png" align="right" />

Package website: [release](https://mlr3pipelines.mlr-org.com/) |
[dev](https://mlr3pipelines.mlr-org.com/dev/)

Dataflow Programming for Machine Learning in R.

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3pipelines/workflows/tic/badge.svg?branch=master)](https://github.com/mlr-org/mlr3pipelines/actions)
[![CRAN](https://www.r-pkg.org/badges/version/mlr3pipelines)](https://cran.r-project.org/package=mlr3pipelines)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
<!-- badges: end -->

## What is `mlr3pipelines`?

Watch our "WhyR 2020" Webinar Presentation on Youtube for an introduction! Find the slides [here](https://github.com/mlr-org/mlr-outreach/raw/master/2020_whyr/slides.pdf).

[![WhyR 2020
mlr3pipelines](https://img.youtube.com/vi/4r8K3GO5wk4/0.jpg)](https://www.youtube.com/watch?v=4r8K3GO5wk4)

**`mlr3pipelines`** is a [dataflow
programming](https://en.wikipedia.org/wiki/Dataflow_programming) toolkit
for machine learning in R utilising the
**[mlr3](https://github.com/mlr-org/mlr3)** package. Machine learning
workflows can be written as directed “Graphs” that represent data flows
between preprocessing, model fitting, and ensemble learning units in an
expressive and intuitive language. Using methods from the
**[mlr3tuning](https://github.com/mlr-org/mlr3tuning)** package, it is
even possible to simultaneously optimize parameters of multiple
processing units.

In principle, *mlr3pipelines* is about defining singular data and model
manipulation steps as “PipeOps”:

``` r
pca        = po("pca")
filter     = po("filter", filter = mlr3filters::flt("variance"), filter.frac = 0.5)
learner_po = po("learner", learner = lrn("classif.rpart"))
```

These pipeops can then be combined together to define machine learning
pipelines. These can be wrapped in a `GraphLearner` that behave like any
other `Learner` in `mlr3`.

``` r
graph = pca %>>% filter %>>% learner_po
glrn = GraphLearner$new(graph)
```

This learner can be used for resampling, benchmarking, and even tuning.

``` r
resample(tsk("iris"), glrn, rsmp("cv"))
#> <ResampleResult> of 10 iterations
#> * Task: iris
#> * Learner: pca.variance.classif.rpart
#> * Warnings: 0 in 0 iterations
#> * Errors: 0 in 0 iterations
```

## Feature Overview

Single computational steps can be represented as so-called **PipeOps**,
which can then be connected with directed edges in a **Graph**. The
scope of *mlr3pipelines* is still growing; currently supported features
are:

  - Simple data manipulation and preprocessing operations, e.g. PCA,
    feature filtering
  - Task subsampling for speed and outcome class imbalance handling
  - *mlr3* *Learner* operations for prediction and stacking
  - Simultaneous path branching (data going both ways)
  - Alternative path branching (data going one specific way, controlled
    by hyperparameters)
  - Ensemble methods and aggregation of predictions

## Documentation

The easiest way to get started is reading some of the vignettes that are
shipped with the package, which can also be viewed online:

  - [Quick Introduction](https://mlr3book.mlr-org.com/pipelines.html),
    with short examples to get started
  - [Detailed
    Introduction](https://mlr3pipelines.mlr-org.com/articles/introduction.html),
    diving into concepts and describing the objects involved
  - [Comparison](https://mlr3pipelines.mlr-org.com/articles/comparison_mlr3pipelines_mlr_sklearn.html)
    of `mlr3pipelines` with other packages (not yet authoritative)

## Bugs, Questions, Feedback

*mlr3pipelines* is a free and open source software project that
encourages participation and feedback. If you have any issues,
questions, suggestions or feedback, please do not hesitate to open an
“issue” about it on the GitHub page\!

In case of problems / bugs, it is often helpful if you provide a
“minimum working example” that showcases the behaviour (but don’t
worry about this if the bug is obvious).

Please understand that the resources of the project are limited:
response may sometimes be delayed by a few days, and some feature
suggestions may be rejected if they are deemed too tangential to the
vision behind the project.

## Similar Projects

A predecessor to this package is the
[*mlrCPO*-package](https://github.com/mlr-org/mlrCPO), which works with
*mlr* 2.x. Other packages that provide, to varying degree, some
preprocessing functionality or machine learning domain specific
language, are the *[caret](https://github.com/topepo/caret)* package and
the related *[recipes](https://recipes.tidymodels.org/)* project,
and the *[dplyr](https://github.com/tidyverse/dplyr)* package.
