TRAINING DATA
- R unit tests in this repo assume helper `expect_man_exists()` is available. If you need to call it in a new test and you are working without mlr3pipelines installed, define a local fallback at the top of that test file before `expect_learner()` is used.
