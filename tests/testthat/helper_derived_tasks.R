# Task classes derived from TaskRegr / TaskClassif / TaskSupervised that have their own task type.
# They simulate Task types defined by extension packages, e.g. TaskRegrST / TaskClassifST from mlr3spatiotempcv
# (task types "regr_st" / "classif_st") or TaskSurv from mlr3proba (task type "surv").
# The corresponding task types are registered in mlr_reflections in setup.R.

TaskRegrDerived = R6Class("TaskRegrDerived", inherit = TaskRegr,
  public = list(
    initialize = function(id, backend, target, label = NA_character_, extra_args = list()) {
      super$initialize(id = id, backend = backend, target = target, label = label, extra_args = extra_args)
      self$task_type = "regr_derived"
    }
  )
)

TaskClassifDerived = R6Class("TaskClassifDerived", inherit = TaskClassif,
  public = list(
    initialize = function(id, backend, target, positive = NULL, label = NA_character_, extra_args = list()) {
      super$initialize(id = id, backend = backend, target = target, positive = positive, label = label,
        extra_args = extra_args)
      self$task_type = "classif_derived"
    }
  )
)

# TaskSupervised that is neither a TaskRegr nor a TaskClassif (like TaskSurv), possibly with multiple target columns
TaskSupervisedDerived = R6Class("TaskSupervisedDerived", inherit = TaskSupervised,
  public = list(
    initialize = function(id, backend, target, label = NA_character_) {
      super$initialize(id = id, task_type = "supervised_derived", backend = backend, target = target, label = label)
    }
  )
)

# Create a TaskRegrDerived / TaskClassifDerived with the same data as the given TaskRegr / TaskClassif
as_derived_task = function(task) {
  switch(task$task_type,
    regr = TaskRegrDerived$new(task$id, task$data(), target = task$target_names),
    classif = TaskClassifDerived$new(task$id, task$data(), target = task$target_names,
      positive = if ("twoclass" %in% task$properties) task$positive),
    stop("as_derived_task() expects a TaskRegr or TaskClassif")
  )
}
