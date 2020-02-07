#' @title PipeOpDateFeatures
#'
#' @usage NULL
#' @name mlr_pipeops_datefeaturs
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @description
#' Based on a specified `POSIXct` column of the data, a set of date related features is computed and
#' added to the feature set of the output task. If no `POSIXct` column is specified, the first
#' `POSIXct` column found in the data is used. If none is found, the original task is returned
#' unaltered. This functionality is based on the `add_datepart()` and `add_cyclic_datepart()`
#' functions from the `fastai` library.
#'
#' @section Construction:
#' ```
#' PipeOpDateFeatures$new(id = "datefeatures", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"datefeatures"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreprocSimple`].
#'
#' The output is the input [`Task`][mlr3::Task] with date-related features computed and added to the
#' feature set of the output task and the specified `POSIXct` column of the data removed from the
#' feature set (depending on the value of `keep_date_var`).
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from
#' [`PipeOpTaskPreprocSimple`], as well as:
#' * `date_var` the name of the `POSIXct` column that was used for the feature engineering.
#' * `keep_date_var` logical value indicating whether the `date_var` column of the data should be
#'   removed from the feature set.
#' * `cyclic` logical value indicating whether cyclic features should be computed.
#' * `features` vector of computed feature names.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`], as well as:
#' * `date_var` :: `character(1)`\cr
#'   The name of the `POSIXct` column of the data that should be used for the feature engineering.
#'   If not specified, the first `POSIXct` column found in the data will be used.
#' * `keep_date_var` :: `logical(1)`\cr
#'   Whether the `date_var` column should be kept as a feature. Default FALSE.
#' * `cyclic` :: `logical(1)`\cr
#'   Whether cyclic features should be computed. See Internals. Default FALSE.
#' * `year` :: `logical(1)`\cr
#'   Whether the year should be extracted as a feature. Default TRUE.
#' * `month` :: `logical(1)`\cr
#'   Whether the month should be extracted as a feature. Default TRUE.
#' * `week_of_year` :: `logical(1)`\cr
#'   Whether the week of the year should be extracted as a feature. Default TRUE.
#' * `day_of_year` :: `logical(1)`\cr
#'   Whether the day of the year should be extracted as a feature. Default TRUE.
#' * `day_of_month` :: `logical(1)`\cr
#'   Whether the day of the month should be extracted as a feature. Default TRUE.
#' * `day_of_week` :: `logical(1)`\cr
#'   Whether the day of the week should be extracted as a feature. Default TRUE.
#' * `hour` :: `logical(1)`\cr
#'   Whether the hour should be extracted as a feature. Default TRUE.
#' * `minute` :: `logical(1)`\cr
#'   Whether the minute should be extracted as a feature. Default TRUE.
#' * `second` :: `logical(1)`\cr
#'   Whether the second should be extracted as a feature. Default TRUE.
#' * `is_day` :: `logical(1)`\cr
#'   Whether a feature should be extracted indicating whether it is day time (06:00am - 08:00pm).
#'   Default TRUE.
#'
#' @section Internals:
#' If `cyclic = TRUE`, cyclic features are computed for the features `"month"`, `"week_of_year"`,
#' `"day_of_year"`, `"day_of_month"`, `"day_of_week"`, `"hour"`, `"minute"` and `"second"`. This
#' means that for each feature, `x`, two additional features are computed, namely the sinus and
#' cosinus transformation of `2 * pi * x / max_x`. This is useful to respect the cyclical nature of
#' features such as seconds, i.e., second 21 and second 22 are one second apart, but so are second
#' 60 and second 01. The transformation always assumes that `min_x = 0`, therefore prior shifting
#' the values internally by minus one may occur if necessary.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' dat = iris
#' set.seed(1)
#' dat$date = sample(seq(as.POSIXct("2020-02-01"), to = as.POSIXct("2020-02-29"), by = "hour"),
#'   size = 150L)
#' task = TaskClassif$new("iris_date", backend = dat, target = "Species")
#' pop = po("datefeatures", param_vals = list(date_var = "date", cyclic = FALSE, minute = FALSE,
#'   second = FALSE))
#' pop$train(list(task))
#' pop$state
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpDateFeatures = R6Class("PipeOpDateFeatures",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "datefeatures", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("date_var", custom_check = check_character, tags = c("train")),
        ParamLgl$new("keep_date_var", default = FALSE, tags = "train"),
        ParamLgl$new("cyclic", default = FALSE, tags = "train"),
        ParamLgl$new("year", default = TRUE, tags = c("train")),
        ParamLgl$new("month", default = TRUE, tags = c("train")),
        ParamLgl$new("week_of_year", default = TRUE, tags = c("train")),
        ParamLgl$new("day_of_year", default = TRUE, tags = c("train")),
        ParamLgl$new("day_of_month", default = TRUE, tags = c("train")),
        ParamLgl$new("day_of_week", default = TRUE, tags = c("train")),
        ParamLgl$new("hour", default = TRUE, tags = c("train")),
        ParamLgl$new("minute", default = TRUE, tags = c("train")),
        ParamLgl$new("second", default = TRUE, tags = c("train")),
        ParamLgl$new("is_day", default = TRUE, tags = c("train"))
      ))
      ps$values = list(date_var = character(0L), keep_date_var = FALSE, cyclic = FALSE, year = TRUE,
        month = TRUE, week_of_year = TRUE, day_of_year = TRUE, day_of_month = TRUE,
        day_of_week = TRUE, hour = TRUE, minute = TRUE, second = TRUE, is_day = TRUE)
      super$initialize(id = id, param_set = ps, param_vals = param_vals)
    },

    get_state = function(task) {
      date_var = self$param_set$values$date_var
      # if no POSIXct column was specified use the first POSIXct column found
      if (length(date_var) == 0L) {
        date_var = task$feature_names[task$feature_types$type == "POSIXct"][1L]
        # this is NA if no POSIXct column was found
      }
      list(
        date_var = date_var,
        keep_date_var = self$param_set$values$keep_date_var,
        cyclic = self$param_set$values$cyclic,
        features = setdiff(self$param_set$ids("ParamLgl")[unlist(self$param_set$get_values("ParamLgl"))],
          c("keep_date_var", "cyclic"))
      )
    },

    transform = function(task) {
      # early exit if no POSIXct column was specified or found, or no features are to be computed
      # return the unaltered task
      if (is.na(self$state$date_var) || (length(self$state$features) == 0L)) {
        warning("No `POSIXct` column found or no features to compute. ",
          "Returning the task unaltered.")
        return(task)
      }
      dates = task$data(cols = self$state$date_var)
      assert_posixct(dates[[1L]])
      # special handling of year because this is needed for day_of_year and day_of_month
      features = unique(c("year", self$state$features))

      dates[, (features) := compute_date_features(get(self$state$date_var), features = features)]
      # if cyclic = TRUE for month, week_of_year, day_of_year, day_of_month, day_of_week, hour,
      # minute and second two columns are additionally added, each consisting of their sinus and
      # cosinus transformation of in general 2 * pi * x / max_x (x starting from 0)
      if (self$state$cyclic) {
        cyclic_features = features[features %in%
          c("month", "week_of_year", "day_of_year", "day_of_month", "day_of_week",
            "hour", "minute", "second")]
        dates[, paste0(rep(cyclic_features, each = 2L), "_", c("sin", "cos")) :=
          compute_cyclic_date_features(.SD, features = cyclic_features)]
        #dates[, (cyclic_features) := NULL] # drop the original features
      }

      task$cbind(dates)
      if (self$state$keep_date_var) {
        task$select(setdiff(task$feature_names, setdiff(features, self$state$features)))
      } else {
        task$select(setdiff(task$feature_names,
           c(setdiff(features, self$state$features), self$state$date_var)))
      }
    }
  )
)

mlr_pipeops$add("datefeatures", PipeOpDateFeatures)

# helper function to compute date features of a vector of dates
compute_date_features = function(dates, features) {
  lapply(features, FUN = function(feature) {
    formatting = switch(feature,
      year = "%Y",
      month = "%m",
      week_of_year = "%U", # starting on Sunday
      day_of_year = "%j",
      day_of_month = "%d",
      day_of_week = "%w", # 0 = Sunday
      hour = "%H",
      minute = "%M",
      second = "%S",
      is_day = "%H")
    if (feature == "is_day") {
      hours = as.numeric(format(dates, formatting))
      return((6L <= hours) & (hours <= 20L)) # early exit
    }
    as.numeric(format(dates, formatting))
  })
}

# helper function to compute cyclic date features of date features, i.e.,
# sinus and cosinus transformations of 2 * pi * x / max_x
compute_cyclic_date_features = function(date_features, features) {
  do.call(c, args = lapply(features, FUN = function(feature) {
    # all values are expected to start at 0 and therefore may be shifted by - 1
    value = if (feature %in% c("month", "week_of_year", "day_of_year", "day_of_month")) {
      date_features[[feature]] - 1L
    } else {
      date_features[[feature]]
    }
    maximum = switch(feature,
      month = 12L,
      week_of_year = 52L,
      day_of_year = (365L + as.integer((date_features[["year"]] %% 400L == 0L) |
        (date_features[["year"]] %% 4L == 0L & date_features[["year"]] %% 100L != 0L))),
      day_of_month = get_days_per_month(date_features[["year"]],
        month = date_features[["month"]]),
      day_of_week = 7L,
      hour = 24L,
      minute = 60L,
      second = 60L)
    value_scaled = 2L * pi * value / maximum
    list(sin(value_scaled), cos(value_scaled))
  }))
}

# helper function to get the number of days per month respecting leap years
get_days_per_month = function(year, month) {
  stopifnot(length(year) == length(month))
  if (length(year) > 1L) {
    mapply(get_days_per_month, year = year, month = month)
  } else {
    days_per_month = c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    if (is.na(year)) {
      return(NA) # early exit if NA
    }
    if (((year %% 4L == 0L && year %% 100L != 0L) || (year %% 400L == 0L)) && month == 2) {
      return(29L) # early exit if leap year and February
    }
    days_per_month[month]
  }
}
