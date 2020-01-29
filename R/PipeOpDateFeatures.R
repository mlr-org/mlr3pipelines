#' @title PipeOpDateFeatures
#'
#' @usage NULL
#' @name mlr_pipeops_datefeaturs
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @description
#' Based on a specified `POSIXct` column of the data, a set of date related features is computed and
#' added to the feature set of the output task.
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
#' feature set.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from
#' [`PipeOpTaskPreprocSimple`], as well as:
#' * `date_variable` the name of the `POSIXct` column that was used for the feature engineering.
#' * `cyclic` logical value indicating whether cyclic features were computed.
#' * `features` a vector of the feature names that were computed.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`], as well as:
#' * `date_variable` :: `character(1)`\cr
#'   The name of the `POSIXct` column of the data that should be used for the feature engineering.
#'   Must be specified during construction.
#' * `cyclic` :: logical(1)`\cr
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
#'   Whether a feature should be extracted indicating whether it is day time (06:00 am - 08:00pm).
#'   Default TRUE.
#'
#' @section Internals:
#' If `cyclic = TRUE`, cyclic features are computed for the features `"month"`, `"week_of_year"`,
#' `"day_of_year"`, `"day_of_month"`, `"day_of_week"`, `"hour"`, `"minute"` and `"second"`. This
#' means that each feature, `x`, is replaced by two features, namely the sinus and cosinus
#' transformation of `2 * pi * x / max(x)`. This is useful to respect the cyclical nature of
#' features such as seconds, i.e., second 21 and second 22 are one second apart, but so are second
#' 60 and second 01. The transformation always assumes that `min(x) = 0`, therefore prior shifting
#' the values internally by minus one may occurr if necessary.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' dat = iris
#' set.seed(1)
#' dat$date = sample(seq(from = as.POSIXct("2020-01-29"), to = as.POSIXct("2020-02-28"), by = "hour"),
#'   size = 150L)
#' task = TaskClassif$new("iris_date", backend = dat, target = "Species")
#' pop = po("datefeatures", param_vals = list(date_variable = "date", cyclic = FALSE, minute = FALSE,
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
        ParamUty$new("date_variable", custom_check = check_character, tags = "required"),
        ParamLgl$new("cyclic", default = FALSE),
        ParamLgl$new("year", default = TRUE, tags = "date_feature"),
        ParamLgl$new("month", default = TRUE, tags = "date_feature"),
        ParamLgl$new("week_of_year", default = TRUE, tags = "date_feature"),
        ParamLgl$new("day_of_year", default = TRUE, tags = "date_feature"),
        ParamLgl$new("day_of_month", default = TRUE, tags = "date_feature"),
        ParamLgl$new("day_of_week", default = TRUE, tags = "date_feature"),
        ParamLgl$new("hour", default = TRUE, tags = "date_feature"),
        ParamLgl$new("minute", default = TRUE, tags = "date_feature"),
        ParamLgl$new("second", default = TRUE, tags = "date_feature"),
        ParamLgl$new("is_day", default = TRUE, tags = "date_feature")
      ))
      ps$values$date_variable = "date" # needed for mlr_pipeops overview
      super$initialize(id = id, param_set = ps, param_vals = param_vals)
    },

    get_state = function(task) {
      date_variable = self$param_set$values$date_variable
      features = self$param_set$ids(tags = "date_feature") # default = TRUE for all date features
      dropped = names(which(unlist(self$param_set$get_values(tags = "date_feature")) == FALSE))
      features = features[features %nin% dropped]
      cyclic = ifelse(is.null(self$param_set$values$cyclic), yes = self$param_set$default$cyclic,
        no = self$param_set$values$cyclic)
      list(date_variable = date_variable, cyclic = cyclic, features = features)
    },

    transform = function(task) {
      dates = task$data()[[self$state$date_variable]]
      assert_posixct(dates)
      features = self$state$features
      date_features = as.data.table(lapply(features, function(feature) {
        compute_date_features(dates, feature)
      }))
      colnames(date_features) = features
      # if cyclic = TRUE, month, week_of_year, day_of_year, day_of_month, day_of_week, hour, minute
      # and second are dropped and replaced with two columns each consisting of their sin and cos
      # transformation of in general 2 * pi * x / max(x) (x starting from 0)
      if (self$state$cyclic) {
        cyclic_features = features[features %in%
          c("month", "week_of_year", "day_of_year", "day_of_month", "day_of_week",
            "hour", "minute", "second")]
        cyclic_date_features = as.data.table(lapply(cyclic_features, function(feature) {
          compute_cyclic_date_features(date_features, feature)
        }))
        colnames(cyclic_date_features) = paste0(rep(cyclic_features, each = 2L), "_",
          c("sin", "cos"))
        date_features[, (cyclic_features) := NULL] # drop the original features
        date_features = cbind(date_features, cyclic_date_features)
      }
      task$cbind(date_features)
      task$select(setdiff(task$feature_names, self$param_set$values$date_variable))
    }
  )
)

mlr_pipeops$add("datefeatures", PipeOpDateFeatures)

# helper function to compute date features of a vector of dates
compute_date_features = function(dates, feature) {
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
    return((6 <= hours) & (hours <= 20)) # early exit
  }
  as.numeric(format(dates, formatting))
}

# helpfer function to compute cyclic date features of date features, i.e.,
# sin and cos transformations of 2 * pi * x / max(x)
compute_cyclic_date_features = function(date_features, feature) {
  # all values are expected to start at 0L and therefore may be shifted by - 1
  value = if (feature %in% c("month", "week_of_year", "day_of_year", "day_of_month")) {
    date_features[[feature]] - 1L
  } else {
    date_features[[feature]]
  }
  maximum = switch(feature,
    month = 11L,
    week_of_year = 51L,
    day_of_year = (364L + as.integer((date_features[["year"]] %% 400L == 0L) |
      (date_features[["year"]] %% 4L == 0L & date_features[["year"]] %% 100L != 0L))),
    day_of_month = get_days_per_month(date_features[["year"]],
      month = date_features[["month"]]) - 1L,
    day_of_week = 6L,
    hour = 23L,
    minute = 59L,
    second = 59L)
  value_scaled = 2L * pi * value / maximum
  cbind(sin = sin(value_scaled), cos = cos(value_scaled))
}

# helper function to get the number of days per month respecting leap years
get_days_per_month = function(year, month) {
  stopifnot(length(year) == length(month))
  if (length(year) > 1L) {
    mapply(get_days_per_month, year = year, month = month)
  } else {
    days_per_month = c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    if (((year %% 4L == 0L && year %% 100L != 0L) || (year %% 400L == 0L)) && month == 2) {
      return(29L) # early exit if leap year and february
    }
    days_per_month[month]
  }
}
