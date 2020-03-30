#' @title PipeOpDateFeatures
#'
#' @usage NULL
#' @name mlr_pipeops_datefeatures
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Based on `POSIXct` columns of the data, a set of date related features is computed and added to
#' the feature set of the output task. If no `POSIXct` column is found, the original task is
#' returned unaltered. This functionality is based on the `add_datepart()` and
#' `add_cyclic_datepart()` functions from the `fastai` library. If operation on only particular
#' `POSIXct` columns is requested, use the `affect_columns` parameter inherited from
#' [`PipeOpTaskPreprocSimple`].
#'
#' If `cyclic = TRUE`, cyclic features are computed for the features `"month"`, `"week_of_year"`,
#' `"day_of_year"`, `"day_of_month"`, `"day_of_week"`, `"hour"`, `"minute"` and `"second"`. This
#' means that for each feature `x`, two additional features are computed, namely the sine and cosine
#' transformation of `2 * pi * x / max_x` (here `max_x` is the largest possible value the feature
#' could take on `+ 1`, assuming the lowest possible value is given by 0, e.g., for hours from 0 to
#' 23, this is 24). This is useful to respect the cyclical nature of features such as seconds, i.e.,
#' second 21 and second 22 are one second apart, but so are second 60 and second 1 of the next
#' minute.
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
#' feature set of the output task and the `POSIXct` columns of the data removed from the
#' feature set (depending on the value of `keep_date_var`).
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from
#' [`PipeOpTaskPreprocSimple`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`], as well as:
#' * `keep_date_var` :: `logical(1)`\cr
#'   Should the `POSIXct` columns be kept as features? Default FALSE.
#' * `cyclic` :: `logical(1)`\cr
#'   Should cyclic features be computed? See Internals. Default FALSE.
#' * `year` :: `logical(1)`\cr
#'   Should the year be extracted as a feature? Default TRUE.
#' * `month` :: `logical(1)`\cr
#'   Should the month be extracted as a feature? Default TRUE.
#' * `week_of_year` :: `logical(1)`\cr
#'   Should the week of the year be extracted as a feature? Default TRUE.
#' * `day_of_year` :: `logical(1)`\cr
#'   Should the day of the year be extracted as a feature? Default TRUE.
#' * `day_of_month` :: `logical(1)`\cr
#'   Should the day of the month be extracted as a feature? Default TRUE.
#' * `day_of_week` :: `logical(1)`\cr
#'   Should the day of the week be extracted as a feature? Default TRUE.
#' * `hour` :: `logical(1)`\cr
#'   Should the hour be extracted as a feature? Default TRUE.
#' * `minute` :: `logical(1)`\cr
#'   Should the minute be extracted as a feature? Default TRUE.
#' * `second` :: `logical(1)`\cr
#'   Should the second be extracted as a feature? Default TRUE.
#' * `is_day` :: `logical(1)`\cr
#'   Should a feature be extracted indicating whether it is day time (06:00am - 08:00pm)?
#'   Default TRUE.
#'
#' @section Internals:
#' The cyclic feature transformation always assumes that values range from 0, so some values
#' (e.g. day of the month) are shifted before sine/cosine transform.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#'library("mlr3")
#'dat = iris
#'set.seed(1)
#'dat$date = sample(seq(as.POSIXct("2020-02-01"), to = as.POSIXct("2020-02-29"), by = "hour"),
#'  size = 150L)
#'task = TaskClassif$new("iris_date", backend = dat, target = "Species")
#'pop = po("datefeatures", param_vals = list(cyclic = FALSE, minute = FALSE, second = FALSE))
#'pop$train(list(task))
#'pop$state
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpDateFeatures = R6Class("PipeOpDateFeatures",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "datefeatures", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamLgl$new("keep_date_var", tags = c("train", "predict", "required")),
        ParamLgl$new("cyclic", tags = c("train", "predict", "required")),
        ParamLgl$new("year", tags = c("train", "predict", "datepart", "required")),
        ParamLgl$new("month", tags = c("train", "predict", "datepart", "required")),
        ParamLgl$new("week_of_year", tags = c("train", "predict", "datepart", "required")),
        ParamLgl$new("day_of_year", tags = c("train", "predict", "datepart", "required")),
        ParamLgl$new("day_of_month", tags = c("train", "predict", "datepart", "required")),
        ParamLgl$new("day_of_week", tags = c("train", "predict", "datepart", "required")),
        ParamLgl$new("hour", tags = c("train", "predict", "datepart", "required")),
        ParamLgl$new("minute", tags = c("train", "predict", "datepart", "required")),
        ParamLgl$new("second", tags = c("train", "predict", "datepart", "required")),
        ParamLgl$new("is_day", tags = c("train", "predict", "datepart", "required"))
      ))
      ps$values = list(keep_date_var = FALSE, cyclic = FALSE, year = TRUE,
        month = TRUE, week_of_year = TRUE, day_of_year = TRUE, day_of_month = TRUE,
        day_of_week = TRUE, hour = TRUE, minute = TRUE, second = TRUE, is_day = TRUE)
      super$initialize(id = id, param_set = ps, param_vals = param_vals)
    },

    select_cols = function(task) {
      task$feature_types[get("type") == "POSIXct", get("id")]
    },

    transform_dt = function(dt, levels) {
      features = names(which(unlist(self$param_set$get_values(tags = "datepart"))))
      # early exit if no features are to be computed
      if (length(features) == 0L) {
        return(dt)
      }

      # special handling of year because this is needed for day_of_year and day_of_month
      drop_year = "year" %nin% features
      features = unique(c("year", features))

      cyclic_features = features[features %in% c("month", "week_of_year", "day_of_year",
        "day_of_month", "day_of_week", "hour", "minute", "second")]

      date_vars = colnames(copy(dt))

      for (date_var in date_vars) {
        dt[, paste0(date_var, ".", features) :=
          compute_date_features(get(date_var), features = features)]
      }

      # if cyclic = TRUE for month, week_of_year, day_of_year, day_of_month, day_of_week, hour,
      # minute and second, two columns are additionally added, each consisting of their sine and
      # cosine transformation of in general 2 * pi * x / max_x (x starting from 0)
      if (self$param_set$values$cyclic && (length(cyclic_features) > 0L)) {
        for (date_var in date_vars) {
          dt[, paste0(date_var, ".", rep(cyclic_features, each = 2L), "_", c("sin", "cos")) :=
            compute_cyclic_date_features(.SD, features = cyclic_features, date_var = date_var)]
        }
      }

      if (self$param_set$values$keep_date_var == FALSE) {
        dt[, (date_vars) := NULL]
      }

      if (drop_year) {
        dt[, paste0(date_vars, ".", "year") := NULL]
      }

      dt
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
# sine and cosine transformations of 2 * pi * x / max_x
compute_cyclic_date_features = function(date_features, features, date_var) {
  # drop the date_var-specific colnames here, this makes it easier in lapply
  column_names = colnames(date_features)
  colnames(date_features) = c(column_names[1L],
    gsub(paste0(date_var, "."), replacement = "", x = column_names[-1L]))
  unlist(lapply(features, FUN = function(feature) {
    # all values are expected to start at 0 and therefore may be shifted by - 1
    value = if (feature %in% c("month", "week_of_year", "day_of_year", "day_of_month")) {
      date_features[[feature]] - 1L
    } else {
      date_features[[feature]]
    }
    maximum = switch(feature,
      month = 12L,
      week_of_year = get_weeks_per_year(date_features[["year"]]),
      day_of_year = (365L + as.integer(is_leap_year(date_features[["year"]]))),
      day_of_month = get_days_per_month(date_features[["year"]], month = date_features[["month"]]),
      day_of_week = 7L,
      hour = 24L,
      minute = 60L,
      second = 60L)
    value_scaled = 2L * pi * value / maximum
    list(sin(value_scaled), cos(value_scaled))
  }), recursive = FALSE)
}

# helper function to get the number of weeks per year, this can sometimes be 53 instead of 52 (works
# also vectorized)
get_weeks_per_year = function(year) {
  as.numeric(format(as.POSIXct(paste0(year, "-12-31"), format = c("%Y-%m-%d")), "%U"))
}

# helper function to check whether the year is a leap year (works also vectorized)
is_leap_year = function(year) {
  (year %% 4L == 0L & year %% 100L != 0L) | (year %% 400L == 0L)
}

# helper function to get the number of days per month respecting leap years
get_days_per_month = function(year, month) {
    c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[month] +
      ((month == 2L) & is_leap_year(year))
}
