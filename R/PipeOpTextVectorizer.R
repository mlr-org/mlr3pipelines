#' @title PipeOpTextVectorizer
#'
#' @usage NULL
#' @name mlr_pipeops_text_vectorizer
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Computes a bag-of-word representation from a (set of) columns.
#' Columns of type `character` are split up into words.
#' Uses the [`quanteda::dfm()`][quanteda::dfm] and
#' [`quanteda::dfm_trim()`][quanteda::dfm_trim] functions from the
#' 'quanteda' package.
#' Parameters specify arguments to 'dfm' and 'dfm_trim', i.e. how to
#' tokenize the data and how to trim the bag-of-words matrix.
#'
#' @section Construction:
#' ```
#' PipeOpTextVectorizer$new(id = "text_vectorizer", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"text_vectorizer"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with all affected features converted to a bag-of-words
#' representation.
#'
#' @section State:
#' The `$state` is a list with element 'cols': A vector of extracted columns.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `language` :: `character(1)`\cr
#'   Language to use for stopword filtering. Needs to be either in 
#'   `stopwords::stopwords_getlanguages("snowball")` or `"smart"`.
#'   'smart' coresponds to `stopwords::stopwords(source = "smart")`, which
#'   also removes one-character strings. Default: 'smart'.
#' * `remove_stopwords` :: `logical(1)`\cr
#'   Remove stopwords according to 'language'? Default: `TRUE`.
#' * `tolower` :: `logical(1)`\cr
#'   Convert to lower case? Default: `TRUE`.
#' * `remove_punct` :: `logical(1)`\cr
#'   See `quanteda::tokens`. Default: `FALSE`.
#' * `remove_punct` :: `logical(1)`\cr
#'   See `quanteda::tokens`. Default: `FALSE`.
#' * `remove_symbols` :: `logical(1)`\cr
#'   See `quanteda::tokens`. Default: `FALSE`.
#' * `remove_numbers` :: `logical(1)`\cr
#'   See `quanteda::tokens`. Default: `FALSE`.
#' * `remove_separators` :: `logical(1)`\cr
#'   See `quanteda::tokens`. Default: `TRUE`.
#' * `sparsity` :: `numeric(1)`\cr
#'   Desired sparsity of the 'tfm' matrix. See `quanteda::dfm_trim`. Default: `NULL`.
#' * `max_termfreq` :: `numeric(1)`\cr
#'   Maximum term frequency in the 'tfm' matrix. See `quanteda::dfm_trim`. Default: `NULL`.
#' * `min_termfreq` :: `numeric(1)`\cr
#'   Minimum term frequency in the 'tfm' matrix. See `quanteda::dfm_trim`. Default: `NULL`.
#' * `termfreq_type` :: `character`\cr
#'   How to asess term frequency. See `quanteda::dfm_trim`. Default: 'count'.
#'
#' @section Internals:
#' Uses the [`quanteda::dfm()`][quanteda::dfm] and
#' [`quanteda::dfm_trim()`][quanteda::dfm_trim] functions from the
#' 'quanteda' package.
#' All columns selected via `affect_columns` are concatenated before computing the bag of words.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' # create some text data
#' dt = data.table(
#'   txt = replicate(150, paste0(sample(letters, 3), collapse = " "))
#' )
#' task = tsk("iris")$cbind(dt)
#' 
#' pos = po("text_vectorizer", param_vals = list(language = "en"))
#'
#' pos$train(list(task))[[1]]$data()
#'
#' one_line_of_iris = task$filter(13)
#'
#' one_line_of_iris$data()
#'
#' pos$predict(list(one_line_of_iris))[[1]]$data()
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpTextVectorizer = R6Class("PipeOpTextVectorizer",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "text_vectorizer", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamFct$new("language", default = "en", tags = c("train", "predict"),
          levels = c(stopwords::stopwords_getlanguages("snowball"), "smart")),
        ParamLgl$new("remove_stopwords", default = TRUE, tags = c("train", "predict")),
        ParamLgl$new("tolower", default = TRUE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("stem", default = FALSE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("remove_punct", default = TRUE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("remove_symbols", default = FALSE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("remove_numbers", default = FALSE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("remove_url", default = FALSE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("remove_separators", default = TRUE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("split_hyphens", default = FALSE, tags = c("train", "predict", "tokenizer")),
        ParamDbl$new("sparsity", lower = 0, upper = 1, default = NULL,
          tags = c("train", "predict", "dfm_trim"), special_vals = list(NULL)),
        ParamFct$new("termfreq_type", default = "count", tags = c("train", "predict", "dfm_trim"),
          levels = c("count", "prop", "rank", "quantile")),
        ParamDbl$new("min_termfreq", lower = 0, upper = Inf, default = NULL,
          tags = c("train", "predict", "dfm_trim"), special_vals = list(NULL)),
        ParamDbl$new("max_termfreq", lower = 0, upper = Inf, default = NULL,
          tags = c("train", "predict", "dfm_trim"), special_vals = list(NULL))
      ))
      ps$values = list("remove_stopwords" = TRUE, language = "smart")
      super$initialize(id = id, param_set = ps, param_vals = param_vals,
        packages = c("quanteda", "stopwords"))
    },

    select_cols = function(task) {
      task$feature_types[get("type") %in% c("character"), get("id")]
    },

    train_dt = function(dt, levels, target) {
      dt = private$transform_bow(dt)
      self$state = list(cols = colnames(dt))
      return(dt)
    },
    predict_dt = function(dt, levels, target) {
        dt = private$transform_bow(dt)
        # make sure all columns occur (fill with 0), remove cols not seen in train:
        dt = rbindlist(list(data.table()[,self$state$cols := numeric(0)], dt), fill = TRUE)
        setnafill(dt, fill=0)
        dt[, self$state$cols, with = FALSE]
    }
  ),
  private = list(
      transform_bow = function(dt) {
        # Collapse all columns into one if > 1 columns are provied.
        if (ncol(dt) > 1) {
          dt = data.table("text" = dt[, do.call(paste, c(.SD, sep = ". ")), .SDcols = colnames(dt)])
        }
        corps = quanteda::corpus(data.frame(dt), text_field = colnames(dt))
        pv = self$param_set$get_values()

        remove = NULL
        if (pv$remove_stopwords) {
          if (pv$language == "smart")
            remove = stopwords::stopwords(source = "smart")
          else 
            remove = stopwords::stopwords(language = pv$language)
        }
        tdm = invoke(quanteda::dfm, .args = c(list(x = corps, remove = remove), self$param_set$get_values(tags = "tokenizer")))
        tdm = invoke(quanteda::dfm_trim, .args = c(tdm, self$param_set$get_values(tags = "dfm_trim")))
        dt = data.table(quanteda::convert(tdm, "matrix"))
      }
   )
)

mlr_pipeops$add("text_vectorizer", PipeOpTextVectorizer)
