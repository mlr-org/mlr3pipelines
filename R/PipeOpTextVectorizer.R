#' @title PipeOpTextVectorizer
#'
#' @usage NULL
#' @name mlr_pipeops_text_vectorizer
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Computes a bag-of-word representation from a (set of) columns.
#' Columns of type `character` are split up into words.
#' Uses the [`quanteda::dfm()`][quanteda::dfm],
#' [`quanteda::dfm_trim()`][quanteda::dfm_trim] from the 'quanteda' package.
#' TF-IDF computation works similarly to [`quanteda::dfm_tfidf()`][quanteda::dfm_tfidf]
#' but has been adjusted for train/test data split using [`quanteda::docfreq()`][quanteda::docfreq] 
#' and [`quanteda::dfm_weight()`][quanteda::dfm_weight]
#' 
#' In short:
#' * Per default, produces a bag-of-words representation 
#' * If 'n' is set to values > 1 ngrams are computed
#' * If 'df_trim' parameters are set, the bag-of-words is trimmed.
#' * If 'scheme' parameters are set, term frequence - inverse document frequency is computed.
#' 
#' Parameters specify arguments to quanteda's dfm', 'dfm_trim', 'docfreq' and 'dfm_weight'.
#' What belongs to what can be obtained from each params `tags` where `tokenizer` are
#' arguments passed on to [`quanteda::dfm()`][quanteda::dfm].
#' Defaults to a bag-of-words representation with token counts as matrix entries.
#' 
#' In order to do `tf_idf` weighting, set the 'scheme' parameter to 'inverse*'.
#' 
#' The pipeop works as follows:
#' 1. Words are tokenized using [`quanteda::tokens`].
#' 2. Ngrams are computed using [`quanteda::tokens_ngrams`]
#' 3. A document-frequency matrix is computed using [`quanteda::dfm`]
#' 4. The document-frequency matrix is trimmed using [`quanteda::dfm_trim`]
#' 5. The document-frequency matrix is re-weighted (e.g. tfidf) using [`quanteda::dfm_tfidf`]
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
#' 
#' * `language` :: `character(1)`\cr
#'   Language to use for stopword filtering. Needs to be either in 
#'   `stopwords::stopwords_getlanguages("snowball")` or `"smart"`.
#'   'smart' coresponds to `stopwords::stopwords(source = "smart")`, which
#'   also removes one-character strings. Default: 'smart'.
#' * `remove_stopwords` :: `logical(1)`\cr
#'   Remove stopwords according to 'language'? Default: `TRUE`.
#' 
#' * `tolower` :: `logical(1)`\cr
#'   Convert to lower case? See [`quanteda::dfm`]. Default: `TRUE`.
#' * `stem` :: `logical(1)`\cr
#'   Stemming? See [`quanteda::dfm`]. Default: `FALSE`.
#' 
#' * `what` :: `character(1)`\cr
#'   Tokenization splitter. See [`quanteda::tokens`]. Default: `word`.
#' * `remove_punct` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default: `FALSE`.
#' * `remove_url` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default: `FALSE`.
#' * `remove_symbols` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default: `FALSE`.
#' * `remove_numbers` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default: `FALSE`.
#' * `remove_separators` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default: `TRUE`.
#' * `split_hypens` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default: `FALSE`.
#' 
#' * `n` :: `integer`\cr
#'   Vector of ngram lengths. See [`quanteda::tokens_ngrams`]. Default: 1.
#' * `skip` :: `integer`\cr
#'   Vector of skips. See [`quanteda::tokens_ngrams`]. Default: 0.
#' * `sparsity` :: `numeric(1)`\cr
#' 
#'   Desired sparsity of the 'tfm' matrix. See [`quanteda::dfm_trim`]. Default: `NULL`.
#' * `max_termfreq` :: `numeric(1)`\cr
#'   Maximum term frequency in the 'tfm' matrix. See [`quanteda::dfm_trim`]. Default: `NULL`.
#' * `min_termfreq` :: `numeric(1)`\cr
#'   Minimum term frequency in the 'tfm' matrix. See [`quanteda::dfm_trim`]. Default: `NULL`.
#' * `termfreq_type` :: `character(1)`\cr
#'   How to asess term frequency. See [`quanteda::dfm_trim`]. Default: 'count'.
#' * `scheme_tf` :: `character(1)` \cr
#'   Weighting scheme for term frequency: See [`quanteda::dfm_weight`]. Default: 'count'.
#' * `scheme` :: `character(1)` \cr
#'   Weighting scheme for document frequency: See [`quanteda::docfreq`]. Default: 'unary' (1 for each document).
#' * `smoothing` :: `numeric(1)`\cr
#'   See [`quanteda::docfreq`]. Default: 0.
#' * `k` :: `numeric(1)`\cr
#'   See [`quanteda::docfreq`]. Default: 0.
#' * `threshold` :: `numeric(1)`\cr
#'   See [`quanteda::docfreq`]. Default: 0.
#' * `base` :: `numeric(1)`\cr
#'   See [`quanteda::docfreq`]. Default: 10.
#' 
#' @section Internals:
#' See Description. Internally uses the 'quanteda' package.
#' All columns selected via `affect_columns` are concatenated before computing the bag of words.
#' Tokens not seen during training are silently dropped.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' library("data.table")
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
        ParamFct$new("language", default = "smart", tags = c("train", "predict"),
          levels = c(stopwords::stopwords_getlanguages("snowball"), "smart")),
        ParamLgl$new("remove_stopwords", default = TRUE, tags = c("train", "predict")),

        ParamLgl$new("tolower", default = TRUE, tags = c("train", "predict", "dfm")),
        ParamLgl$new("stem", default = FALSE, tags = c("train", "predict", "dfm")),

        ParamFct$new("what", default = "word", tags = c("train", "predict", "tokenizer"),
          levels = c("word", "word1", "fasterword", "fastestword", "character", "sentence")),
        ParamLgl$new("remove_punct", default = FALSE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("remove_symbols", default = FALSE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("remove_numbers", default = FALSE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("remove_url", default = FALSE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("remove_separators", default = TRUE, tags = c("train", "predict", "tokenizer")),
        ParamLgl$new("split_hyphens", default = FALSE, tags = c("train", "predict", "tokenizer")),

        ParamUty$new("n", default = 1, tags = c("train", "predict", "ngrams"), custom_check = check_integerish),
        ParamUty$new("skip", default = 0, tags = c("train", "predict", "ngrams"), custom_check = check_integerish),

        ParamDbl$new("sparsity", lower = 0, upper = 1, default = NULL,
          tags = c("train", "predict", "dfm_trim"), special_vals = list(NULL)),
        ParamFct$new("termfreq_type", default = "count", tags = c("train", "predict", "dfm_trim"),
          levels = c("count", "prop", "rank", "quantile")),
        ParamDbl$new("min_termfreq", lower = 0, upper = Inf, default = NULL,
          tags = c("train", "predict", "dfm_trim"), special_vals = list(NULL)),
        ParamDbl$new("max_termfreq", lower = 0, upper = Inf, default = NULL,
          tags = c("train", "predict", "dfm_trim"), special_vals = list(NULL)),

        ParamFct$new("scheme", default = "unary", tags = c("train", "predict", "docfreq"),
          levels = c("count", "inverse", "inversemax", "inverseprob", "unary")),
        ParamFct$new("scheme_tf", default = "count", tags = c("train", "predict"),
          levels = c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave")),
        ParamDbl$new("smoothing", lower = 0, upper = Inf, default = 0, tags = c("train", "predict", "docfreq")),
        ParamDbl$new("k", lower = 0, upper = Inf, default = 0, tags = c("train", "predict", "docfreq", "dfm_weight")),       
        ParamDbl$new("threshold", lower = 0, upper = Inf, default = 0, tags = c("train", "predict", "docfreq", "dfm_weight")),
        ParamDbl$new("base", lower = 0, upper = Inf, default = 10, tags = c("train", "predict", "docfreq"))
      ))
      ps$add_dep("base", "scheme", CondAnyOf$new(c("inverse", "inversemax", "inverseprob")))
      ps$add_dep("smoothing", "scheme", CondAnyOf$new(c("inverse", "inversemax", "inverseprob")))
      ps$add_dep("k", "scheme", CondAnyOf$new(c("inverse", "inversemax", "inverseprob")))
      ps$add_dep("base", "scheme", CondAnyOf$new(c("inverse", "inversemax", "inverseprob")))
      ps$add_dep("threshold", "scheme", CondAnyOf$new(c("inverse", "inversemax", "inverseprob")))
      ps$values = list("remove_stopwords" = TRUE, language = "smart", n = 1, scheme = "unary", scheme_tf = "count")
      super$initialize(id = id, param_set = ps, param_vals = param_vals, packages = c("quanteda", "stopwords"))
    },

    select_cols = function(task) {
      task$feature_types[get("type") == "character", get("id")]
    },

    train_dt = function(dt, levels, target) {
      tdm = private$transform_bow(dt)                     # transform to BOW, return doc-freq matrix
      self$state = list(tdm = quanteda::dfm_subset(tdm, FALSE))     # empty tdm so we have vocab of training data
      # tf-idf
      if (self$param_set$get_values()$scheme != "unary") {
          self$state$docfreq = invoke(quanteda::docfreq, .args = c(tdm, self$param_set$get_values(tags = "docfreq")))
          tdm = private$transform_tfidf(tdm)
      }
      quanteda::convert(tdm, "matrix")
    },
    predict_dt = function(dt, levels, target) {
      if (nrow(dt)) {
        tdm = private$transform_bow(dt)
        tdm = rbind(tdm, self$state$tdm) # make sure all columns occur
        # tf-idf
        if (self$param_set$get_values()$scheme != "unary") tdm = private$transform_tfidf(tdm)
        tdm = tdm[, colnames(self$state$tdm)]   # Ensure only train-time features are pased on
      } else {
        tdm = self$state$tdm
      }
      quanteda::convert(tdm, "matrix")
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
      # tokenize
      tkn = invoke(quanteda::tokens, .args = c(list(x = corps), self$param_set$get_values(tags = "tokenizer")))
      tkn = invoke(quanteda::tokens_ngrams, .args = c(list(x = tkn), self$param_set$get_values(tags = "ngrams")))
      # document-feature matrix
      tdm = invoke(quanteda::dfm, .args = c(list(x = tkn, remove = remove), self$param_set$get_values(tags = "dfm")))
      # trim rare tokens
      invoke(quanteda::dfm_trim, .args = c(tdm, self$param_set$get_values(tags = "dfm_trim")))
    },
    transform_tfidf = function(tdm) {
      if (!quanteda::nfeat(tdm) || !quanteda::ndoc(tdm)) return(tdm)
      # code copied from quanteda:::dfm_tfidf.dfm (adapting here to avoid train/test leakage)
      x = invoke(quanteda::dfm_weight, .args = c(x = tdm, scheme = self$param_set$get_values()$scheme_tf, self$param_set$get_values("dfm_weight")))
      v = self$state$docfreq
      j = as(x, "dgTMatrix")@j + 1L
      x@x = x@x * v[j]
      return(x)
    }
  )
)

mlr_pipeops$add("text_vectorizer", PipeOpTextVectorizer)
