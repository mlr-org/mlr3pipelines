#' @title PipeOpTextVectorizer
#'
#' @usage NULL
#' @name mlr_pipeops_textvectorizer
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
#' * If `n` is set to values > 1, ngrams are computed
#' * If `df_trim` parameters are set, the bag-of-words is trimmed.
#' * The `scheme_tf` parameter controls term-frequency (per-document, i.e. per-row) weighting
#' * The `scheme_df` parameter controls the document-frequency (per token, i.e. per-column) weighting.
#'
#' Parameters specify arguments to quanteda's `dfm`, `dfm_trim`, `docfreq` and `dfm_weight`.
#' What belongs to what can be obtained from each params `tags` where `tokenizer` are
#' arguments passed on to [`quanteda::dfm()`][quanteda::dfm].
#' Defaults to a bag-of-words representation with token counts as matrix entries.
#'
#' In order to perform the *default* `dfm_tfidf` weighting, set the `scheme_df` parameter to `"inverse"`.
#' The `scheme_df` parameter is initialized to `"unary"`, which disables document frequency weighting.
#'
#' The pipeop works as follows:
#' 1. Words are tokenized using [`quanteda::tokens`].
#' 2. Ngrams are computed using [`quanteda::tokens_ngrams`]
#' 3. A document-frequency matrix is computed using [`quanteda::dfm`]
#' 4. The document-frequency matrix is trimmed using [`quanteda::dfm_trim`] during train-time.
#' 5. The document-frequency matrix is re-weighted (similar to [`quanteda::dfm_tfidf`]) if `scheme_df` is not set to `"unary"`.
#'
#' @section Construction:
#' ```
#' PipeOpTextVectorizer$new(id = "textvectorizer", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"textvectorizer"`.
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
#' * `return_type` :: `character(1)`\cr
#'    Whether to return an integer representation ("integer-sequence") or a Bag-of-words ("bow").
#'    If set to "integer_sequence", tokens are replaced by an integer and padded/truncated to `sequence_length`.
#'    If set to "factor_sequence", tokens are replaced by a factor and padded/truncated to `sequence_length`.
#'    If set to 'bow', a possibly weighted bag-of-words matrix is returned.
#'    Defaults to `bow`.
#'
#' * `stopwords_language` :: `character(1)`\cr
#'   Language to use for stopword filtering. Needs to be either `"none"`, a language identifier listed in
#'   `stopwords::stopwords_getlanguages("snowball")` (`"de"`, `"en"`, ...) or `"smart"`.
#'   `"none"` disables language-specific stopwords.
#'   `"smart"` coresponds to `stopwords::stopwords(source = "smart")`, which
#'   contains *English* stopwords and also removes one-character strings. Initialized to `"smart"`.\cr
#' * `extra_stopwords` :: `character`\cr
#'   Extra stopwords to remove. Must be a `character` vector containing individual tokens to remove. Initialized to `character(0)`.
#'   When `n` is set to values greater than 1, this can also contain stop-ngrams.
#'
#' * `tolower` :: `logical(1)`\cr
#'   Convert to lower case? See [`quanteda::dfm`]. Default: `TRUE`.
#' * `stem` :: `logical(1)`\cr
#'   Perform stemming? See [`quanteda::dfm`]. Default: `FALSE`.
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
#'   Vector of ngram lengths. See [`quanteda::tokens_ngrams`]. Initialized to 1, deviating from the base function's default.
#'   Note that this can be a *vector* of multiple values, to construct ngrams of multiple orders.
#' * `skip` :: `integer`\cr
#'   Vector of skips. See [`quanteda::tokens_ngrams`]. Default: 0. Note that this can be a *vector* of multiple values.
#'
#' * `sparsity` :: `numeric(1)`\cr
#'   Desired sparsity of the 'tfm' matrix. See [`quanteda::dfm_trim`]. Default: `NULL`.
#' * `max_termfreq` :: `numeric(1)`\cr
#'   Maximum term frequency in the 'tfm' matrix. See [`quanteda::dfm_trim`]. Default: `NULL`.
#' * `min_termfreq` :: `numeric(1)`\cr
#'   Minimum term frequency in the 'tfm' matrix. See [`quanteda::dfm_trim`]. Default: `NULL`.
#' * `termfreq_type` :: `character(1)`\cr
#'   How to asess term frequency. See [`quanteda::dfm_trim`]. Default: `"count"`.
#'
#' * `scheme_df` :: `character(1)` \cr
#'   Weighting scheme for document frequency: See [`quanteda::docfreq`]. Initialized to `"unary"` (1 for each document, deviating from base function default).
#' * `smoothing_df` :: `numeric(1)`\cr
#'   See [`quanteda::docfreq`]. Default: 0.
#' * `k_df` :: `numeric(1)`\cr
#'   `k` parameter given to [`quanteda::docfreq`] (see there).
#'    Default is 0.
#' * `threshold_df` :: `numeric(1)`\cr
#'   See [`quanteda::docfreq`]. Default: 0. Only considered for `scheme_df` = `"count"`.
#' * `base_df` :: `numeric(1)`\cr
#'   The base for logarithms in [`quanteda::docfreq`] (see there). Default: 10.
#'
#' * `scheme_tf` :: `character(1)` \cr
#'   Weighting scheme for term frequency: See [`quanteda::dfm_weight`]. Default: `"count"`.
#' * `k_tf` :: `numeric(1)`\cr
#'   `k` parameter given to [`quanteda::dfm_weight`] (see there).
#'    Default behaviour is 0.5.
#' * `base_df` :: `numeric(1)`\cr
#'   The base for logarithms in [`quanteda::dfm_weight`] (see there). Default: 10.
#'
#' #' * `sequence_length` :: `integer(1)`\cr
#'   The length of the integer sequence. Defaults to `Inf`, i.e. all texts are padded to the length
#'   of the longest text. Only relevant for "return_type" : "integer_sequence"
#'
#' @section Internals:
#' See Description. Internally uses the `quanteda` package. Calls [`quanteda::tokens`], [`quanteda::tokens_ngrams`] and [`quanteda::dfm`]. During training,
#' [`quanteda::dfm_trim`] is also called. Tokens not seen during training are dropped during prediction.
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
#' pos = po("textvectorizer", param_vals = list(stopwords_language = "en"))
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
    initialize = function(id = "textvectorizer", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamFct$new("stopwords_language", tags = c("train", "predict"),
          levels = c("da", "de",    "en",   "es",    "fi",   "fr",   "hu",     "ir",   "it",
                     "nl", "no",    "pt",   "ro",    "ru",   "sv" ,   "smart", "none")),
        ParamUty$new("extra_stopwords", tags = c("train", "predict"), custom_check = check_character),

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

        ParamUty$new("n", default = 2, tags = c("train", "predict", "ngrams"), custom_check = curry(check_integerish, min.len = 1, lower = 1, any.missing = FALSE)),
        ParamUty$new("skip", default = 0, tags = c("train", "predict", "ngrams"), custom_check = curry(check_integerish, min.len = 1, lower = 0, any.missing = FALSE)),

        ParamDbl$new("sparsity", lower = 0, upper = 1, default = NULL,
          tags = c("train", "dfm_trim"), special_vals = list(NULL)),
        ParamFct$new("termfreq_type", default = "count", tags = c("train", "dfm_trim"),
          levels = c("count", "prop", "rank", "quantile")),
        ParamDbl$new("min_termfreq", lower = 0, default = NULL,
          tags = c("train", "dfm_trim"), special_vals = list(NULL)),
        ParamDbl$new("max_termfreq", lower = 0, default = NULL,
          tags = c("train", "dfm_trim"), special_vals = list(NULL)),

        ParamFct$new("scheme_df", default = "count", tags = c("train", "docfreq"),
          levels = c("count", "inverse", "inversemax", "inverseprob", "unary")),
        ParamDbl$new("smoothing_df", lower = 0, default = 0, tags = c("train", "docfreq")),
        ParamDbl$new("k_df", lower = 0, tags = c("train", "docfreq")),
        ParamDbl$new("threshold_df", lower = 0, default = 0, tags = c("train", "docfreq")),
        ParamDbl$new("base_df", lower = 0, default = 10, tags = c("train", "docfreq")),

        ParamFct$new("scheme_tf", default = "count", tags = c("train", "predict", "dfm_weight"),
          levels = c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave")),
        ParamDbl$new("k_tf", lower = 0, upper = 1, tags = c("train", "predict", "dfm_weight")),
        ParamDbl$new("base_tf", lower = 0, default = 10, tags = c("train", "predict", "dfm_weight")),

        ParamFct$new("return_type", levels = c("bow", "integer_sequence", "factor_sequence"), tags = c("train", "predict")),
        ParamInt$new("sequence_length", default = 0, lower = 0, upper = Inf, tags = c("train", "predict", "integer_sequence"))
      ))$
        add_dep("base_df", "scheme_df", CondAnyOf$new(c("inverse", "inversemax", "inverseprob")))$
        add_dep("smoothing_df", "scheme_df", CondAnyOf$new(c("inverse", "inversemax", "inverseprob")))$
        add_dep("k_df", "scheme_df", CondAnyOf$new(c("inverse", "inversemax", "inverseprob")))$
        add_dep("base_df", "scheme_df", CondAnyOf$new(c("inverse", "inversemax", "inverseprob")))$
        add_dep("threshold_df", "scheme_df", CondEqual$new("count"))$
        add_dep("k_tf", "scheme_tf", CondEqual$new("augmented"))$
        add_dep("base_tf", "scheme_tf", CondAnyOf$new(c("logcount", "logave")))$
        add_dep("scheme_tf", "return_type", CondEqual$new("bow"))$
        add_dep("sparsity", "return_type", CondEqual$new("bow"))$
        add_dep("sequence_length", "return_type", CondAnyOf$new(c("integer_sequence", "factor_sequence")))

      ps$values = list(stopwords_language = "smart", extra_stopwords = character(0), n = 1, scheme_df = "unary", return_type = "bow")
      super$initialize(id = id, param_set = ps, param_vals = param_vals, packages = c("quanteda", "stopwords"), feature_types = "character")
    }
  ),
  private = list(

    .train_dt = function(dt, levels, target) {
      colwise_results = sapply(dt, function(column) {
        tkn = private$.transform_tokens(column)
        tdm = private$.transform_bow(tkn, trim = TRUE)  # transform to BOW (bag of words), return term count matrix
        state = list(
          tdm = quanteda::dfm_subset(tdm, FALSE),  # empty tdm so we have vocab of training data
          docfreq = invoke(quanteda::docfreq, .args = c(list(x = tdm),  # column weights
            rename_list(self$param_set$get_values(tags = "docfreq"), "_df$", "")))
        )
        if (self$param_set$values$return_type == "bow") {
          matrix = quanteda::convert(private$.transform_tfidf(tdm, state$docfreq), "matrix")
        } else {
          matrix = private$.transform_integer_sequence(tkn, tdm, state)
        }
        list(state = state, matrix = matrix)
      }, simplify = FALSE)
      self$state = list(colmodels = map(colwise_results, "state"))
      as.data.frame(map(colwise_results, "matrix"))
    },

    .predict_dt = function(dt, levels, target) {
      colwise_results = imap(dt, function(column, colname) {
        state = self$state$colmodels[[colname]]
        if (nrow(dt)) {
          tkn = private$.transform_tokens(column)
          tdm = private$.transform_bow(tkn, trim = TRUE)
          tdm = rbind(tdm, state$tdm)  # make sure all columns occur
          tdm = tdm[, colnames(state$tdm)]  # Ensure only train-time features are pased on

          if (self$param_set$values$return_type == "bow") {
            tdm = quanteda::convert(private$.transform_tfidf(tdm, state$docfreq), "matrix")
          } else {
            tdm = private$.transform_integer_sequence(tkn, tdm, state)
          }
        } else {
          tdm = quanteda::convert(state$tdm, "matrix")
        }
        tdm
      })
      as.data.frame(colwise_results)
    },
    # text: character vector of feature column
    .transform_tokens = function(text) {
      corpus = quanteda::corpus(text)
      # tokenize
      tkn = invoke(quanteda::tokens, .args = c(list(x = corpus), self$param_set$get_values(tags = "tokenizer")))
      invoke(quanteda::tokens_ngrams, .args = c(list(x = tkn), self$param_set$get_values(tags = "ngrams")))
    },
    # tkn: tokenized text, result from `.transform_tokens`
    # trim: TRUE during training: trim infrequent features
    .transform_bow = function(tkn, trim) {
      pv = self$param_set$get_values()
      remove = NULL
      if (pv$stopwords_language != "none") {
        if (pv$stopwords_language == "smart") {
          remove = stopwords::stopwords(source = "smart")
        } else {
          remove = stopwords::stopwords(language = self$param_set$get_values()$stopwords_language)
        }
      }
      remove = c(remove, pv$extra_stopwords)
      # document-feature matrix
      tdm = invoke(quanteda::dfm, .args = c(list(x = tkn, remove = remove), self$param_set$get_values(tags = "dfm")))
      # trim rare tokens
      if (trim) {
        invoke(quanteda::dfm_trim, .args = c(list(x = tdm), self$param_set$get_values(tags = "dfm_trim")))
      } else {
        tdm
      }
    },
    .transform_integer_sequence = function(tkn, tdm, state) {
      # List of allowed tokens:
      pv = insert_named(list(min_termfreq = 0, max_termfreq = Inf), self$param_set$get_values(tags = "dfm_trim"))
      dt = data.table(data.table(feature = names(state$docfreq), frequency = state$docfreq))
      tokens = unname(unclass(tkn))
      dict = attr(tokens, "types")
      dict = setkeyv(data.table(k = dict, v = seq_along(dict)), "k")
      dict = dict[dt][pv$min_termfreq < get("frequency") & get("frequency") < pv$max_termfreq,]

      # pad or cut x to length l
      pad0 = function(x, l) {
        c(x[seq_len(min(length(x), l))], rep(0, max(0, l - length(x))))
      }

      il = self$param_set$values$sequence_length
      if (is.null(il)) il = max(map_int(tokens, length))
      tokens = map(tokens, function(x) {
        x = pad0(ifelse(x %in% dict$v, x, 0), il)
        data.table(matrix(x, nrow = 1))
      })
      tokens = rbindlist(tokens)
      if (self$param_set$values$return_type == "factor_sequence") {
        tokens = map_dtc(tokens, factor, levels = dict$v[!is.na(dict$v)], labels = dict$k[!is.na(dict$v)])
      }
      tokens
    },
    .transform_tfidf = function(tdm, docfreq) {
      if (!quanteda::nfeat(tdm)) return(tdm)
      # code copied from quanteda:::dfm_tfidf.dfm (adapting here to avoid train/test leakage)
      x = invoke(quanteda::dfm_weight, .args = c(list(x = tdm),
        rename_list(self$param_set$get_values("dfm_weight"), "_tf$", "")))
      v = docfreq
      j = methods::as(x, "dgTMatrix")@j + 1L
      x@x = x@x * v[j]
      x
    }
  )
)

mlr_pipeops$add("textvectorizer", PipeOpTextVectorizer)
