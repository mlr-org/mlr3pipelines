#' @title Bag-of-word Representation of Character Features
#'
#' @usage NULL
#' @name mlr_pipeops_textvectorizer
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Computes a bag-of-word representation from a (set of) columns.
#' Columns of type `character` are split up into words.
#' Uses the [`quanteda::dfm()`][quanteda::dfm] and [`quanteda::dfm_trim()`][quanteda::dfm_trim] functions.
#' TF-IDF computation works similarly to [`quanteda::dfm_tfidf()`][quanteda::dfm_tfidf]
#' but has been adjusted for train/test data split using [`quanteda::docfreq()`][quanteda::docfreq]
#' and [`quanteda::dfm_weight()`][quanteda::dfm_weight].
#'
#' In short:
#' * Per default, produces a bag-of-words representation
#' * If `n` is set to values > 1, ngrams are computed
#' * If `df_trim` parameters are set, the bag-of-words is trimmed.
#' * The `scheme_tf` parameter controls term-frequency (per-document, i.e. per-row) weighting
#' * The `scheme_df` parameter controls the document-frequency (per token, i.e. per-column) weighting.
#'
#' Parameters specify arguments to `quanteda`'s `dfm`, `dfm_trim`, `docfreq` and `dfm_weight`.
#' What belongs to what can be obtained from each parameter's `tags` where `tokenizer` are
#' arguments passed on to [`quanteda::dfm()`][quanteda::dfm].
#' Defaults to a bag-of-words representation with token counts as matrix entries.
#'
#' In order to perform the *default* `dfm_tfidf` weighting, set the `scheme_df` parameter to `"inverse"`.
#' The `scheme_df` parameter is initialized to `"unary"`, which disables document frequency weighting.
#'
#' The [`PipeOp`] works as follows:
#' 1. Words are tokenized using [`quanteda::tokens`].
#' 2. Ngrams are computed using [`quanteda::tokens_ngrams`].
#' 3. A document-frequency matrix is computed using [`quanteda::dfm`].
#' 4. The document-frequency matrix is trimmed using [`quanteda::dfm_trim`] during train-time.
#' 5. The document-frequency matrix is re-weighted (similar to [`quanteda::dfm_tfidf`]) if `scheme_df` is not set to `"unary"`.
#'
#' @section Construction:
#' ```
#' PipeOpTextVectorizer$new(id = "textvectorizer", param_vals = list())
#' ```
#'
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
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`], as well as:
#' * `colmodels` :: named `list`\cr
#'   Named list with one entry per extracted column. Each entry has two further elements:
#'   * `tdm`: sparse document-feature matrix resulting from [`quanteda::dfm()`][quanteda::dfm]
#'   * `docfreq`: (weighted) document frequency resulting from [`quanteda::docfreq()`][quanteda::docfreq]
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `return_type` :: `character(1)`\cr
#'    Whether to return an integer representation (`"integer-sequence"`) or a Bag-of-words (`"bow"`).
#'    If set to `"integer_sequence"`, tokens are replaced by an integer and padded/truncated to `sequence_length`.
#'    If set to `"factor_sequence"`, tokens are replaced by a factor and padded/truncated to `sequence_length`.
#'    If set to `"bow"`, a possibly weighted bag-of-words matrix is returned.
#'    Defaults to `bow`.
#' * `stopwords_language` :: `character(1)`\cr
#'   Language to use for stopword filtering. Needs to be either `"none"`, a language identifier listed in
#'   `stopwords::stopwords_getlanguages("snowball")` (`"de"`, `"en"`, ...) or `"smart"`.
#'   `"none"` disables language-specific stopwords.
#'   `"smart"` coresponds to `stopwords::stopwords(source = "smart")`, which
#'   contains *English* stopwords and also removes one-character strings. Initialized to `"smart"`.
#' * `extra_stopwords` :: `character`\cr
#'   Extra stopwords to remove. Must be a `character` vector containing individual tokens to remove.
#'   When `n` is set to values greater than `1`, this can also contain stop-ngrams.
#'   Initialized to `character(0)`.
#' * `tolower` :: `logical(1)`\cr
#'   Whether to convert to lower case. See [`quanteda::dfm`]. Default is `TRUE`.
#' * `stem` :: `logical(1)`\cr
#'   Whether to perform stemming. See [`quanteda::dfm`]. Default is `FALSE`.
#' * `what` :: `character(1)`\cr
#'   Tokenization splitter. See [`quanteda::tokens`]. Default is `"word"`.
#' * `remove_punct` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default is `FALSE`.
#' * `remove_url` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default is `FALSE`.
#' * `remove_symbols` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default is `FALSE`.
#' * `remove_numbers` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default is `FALSE`.
#' * `remove_separators` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default is  `TRUE`.
#' * `split_hypens` :: `logical(1)`\cr
#'   See [`quanteda::tokens`]. Default is `FALSE`.
#' * `n` :: `integer`\cr
#'   Vector of ngram lengths. See [`quanteda::tokens_ngrams`]. Initialized to `1`, deviating from the base function's default.
#'   Note that this can be a *vector* of multiple values, to construct ngrams of multiple orders.
#' * `skip` :: `integer`\cr
#'   Vector of skips. See [`quanteda::tokens_ngrams`]. Default is `0`. Note that this can be a *vector* of multiple values.
#' * `sparsity` :: `numeric(1)`\cr
#'   Desired sparsity of the 'tfm' matrix. See [`quanteda::dfm_trim`]. Default is `NULL`.
#' * `max_termfreq` :: `numeric(1)`\cr
#'   Maximum term frequency in the 'tfm' matrix. See [`quanteda::dfm_trim`]. Default is `NULL`.
#' * `min_termfreq` :: `numeric(1)`\cr
#'   Minimum term frequency in the 'tfm' matrix. See [`quanteda::dfm_trim`]. Default is `NULL`.
#' * `termfreq_type` :: `character(1)`\cr
#'   How to asess term frequency. See [`quanteda::dfm_trim`]. Default is `"count"`.
#' * `scheme_df` :: `character(1)` \cr
#'   Weighting scheme for document frequency: See [`quanteda::docfreq`]. Initialized to `"unary"` (`1` for each document, deviating from base function default).
#' * `smoothing_df` :: `numeric(1)`\cr
#'   See [`quanteda::docfreq`]. Default is `0`.
#' * `k_df` :: `numeric(1)`\cr
#'   `k` parameter given to [`quanteda::docfreq`] (see there).
#'   Default is `0`.
#' * `threshold_df` :: `numeric(1)`\cr
#'   See [`quanteda::docfreq`]. Default is `0`. Only considered if `scheme_df` is set to `"count"`.
#' * `base_df` :: `numeric(1)`\cr
#'   The base for logarithms in [`quanteda::docfreq`] (see there). Default is `10`.
#' * `scheme_tf` :: `character(1)` \cr
#'   Weighting scheme for term frequency: See [`quanteda::dfm_weight`]. Default is `"count"`.
#' * `k_tf` :: `numeric(1)`\cr
#'   `k` parameter given to [`quanteda::dfm_weight`] (see there).
#'    Default is `0.5`.
#' * `base_df` :: `numeric(1)`\cr
#'   The base for logarithms in [`quanteda::dfm_weight`] (see there). Default is `10`.
#' * `sequence_length` :: `integer(1)`\cr
#'   The length of the integer sequence. Defaults to `Inf`, i.e. all texts are padded to the length
#'   of the longest text. Only relevant for `return_type` is set to `"integer_sequence"`.
#'
#' @section Internals:
#' See Description. Internally uses the `quanteda` package. Calls [`quanteda::tokens`], [`quanteda::tokens_ngrams`] and [`quanteda::dfm`]. During training,
#' [`quanteda::dfm_trim`] is also called. Tokens not seen during training are dropped during prediction.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examplesIf mlr3misc::require_namespaces(c("stopwords", "quanteda"), quietly = TRUE)
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
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpTextVectorizer = R6Class("PipeOpTextVectorizer",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "textvectorizer", param_vals = list()) {
      ps = ps(
        stopwords_language = p_fct(tags = c("train", "predict"),
          levels = c("da", "de",    "en",   "es",    "fi",   "fr",   "hu",     "ir",   "it",
                     "nl", "no",    "pt",   "ro",    "ru",   "sv" ,   "smart", "none")),
        extra_stopwords = p_uty(tags = c("train", "predict"), custom_check = check_character),

        tolower = p_lgl(default = TRUE, tags = c("train", "predict", "dfm")),
        stem = p_lgl(default = FALSE, tags = c("train", "predict", "dfm")),

        what = p_fct(default = "word", tags = c("train", "predict", "tokenizer"),
          levels = c("word", "word1", "fasterword", "fastestword", "character", "sentence")),
        remove_punct = p_lgl(default = FALSE, tags = c("train", "predict", "tokenizer")),
        remove_symbols = p_lgl(default = FALSE, tags = c("train", "predict", "tokenizer")),
        remove_numbers = p_lgl(default = FALSE, tags = c("train", "predict", "tokenizer")),
        remove_url = p_lgl(default = FALSE, tags = c("train", "predict", "tokenizer")),
        remove_separators = p_lgl(default = TRUE, tags = c("train", "predict", "tokenizer")),
        split_hyphens = p_lgl(default = FALSE, tags = c("train", "predict", "tokenizer")),

        n = p_uty(default = 2, tags = c("train", "predict", "ngrams"), custom_check = curry(check_integerish, min.len = 1, lower = 1, any.missing = FALSE)),
        skip = p_uty(default = 0, tags = c("train", "predict", "ngrams"), custom_check = curry(check_integerish, min.len = 1, lower = 0, any.missing = FALSE)),

        sparsity = p_dbl(lower = 0, upper = 1, default = NULL,
          tags = c("train", "dfm_trim"), special_vals = list(NULL),
          depends = quote(return_type == "bow")),
        termfreq_type = p_fct(default = "count", tags = c("train", "dfm_trim"),
          levels = c("count", "prop", "rank", "quantile")),
        min_termfreq = p_dbl(lower = 0, default = NULL,
          tags = c("train", "dfm_trim"), special_vals = list(NULL)),
        max_termfreq = p_dbl(lower = 0, default = NULL,
          tags = c("train", "dfm_trim"), special_vals = list(NULL)),

        scheme_df = p_fct(default = "count", tags = c("train", "docfreq"),
          levels = c("count", "inverse", "inversemax", "inverseprob", "unary")),
        smoothing_df = p_dbl(lower = 0, default = 0, tags = c("train", "docfreq"), depends = quote(scheme_df %in% c("inverse", "inversemax", "inverseprob"))),
        k_df = p_dbl(lower = 0, tags = c("train", "docfreq"), depends = quote(scheme_df %in% c("inverse", "inversemax", "inverseprob"))),
        threshold_df = p_dbl(lower = 0, default = 0, tags = c("train", "docfreq"), depends = quote(scheme_df == "count")),
        base_df = p_dbl(lower = 0, default = 10, tags = c("train", "docfreq"),
          depends = quote(scheme_df %in% c("inverse", "inversemax", "inverseprob"))),

        scheme_tf = p_fct(default = "count", tags = c("train", "predict", "dfm_weight"), depends = quote(return_type == "bow"),
          levels = c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave")),
        k_tf = p_dbl(lower = 0, upper = 1, tags = c("train", "predict", "dfm_weight"), depends = quote(scheme_tf == "augmented")),
        base_tf = p_dbl(lower = 0, default = 10, tags = c("train", "predict", "dfm_weight"), depends = quote(scheme_tf %in% c("logcount", "logave"))),

        return_type = p_fct(levels = c("bow", "integer_sequence", "factor_sequence"), tags = c("train", "predict")),
        sequence_length = p_int(default = 0, lower = 0, upper = Inf, tags = c("train", "predict", "integer_sequence"),
          depends = quote(return_type %in% c("integer_sequence", "factor_sequence")))
      )

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
      tdm = invoke(quanteda::dfm, .args = c(list(x = quanteda::tokens_remove(x = tkn, remove, valuetype = "fixed")), self$param_set$get_values(tags = "dfm")))
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
