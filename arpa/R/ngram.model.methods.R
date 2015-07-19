#' @export
setGeneric('unigrams', function(object) {
     standardGeneric('unigrams')
})

#' @export
setGeneric('bigrams', function(object) {
     standardGeneric('bigrams')
})

#' @export
setGeneric('trigrams', function(object) {
     standardGeneric('trigrams')
})

#' Return the unigrams of a language model
#'
#' @author Adam Acosta
#' @param object An ngram.model object
#' @return unigrams The unigrams of the model
#' @export
setMethod('unigrams', signature(object='ngram.model'), function(object) {
     object@unigrams
})

#' Return the bigrams of a language model
#'
#' @author Adam Acosta
#' @param object An ngram.model object
#' @return bigrams The bigrams of the model
#' @export
setMethod('bigrams', signature(object='ngram.model'), function(object) {
     object@bigrams
})

#' Return the trigrams of a language model
#'
#' @author Adam Acosta
#' @param object An ngram.model object
#' @return trigrams The trigrams of the model
#' @export
setMethod('trigrams', signature(object='ngram.model'), function(object) {
     object@trigrams
})

#' @export
setGeneric('contains', function(object, key) {
     standardGeneric('contains')
})

#' Tests whether or not a language model contains an ngram
#'
#' @author Adam Acosta
#' @param object An ngram.model object
#' @param key A character string
#' @return boolean Whether or not the string is in the language model
#' @export
setMethod('contains', signature(object='ngram.model', key='character'),
          function(object, key) {
               return(has.key(key, unigrams(object)) |
                      has.key(key, bigrams(object)) |
                      has.key(key, trigrams(object)))
          }
)

#' @export
setGeneric('prob', function(object, key) {
     standardGeneric('prob')
})

#' Returns the log probability of an ngram in a language model
#'
#' @author Adam Acosta
#' @param object An ngram.model object
#' @param key A character string
#' @return numeric The log probability of the provided ngram
#' @export
setMethod('prob', signature(object='ngram.model', key='character'),
          function(object, key) {
               if (!contains(object, key)) stop('key not in ngram.model')
               len <- length(str_to_vec(key, ' '))
               if (len == 1) return(unigrams(object)[[key]])
               if (len == 2) return(bigrams(object)[[key]])
               if (len == 3) return(trigrams(object)[[key]])
          }
)
