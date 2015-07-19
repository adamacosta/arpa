setGeneric('unigrams', function(object) {
     standardGeneric('unigrams')
})

setGeneric('bigrams', function(object) {
     standardGeneric('bigrams')
})

setGeneric('trigrams', function(object) {
     standardGeneric('trigrams')
})

#' Return the unigrams of a language model
#'
#' @author Adam Acosta
#' @param object An ngram.model object
#' @return unigrams The unigrams of the model
setMethod('unigrams', signature(object='ngram.model'), function(object) {
     object@unigrams
})

#' Return the bigrams of a language model
#'
#' @author Adam Acosta
#' @param object An ngram.model object
#' @return bigrams The bigrams of the model
#' @exportMethod
setMethod('bigrams', signature(object='ngram.model'), function(object) {
     object@bigrams
})

#' Return the trigrams of a language model
#'
#' @author Adam Acosta
#' @param object An ngram.model object
#' @return trigrams The trigrams of the model
#' @exportMethod
setMethod('trigrams', signature(object='ngram.model'), function(object) {
     object@trigrams
})

setGeneric('contains', function(object, key) {
     standardGeneric('contains')
})

#' Tests whether or not a language model contains an ngram
#'
#' @author Adam Acosta
#' @param object An ngram.model object
#' @param key A character string
#' @return boolean Whether or not the string is in the language model
#' @exportMethod
setMethod('contains', signature(object='ngram.model', key='character'),
          function(object, key) {
               return(has.key(key, unigrams(object)) |
                      has.key(key, bigrams(object)) |
                      has.key(key, trigrams(object)))
          }
)
