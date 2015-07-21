#' @include class-ngram.model.R
NULL

#' Return the unigrams of a language model
#'
#' @author Adam Acosta
#'
#' @param object An ngram.model object
#' @return unigrams The unigrams of the model
#'
#' @importFrom methods setGeneric
#' @export
setGeneric('unigrams', function(object) {
     standardGeneric('unigrams')
})

#' Return the bigrams of a language model
#'
#' @author Adam Acosta
#'
#' @param object An ngram.model object
#' @return bigrams The bigrams of the model
#'
#' @importFrom methods setGeneric
#' @export
setGeneric('bigrams', function(object) {
     standardGeneric('bigrams')
})

#' Return the trigrams of a language model
#'
#' @author Adam Acosta
#'
#' @param object An ngram.model object
#' @return trigrams The trigrams of the model
#'
#' @importFrom methods setGeneric
#' @export
setGeneric('trigrams', function(object) {
     standardGeneric('trigrams')
})

#' @describeIn ngram.model
#' Return the unigrams of a ngram.model
#'
#' @author Adam Acosta
#'
#' @inheritParams unigrams
#'
#' @importFrom methods setMethod
#' @export
setMethod('unigrams', signature(object='ngram.model'), function(object) {
     object@unigrams
})

#' @describeIn ngram.model
#' Return the bigrams of a ngram.model
#'
#' @author Adam Acosta
#'
#' @inheritParams bigrams
#'
#' @importFrom methods setMethod
#' @export
setMethod('bigrams', signature(object='ngram.model'), function(object) {
     object@bigrams
})

#' @describeIn ngram.model
#' Return the trigram of a ngram.model
#'
#' @author Adam Acosta
#'
#' @inheritParams trigrams
#'
#' @importFrom methods setMethod
#' @export
setMethod('trigrams', signature(object='ngram.model'), function(object) {
     object@trigrams
})

#' Tests whether or not a language model contains an ngram
#'
#' @author Adam Acosta
#'
#' @param object An ngram.model object
#' @param key A character string
#' @return boolean Whether or not the string is in the language model
#'
#' @export
setGeneric('contains', function(object, key) {
     standardGeneric('contains')
})

#' @describeIn ngram.model
#' Test whether or not a ngram.model contain an ngram
#'
#' @author Adam Acosta
#'
#' @inheritParams contains
#'
#' @importFrom methods setMethod
#' @export
setMethod('contains', signature(object='ngram.model', key='character'),
          function(object, key) {
               contains.u <- function(object, tokens) {
                    u <- unigrams(object)
                    return(tokens[1] %in% u$w1)
               }
               contains.b <- function(object, tokens) {
                    b <- bigrams(object)
                    return(tokens[1] %in% b$w1 &&
                           tokens[2] %in% b$w2)
               }
               contains.t <- function(object, tokens) {
                    t <- trigrams(object)
                    return(tokens[1] %in% t$w1 &&
                           tokens[2] %in% t$w2 &&
                           tokens[3] %in% t$w3)
               }
               tokens <- tokenize(key)
               return(select.func(object, args=tokens,
                                  funcs=list(contains.u, contains.b, contains.t)))
          }
)

#' Return the subset of the language model contain the partial key
#'
#' @author Adam Acosta
#'
#' @param object An ngram.model object
#' @param key A character string
#' @return data.table The subset of the ngram.model containing key
#'
#' @importFrom methods setGeneric
setGeneric('subset', function(object, key) {
     standardGeneric('subset')
})

#' @describeIn ngram.model
#' Return the subset of an ngram.model containing a partial key
#'
#' @author Adam Acosta
#'
#' @inheritParams subset
#'
#' @importFrom methods setMethod
setMethod('subset', signature(object='ngram.model', key='character'),
          function(object, key) {
               # No check to ensure existence because this is not
               # exported and should be checked prior to call
               tokens <- tokenize(key)
               len <- length(tokens)
               if (len == 1) {
                    u <- unigrams(object)
                    return(u[u$w1==tokens[1]])
               }
               if (len == 2) {
                    b <- bigrams(object)
                    return(b[b$w1==tokens[1] & b$w2==tokens[2]])
               }
               t <- trigrams(object)
               return(t[t$w1==tokens[1] & t$w2==tokens[2] & t$w3==tokens[3]])
          }
)

#' Returns the log probability of an ngram in a language model
#'
#' @author Adam Acosta
#'
#' @param object An ngram.model object
#' @param key A character string
#' @return numeric The log probability of the provided ngram
#'
#' @importFrom methods setGeneric
#' @export
setGeneric('prob', function(object, key) {
     standardGeneric('prob')
})

#' @describeIn ngram.model
#' Return the log probability of an ngram in a ngram.model
#'
#' @author Adam Acosta
#'
#' @inheritParams  prob
#'
#' @importFrom methods setMethod
#' @export
setMethod('prob', signature(object='ngram.model', key='character'),
          function(object, key) {
               if (!contains(object, key)) stop('key not in ngram.model')
               return(subset(object, key)$logp)
          }
)
