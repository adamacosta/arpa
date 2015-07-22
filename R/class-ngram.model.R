# TODO: Create a more efficient data structure
# Can be done as follows:
# key <- 'w1'
# val1 <- c('w2'=log(p1))
# h <- hash(keys=key, values=val1)
# to add a bigram starting with 'w1':
# h[['w1']] <- c(h[['w1']], 'w3'=log(p2))
# to access:
# return(h[['w1']][['w2']])

#' An S4 class to represent an ngram language model
#'
#' @author Adam Acosta
#' @description Provides an efficient, fast way to map ngrams in a language
#' model to their log probability, allowing for easy next-word prediction.
#'
#' @slot unigrams A data.table mapping unigrams to their log probability
#' @slot bigrams A data.table mapping bigrams to their log probability
#' @slot trigrams A data.table mapping trigrams to their log probability
#'
#' @export
#'
#' @importFrom methods setClass
#' @importFrom data.table data.table
ngram.model <- setClass("ngram.model",
         representation(unigrams="data.table",
                        bigrams="data.table",
                        trigrams="data.table"),
         prototype(unigrams=data.table(logp=numeric(0),
                                       w1=character(0)),
                   bigrams=data.table(logp=numeric(0),
                                      w1=character(0),
                                      w2=character(0)),
                   trigrams=data.table(logp=numeric(0),
                                       w1=character(0),
                                       w2=character(0),
                                       w3=character(0)))
)
