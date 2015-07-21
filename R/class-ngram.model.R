# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Class: arpa.lm
# Contains: a list of three hash tables mapping unigrams, bigrams, and trigrams
# in a language model to their log probability
# Options: for memory efficiency but less speed, some other internal representation


# TODO:
# The hashes themselves have a 'hash_table$key' syntax, so this will require
# the somewhat odd-looking model@unigrams$'unigram' syntax to access values.
# The quote marks are needed because the hash can contain spaces and '<' and '>'
# Document this somewhere with some vignettes.

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
