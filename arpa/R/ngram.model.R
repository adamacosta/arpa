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

check_ngram <- function(object) {
     errors <- character()
     check_keys <- function(keys) {
          if (!is.character(keys)) {
               msg <- paste('Keys are ', class(keys), '. Must be character.')
               errors <- c(errors, msg)
          }
     }
     check_vals <- function(vals) {
          if (!is.numeric(vals)) {
               msg <- paste('Values are ', class(vals), '. Must be numeric.')
               errors <- c(errors, msg)
          }
     }
     check_keys(keys(object@unigrams))
     check_vals(values(object@unigrams))
     check_keys(keys(object@bigrams))
     check_vals(values(object@bigrams))
     check_keys(keys(object@trigrams))
     check_vals(values(object@trigrams))
     if (length(errors) == 0) TRUE else errors
}

#' An S4 class to represent an ngram language model
#'
#' @author Adam Acosta
#' @description Provides an efficient, fast way to map ngrams in a language
#' model to their log probability, allowing for easy next-word prediction.
#' @slot unigrams A hash table mapping unigrams to their log probability
#' @slot bigrams A hash table mapping bigrams to their log probability
#' @slot trigrams A hash table mapping trigrams to their log probability
#' @exportClass
ngram.model <- setClass("ngram.model",
         representation(unigrams="hash",
                        bigrams="hash",
                        trigrams="hash"),
         prototype(unigrams=hash(keys=c('<s>'), values=c(-99)),
                   bigrams=hash(keys=c('<s> </s>'), values=c(-99)),
                   trigrams=hash(keys=c('<s> <unk> </s>'), values=c(-99))),
         validity=check_ngram
)
