#' @include ngram.model.methods.R
NULL

#' Word prediction engine
#'
#' @author Adam Acosta
#' @description Receives input text and predicts what the next word will be.
#'
#' @param object ngram.model
#' @param text character
#' @return The most likely next word in the sentence
#'
#' @export
setGeneric('predict', function(object, text) {
     standardGeneric('predict')
})

#' @describeIn predict
#' Next-word prediction engine
#'
#' @author Adam Acosta
#'
#' @inheritParams predict
#' @export
setMethod('predict', signature(object='ngram.model', text='character'),
          function(object, text) {
               if (text == '') {
                    predict.u(object, '<s>')
               } else {
                    tokens <- tokenize(text)
                    len <- length(tokens)
                    if (len < 3) {
                         select.func(object, args=tokens,
                                     funcs=list(predict.u,
                                                predict.b,
                                                predict.t))
                    } else {
                         # We will only consider the last two words for now
                         predict.b(object, tokens[(len - 1):len])
                    }
               }
          }
)

# This is completely determined by the model probability of the bigram
# in the case with only one preceding word.
predict.u <- function(object, text) {
     # TODO: Deal with oov words - use <unk> token
     # TODO: Return three most likely words
     b <- bigrams(object)
     ans <- b[b$w1==text][order(logp, decreasing=TRUE)][1:3,]$w2
     if (NA %in% ans) {
          predict.u(object, '<s>')
     } else {
          sub('</s>', '.', ans)
     }
}

# Likewise, the case with two preceding words is completely determined
# by the trigram probability
predict.b <- function(object, text) {
     t <- trigrams(object)
     ans <- t[t$w1==text[1] & t$w2==text[2]][order(logp, decreasing=TRUE)][1:3,]$w3
     if (NA %in% ans) {
          predict.u(object, '<s>')
     } else {
          sub('</s>', '.', ans)
     }
}

# For now, only considering last words to allow efficient prediction by
# table lookup. Keeping this here to extend just in case.
predict.t <- function(object, text) {
     return('NOT IMPLEMENTED')
}
