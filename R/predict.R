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
                         # We will only consider the last three words for now
                         predict.t(object, tokens[(len - 2):len])
                    }
               }
          }
)

# This is completely determined by the model probability of the bigram
# in the case with only one preceding word.
predict.u <- function(object, txt) {
     # TODO: Deal with oov words - use <unk> token
     # TODO: Return three most likely words
     b <- bigrams(object)
     ans <- b[w1==txt][order(logp, decreasing=TRUE)][1:3,]$w2
     ifelse(is.na(ans), predict.u(object, '<s>'), sub('</s>', '.', ans))
}

# Likewise, the case with two preceding words is completely determined
# by the trigram probability
predict.b <- function(object, txt) {
     t <- trigrams(object)
     ans <- t[w1==txt[1] & w2==txt[2]][order(logp, decreasing=TRUE)][1:3,]$w3
     ifelse(is.na(ans), predict.u(object, txt[2]), sub('</s>', '.', ans))
}

# For now, only considering last words to allow efficient prediction by
# table lookup. Keeping this here to extend just in case.
predict.t <- function(object, txt) {
     t <- tetragrams(object)
     ans <- t[w1==txt[1]&w2==txt[2]&w3==txt[3]][order(logp, decreasing=T)][1:3,]$w4
     ifelse(is.na(ans), predict.b(object, txt[2:3]), sub('</s>', '.', ans))
}
