#' Word prediction engine
#'
#' @author Adam Acosta
#' @description Receives input text and predicts what the next word will be.
#' @param object ngram.model
#' @param text character
#' @return The most likely next word in the sentence
#' @export
setGeneric('predict.next', function(object, text) {
     standardGeneric('predict.next')
})

#' @inheritParams predict.next
#' @export
setMethod('predict.next', signature(object='ngram.model', text='character'),
          function(object, text) {
               tokens <- tokenize(text)
               return(select.func(object, args=tokens,
                                  funcs=list(predict.u, predict.b, predict.t)))
          }
)

predict.u <- function(object, text) {
     # TODO: Deal with oov words - use <unk> token
     b <- bigrams(object)
     return(b[b$w1==text][order(logp, decreasing=TRUE)][1,]$w2)
}

predict.b <- function(object, text) {
     return('NOT IMPLEMENTED')
}

predict.t <- function(object, text) {
     return('NOT IMPLEMENTED')
}
