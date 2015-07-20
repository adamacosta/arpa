setGeneric('predict.next', function(object, text) {
     standardGeneric('predict.next')
})

#' Return the predicted next word in a sentence
#'
#' @author Adam Acosta
#' @param object ngram.model An ngram.model object
#' @param text character A string of input text
#' @return word character The predicted next word
setMethod('predict.next', signature(object='ngram.model', text='character'),
          function(object, text) {
               return(NULL)
          }
)
