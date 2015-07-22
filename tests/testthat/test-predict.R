context('test the predict.ngram method')

u <- data.table(logp=c(log(0.2), log(0.8)), w1=c('a', 'unigram'))
b <- data.table(logp=c(log(0.3), log(0.2), log(0.5)), w1=c('<s>', 'a', 'another'),
                w2=c('i', 'bigram', 'bigram'))
t <- data.table(logp=c(log(0.5), log(0.4), log(0.1)), w1=c('<s>', 'i', 'so'),
                w2=c('the', 'am', 'am'), w3=c('cat', 'trigram', 'am'))
nm <- new('ngram.model',
          unigrams=u,
          bigrams=b,
          trigrams=t)

test_that('predict.ngram correct predicts from one trailing word', {
     skip('broke on purpose')
     expect_equal(predict(nm, 'a'), 'bigram')
})

test_that('predict.ngram correctly predicts from two trailing words', {
     skip('broke on purpose')
     expect_equal(predict(nm, 'i am'), 'trigram')
})

test_that('predict.ngram correctly predicts from three trailing words', {
     skip('NOT IMPLEMENTED')
})
