context('test the predict.next method')

u <- data.table(logp=c(log(0.2), log(0.8)), w1=c('a', 'unigram'))
b <- data.table(logp=c(log(0.3), log(0.7)), w1=c('a', 'another'),
                w2=c('bigram', 'bigram'))
t <- data.table(logp=c(log(0.5), log(0.5)), w1=c('i', 'so'),
                w2=c('am', 'am'), w3=c('trigram', 'i'))
nm <- new('ngram.model',
          unigrams=u,
          bigrams=b,
          trigrams=t)

test_that('predict.next correct predicts from one trailing word', {
     expect_equal(predict.next(nm, 'a'), 'bigram')
})

test_that('predict.next correctly predicts from two trailing words', {
     skip('NOT IMPLEMENTED')
     expect_equal(predict.next(nm, 'i am'), 'trigram')
})
