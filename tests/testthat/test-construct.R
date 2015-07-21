context('test class constructor')

u <- data.table(logp=c(log(0.2), log(0.8)), w1=c('a', 'unigram'))
b <- data.table(logp=c(log(0.3), log(0.7)), w1=c('a', 'another'),
                w2=c('bigram', 'bigram'))
t <- data.table(logp=c(log(0.5), log(0.5)), w1=c('i', 'so'),
                w2=c('am', 'am'), w3=c('trigram', 'i'))

test_that('ngram.model can be constructed from data.tables', {
     nm <- new('ngram.model',
               unigrams=u,
               bigrams=b,
               trigrams=t)
     expect_true(isS4(nm))
     expect_is(nm, 'ngram.model')
})
