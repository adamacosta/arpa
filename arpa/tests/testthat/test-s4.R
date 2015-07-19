context('S4 compatibility')

ukeys <- c('a', 'unigram')
uvals <- c(log(0.2), log(0.8))
bkeys <- c('a bigram', 'another bigram')
bvals <- c(log(0.3), log(0.7))
tkeys <- c('i am trigram', 'so am i')
tvals <- c(log(0.5), log(0.5))

u <- hash(keys=ukeys, values=uvals)
b <- hash(keys=bkeys, values=bvals)
t <- hash(keys=tkeys, values=tvals)

test_that('ngram.model can be constructed from hash tables', {
     nm <- new('ngram.model',
               unigrams=u,
               bigrams=b,
               trigrams=t)
     expect_true(isS4(nm))
     expect_is(nm, 'ngram.model')
     expect_equal(nm@unigrams$'a', log(0.2))
})

test_that('ngram.model can be constructed from <key:value> pairs', {
     nm <- new('ngram.model',
               unigrams=hash(keys=ukeys, values=uvals),
               bigrams=hash(keys=bkeys, values=bvals),
               trigrams=hash(keys=tkeys, values=tvals))
     expect_true(isS4(nm))
     expect_is(nm, 'ngram.model')
     expect_equal(nm@unigrams$'a', log(0.2))
})
