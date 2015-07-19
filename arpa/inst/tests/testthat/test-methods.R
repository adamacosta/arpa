context('test methods')

ukeys <- c('a', 'unigram')
uvals <- c(log(0.2), log(0.8))
bkeys <- c('a bigram', 'another bigram')
bvals <- c(log(0.3), log(0.7))
tkeys <- c('i am trigram', 'so am i')
tvals <- c(log(0.5), log(0.5))

u <- hash(keys=ukeys, values=uvals)
b <- hash(keys=bkeys, values=bvals)
t <- hash(keys=tkeys, values=tvals)
nm <- new('ngram.model',
          unigrams=u,
          bigrams=b,
          trigrams=t)

test_that('ngram.model unigrams returned from unigrams() call', {
     expect_is(unigrams(nm), 'hash')
     expect_equal(unigrams(nm), nm@unigrams)
})

test_that('ngram.model bigrams returned from bigrams() call', {
     expect_is(bigrams(nm), 'hash')
     expect_equal(bigrams(nm), nm@bigrams)
})

test_that('ngram.model trigrams returned from trigrams() call', {
     expect_is(trigrams(nm), 'hash')
     expect_equal(trigrams(nm), nm@trigrams)
})

test_that('contains correctly finds an ngram in the model', {
     expect_true(contains(nm, 'i am trigram'))
})

test_that('contains fails to find an ngram not in the model', {
     expect_false(contains(nm, 'i am not'))
})

test_that('prob return correct log probabilities', {
     expect_equal(prob(nm, 'a bigram'), log(0.3))
     expect_equal(prob(nm, 'so am i'), log(0.5))
     expect_equal(prob(nm, 'a'), log(0.2))
})
