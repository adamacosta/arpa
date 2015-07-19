context('test read.arpa')

filename <- '../../extdata/sample.arpa'

test_that('read.arpa correctly parses sample file with just filename', {
     nm <- read.arpa(filename)
     expect_is(nm, 'ngram.model')
     expect_true(contains(nm, 'i love you'))
     expect_false(contains(nm, 'i do not'))
})
