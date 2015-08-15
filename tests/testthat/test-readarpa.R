context('test read.arpa')

good <- system.file('extdata', 'good.arpa', package='arpa')
bad <- system.file('extdata', 'bad.arpa', package='arpa')

test_that('read.arpa correctly parses sample file with just filename', {
    skip('n')
    nm <- read.arpa(good)
    expect_is(nm, 'ngram.model')
    expect_true(contains(nm, 'i love you'))
    expect_true(contains(nm, 'on'))
    expect_false(contains(nm, 'i do not'))
})

test_that('read.arpa returns an error when header is malformed', {
    skip('n')
    expect_error(read.arpa(bad))
})

test_that('read.arpa correctly parses a sample file with parameters provided', {
    skip('n')
    nm <- read.arpa(good, header=FALSE, nrow=42, ugrams=8, bgrams=12, tgrams=18)
    expect_is(nm, 'ngram.model')
    expect_true(contains(nm, 'love a cat'))
    expect_false(contains(nm, 'sorry friend'))
})
