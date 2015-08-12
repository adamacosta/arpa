context('test tokenizer')

in_text1 <- 'bla bla bla'
in_text2 <- 'adfkl777(((,,jh,'
in_text3 <- 'love a cat'
in_text4 <- '              aa%nope ha ha'

test_that('input is correctly tokenized', {
     expect_equal(tokenize(in_text1), c('bla', 'bla', 'bla'))
     expect_equal(tokenize(in_text2), c('adfkl', 'jh'))
     expect_equal(tokenize(in_text3), c('love', 'a', 'cat'))
     expect_equal(tokenize(in_text4), c('aa', 'nope', 'ha', 'ha'))
})
