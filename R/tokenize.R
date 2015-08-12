#' Return cleaned version of input text
#'
#' @author Adam Acosta
#'
#' @param text Unformatted character vector
#' @return tokenizable character vectors
#'
#' @importFrom stringi stri_subset_charclass
#' @importFrom stringi stri_replace_all_regex
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom stringi stri_trim_both
clean_text <- function(text) {
     # should remove strange characters - TEST, TEST, TEST!!!
     new_text <- iconv(text, from = 'UTF-8', to = 'latin1', sub = ' ')
     new_text <- tolower(new_text)
     # remove numbers
     new_text <- stri_subset_charclass(new_text,
                                       pattern="[^0-9]",
                                       omit_na=TRUE)
     # remove everything that isn't a letter or apostrophe
     new_text <- stri_subset_charclass(new_text,
                                       pattern="[a-z']")
     # squeeze whitespace
     new_text <- stri_replace_all_regex(new_text,
                                        pattern=" +",
                                        replacement=" ")
     # remove stray apostrophes - can used fixed because whitespace was squeezed
     new_text <- stri_replace_all_fixed(new_text,
                                        pattern=" ' ",
                                        replacement=" ")
     new_text <- stri_trim_both(new_text)
     # TODO: Insert <UNK> etc model tokens?
     # TODO: Profanity filter?
     new_text
}

#' Return vector of tokens from input text
#'
#' @author Adam Acosta
#'
#' @param text Unformatted character vector
#' @return words A list of tokens parsed from the input
#'
#' @importFrom stringi stri_split_charclass
tokenize <- function(text) {
     # TODO: Check for NAs?
     words <- unlist(stri_split_charclass(clean_text(text),
                                          pattern="[^a-z']"))
     words[words != '']
}
