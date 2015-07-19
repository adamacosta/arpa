#' @name read.arpa
#' @author Adam Acosta
#' @description Reads an ARPA file and returns the language model. See
#' http://www.speech.sri.com/projects/srilm/manpages/ngram-format.5.html
#' for a description of the ARPA file format.
#' @param input A filename
#' @param header boolean indicating whether or not the file has a header.
#' Default is TRUE
#' @param unk.string A string indicating unknown tokens in the model.
#' Default is <unk> (lowercase)
#' @param start.string A string indicating the start of sentence token.
#' Default is <s> (lowercase)
#' @param stop.string A string indicating the end of sentence token.
#' Default is </s> (lowercase)
#' @param verbose A boolean indicating whether you want the function to
#' print information to the console as it is parsing the file. Default is FALSE.
#' @param nrow The number of rows in the file. This is optional, as the format
#' itself dictates that the file header must give the number of each ngram,
#' from which the number of lines in the file can be inferred. If this is passed
#' as a parameter, pass the number of unigrams, bigrams, and trigrams as well,
#' and it will speed up the parsing process.
#' @param skip The number of rows, if any, to skip in the file. Default is 0.
#' @params ugrams, grams, tgrams integer values indicating the number of
#' unigrams, bigrams, and trigrams. Should be in the header but can be passed
#' to the function directly.
#' @return An ngram.model object, stored internally as a list of three hash
#' tables mapping each ngram to its log probability.
#' @export
#' @importFrom hash hash
#' @importFrom stringi stri_split_fixed
read.arpa <- function(input="", header=TRUE, verbose=FALSE, nrow=-1L, skip=0L,
                      ugrams=-1L, bgrams=-1L, tgrams=-1L) {
     if (!file.exists(input)) stop("file does not exist")
     debug.info(paste0("loading ", input))
     con <- file(input)
     if (header) {
          # Read lines 3 through 5 of form ngram N=Int, parsing each 'Int'
          head <- unname(sapply(readLines(con, (skip + 5))[(skip+3):(skip+5)],
                                function(x) {as.integer(str_to_vec(x, '=')[2])}))
          if (class(head) != "integer") {
               bad_format <- paste0("Header not formatted properly. See ",
                                    "http://www.speech.sri.com/projects/",
                                    "srilm/manpages/ngram-format.5.html")
               stop(bad_format)
          }
     }
     # Should be the 5-line header, 6 lines for the ngram delimiters, and
     # the number of total ngrams, plus any lines skipped
     if (nrow == -1L) {
          if (!header) stop("must provide either nrow or have a header")
          nrow <- skip + 5 + 6 + sum(head)
          ugrams <- head[1]
          bgrams <- head[2]
          tgrams <- head[3]
     }
     # TODO: Don't close connection when helper is moved
     close(con)
     debug.info("found", ugrams, bgrams, tgrams)
     debug.info("      unigrams bigrams trigrams")
     uskip <- skip + 8
     umax <- skip + 7 + ugrams
     bskip <- skip + 9 + ugrams
     bmax <- skip + 9 + ugrams + bgrams
     tskip <- skip + 11 + ugrams + bgrams
     tmax <- skip + 11 + ugrams + bgrams + tgrams
     # TODO: Use R to test small files while working on C code for faster
     # more memory-efficient parsing
     res <- parseFileR(input, verbose, uskip, umax, bskip, bmax, tskip, tmax)
     #res <- parseFileC(input, verbose, uskip, umax, bskip, bmax, tskip, tmax)
     return(res)
}
