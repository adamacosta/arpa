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
read.arpa <- function(input="", header=TRUE, unk.string='<unk>',
                      start.string='<s>', stop.string='</s>', verbose=FALSE,
                      nrow=-1L, skip=0L, ugrams=-1L, bgrams=-1L, tgrams=-1L) {
     if (!file.exists(input)) stop("file does not exist")
     bad_format <- paste0("File not formatted properly. See ",
                          "http://www.speech.sri.com/projects/",
                          "srilm/manpages/ngram-format.5.html")
     debug.info(paste0("loading ", input))
     if (header) {
          tmp <- readLines(con, (6 + skip))[(3 + skip):(5 + skip)]
          # Sorry, this is ugly, but it reads the 3rd, 4th, and 5th line of the
          # file and parses the number of unigrams, bigrams, and trigrams
          head <- unlist(lapply(unlist(lapply(head, function(x) {
                                                    strsplit(x, '=', fixed=TRUE)
                                                    })), as.integer))
          head <- na.omit(head)[1:3]
          if (class(head) != "integer") stop(bad_format)
     }
     # Should be the 5-line header, 6 lines for the ngram delimiters, and
     # the number of total ngrams, plus any lines skipped
     if (nrow == -1L) {
          nrow <- skip + 5 + 6 + sum(head)
          ugrams <- head[1]
          bgrams <- head[2]
          tgrams <- head[3]
     }
     debug.info("found", ugrams, bgrams, tgrams)
     debug.info("      unigrams bigrams trigrams")
     uskip <- skip + 8
     umax <- skip + 7 + ugrams
     bskip <- skip + 9 + ugrams
     bmax <- skip + 9 + ugrams + bgrams
     tskip <- skip + 11 + ugrams + bgrams
     tmax <- skip + 11 + ugrams + bgrams + tgrams
     # Use low-level C i/o calls to parse the file
     dyn.load('readarpa.so')
     u <- .C('readarpa', as.character(input), as.integer(uskip),
             as.integer(umax), up=numeric(ugrams), unis=character(ugrams))
     b <- .C('readarpa', as.character(input), as.integer(bskip),
             as.integer(bmax), bp=numeric(bgrams), bis=character(bgrams))
     t <- .C('readarpa', as.character(input), as.integer(tskip),
             as.integer(tmax), tp=numeric(tgrams), tris=character(tgrams))
     dyn.unload('readarpa.so')
     # Construct and return the ngram.model
     ans <- list(unigrams=hash(keys=u[['unis']], values=u[['up']]),
                 bigrams=hash(keys=b[['bis']], values=b[['bp']]),
                 trigrams=hash(keys=t[['tris']], values=t[['tp']]))
     return(ans)
}
