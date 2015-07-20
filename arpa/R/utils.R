# Internal helpers - export none of these

# strtovec
str_to_vec <- function(string, char) {
     # simplifies the list semantics
     return(unlist(stri_split_fixed(string, char)))
}

#' Helper called by read.arpa
#' @importFrom data.table data.table
parseFileR <- function(input, verbose, uskip, umax, bskip, bmax, tskip, tmax) {
     con <- file(input)
     lines <- readLines(con)
     close(con)
     buildTable <- function(nwords, first, last, offset) {
          nrow <- last - first + 1
          if (nwords == 1) {
               dt <- data.table(logp=numeric(nrow), w1=character(nrow))
          } else if (nwords == 2) {
               dt <- data.table(logp=numeric(nrow), w1=character(nrow),
                                w2=character(nrow))
          } else {
               dt <- data.table(logp=numeric(nrow), w1=character(nrow),
                                w2=character(nrow), w3=character(nrow))
          }

          parse_line <- function(line) {
               strings <- str_to_vec(line, '\t')[1:2]
               logp <- as.numeric(strings[1])
               ngram <- tokenize(strings[2])
               return(list(logp, ngram[1], ngram[2], ngram[3]))
          }

          set_row <- function(line_no) {
               set(dt, line_no - offset, names(dt), parse_line(lines[line_no]))
          }

          lapply(first:last, set_row)

          return(dt)
     }
     # TODO: R's copy semantics make this stupidly inefficient, using 2 gigs
     # to a parse a 300 MB file. Move to C.
     res <- new('ngram.model',
                unigrams=buildTable(1, uskip + 1, umax, uskip),
                bigrams=buildTable(2, bskip + 1, bmax, bskip),
                trigrams=buildTable(3, tskip + 1, tmax, tskip))
     return(res)
}

# Call the C interface to implement faster, more memory-efficient read.arpa
parseFileC <- function(input, verbose, uskip, umax, bskip, bmax, tskip, tmax) {
     # TODO: Build and call C library to do the dirty work
     dyn.load('readarpa.so')
     if (!is.loaded('readarpa')) stop("could not load C library")
     ans <- .Call('readarpa', as.character(input), as.logical(verbose),
                  as.integer(uskip), as.integer(umax), as.integer(bskip),
                  as.integer(bmax), as.integer(tskip), as.integer(tmax))
     dyn.unload('readarpa.so')
     # TODO: Does this need a constructor or can I construct an S4 class
     # from C?
     return(ans)
}
