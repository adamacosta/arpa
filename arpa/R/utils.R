# Internal helpers - export none of these

# strtovec
str_to_vec <- function(string, char) {
     # simplifies the list semantics
     return(unlist(stri_split_fixed(string, char)))
}

# Helper called by read.arpa
parseFileR <- function(input, verbose, uskip, umax, bskip, bmax, tskip, tmax) {
     con <- file(input)
     lines <- readLines(con)
     parseLine <- function(line) return(str_to_vec(line, '\t')[1:2])
     buildHash <- function(lines) {
          # NOTE: The hash package uses pass-by-reference semantics, so
          # so this data.table continues to contain valid pointers to the
          # same objects stored in the hash. Make sure it is not exposed
          # or touched anywhere else
          tmp_dt <- t(as.data.frame(lapply(lines, parseLine)))
          return(hash(keys=as.character(tmp_dt[,2]),
                      values=as.numeric(tmp_dt[,1])))
     }
     # TODO: R's copy semantics make this stupidly inefficient, using 2 gigs
     # to a parse a 300 MB file. Move to C.
     res <- new('ngram.model',
                unigrams=buildHash(lines[(uskip+1):umax]),
                bigrams=buildHash(lines[(bskip+1):bmax]),
                trigrams=buildHash(lines[(tskip+1):tmax]))
     close(con)
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
