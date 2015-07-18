# Internal helpers - export none of these

debug.info <- function(msg) {
     if (verbose) cat(msg)
}

debug.warn <- function(msg) {
     if (verbose) message(msg)
}
