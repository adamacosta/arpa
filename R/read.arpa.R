#' Read ARPA file
#'
#' @author Adam Acosta
#' @description Reads an ARPA file and returns the language model. See
#' http://www.speech.sri.com/projects/srilm/manpages/ngram-format.5.html
#' for a description of the ARPA file format.
#'
#' @param input A filename
#' @param header boolean indicating whether or not the file has a header.
#' Default is TRUE
#' @param verbose A boolean indicating whether you want the function to
#' print information to the console as it is parsing the file. Default is FALSE.
#' @param nrow The number of rows in the file. This is optional, as the format
#' itself dictates that the file header must give the number of each ngram,
#' from which the number of lines in the file can be inferred. If this is passed
#' as a parameter, pass the number of unigrams, bigrams, and trigrams as well,
#' and it will speed up the parsing process.
#' @param skip The number of rows, if any, to skip in the file. Default is 0.
#' @param ugrams See below
#' @param bgrams See below
#' @param tgrams integer values indicating the number of unigrams, bigrams,
#' and trigrams. Should be in the header but can be passed to the function
#' directly to avoid header parsing overhead.
#' @return An ngram.model object, stored internally as a list of three hash
#' tables mapping each ngram to its log probability.
#'
#' @export
#' @importFrom stringi stri_split_fixed
#' @importFrom data.table fread
#' @importFrom data.table setnames
read.arpa <- function(inFile) {

    parseHeader <- function(inFile) {
        parseNum <- function(line) {
            as.integer(unlist(stri_split_fixed(line, '='))[-1])
        }
        con <- file(inFile)
        head <- readLines(con, 7)[3:6]
        close(con)
        vapply(head, parseNum, 0L)
    }

    ngram <- parseHeader(inFile)
    ustart <- 9
    uend <- ustart + ngram[1] - 1
    bstart <- uend + 2
    bend <- bstart + ngram[2] - 1
    tstart <- bend + 2
    tend <- tstart + ngram[3] - 1
    qstart <- tend + 2
    qend <- qstart + ngram[4] - 1

    system(paste('sed', '-n',
                 paste0(as.character(bstart + 1), ',', as.character(bend), 'p'),
                 '<', inFile, '|',
                 'tr', "\'\t\'", "\' \'", '|',
                 'sed', 's/-0\\.[0-9]*$//g', '>', 'bi.tmp'))

    bi_dt <- fread('bi.tmp', header = FALSE)
    setnames(bi_dt, c('logp', 'w1', 'w2'))
    system('rm -f bi.tmp')

    system(paste('sed', '-n',
                 paste0(as.character(tstart + 1), ',', as.character(tend), 'p'),
                 '<', inFile, '|',
                 'tr', "\'\t\'", "\' \'", '|',
                 'sed', 's/-0\\.[0-9]*$//g', '>', 'tri.tmp'))

    tri_dt <- fread('tri.tmp', header = FALSE)
    setnames(tri_dt, c('logp', 'w1', 'w2', 'w3'))
    system('rm -f tri.tmp')

    system(paste('sed', '-n',
                 paste0(as.character(qstart + 1), ',', as.character(qend), 'p'),
                 '<', inFile, '|',
                 'tr', "\'\t\'", "\' \'", '|',
                 'sed', 's/-0\\.[0-9]*$//g', '>', 'tet.tmp'))

    tet_dt <- fread('tet.tmp', header = FALSE)
    setnames(tet_dt, c('logp', 'w1', 'w2', 'w3', 'w4'))
    system('rm -f tet.tmp')

    new('ngram.model',
        unigrams = data.table(logp = numeric(0), w1 = character(0)),
        bigrams = bi_dt,
        trigrams = tri_dt,
        tetragrams = tet_dt)
}
