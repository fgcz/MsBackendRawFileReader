#' @include hidden_aliases.R
NULL


#' @export MsBackendRawFileReader
#' @importFrom methods new
#' @aliases MsBackendRawFileReader
MsBackendRawFileReader <- function() {
   #if (!requireNamespace("rawDiag", quietly = TRUE))
   #     stop("The use of 'MsBackendRawFileReader' requires package 'rawDiag'. Please ",
   #          "install.")
    new("MsBackendRawFileReader")
}

#' Read the header for each spectrum from the MS file `x`
#'
#' @author Christian Panse 2019-06-15 
#' adapted from the MsBackendMzR-function.R file by Johannes Rainer
#'
#' @return `DataFrame` with the header.
#' @importFrom S4Vectors DataFrame
#' @noRd
.MsBackendRawFileReader_header <- function(x, extra=TRUE) {
    stopifnot(class(x) == "rDotNet")
    stopifnot(x$check())
    requireNamespace("MsBackendRawFileReader", quietly = TRUE)
    
    first <- x$GetFirstScanNumber()
    last <- x$GetLastScanNumber()
    
    if (extra)
        return(.MsBackendRawFileReader_extra(x))
    else
        return(DataFrame(scanIndex = first:last))
}

#' Read mz values of each peaks from a single raw file.
#'
#' @param x 
#' @param scanIndex (required) indices of spectra from which the data should be
#'     retrieved.
#' @return a numeric vector
#'
#' @examples
#' # Debug
#' (rawfile <- file.path(path.package(package = 'MsBackendRawFileReader'),
#'   'extdata', 'sample.raw'))
#'   
#' x <- .cnew ("Rawfile", rawfile)
#' MsBackendRawFileReader:::.MsBackendRawFileReader_mz(x, 1:2)
#' @author Christian Panse, June 2019
#' @noRd
.MsBackendRawFileReader_mz <- function(x, scanIndex = integer()) {
  stopifnot(class(x) == "rDotNet")
  stopifnot(x$check())
  requireNamespace("MsBackendRawFileReader", quietly = TRUE)
  # TODO(cp) check scanIds
  
  lapply(scanIndex, function(z) {
    x$GetSpectrumMasses(z)
  })
}

#' Read intensity values of each peaks from a single raw file.
#'
#' @param x 
#' @param scanIndex (required) indices of spectra from which the data should be
#'     retrieved.
#' @return a numeric vector
#' @examples
#' # Debug
#' (rawfile <- file.path(path.package(package = 'MsBackendRawFileReader'), 'extdata', 'sample.raw'))
#'   
#' x <- .cnew ("Rawfile", rawfile)
#' MsBackendRawFileReader:::.MsBackendRawFileReader_intensity(x, 1:2)
#' @author Christian Panse, June 2019
#' @noRd
.MsBackendRawFileReader_intensity <- function(x, scanIndex = integer()) {
  stopifnot(class(x) == "rDotNet")
  stopifnot(x$check())
  requireNamespace("MsBackendRawFileReader", quietly = TRUE)
  # TODO(cp) check scanIds
  
  lapply(scanIndex, function(z) {
    x$GetSpectrumIntensities(z)
  })
}

# ==== GetExtraHeaderDataFrame ====
.MsBackendRawFileReader_extra <- function(x){
    
    from <- x$GetFirstScanNumber() 
    to <- x$GetLastScanNumber() 
    
    df_string <- DataFrame(do.call('rbind', lapply(seq(from, to), function(i){
        x$GetTrailerExtraHeaderInformationValueAsString(i)
    })))
    
    df <- DataFrame(do.call('rbind', lapply(seq(from, to), function(i){
        x$GetTrailerExtraHeaderInformationValue(i)
    })))
    
    colnames(df) <- x$GetTrailerExtraHeaderInformationLabel()
    
    idx <- which(sapply(df@listData, function(z){z[1]==-123456}))
    
    for (i in idx){
        df[, i] <- df_string[, i]
    }
    
    row.names(df) <- seq(from, to) 
    df$scanIndex = seq(from, to)
    df
}

