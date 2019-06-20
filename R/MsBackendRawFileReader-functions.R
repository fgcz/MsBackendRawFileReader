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
#' @noRd
.MsBackendRawFileReader_header <- function(x) {
   
    stopifnot(x$check())
    requireNamespace("MsBackendRawFileReader", quietly = TRUE)
    
    first <- x$getFirstScanNumber()
    last <- x$getLastScanNumber()
    
    S4Vectors::DataFrame(
      scanIndex = first:last,
      msLevel = vapply(first:last, FUN=function(z){x$GetMsLevel(z)}, FUN.VALUE = as.integer(1)),
      precursorMz = vapply(first:last, FUN=function(z){x$GetPepmass(z)}, FUN.VALUE = as.double(1.0)),
      precursorCharge = as.integer(vapply(first:last, FUN=function(z){x$GetCharge(z)}, FUN.VALUE = as.character(1.0))),
      rtime =   vapply(first:last, FUN=function(z){x$GetRTinSeconds(z)}, FUN.VALUE = as.double(1.0))
    )
}

#' Read mz vlaues of each peaks from a single raw file.
#'
#' @param x 
#' @param scanIndex (required) indices of spectra from which the data should be
#'     retrieved.
#' @return a numeric vector
#'
#' @examples
#' 
#' @noRd
.MsBackendRawFileReader_mz <- function(x, scanIndex = integer()) {
  stopifnot(x$check())
  requireNamespace("MsBackendRawFileReader", quietly = TRUE)
  # TODO(cp) check scanIds
  
  lapply(scanIndex, function(z) {
    mz <- x$GetSpectrumMz(z, "")
    mz
  })
}

