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
.MsBackendRawFileReader_header <- function(x) {
    stopifnot(class(x) == "rDotNet")
    stopifnot(x$check())
    requireNamespace("MsBackendRawFileReader", quietly = TRUE)
    
    first <- x$getFirstScanNumber()
    last <- x$getLastScanNumber()
    
    DataFrame(
      scanIndex = first:last,
      acquisitionNum = first:last,
      msLevel = vapply(first:last, FUN=function(z){x$GetMsLevel(z)}, FUN.VALUE = as.integer(1)),
      precursorMz = vapply(first:last, FUN=function(z){x$GetPrecursorMz(z)}, FUN.VALUE = as.double(1.0)),
      precursorCharge = as.integer(vapply(first:last, FUN=function(z){x$GetCharge(z)}, FUN.VALUE = as.character(1.0))),
      rtime =   vapply(first:last, FUN=function(z){x$GetRTinSeconds(z)}, FUN.VALUE = as.double(1.0)),
      centroided = vapply(first:last, FUN=function(z){x$IsCentroidScan(z)}, FUN.VALUE = FALSE), 
      polarity = vapply(first:last, FUN=function(z){x$GetPolarity(z)}, FUN.VALUE = as.integer(-1)),
      injectionTime = NA,
      #collisionEnergy = vapply(first:last, FUN=function(z){x$GetCollisionEnergy(z)}, FUN.VALUE = as.double(1.0)),
      #isolationWindowTargetMz = vapply(first:last, FUN=function(z){x$GetIsolationWidth(z)}, FUN.VALUE = as.double(1.0)),
      scanFilter = vapply(first:last, FUN=function(z){x$GetScanFilter(z)}, FUN.VALUE = as.character("")),
      basePeakMZ = vapply(first:last, FUN=function(z){x$GetBasepeakMass(z)}, FUN.VALUE = as.double(1.0)),
      basePeakIntensity = vapply(first:last, FUN=function(z){x$GetBasepeakIntensity(z)}, FUN.VALUE = as.double(1.0))
    )
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
    mz <- x$GetSpectrumMz(z, "")
    mz
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
    intensity <- x$GetSpectrumIntensities(z, "")
    intensity
  })
}
