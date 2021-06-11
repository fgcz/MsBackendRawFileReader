#' @include hidden_aliases.R
NULL

.valid_ms_backend_data_storage <- function(x) {
  if (anyNA(x))
    return("'NA' values in dataStorage are not allowed.")
  NULL
}

.valid_ms_backend_files_exist <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) && !all(file.exists(x)))
    return(paste0("File(s) ", paste(x[!file.exists(x)], collapse = ", "),
                  " not found"))
  NULL
}

.MsBackendRawFileReader_header <- function(x = character()) {
  if (length(x) != 1)
    stop("'x' should have length 1")
  requireNamespace("rawrr", quietly = TRUE)
 
  hdr <- rawrr::readIndex(x)
  
  colnames(hdr)[colnames(hdr) == "scan"] <- "scanIndex"
  #colnames(hdr)[colnames(hdr) == "precursorScanNum"] <- "precScanNum"
  colnames(hdr)[colnames(hdr) == "precursorMass"] <- "precursorMz"
  colnames(hdr)[colnames(hdr) == "rtinseconds"] <- "rtime"
  colnames(hdr)[colnames(hdr) == "MSOrder"] <- "msLevel"
  #colnames(hdr)[colnames(hdr) == "isolationWindowTargetMZ"] <- "isolationWindowTargetMz"
  #hdr$isolationWindowLowerMz <- hdr$isolationWindowTargetMz -
  #  hdr$isolationWindowLowerOffset
  #hdr$isolationWindowUpperMz <- hdr$isolationWindowTargetMz +
  #  hdr$isolationWindowUpperOffset
  #hdr$isolationWindowUpperOffset <- NULL
  #hdr$isolationWindowLowerOffset <- NULL
  ## Remove core spectra variables that contain only `NA`
  S4Vectors::DataFrame(hdr[, !(MsCoreUtils::vapply1l(hdr, function(z) all(is.na(z))) &
                                 colnames(hdr) %in%
                                 names(Spectra:::.SPECTRA_DATA_COLUMNS))
  ])
}


#' @importFrom MsCoreUtils i2index
.subset_backend_MsBackendRawFileReader <- function(x, i) {
  if (missing(i))
    return(x)
  idx <- i
  i <- MsCoreUtils::i2index(i, length(x), rownames(x@spectraData))
  
  slot(x, "spectraData", check = FALSE) <- extractROWS(x@spectraData, i)
  
  # check if item is complete otherwise retrieval of data through using 
  # rawrr::readSpectrum(i)
  message(sprintf("supposed to fetch index %d from %d", i, idx))
  x
}

#' @rdname MsBackend
#' @exportClass MsBackendRawFileReader
#' @export MsBackendRawFileReader
MsBackendRawFileReader <- function() {
  if (!requireNamespace("rawrr", quietly = TRUE))
    stop("The use of 'MsBackendRawFileReader' requires package 'rawrr'. Please ",
         "install with 'BiocInstaller::install(\"rawrr\")'")
  new("MsBackendRawFileReader")
}

.RawFileReader_read_peaks <- function(x = character(), scanIndex = integer(),
                           modCount = 0L) {
  if (length(x) != 1)
    stop("'x' should have length 1")
  if (!length(scanIndex))
    return(list(matrix(ncol = 2, nrow = 0,
                       dimnames = list(character(), c("mz", "intensity")))))
  requireNamespace("rawrr", quietly = TRUE)
  message('.RawFileReader_read_peaks ...')
  message(sprintf("scanIndex: %s", paste(scanIndex, collapse = ", ")))
  lapply(rawrr::readSpectrum(x, scanIndex), function(p){
    m <- as.matrix(cbind(p$mZ, p$intensity))
    colnames(m) <- c("mz", "intensity")
    m})
}
