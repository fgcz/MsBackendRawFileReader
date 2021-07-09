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

#' data types of spectraData columns
#'
#' @noRd
.SPECTRA_DATA_COLUMNS <- c(
  msLevel = "integer",
  rtime = "numeric",
  acquisitionNum = "integer",
  scanIndex = "integer",
  mz = "NumericList",
  intensity = "NumericList",
  dataStorage = "character",
  dataOrigin = "character",
  centroided = "logical",
  smoothed = "logical",
  polarity = "integer",
  precScanNum = "integer",
  precursorMz = "numeric",
  precursorIntensity = "numeric",
  precursorCharge = "integer",
  collisionEnergy = "numeric",
  isolationWindowLowerMz = "numeric",
  isolationWindowTargetMz = "numeric",
  isolationWindowUpperMz = "numeric"
)


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
                                 names(.SPECTRA_DATA_COLUMNS))
  ])
}


#' @importFrom S4Vectors extractROWS
#' @importFrom MsCoreUtils i2index
#' @importFrom methods slot<-
.subset_backend_MsBackendRawFileReader <- function(x, i) {
  if (missing(i))
    return(x)
  idx <- i
  i <- MsCoreUtils::i2index(i, length(x), rownames(x@spectraData))
  
  slot(x, "spectraData", check = FALSE) <- S4Vectors::extractROWS(x@spectraData, i)
  
  # check if item is complete otherwise retrieval of data through using 
  # rawrr::readSpectrum(i)
  # message(sprintf("supposed to fetch index %d from %d", i, idx))
  x
}

#' MsBackendRawFileReader
#' 
#' @importFrom methods new
#' @exportClass MsBackendRawFileReader
#' @export MsBackendRawFileReader
MsBackendRawFileReader <- function() {
  if (isFALSE(requireNamespace("rawrr", quietly = TRUE)))
    stop("The use of 'MsBackendRawFileReader' requires package 'rawrr'. Please ",
         "install with 'BiocInstaller::install(\"rawrr\")'")
  
  if (isFALSE(requireNamespace("Spectra", quietly = TRUE)))
    stop("The use of 'MsBackendRawFileReader' requires package 'Spectra'. Please ",
         "install with 'BiocInstaller::install(\"Spectra\")'")
  
  new("MsBackendRawFileReader")
}


# read peaks =======
.RawFileReader_read_peaks <- function(x = character(), scanIndex = integer(),
                                       maxGroupSize = 150,
                                       tmpdir=tempdir(),
                                       BPPARAM = bpparam()) {
  if (length(x) != 1)
    stop("'x' should have length 1")
  if (!length(scanIndex))
    return(list(matrix(ncol = 2, nrow = 0,
                       dimnames = list(character(), c("mz", "intensity")))))
  requireNamespace("rawrr", quietly = TRUE)
 
  if (length(scanIndex) < maxGroupSize)
    maxGroupSize <- length(scanIndex)
  
  BiocParallel::bplapply(FUN = function(i){
    # print(i)
    lapply(rawrr::readSpectrum(x, i, tmpdir=tmpdir), function(p){
      m <- as.matrix(cbind(p$mZ, p$intensity))
      colnames(m) <- c("mz", "intensity")
      m
    })
  },
  split(scanIndex, ceiling(seq_along(scanIndex) / maxGroupSize)),
  BPPARAM = BPPARAM) |>
    unlist(recursive = FALSE)
}

# read peaks =======
.RawFileReader_read_peaks2 <- function(x = character(), scanIndex = integer(),
                                      maxGroupSize = 150,
                                      tmpdir=tempdir(),
                                      BPPARAM = bpparam()) {
  if (length(x) != 1)
    stop("'x' should have length 1")
  if (!length(scanIndex))
    return(list(matrix(ncol = 2, nrow = 0,
                       dimnames = list(character(), c("mz", "intensity")))))
  requireNamespace("rawrr", quietly = TRUE)
  
  if (length(scanIndex) < maxGroupSize)
    maxGroupSize <- length(scanIndex)
  
  BiocParallel::bplapply(FUN = function(i){
    rawrr::readSpectrum(x, i, tmpdir=tmpdir)
    },
  split(scanIndex, ceiling(seq_along(scanIndex) / maxGroupSize)),
  BPPARAM = BPPARAM) |>
    unlist(recursive = FALSE)
}

.RawFileReader_filter <- function(x = character(), filter = character()){
  if (length(x) != 1)
    stop("'x' should have length 1")
  
  # TODO(cp): check if filterString is a valid filter
  
  requireNamespace("rawrr", quietly = TRUE)
  rawrr:::filter(x, filter)
}
