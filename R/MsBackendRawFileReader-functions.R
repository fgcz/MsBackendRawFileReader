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


#' @importFrom S4Vectors DataFrame
#' 
.MsBackendRawFileReader_header <- function(x = character()) {
  if (length(x) != 1)
    stop("'x' should have length 1")
  requireNamespace("rawrr", quietly = TRUE)
 
  hdr <- rawrr::readIndex(x)
  
  colnames(hdr)[colnames(hdr) == "scan"] <- "scanIndex"
  #colnames(hdr)[colnames(hdr) == "precursorScanNum"] <- "precScanNum"
  colnames(hdr)[colnames(hdr) == "precursorMass"] <- "precursorMz"
  colnames(hdr)[colnames(hdr) == "StartTime"] <- "rtime"
  colnames(hdr)[colnames(hdr) == "MSOrder"] <- "msLevel"
  #colnames(hdr)[colnames(hdr) == "isolationWindowTargetMZ"] <- "isolationWindowTargetMz"
  #hdr$isolationWindowLowerMz <- hdr$isolationWindowTargetMz -
  #  hdr$isolationWindowLowerOffset
  #hdr$isolationWindowUpperMz <- hdr$isolationWindowTargetMz +
  #  hdr$isolationWindowUpperOffset
  #hdr$isolationWindowUpperOffset <- NULL
  #hdr$isolationWindowLowerOffset <- NULL
  
  hdr$msLevel[hdr$msLevel == "Ms"] <- 1
  hdr$msLevel[hdr$msLevel == "Ms2"] <- 2
  hdr$msLevel[hdr$msLevel == "Ms3"] <- 3
  hdr$msLevel <- as.integer(hdr$msLevel)
  
  ## MS1 scans have no precursorMz
  if (any(hdr$msLevel == 1)) hdr$precursorMz[hdr$msLevel == 1] <- NA
  
  hdr_full <- .get_spectrum_metadata(x)
  
  hdr <- cbind(hdr, hdr_full)

  ## Remove core spectra variables that contain only `NA`
  hdr <- S4Vectors::DataFrame(hdr[, !(MsCoreUtils::vapply1l(hdr, function(z) all(is.na(z))) &
                                      colnames(hdr) %in%
                                      names(.SPECTRA_DATA_COLUMNS))
  ])
  .post_process_header(hdr)
}

.SPECTRA_METADATA_COLS <- list(
    "injectionTime" = "Ion Injection Time (ms):",
    "collisionEnergyList" = "HCD Energy:",
    "isolationWidth" = "MS2 Isolation Width:",
    "isolationOffset" = "MS2 Isolation Offset:",
    "totIonCurrent" = "TIC",
    "resolution" = "FT Resolution:",
    "AGC" = "AGC:",
    "AGCTarget" = "AGC Target:",
    "AGCFill" = "AGC Fill:"
)


.get_spectrum_metadata <- function(x){
    varNames <- rawrr::readTrailer(x)
    varNames <- varNames[varNames %in% .SPECTRA_METADATA_COLS]
    if (!length(varNames)) return(S4Vectors::DataFrame())
    DF <- lapply(varNames, function(x, var) rawrr::readTrailer(x, var), x = x)
    names(DF) <- names(.SPECTRA_METADATA_COLS)[match(varNames,
                                                     .SPECTRA_METADATA_COLS)]
    DF <- lapply(DF, I) #Protect variables to allow lists of vectors
    S4Vectors::DataFrame(do.call(cbind, DF))
}


.post_process_header <- function(header){
    varsToNumeric <- c("isolationWidth", "isolationOffset", "injectionTime",
                       "AGCTarget", "AGCFill")
    if (any(colnames(header) %in% varsToNumeric)) {
        found <- colnames(header)[colnames(header) %in% varsToNumeric]
        for (var in found) {
            header[[var]] <- as.numeric(header[[var]])
        }
    }
    varsToRemoveSpace <- c("AGC", "collisionEnergyList")
    if (any(colnames(header) %in% varsToRemoveSpace)) {
        found <- colnames(header)[colnames(header) %in% varsToRemoveSpace]
        for (var in found) {
            header[[var]] <- gsub(" ", "", header[[var]])
        }
    }
    if ("collisionEnergyList" %in% colnames(header)) {
        header$collisionEnergyList <- lapply(header$collisionEnergyList, function(x){
            as.numeric(strsplit(x, ",")[[1]])
        })
        header$isStepped <- lengths(header$collisionEnergyList) > 1
        header$collisionEnergy <- as.numeric(sapply(header$collisionEnergyList,
                                                    mean))
    }
    if (all(c("precursorMz", "isolationWidth", "isolationOffset") %in% 
            colnames(header))) {
        header$isolationWindowTargetMz <-
            header$precursorMz + header$isolationOffset
        header$isolationWindowLowerMz <-
            header$isolationWindowTargetMz - header$isolationWidth / 2
        header$isolationWindowUpperMz <-
            header$isolationWindowTargetMz + header$isolationWidth / 2
    }
    header
}


#' @importFrom S4Vectors extractROWS
#' @importFrom MsCoreUtils i2index
#' @importFrom methods slot<-
.subset_backend_MsBackendRawFileReader <- function(x, i = integer()) {
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




## Define a function that takes a matrix as input and derives
## the top n most intense peaks within a mass window.
## Of note, here, we require centroided data. (no profile mode!)
.top_n <- function(x, n = 10, mass_window = 100, ...){

  # message(paste("DEBUG:", nrow(x), ncol(x), class(x), "\n"))

  if (nrow(x) < n ){
	  return (x)
	  }

  idx <- lapply(seq(0, 2000, by = mass_window), function(mZ){
    i <- (mZ < x[, 1] & x[, 1] <= mZ + mass_window) |> which()

    r <- i[order(x[, 2][i], decreasing = TRUE)]

    if (length(x[, 2]) > length(i))
      return(r[1:n])

    return(r)
  }, ...) |> unlist()

  x[idx[!is.na(idx)] |> sort(), ]
}

