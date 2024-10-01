#' @include hidden_aliases.R
NULL

#' @title RawFileReader-based backend
#' @description
#' The `MsBackendRawFileReader` inherits all slots and methods from the base
#' `MsBackendDataFrame` (in-memory) backend. It overrides the base `mz` and
#' `intensity` methods as well as `peaksData` to read the respective data from
#' the original raw data files.
#'
#' The validator function has to ensure that the files exist and that required
#' column names are present.
#'
#' The `backendInitialize` method reads the header data from the raw files and
#' hence fills the `spectraData` slot.
#' 
#'
#' @author Christian Panse (2019-2021)
#' 
#' @importClassesFrom Spectra MsBackendDataFrame
#' 
#' @examples 
#' beRaw <- Spectra::backendInitialize(MsBackendRawFileReader::MsBackendRawFileReader(),
#'   files = rawrr::sampleFilePath())
#' beRaw
#' Spectra::msLevel(beRaw)
setClass("MsBackendRawFileReader",
         contains = "MsBackendDataFrame",
         prototype = prototype(version = "0.1", readonly = TRUE))

setValidity("MsBackendRawFileReader", function(object) {
  #msg <- .valid_spectra_data_required_columns(object@spectraData,
  #                                            c("dataStorage", "scanIndex"))
  #msg <- c(msg, .valid_ms_backend_files_exist(
  #  unique(object@spectraData$dataStorage)))
  #if (length(msg)) msg
  #else TRUE
  TRUE
})


#' @rdname hidden_aliases
#'
#' @importFrom methods callNextMethod
#' @importFrom MsCoreUtils rbindFill
#'
#' @importMethodsFrom BiocParallel bpmapply bplapply
#'
#' @importMethodsFrom ProtGenerics backendInitialize
#' 
#' @importFrom BiocParallel bpparam 
#' @importFrom rawrr readIndex
setMethod("backendInitialize", "MsBackendRawFileReader",
          function(object, files, ..., BPPARAM = bpparam()) {
            if (missing(files) || !length(files))
              stop("Parameter 'files' is mandatory for 'MsBackendRawFileReader'")
            if (!is.character(files))
              stop("Parameter 'files' is expected to be a character vector",
                   " with the files names from where data should be",
                   " imported")
            files <- normalizePath(files, mustWork = FALSE)
            msg <- .valid_ms_backend_files_exist(files)
            
            if (length(msg))
              stop(msg)
            spectraData <- do.call(
              MsCoreUtils::rbindFill, BiocParallel::bplapply(files,
                                  FUN = function(fl) {
                                    cbind(.MsBackendRawFileReader_header(fl),
                                          dataStorage = fl)
                                  }, BPPARAM = BPPARAM))
            spectraData$dataOrigin <- spectraData$dataStorage
            object@spectraData <- spectraData
            #validObject(object)
            object
          })

#' @rdname hidden_aliases
setMethod("show", "MsBackendRawFileReader", function(object) {
  callNextMethod()
  fls <- unique(object@spectraData$dataStorage)
  if (length(fls)) {
    to <- min(3, length(fls))
    cat("\nfile(s):\n", paste(basename(fls[seq_len(to)]), collapse = "\n"),
        "\n", sep = "")
    if (length(fls) > 3)
      cat(" ...", length(fls) - 3, "more files\n")
  }
})

#' @rdname hidden_aliases
#'
#' @importMethodsFrom ProtGenerics peaksData
#'
#' @importMethodsFrom ProtGenerics dataStorage
setMethod("peaksData", "MsBackendRawFileReader",
          function(object, ..., BPPARAM = bpparam()) {
            if (!length(object))
              return(list())
            fls <- unique(object@spectraData$dataStorage)
            
            f <- factor(dataStorage(object), levels = fls)
            unsplit(mapply(FUN = function(x, scanIndex){
              
              pls <- MsBackendRawFileReader:::.RawFileReader_read_peaks2(x,
                                                                         scanIndex, BPPARAM=BPPARAM)
              
              rv <- lapply(pls, function(p){
                if (all(c("centroid.mZ", "centroid.intensity", "noises",
                          "resolutions", "baselines") %in% names(p))){
                  
                  m <- as.matrix(cbind(p$centroid.mZ,
                                       p$centroid.intensity))
## TODO(cp): add it 
                                       #p$noises,
                                       #p$resolutions,
                                       #p$baselines))
                  
                  colnames(m) <- c("mz", "intensity")
                            #       "noises", "resolutions", "baselines")
                }else if (all(c("centroid.mZ", "centroid.intensity") %in% names(p) )){
                  m <- as.matrix(cbind(p$centroid.mZ, p$centroid.intensity))
                  colnames(m) <- c("mz", "intensity")
                }else if (length(p$mZ) > 0){
                  # warning(paste0("No centroid stream set for ", p$scan, "."))
                  m <- as.matrix(cbind(p$mZ, p$intensity))
                  colnames(m) <- c("mz", "intensity")
                }
                else{
                  warning(paste0("Scan ", p$scan, " has an empty peaklist!"))
                  m <- matrix(, 0, 2)
                  colnames(m) <- c("mz", "intensity")
                }
                m
              })
              rv 
            },
            x = fls,
            scanIndex = split(scanIndex(object), f),
            SIMPLIFY = FALSE, USE.NAMES = FALSE), f)
          })


#' @importFrom IRanges NumericList
#' @exportMethod [
setMethod("[", "MsBackendRawFileReader", function(x, i, j, ..., drop = FALSE) {
    i <- MsCoreUtils::i2index(i, length(x), rownames(x@spectraData))
    extractByIndex(x, i)
})

#' @rdname hidden_aliases
#' @importMethodsFrom Spectra extractByIndex
#' @export
setMethod("extractByIndex", c("MsBackendRawFileReader", "ANY"),
          function(object, i) {
              .subset_backend_MsBackendRawFileReader(object, i)
          })

#' @importClassesFrom IRanges NumericList
#' @rdname hidden_aliases
#' @importMethodsFrom ProtGenerics intensity
#' @exportMethod intensity
setMethod("intensity", "MsBackendRawFileReader", function(object, ..., BPPARAM = bpparam()) {
  IRanges::NumericList(lapply(peaksData(object, BPPARAM = BPPARAM), "[", , 2), compress = FALSE)
})

#' @importClassesFrom IRanges NumericList
#' @rdname hidden_aliases
#' @importMethodsFrom ProtGenerics mz
#' @exportMethod mz
setMethod("mz", "MsBackendRawFileReader", function(object, ..., BPPARAM = bpparam()) {
  IRanges::NumericList(lapply(peaksData(object, BPPARAM = BPPARAM), "[", , 1), compress = FALSE)
})

#' @exportMethod filterScan
#' @rdname MsBackendRawFileReader
#' @param object MsBackendRawFileReader object
#' @param filter filter string
#' @param ... Arguments to be passed to methods.
#' @return a MsBackendRawFileReader object.
#' @examples 
#' beRaw <- Spectra::backendInitialize(MsBackendRawFileReader::MsBackendRawFileReader(),
#'   files = rawrr::sampleFilePath())
#' beRaw |> MsBackendRawFileReader::filterScan('Ms')
setMethod("filterScan", "MsBackendRawFileReader",
          function(object, filter=character(), ...) {
            BPPARAM = bpparam()
            fls <- unique(object@spectraData$dataStorage)
            idx <- BiocParallel::bplapply(fls, FUN=.RawFileReader_filter, filter = filter, BPPARAM = BPPARAM)
            keep <- mapply(FUN=function(f, i){which(scanIndex(object) %in% i & dataStorage(object) %in% f)}, f=fls, idx)
            object[unlist(keep)]
          })


#' @exportMethod scanType
#' @rdname MsBackendRawFileReader
#' @return a character vector of scan types.
#' @examples 
#' beRaw <- Spectra::backendInitialize(MsBackendRawFileReader::MsBackendRawFileReader(),
#'   files = rawrr::sampleFilePath())
#' scanType(beRaw) |> head()
setMethod("scanType", "MsBackendRawFileReader",
          function(object, ...) {
            if (!length(object))
              return(list())
            object@spectraData$scanType
          })

#' @exportMethod scanIndex
#' @importMethodsFrom ProtGenerics scanIndex
#' @rdname MsBackendRawFileReader
#' @return a character vector of scan index.
#' @examples
#' beRaw <- Spectra::backendInitialize(MsBackendRawFileReader::MsBackendRawFileReader(),
#'   files = rawrr::sampleFilePath())
#' scanIndex(beRaw) |> head()
setMethod("scanIndex", "MsBackendRawFileReader",
          function(object, ...) {
            if (!length(object))
              return(list())
            object@spectraData$scanIndex
          })

