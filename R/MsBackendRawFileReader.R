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
#' @import Spectra
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
setMethod("peaksData", "MsBackendRawFileReader",
          function(object, ..., BPPARAM = bpparam()) {
            if (!length(object))
              return(list())
            fls <- unique(object@spectraData$dataStorage)
            
            f <- factor(dataStorage(object), levels = fls)
            unsplit(mapply(FUN = function(x, scanIndex){
              
              pls <- MsBackendRawFileReader:::.RawFileReader_read_peaks2(x, scanIndex, BPPARAM=BPPARAM)
              
              rv <- lapply(pls, function(p){
                if (all(c("mz", "intensity", "noises", "resolutions", "baselines") %in% colnames(p))){
                  
                  m <- as.matrix(cbind(p$centroid.mZ,
                                       p$centroid.intensity,
                                       p$noises,
                                       p$resolutions,
                                       p$baselines))
                  
                  colnames(m) <- c("mz", "intensity",
                                   "noises", "resolutions", "baselines")
                }else{
                  m <- as.matrix(cbind(p$centroid.mZ,
                                       p$centroid.intensity))
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

#' @rdname hidden_aliases
#setMethod("exportSparceVector", "MsBackendRawFileReader",
#          function(object, ..., BPPARAM = bpparam()) {
#            #message("DEBUG")
#            if (!length(object))
#              return(list())
#            fls <- unique(object@spectraData$dataStorage)
#            
#            f <- factor(dataStorage(object), levels = fls)
#            unsplit(mapply(FUN = function(x, scanIndex){
#              
##              pls <- MsBackendRawFileReader:::.RawFileReader_read_peaks2(x, scanIndex, BPPARAM=BPPARAM)
#              
#              rv <- lapply(pls, function(p){
#                if (all(c("mz", "intensity", "noises", "resolutions", "baselines") %in% colnames(p))){
#                  
#                  ### 1. construct data frame that contains the peak list
#                  ### Note: a barebones version of cut() is .bincode() which retuns integer vector
#                  
#                  df <- data.frame(pos = p$centroid.mZ,
#                                   int = p$centroid.intensity,
#                                   z = p$charges,
#                                   n = p$noises,
#                                   r = p$resolutions,
#                                   b = p$baselines,
#                                   bin = cut(p$centroid.mZ,
#                                             breaks = seq(mzRange[1], mzRange[2],
#                                                          by = mzBinSize)
#                                   ),
#                                   sn = p$centroid.intensity / p$noises,
#                                   order = order(p$centroid.intensity / p$noises, decreasing = FALSE))
#                  
#                  
#                  return (df)
#                }else{
#                  return (NULL)
#                }
#                m
#              })
#              rv 
#            },
#            x = fls,
#            scanIndex = split(scanIndex(object), f),
#            SIMPLIFY = FALSE, USE.NAMES = FALSE), f)
#          })


#' @importFrom IRanges NumericList
#' @exportMethod [
setMethod("[", "MsBackendRawFileReader", function(x, i, j, ..., drop = FALSE) {
  .subset_backend_MsBackendRawFileReader(x, i)
})

#' @importClassesFrom IRanges NumericList
#' @rdname hidden_aliases
#' @exportMethod intensity
setMethod("intensity", "MsBackendRawFileReader", function(object, ..., BPPARAM = bpparam()) {
  IRanges::NumericList(lapply(peaksData(object, BPPARAM = BPPARAM), "[", , 2), compress = FALSE)
})


#' @exportMethod filterScan
#' @rdname MsBackendRawFileReader
#' @param object MsBackendRawFileReader object
#' @param filter filter string
#' @param ... Arguments to be passed to methods.
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
setMethod("scanType", "MsBackendRawFileReader",
          function(object, ...) {
            if (!length(object))
              return(list())
            object@spectraData$scanType
          })
