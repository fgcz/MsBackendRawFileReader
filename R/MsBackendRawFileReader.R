#' @include hidden_aliases.R
NULL

#' @title RawFileReader-based backend
#'
#' @description
#'
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
#' @author Johannes Rainer, Christian Panse (2019-2021)
#'
#' @noRd
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


' @rdname hidden_aliases
#'
#' @importFrom methods callNextMethod
#'
#' @importFrom MsCoreUtils rbindFill
#'
#' @importMethodsFrom BiocParallel bpmapply
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
              MsCoreUtils::rbindFill, bplapply(files,
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
setMethod("peaksData", "MsBackendRawFileReader", function(object) {
  if (!length(object))
    return(list())
  fls <- unique(object@spectraData$dataStorage)
  print(fls)
  if (length(fls) > 1) {
    f <- factor(dataStorage(object), levels = fls)
    unsplit(bpmapply(
      FUN = .RawFileReader_read_peaks,
      fls,
      split(scanIndex(object), f),
      object@modCount,
      SIMPLIFY = FALSE, USE.NAMES = FALSE, BPPARAM = bpparam()),
      f)
  } else
    .RawFileReader_read_peaks(fls, scanIndex(object), object@modCount)
})

#' @exportMethod [
#'
#' @rdname MsBackend
setMethod("[", "MsBackendRawFileReader", function(x, i, j, ..., drop = FALSE) {
  .subset_backend_MsBackendRawFileReader(x, i)
})



