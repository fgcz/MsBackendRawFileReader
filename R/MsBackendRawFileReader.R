#' @include hidden_aliases.R
NULL


#' @title RawFileReader-based backend
#' @aliases MsBackendRawFileReader
#' @description

#' The `MsBackendRawFileReader` inherits all slots and methods from the base
#' `MsBackendDataFrame` (in-memory) backend. It overrides the base `mz` and
#' `intensity` methods as well as `peaks` to read the respective data from
#' the original raw data files.
#'
#' The validator function has to ensure that the files provided in the
#' `files` slot exist.
#'
#' The `backendInitialize` method reads the header data from the raw files and
#' hence fills the `spectraData` slot. Note that this method could be called
#' several times, e.g. also to *re-fill* `spectraData` after dropping some of
#' its columns.
#'
#' @author Christian Panse 2019-06-15 
#' adapted from the MsBackendMzR.R file by Johannes Rainer
#' 
#' @importClassesFrom Spectra MsBackendDataFrame 
setClass("MsBackendRawFileReader",
         contains = "MsBackendDataFrame",
         prototype = prototype(version = "0.1",
                               readonly = TRUE),
         slots = c(rawfileReaderObj = "list")
         )

setValidity("MsBackendRawFileReader", function(object) {
    msg <- Spectra:::.valid_spectra_data_required_columns(object@spectraData,
                                                c("fromFile", "scanIndex"))
    msg <- c(msg, Spectra:::.valid_ms_backend_files_exist(object@files))
    if (length(msg)) msg
    else TRUE
})


#' @importFrom methods callNextMethod
#' @importFrom rDotNet .cnew .cinit
#' @importFrom IRanges NumericList
#' @rdname hidden_aliases
setMethod("backendInitialize", "MsBackendRawFileReader",
          function(object, files, spectraData, ..., BPPARAM = bpparam()) {
              if (missing(files) || !length(files))
                  stop("Parameter 'files' is mandatory for 'MsBackendRawFileReader'")
            
              files <- normalizePath(files) 
              
              if (!all(file.exists(files)))
                  stop("File(s) ", paste(files[!file.exists(files)]),
                       " not found")
              msg <- Spectra:::.valid_ms_backend_files(files)
              if (length(msg))
                  stop(msg)
              if (!missing(spectraData)) {
                  spectraData$mz <- NULL
                  spectraData$intensity <- NULL
                  #rawfileReaderObj <- NULL
              } else {
                
                  object@rawfileReaderObj <- lapply(files, function(rawfile){.cnew ("Rawfile", rawfile)})
                  
                  spectraData <- do.call(
                      rbind, mapply(object@rawfileReaderObj, seq_along(files),
                                      FUN = function(flObj, index) {
                                          cbind(MsBackendRawFileReader:::.MsBackendRawFileReader_header(flObj),
                                                fromFile = index)
                                      }))
              }
              callNextMethod(object = object,
                             files = files,
                             spectraData = spectraData,
                           #  rawfileReaderObj = rawfileReaderObj,
                             ...)
          })

#' @rdname hidden_aliases
setMethod("show", "MsBackendRawFileReader", function(object) {
    callNextMethod()
    fls <- basename(object@files)
    if (length(fls)) {
        to <- min(3, length(fls))
        cat("\nfile(s):\n", paste(basename(fls[1:to]), collapse = "\n"),
            "\n", sep = "")
        if (length(fls) > 3)
            cat(" ...", length(fls) - 3, "more files\n")
    }
})


#' @rdname hidden_aliases
setMethod("intensity", "MsBackendRawFileReader", function(object) {
  if (!length(object))
  	return(NumericList())

   objs <- unique(object@rawfileReaderObj)
   if (length(objs) > 1) {
  	return(NumericList())
   }else{
	x <- objs[[1]]
	first <- x$getFirstScanNumber()
	last <- x$getLastScanNumber()
  	return (NumericList(.MsBackendRawFileReader_intensity(x, first:last), compress = FALSE))
   }
})

#' @rdname hidden_aliases
setMethod("mz", "MsBackendRawFileReader", function(object) {
  
  #return (NumericList(1:10))
  #NumericList(lapply(object@rawfileReaderObj,
  #                   function(x){
  #                    
  #                     .MsBackendRawFileReader_mz(x, 1:2)
  #                  }), compress = FALSE)

  if (!length(object))
  	return(NumericList())

   objs <- unique(object@rawfileReaderObj)
   if (length(objs) > 1) {
  	return(NumericList())
   }else{
	x <- objs[[1]]
	first <- x$getFirstScanNumber()
	last <- x$getLastScanNumber()
  	return (NumericList(.MsBackendRawFileReader_mz(x, first:last), compress = FALSE))
   }
})

