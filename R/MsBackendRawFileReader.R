#' @include hidden_aliases.R
NULL

#' @title RawFileReader-based backend for Spectra
#' @aliases MsBackendRawFileReader
#' @description
#' The `MsBackendRawFileReader` inherits all slots and methods from the base
#' `MsBackendDataFrame` (in-memory) backend. It overrides the base `mz` and
#' `intensity` methods as well as `peaks` to read the respective data from
#' the original raw data files.
#'
#' The `backendInitialize` method reads the header data from the raw files and
#' hence fills the `spectraData` slot. Note that this method could be called
#' several times, e.g. also to *re-fill* `spectraData` after dropping some of
#' its columns.
#'
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2019-06-15 
#' adapted from the \code{\link[Spectra]{MsBackendMzR}.R} file by Johannes Rainer
#'
#' @seealso \itemize{
#' \item{\href{http://planetorbitrap.com/rawfilereader}{The New RawFileReader} 
#' and the License document in the package directory.}
#' \item{\code{\link[rDotNet]{.cnew}} man page}
#' \item {\code{\link[Spectra]{MsBackendDataFrame}}}
#' }
#'
#' @references \doi{10.1021/acs.jproteome.8b00173}
#'
#' @importClassesFrom Spectra MsBackendDataFrame 
#'
#' @examples 
#' library(MsBackendRawFileReader)
#' (rawfile <- file.path(path.package(package = 'MsBackendRawFileReader'),
#'      'extdata', 'sample.raw'))
#'
#'  be <- backendInitialize(MsBackendRawFileReader(), files = rawfile)
#'  (S <- Spectra(be))
setClass("MsBackendRawFileReader",
    contains = "MsBackendDataFrame",
    prototype = prototype(version = "0.1", readonly = TRUE),
    slots = c(rawfileReaderObj = "list")
)

setValidity("MsBackendRawFileReader", function(object) {
    msg <- Spectra:::.valid_spectra_data_required_columns(object@spectraData,
        c("dataStorage", "scanIndex"))

    msg <- c(msg, Spectra:::.valid_ms_backend_files_exist(unique(
        object@spectraData$dataStorage)))

    if (length(msg)) msg
    else TRUE
})

#----- backendInitialize ----
#' @importFrom methods callNextMethod validObject
#' @importFrom rDotNet .cnew .cinit
#' @importFrom IRanges NumericList
#' @importFrom BiocParallel bpparam
#' @rdname hidden_aliases
setMethod("backendInitialize", "MsBackendRawFileReader",
    function(object, files, ..., BPPARAM = bpparam()) {
        if (missing(files) || !length(files))
            stop("Parameter 'files' is mandatory for 'MsBackendRawFileReader'")
            
        if (!is.character(files))
            stop("Parameter 'files' is expected to be a character vector",
                " with the files names from where data should be",
                " imported")

            files <- normalizePath(files) 
            msg <- Spectra:::.valid_ms_backend_files_exist(files)
            if (length(msg))
                stop(msg)

            # rDotNet RawFileReader specific
            object@rawfileReaderObj <- lapply(files,
                function(rawfile){.cnew ("Rawfile", rawfile)})
            
            # the rDotNet package can not handle bpmapply calls yet.
            spectraData <- do.call(
                'rbind', mapply(object@rawfileReaderObj, files,
                    FUN = function(flObj, fl) {
                        cbind(.MsBackendRawFileReader_header(flObj, ...),
                            dataStorage = fl)
                        }
                    )
                )
            

            spectraData$dataOrigin <- spectraData$dataStorage
            # object@spectraData <- Spectra:::.as_rle_spectra_data(spectraData)
            object@spectraData <- spectraData
            validObject(object)
            object
          })


#' @rdname hidden_aliases
#' @exportMethod show
setMethod("show", "MsBackendRawFileReader", function(object) {
    #callNextMethod()
    fls <- unique(object@spectraData$dataStorage)
    objs <- unique(object@rawfileReaderObj)
   
    if (length(fls)) {
       
        info = lapply(objs, function(x){
            paste(x$GetInfoKeys(), x$GetInfoValues(), sep=":\t", collapse ='\n')
        })
       
        cat("\n")
        to <- min(3, length(objs))
        rv <- sapply(info[1:to], cat, collapse='\n\n')
        
        #cat("\nfile(s):\n", paste(basename(fls[1:to]), collapse = "\n"),
        #    "\n", sep = "")
        if (length(objs) > 3)
            cat(" ...", length(fls) - 3, "more files\n")
    }
})


#----- msLevel ----
#' @rdname hidden_aliases
#' @exportMethod msLevel
setMethod("msLevel", "MsBackendRawFileReader", function(object) {
    if (!length(object))
        return(NULL)
    
    objs <- unique(object@rawfileReaderObj)
    fls <- unique(object@spectraData$dataStorage)
    
    f <- factor(dataStorage(object), levels = fls)
    
    return((unsplit(mapply(FUN = function(x){
        #first <- x$getFirstScanNumber()
        #last <- x$getLastScanNumber()
        
        # vapply(first:last, FUN=function(z){x$GetMsLevel(z)}, FUN.VALUE = as.integer(1))
        x$GetMsLevels()
    },
    objs,
    SIMPLIFY = FALSE, USE.NAMES = FALSE), f)))
})

#----- centroided ----
#' @rdname hidden_aliases
setMethod("centroided", "MsBackendRawFileReader", function(object) {
    if (!length(object))
        return(NULL)
    
    objs <- unique(object@rawfileReaderObj)
    fls <- unique(object@spectraData$dataStorage)
    
    f <- factor(dataStorage(object), levels = fls)
    
    return(unsplit(mapply(FUN = function(x){
        first <- x$getFirstScanNumber()
        last <- x$getLastScanNumber()
        vapply(first:last, FUN=function(z){x$IsCentroidScan(z)}, FUN.VALUE = FALSE)
    },
    objs,
    SIMPLIFY = FALSE, USE.NAMES = FALSE), f))
})


#----- polarity ----
#' @rdname hidden_aliases
setMethod("polarity", "MsBackendRawFileReader", function(object) {
    if (!length(object))
        return(NULL)
    
    objs <- unique(object@rawfileReaderObj)
    fls <- unique(object@spectraData$dataStorage)
    
    f <- factor(dataStorage(object), levels = fls)
    
    return(unsplit(mapply(FUN = function(x){
        first <- x$getFirstScanNumber()
        last <- x$getLastScanNumber()
        vapply(first:last, FUN=function(z){x$GetPolarity(z)}, FUN.VALUE = as.integer(-1))
    },
    objs,
    SIMPLIFY = FALSE, USE.NAMES = FALSE), f))
})

#---- backendInitialize----
#' @rdname hidden_aliases
setMethod("collisionEnergy", "MsBackendRawFileReader", function(object) {
    if (!length(object))
        return(NULL)

    objs <- unique(object@rawfileReaderObj)
    fls <- unique(object@spectraData$dataStorage)

    f <- factor(dataStorage(object), levels = fls)

    return(unsplit(mapply(FUN = function(x){
        first <- x$getFirstScanNumber()
        last <- x$getLastScanNumber()
        vapply(first:last, FUN=function(z){x$GetCollisionEnergy(z)}, FUN.VALUE = as.double(1.0))
    },
    objs,
    SIMPLIFY = FALSE, USE.NAMES = FALSE), f))
})

#---- isolationWindowTargetMz----
#' @rdname hidden_aliases
setMethod("isolationWindowTargetMz", "MsBackendRawFileReader", function(object) {
    if (!length(object))
        return(NULL)
    
    objs <- unique(object@rawfileReaderObj)
    fls <- unique(object@spectraData$dataStorage)

    f <- factor(dataStorage(object), levels = fls)

    return(unsplit(mapply(FUN = function(x){
        first <- x$getFirstScanNumber()
        last <- x$getLastScanNumber()
        vapply(first:last, FUN=function(z){x$GetIsolationWidth(z)}, FUN.VALUE = as.double(1.0))
    },
    objs,
    SIMPLIFY = FALSE, USE.NAMES = FALSE), f))
})

#---- precursorMz----
#' @rdname hidden_aliases
setMethod("precursorMz", "MsBackendRawFileReader", function(object) {
    if (!length(object))
        return(NULL)

    objs <- unique(object@rawfileReaderObj)
    fls <- unique(object@spectraData$dataStorage)

    f <- factor(dataStorage(object), levels = fls)

    return((unsplit(mapply(FUN = function(x){
        #first <- x$getFirstScanNumber()
        #last <- x$getLastScanNumber()
        x$GetPrecursorMzs()
        #vapply(first:last, FUN=function(z){x$GetPrecursorMz(z)}, FUN.VALUE = as.double(1.0))
    },
    objs,
    SIMPLIFY = FALSE, USE.NAMES = FALSE), f)))
})

#---- precursorCharge----
#' @rdname hidden_aliases
setMethod("precursorCharge", "MsBackendRawFileReader", function(object) {
    if (!length(object))
        return(NULL)

   # if ("precursorCharge" %in% colnames(object@backend@spectraData)){
#       
#        return (object@backend@spectraData$precursorCharge)
#    }


    objs <- unique(object@rawfileReaderObj)
    fls <- unique(object@spectraData$dataStorage)
    
    f <- factor(dataStorage(object), levels = fls)


    return (unsplit(mapply(FUN = function(x){
        x$GetCharges()
        }, objs, SIMPLIFY = FALSE, USE.NAMES = FALSE), f))
})

#---- rtime----
#' @rdname hidden_aliases
setMethod("rtime", "MsBackendRawFileReader", function(object) {
  if (!length(object))
    return(NULL)
  
  objs <- unique(object@rawfileReaderObj)
  fls <- unique(object@spectraData$dataStorage)
  
  f <- factor(dataStorage(object), levels = fls)
  
  return((unsplit(mapply(FUN = function(x){
    x$GetRtime()
  },
  objs,
  SIMPLIFY = FALSE, USE.NAMES = FALSE), f)))
})


#---- tic ----
#' @rdname hidden_aliases
setMethod("tic", "MsBackendRawFileReader", function(object) {
  if (!length(object))
    return(NULL)
  
  objs <- unique(object@rawfileReaderObj)
  fls <- unique(object@spectraData$dataStorage)
  
  f <- factor(dataStorage(object), levels = fls)
  
  return((unsplit(mapply(FUN = function(x){
    x$GetTotalIonCounts()
  },
  objs,
  SIMPLIFY = FALSE, USE.NAMES = FALSE), f)))
})


setMethod("tic", "Spectra", function(object) {
  if (!length(object))
    return(NULL)
  
  tic(object@backend)
})

#---- ionCount ----
#' @rdname hidden_aliases
#' @exportMethod ionCount
setMethod("ionCount", "MsBackendRawFileReader", function(object) {
  if (!length(object))
    return(NULL)
  
  objs <- unique(object@rawfileReaderObj)
  fls <- unique(object@spectraData$dataStorage)
  
  f <- factor(dataStorage(object), levels = fls)
  
  return((unsplit(mapply(FUN = function(x){
    x$GetTotalIonCounts()
  },
  objs,
  SIMPLIFY = FALSE, USE.NAMES = FALSE), f)))
})

#---- basePeak ----
#' @rdname hidden_aliases
setMethod("basePeakMass", "MsBackendRawFileReader", function(object) {
  if (!length(object))
    return(NULL)
  
  objs <- unique(object@rawfileReaderObj)
  fls <- unique(object@spectraData$dataStorage)
  
  f <- factor(dataStorage(object), levels = fls)
  
  return((unsplit(mapply(FUN = function(x){
    x$GetBasePeakMasses()
  },
  objs,
  SIMPLIFY = FALSE, USE.NAMES = FALSE), f)))
})

setMethod("basePeakMass", "Spectra", function(object){
  if (!length(object))
    return(NULL)
  
  basePeakMass(object@backend)
})


setMethod("basePeakIntensity", "Spectra", function(object){
  if (!length(object))
    return(NULL)
  
  basePeakIntensity(object@backend)
})

#' @rdname hidden_aliases
setMethod("basePeakIntensity", "MsBackendRawFileReader", function(object) {
  if (!length(object))
    return(NULL)
  
  objs <- unique(object@rawfileReaderObj)
  fls <- unique(object@spectraData$dataStorage)
  
  f <- factor(dataStorage(object), levels = fls)
  
  return((unsplit(mapply(FUN = function(x){
    x$GetBasePeakIntensities()
  },
  objs,
  SIMPLIFY = FALSE, USE.NAMES = FALSE), f)))
})


#' @rdname hidden_aliases
setMethod("intensity", "MsBackendRawFileReader", function(object) {
  if (!length(object))
    return(NumericList())
  
  objs <- unique(object@rawfileReaderObj)
  fls <- unique(object@spectraData$dataStorage)
  
  f <- factor(dataStorage(object), levels = fls)
  
  return(NumericList(unsplit(mapply(FUN = function(x){
    first <- x$getFirstScanNumber()
    last <- x$getLastScanNumber()
    MsBackendRawFileReader:::.MsBackendRawFileReader_intensity(x, first:last)},
    objs,
    SIMPLIFY = FALSE, USE.NAMES = FALSE), f)))
  
})

#' @rdname hidden_aliases
setMethod("mz", "MsBackendRawFileReader", function(object) {
  if (!length(object))
    return(NumericList())

  objs <- unique(object@rawfileReaderObj)
  fls <- unique(object@spectraData$dataStorage)

  f <- factor(dataStorage(object), levels = fls)

  return(NumericList(unsplit(mapply(FUN = function(x){
    first <- x$getFirstScanNumber()
    last <- x$getLastScanNumber()
    MsBackendRawFileReader:::.MsBackendRawFileReader_mz(x,  first:last)},
    objs,
    SIMPLIFY = FALSE, USE.NAMES = FALSE), f)))
})


