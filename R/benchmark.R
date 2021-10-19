#R
#' RawFileReader_read_peaks benchmark
#' 
#' derives numbers to evaluate time performance for reading a single spectrum in
#' dependency from the chunk size (how many spectra are read in one function call)
#' for reading different numbers of overall spectra. 
#'
#' @param nv number of spectra to be read.
#' @param sizev number of spectra write and parsed in one single junk.
#' @param rawfile the Thermo Fisher Scientific raw file.
#'
#' @return \code{data.frame}
#' @export ioBenchmark
#'
#' @examples
#' eh <- ExperimentHub::ExperimentHub()
#' EH4547  <- normalizePath(eh[["EH4547"]])
#' (rawfileEH4547  <- paste0(EH4547 , ".raw"))
#' if (!file.exists(rawfileEH4547 )){
#'   file.link(EH4547 , rawfileEH4547)
#'   }
#' S <- ioBenchmark(1000, c(128, 256, 128, 256), rawfile=rawfileEH4547)
ioBenchmark <- function(nv = c(1000, 5000, 10000),
                        sizev = c(8, 16, 32, 64, 128, 256, 8, 16, 32, 64, 128, 256, 8, 16, 32, 64,
                                128, 256, 8, 16, 32, 64, 128, 256, 8, 16, 32, 64, 128, 256, 8, 16,
                                32, 64, 128, 256), rawfile){
  
  
  b <- lapply(nv, function(n){
    lapply(sizev,
           function(size){
             start_time <- Sys.time()
             rv <- .RawFileReader_read_peaks(rawfile, 1:n, maxGroupSize = size)
             end_time <- Sys.time()
             d <- end_time - start_time
             # message(sprintf("%d - %s", size, d))
             data.frame(size=size,
                        time=d/n,
                        n=n,
                        workers=bpparam()$workers,
                        host = Sys.info()['nodename'])
           })
  }) 
  b |> Reduce(f=rbind) |> Reduce(f=rbind)
}
