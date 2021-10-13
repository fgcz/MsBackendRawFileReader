#R


iobenchmark <- function(){
  eh <- ExperimentHub::ExperimentHub()
  
  EH4547  <- normalizePath(eh[["EH4547"]])
  (rawfileEH4547  <- paste0(EH4547 , ".raw"))
  if (!file.exists(rawfileEH4547 )){
    file.link(EH4547 , rawfileEH4547 )
  }
  
  b <- lapply(c(1000, 5000, 10000), function(n){
    lapply(c(8, 16, 32, 64, 128, 256, 8, 16, 32, 64, 128, 256, 8, 16, 32, 64,
             128, 256, 8, 16, 32, 64, 128, 256, 8, 16, 32, 64, 128, 256, 8, 16,
             32, 64, 128, 256),
           function(size){
             start_time <- Sys.time()
             rv <- MsBackendRawFileReader:::.RawFileReader_read_peaks(rawfileEH4547, 1:n,
                                                                      maxGroupSize = size)
             end_time <- Sys.time()
             d <- end_time - start_time
             message(sprintf("%d - %s", size, d))
             data.frame(size=size, time=d/n, n=n, workers=bpparam()$workers)
           })
  })
}

df <- iobenchmark |>
  Reduce(f=rbind)

# write df into csv file
  
