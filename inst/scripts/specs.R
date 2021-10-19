#R

eh <- ExperimentHub::ExperimentHub()

EH4547  <- normalizePath(eh[["EH4547"]])
(rawfileEH4547  <- paste0(EH4547 , ".raw"))
if (!file.exists(rawfileEH4547 )){
  file.link(EH4547 , rawfileEH4547 )
}


df <- MsBackendRawFileReader::ioBenchmark(rawfile=rawfileEH4547) 

# write df into csv file
  
