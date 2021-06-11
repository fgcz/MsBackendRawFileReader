library("testthat")
library("MsBackendRawFileReader")
library("Spectra")

sample_raw_file <- file.path(system.file(package = "rawrr"),
                                'extdata', 'sample.raw')

#sample_mzXML_file <- file.path(path.package(package = 'MsBackendRawFileReader'),
#                                'extdata', 'sample.mzXML')


sample_raw <-  backendInitialize(MsBackendRawFileReader(), files = sample_raw_file)
#sample_mzr <-  backendInitialize(MsBackendMzR(), files = sample_mzXML_file)



rv <- lapply(1:2, function(x){
  file.copy(from = sample_raw_file, to = tempfile(fileext='.raw'))})


register(SnowParam(workers = 1, type = "SOCK") , default = TRUE); 
sample_raw_2 <- backendInitialize(MsBackendRawFileReader(),
    files = file.path(tempdir(),
         list.files(path = tempdir(), pattern = 'raw$')))


test_check("MsBackendRawFileReader")
