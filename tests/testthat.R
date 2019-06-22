library("testthat")
library("MsBackendRawFileReader")
library("Spectra")

sample_raw_file <- file.path(path.package(package = 'MsBackendRawFileReader'),
                                'extdata', 'sample.raw')

sample_mzXML_file <- file.path(path.package(package = 'MsBackendRawFileReader'),
                                'extdata', 'sample.mzXML')


sample_raw <-  backendInitialize(MsBackendRawFileReader(), files = sample_raw_file)
sample_mzr <-  backendInitialize(MsBackendMzR(), files = sample_mzXML_file)

test_check("MsBackendRawFileReader")
