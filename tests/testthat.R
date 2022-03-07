library("testthat")
library("MsBackendRawFileReader")
library("Spectra")


sample_raw_file <- file.path(system.file(package = "rawrr"),
                                'extdata', 'sample.raw')

sample_mzXML_file <- file.path(path.package(package = 'MsBackendRawFileReader'),
                                'extdata', 'sample.mzXML')


sample_raw <-  backendInitialize(MsBackendRawFileReader::MsBackendRawFileReader(), files = sample_raw_file)


#sample_mzr <-  backendInitialize(MsBackendMzR(), files = sample_mzXML_file)


mgf_file <- file.path(system.file(package = "MsBackendRawFileReader"),
                             'extdata', '3159619b11ed_4590_9594.mgf')


rv <- lapply(1:2, function(x){
  file.copy(from = sample_raw_file, to = tempfile(fileext='.raw'))})


register(SnowParam(workers = 1, type = "SOCK") , default = TRUE); 
sample_raw_2 <- backendInitialize(MsBackendRawFileReader(),
    files = file.path(tempdir(),
         list.files(path = tempdir(), pattern = 'raw$')))


test_check("MsBackendRawFileReader")


be <- Spectra::backendInitialize(MsBackendRawFileReader::MsBackendRawFileReader(),
                                 files = c(sample_raw_file))

## Run the MsBackend spectra variable test suite
test_suite <- system.file("test_backends", "test_MsBackend",
                          package = "Spectra")

#res <- test_file(paste0(test_suite, "/test_spectra_variables.R"),
#                 reporter = check_reporter(), stop_on_failure = TRUE)


## Run the whole suite.
res <- test_dir(test_suite, stop_on_failure = TRUE)
