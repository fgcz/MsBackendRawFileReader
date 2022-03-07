skip_if(isFALSE(require(MsBackendRawMgf)))
skip_if(isFALSE(require(ExperimentHub)))
skip_if(isFALSE(require(tartare)))


eh <- ExperimentHub::ExperimentHub()
query(eh, c('tartare'))
EH4547  <- normalizePath(eh[["EH4547"]])
(rawfileEH4547  <- paste0(EH4547 , ".raw"))
if (!file.exists(rawfileEH4547 )){
  file.link(EH4547 , rawfileEH4547 )
}

beEH4547raw <- Spectra::backendInitialize(
  MsBackendRawFileReader::MsBackendRawFileReader(),
  files = c(rawfileEH4547))


beEH4547mgf <- Spectra::backendInitialize(
  MsBackendMgf::MsBackendMgf(),
  files = c(mgf_file))




test_that("top_10", {
  
  plraw <- beEH4547raw[9594] |> Spectra() |> Spectra::addProcessing(MsBackendRawFileReader:::.top_n, n = 10) 
  plmgf <- beEH4547mgf |> Spectra()
  
  expect_equal(compareSpectra(plraw, plmgf) , 1)
})
