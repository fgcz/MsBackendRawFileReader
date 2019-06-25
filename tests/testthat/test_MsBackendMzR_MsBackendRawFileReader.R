test_that("msLevel", {
	expect_identical(msLevel(sample_raw), msLevel(sample_mzr))
})

test_that("rtime", {
  expect_equal(rtime(sample_raw), rtime(sample_mzr), tolerance = 1E-4)
})

test_that("precursorMz", {
  expect_equal(precursorMz(sample_raw), precursorMz(sample_mzr), tolerance = 1E-5)
})

test_that("precursorCharge", {
  expect_identical(precursorCharge(sample_raw), precursorCharge(sample_mzr))
})

test_that("centroided", {
    expect_identical(centroided(sample_raw), centroided(sample_mzr))
})

test_that("polarity", {
    expect_identical(polarity(sample_raw), polarity(sample_mzr))
})

test_that("collisionEnergy", {
    expect_equal(collisionEnergy(sample_raw),
                     collisionEnergy(sample_mzr))
})

#test_that("isolationWindowTargetMz", {
#    expect_equal(isolationWindowTargetMz(sample_raw),
#                     isolationWindowTargetMz(sample_mzr))
#})
 

test_that("intensity", {
  expect_equal(sum(sum(intensity(sample_raw))), sum(sum(intensity(sample_mzr))))
})

test_that("peaks", {
  
  # Note, there is no peaks method implemented for MsBackendRawFileReader
  
  sample_peaks_raw <- peaks(sample_raw)
  sample_peaks_mzr <- peaks(sample_mzr)
  
  n <- length(sample_peaks_raw)
  
  expect_true(n == 574)
  
  rv <- lapply(1:n, function(i){
    expect_identical(sample_peaks_raw[[i]] , sample_peaks_mzr[[i]])
  })
  
})
