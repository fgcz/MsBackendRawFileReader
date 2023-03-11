test_that("peak", {
  expect_true(length(Spectra((sample_raw_2))) == 1146)
})


test_that("intensity", {
  expect_equal(sum(sum(intensity(sample_raw))) , 2275696879)
  
  expect_equal(sum(sum(intensity(sample_raw_2))),
               (2 * sum(sum(intensity(sample_raw)))))

  expect_equal(sum(sum(intensity(Spectra(sample_raw_2)))),
               (2 * sum(sum(intensity(Spectra(sample_raw))))))
})

test_that("msLevel", {
  expect_true(all(table(msLevel(sample_raw)) == c(27, 546)))
})

test_that("scanType", {
  expect_true(scanType(sample_raw)[1] == "FTMS + c NSI Full ms [350.0000-1800.0000]")
})

test_that("filterScan", {
  expect_true(all(table(scanType(sample_raw)) == c(27, rep(26,21))))
  expect_true(all((lapply(c('ms', 'ms2', ''), FUN=filterScan, object=sample_raw) |>
                     sapply(FUN=length)) == c(27, 546, 573)))
})

test_that("collisionEnergy", {
    expect_type(collisionEnergy(sample_raw), "double")
    # expect_equal(collisionEnergy(sample_raw)[[1]], numeric(0)) #MS1 scan
    expect_equal(collisionEnergy(sample_raw)[[2]], 28) #MS2 scan
})

test_that("isolationWindow funs", {
    expect_type(isolationWindowLowerMz(sample_raw), "double")
    expect_type(isolationWindowTargetMz(sample_raw), "double")
    expect_type(isolationWindowUpperMz(sample_raw), "double")
    
    #MS1 scan, should be NA
    expect_equal(isolationWindowLowerMz(sample_raw)[1], NA_real_)
    expect_equal(isolationWindowTargetMz(sample_raw)[1], NA_real_)
    expect_equal(isolationWindowUpperMz(sample_raw)[1], NA_real_)
    
    #MS2 scan
    expect_equal(isolationWindowLowerMz(sample_raw)[2], 486.5567, tolerance = 1e-4)
    expect_equal(isolationWindowTargetMz(sample_raw)[2], 487.2567, tolerance = 1e-4)
    expect_equal(isolationWindowUpperMz(sample_raw)[2], 487.9567, tolerance = 1e-4)
})

test_that("peaksData", {
  P <- (sample_raw |>
          filterScan("ms2 517.7398@hcd28.00"))[1:2] |>
    peaksData()
  
  R <- joinPeaks(P[2][[1]], P[1][[1]], ppm = 10, type = "inner")
  
  expect_equal(R$x[, 1],  R$y[, 1], tolerance = 1E-5)
})
