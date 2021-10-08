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

test_that("peaksData", {
  P <- (sample_raw |>
          filterScan("ms2 517.7398@hcd28.00"))[1:2] |>
    peaksData()
  
  R <- joinPeaks(P[2][[1]], P[1][[1]], ppm = 10, type = "inner")
  
  expect_equal(R$x[, 1],  R$y[, 1], tolerance = 1E-5)
})