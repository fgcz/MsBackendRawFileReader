test_that("peak", {
  expect_true(length(Spectra((sample_raw_2))) == 1146)
})


test_that("intensity", {
  
  expect_equal(sum(sum(intensity(sample_raw))) , 2275696879)
  
  expect_equal(sum(sum(intensity(sample_raw_2))), (2 * sum(sum(intensity(sample_raw)))))

  expect_equal(sum(sum(intensity(Spectra(sample_raw_2)))), (2 * sum(sum(intensity(Spectra(sample_raw))))))
  
})
