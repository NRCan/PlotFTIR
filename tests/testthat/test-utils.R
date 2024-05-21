test_that("conversion between units works", {
  biodiesel_transmittance <- absorbance_to_transmittance(biodiesel)
  expect_named(biodiesel_transmittance, c("wavenumber", "transmittance", "sample_id"))
  expect_named(transmittance_to_absorbance(biodiesel_transmittance), c("wavenumber", "absorbance", "sample_id"))

  example_data <- data.frame("wavenumber" = 1L, "absorbance" = c(0, .5, 1, 1.5, 2), "sample_id" = "test")
  expect_equal(absorbance_to_transmittance(example_data)$transmittance, c(100, 31.62278, 10, 3.162278, 1), tolerance = 1e-4)

  example_data2 <- data.frame("wavenumber" = 1, "transmittance" = c(100, 50, 10, 5, 1), "sample_id" = "test")
  expect_equal(transmittance_to_absorbance(example_data2)$absorbance, c(0, 0.30103, 1, 1.30103, 2), tolerance = 1e-4)
})
