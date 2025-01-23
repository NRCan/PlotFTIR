test_that("addition works" ,{
  ftir <- add_ftir_class(biodiesel)  # TODO: Once biodiesel object has class assigned this can simplify

  added_ftir <- add_scalar_value(ftir, 1)

  expect_equal(ftir + 1, added_ftir)

  expect_equal(1+ftir, added_ftir)

  expect_error(ftir + ftir, "Can't add two spectra together")

  expect_error(ftir + "test string", "Cannot add a string to a ftir spectra")

  expect_error("test string" + ftir, "Cannot add a string to a ftir spectra")
})

test_that("subtraction works" ,{
  ftir <- add_ftir_class(biodiesel)  # TODO: Once biodiesel object has class assigned this can simplify

  subbed_ftir <- subtract_scalar_value(ftir, 1)

  expect_equal(ftir - 1, subbed_ftir)

  expect_equal(1-ftir, subbed_ftir)  # TODO: This should change?

  expect_error(ftir - ftir, "Can't subtract two spectra")

  expect_error(ftir - "test string", "Cannot subtract a string from a ftir spectra")

  expect_error("test string" - ftir, "Cannot subtract a string from a ftir spectra")
})

test_that("mean(ftir) works", {
  ftir <- add_ftir_class(biodiesel)  # TODO: Once biodiesel object has class assigned this can simplify
  ftir2 <- add_ftir_class(sample_spectra)

  mean_ftir <- average_spectra(ftir)

  expect_equal(mean(ftir), mean_ftir)

  expect_equal(mean(ftir, sample_ids = c("biodiesel_B5", "biodiesel_5_0")), average_spectra(ftir, sample_ids = c("biodiesel_B5", "biodiesel_5_0")))

  expect_equal(suppressWarnings(mean(c(ftir, ftir2))), suppressWarnings(average_spectra(rbind(ftir, ftir2))))
})
