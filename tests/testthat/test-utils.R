test_that("conversion between units works", {
  biodiesel_transmittance <- absorbance_to_transmittance(biodiesel)
  expect_named(
    biodiesel_transmittance,
    c("wavenumber", "transmittance", "sample_id")
  )
  expect_named(
    transmittance_to_absorbance(biodiesel_transmittance),
    c("wavenumber", "absorbance", "sample_id")
  )

  expect_error(transmittance_to_absorbance(biodiesel),
    "`ftir` must contain a `transmittance` column.",
    fixed = TRUE
  )
  expect_error(absorbance_to_transmittance(absorbance_to_transmittance(biodiesel)),
    "`ftir` must contain a `absorbance` column.",
    fixed = TRUE
  )

  example_data <- data.frame(
    "wavenumber" = 1L,
    "absorbance" = c(0, .5, 1, 1.5, 2),
    "sample_id" = "test"
  )
  expect_equal(absorbance_to_transmittance(example_data)$transmittance,
    c(100, 31.62278, 10, 3.162278, 1),
    tolerance = 1e-4
  )

  example_data2 <- data.frame(
    "wavenumber" = 1L,
    "transmittance" = c(100, 50, 10, 5, 1),
    "sample_id" = "test"
  )
  expect_equal(transmittance_to_absorbance(example_data2)$absorbance,
    c(0, 0.30103, 1, 1.30103, 2),
    tolerance = 1e-4
  )

  example_data3 <- data.frame(
    "wavenumber" = 1L,
    "absorbance" = c(0, 0.5, 1, 1.5, 2),
    "sample_id" = "test",
    "transmittance" = c(100, 50, 10, 5, 1)
  )
  expect_error(absorbance_to_transmittance(example_data3),
    "`ftir` cannot contain both `absorbance` and `transmittance` columns.",
    fixed = TRUE
  )
  expect_error(transmittance_to_absorbance(example_data3),
    "`ftir` cannot contain both `absorbance` and `transmittance` columns.",
    fixed = TRUE
  )
})

test_that("Plot SampleID extraction is ok", {
  # Test for ggplot2 else skip
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error(
      get_plot_sample_ids(123),
      "requires ggplot2 package installation",
      fixed = TRUE
    )

    testthat::skip("ggplot2 not available for testing plot production")
  }

  p <- plot_ftir(biodiesel)

  expect_equal(get_plot_sample_ids(p), as.factor(unique(biodiesel$sample_id)))

  expect_error(get_plot_sample_ids(biodiesel),
    "`ftir_spectra_plot` must be a ggplot object. You provided ",
    fixed = TRUE
  )
})

test_that("Intensity Typing works", {
  expect_equal(intensity_type(biodiesel), "absorbance")
  expect_equal(intensity_type(absorbance_to_transmittance(biodiesel)), "transmittance")
  b2 <- biodiesel
  colnames(biodiesel)[colnames(biodiesel) == "absorbance"] <- "intensity"
  expect_equal(intensity_type(b2), "absorbance")
})

test_that("Checking FTIR data works", {
  # Most checks are validated in one way or another by the repeated calling of the check_ftir_data()
  # function in the other code, but we intentionally manually validate here.

  bad_ftir <- biodiesel
  attr(bad_ftir, "intensity") <- "test"
  expect_error(check_ftir_data(bad_ftir), "has unexpected attributes.", fixed = TRUE)

  no_attr_ftir <- biodiesel
  attr(no_attr_ftir, "intensity") <- NULL
  expect_equal(attr(check_ftir_data(no_attr_ftir), "intensity"), "absorbance")
})
