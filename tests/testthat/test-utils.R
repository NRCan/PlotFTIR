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

  expect_identical(
    get_plot_sample_ids(p),
    as.factor(unique(biodiesel$sample_id))
  )

  expect_error(
    get_plot_sample_ids(biodiesel),
    "`ftir_spectra_plot` must be a ggplot object. You provided ",
    fixed = TRUE
  )
})

test_that("Intensity Typing works", {
  expect_identical(intensity_type(biodiesel), "absorbance")
  expect_identical(
    intensity_type(absorbance_to_transmittance(biodiesel)),
    "transmittance"
  )
  b2 <- biodiesel
  colnames(biodiesel)[colnames(biodiesel) == "absorbance"] <- "intensity"
  expect_identical(intensity_type(b2), "absorbance")
})

test_that("Checking FTIR data works", {
  # Most checks are validated in one way or another by the repeated calling of the check_ftir_data()
  # function in the other code, but we intentionally manually validate here.

  bad_ftir <- biodiesel
  attr(bad_ftir, "intensity") <- "test"
  expect_error(
    check_ftir_data(bad_ftir),
    "has unexpected attributes.",
    fixed = TRUE
  )

  no_attr_ftir <- biodiesel
  attr(no_attr_ftir, "intensity") <- NULL
  expect_identical(
    attr(check_ftir_data(no_attr_ftir), "intensity"),
    "absorbance"
  )
})
