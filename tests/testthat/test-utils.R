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
  p <- plot_ftir(biodiesel)

  expect_equal(get_plot_sample_ids(p), as.factor(unique(biodiesel$sample_id)))

  expect_error(get_plot_sample_ids(biodiesel),
    "`ftir_spectra_plot` must be a ggplot object. You provided ",
    fixed = TRUE
  )
})
