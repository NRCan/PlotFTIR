# Extracted from test-maths.R:1572

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "PlotFTIR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
biodiesel_transmittance <- absorbance_to_transmittance(biodiesel)
biodiesel_absorbance <- transmittance_to_absorbance(biodiesel_transmittance)
expect_named(
    biodiesel_transmittance,
    c("wavenumber", "transmittance", "sample_id")
  )
expect_named(
    transmittance_to_absorbance(biodiesel_transmittance),
    c("wavenumber", "absorbance", "sample_id")
  )
expect_equal(attr(biodiesel_transmittance, "intensity"), "transmittance")
expect_equal(attr(biodiesel_absorbance, "intensity"), "absorbance")
biodiesel_normal <- normalize_spectra(biodiesel)
biodiesel_normal_trans <- absorbance_to_transmittance(biodiesel_normal)
expect_equal(
    attr(biodiesel_normal_trans, "intensity"),
    "normalized transmittance"
  )
expect_equal(
    attr(transmittance_to_absorbance(biodiesel_normal_trans), "intensity"),
    "normalized absorbance"
  )
expect_error(
    transmittance_to_absorbance(biodiesel),
    "`ftir` must be transmittance data or contain a `transmittance` column.",
    fixed = TRUE
  )
expect_error(
    absorbance_to_transmittance(absorbance_to_transmittance(biodiesel)),
    "`ftir` must be absorbance data or contain a `absorbance` column.",
    fixed = TRUE
  )
example_data <- data.frame(
    "wavenumber" = 1L,
    "absorbance" = c(0, .5, 1, 1.5, 2),
    "sample_id" = "test"
  )
expect_equal(
    absorbance_to_transmittance(example_data)$transmittance,
    c(100, 31.62278, 10, 3.162278, 1),
    tolerance = 1e-4
  )
example_data2 <- data.frame(
    "wavenumber" = 1L,
    "transmittance" = c(100, 50, 10, 5, 1),
    "sample_id" = "test"
  )
expect_equal(
    transmittance_to_absorbance(example_data2)$absorbance,
    c(0, 0.30103, 1, 1.30103, 2),
    tolerance = 1e-4
  )
example_data3 <- data.frame(
    "wavenumber" = 1L,
    "absorbance" = c(0, 0.5, 1, 1.5, 2),
    "sample_id" = "test",
    "transmittance" = c(100, 50, 10, 5, 1)
  )
expect_error(
    absorbance_to_transmittance(example_data3),
    "`ftir` cannot contain both `absorbance` and `transmittance` columns.",
    fixed = TRUE
  )
