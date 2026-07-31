# Extracted from test-io.R:586

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "PlotFTIR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
if (!requireNamespace("R.utils", quietly = TRUE)) {
    expect_error(
      plotftir_to_chemospec(biodiesel),
      regexp = "requires R.utils package installation for this function.",
      fixed = TRUE
    )
    testthat::skip("R.utils not available for testing interface")
  }
if (!requireNamespace("ChemoSpec", quietly = TRUE)) {
    expect_error(
      chemospec_to_plotftir(data.frame("testdata" = LETTERS)),
      regexp = "requires ChemoSpec package installation for this function.",
      fixed = TRUE
    )
    expect_error(
      plotftir_to_chemospec(biodiesel),
      regexp = "requires ChemoSpec package installation for this function.",
      fixed = TRUE
    )
    testthat::skip("ChemoSpec not available for testing interface")
  }
data("SrE.IR", package = "ChemoSpec", envir = environment())
data("SrE.NMR", package = "ChemoSpec", envir = environment())
expect_error(
    chemospec_to_plotftir(SrE.NMR),
    regexp = "must be of IR spectra, this data appears to be from another instrument.",
    fixed = TRUE
  )
expect_error(
    chemospec_to_plotftir(data.frame("A" = LETTERS)),
    regexp = "must be of class <Spectra>, produced by the ChemoSpec package. You provided ",
    fixed = TRUE
  )
csftir <- chemospec_to_plotftir(SrE.IR)
