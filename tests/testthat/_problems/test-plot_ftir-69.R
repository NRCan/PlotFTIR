# Extracted from test-plot_ftir.R:69

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "PlotFTIR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing manipulations")
  }
full_data_df <- data.frame(
    "sample_id" = LETTERS,
    "wavenumber" = seq_along(LETTERS),
    "absorbance" = runif(length(LETTERS)),
    "transmittance" = runif(length(LETTERS)) * 100
  )
expect_error(
    plot_ftir(ftir = "abc"),
    "`ftir` must be a data frame. You provided a string."
  )
expect_error(
    plot_ftir(ftir = data.frame("a" = 1:10)),
    "It must contain a column named",
    fixed = TRUE
  )
expect_error(
    plot_ftir(ftir = full_data_df[, c("sample_id", "wavenumber")]),
    "Error in `PlotFTIR::plot_ftir()`. `ftir` must have one of `absorbance` or `transmittance` columns.",
    fixed = TRUE
  )
