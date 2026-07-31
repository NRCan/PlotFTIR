# Extracted from test-io.R:275

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "PlotFTIR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
if (!requireNamespace("readJDX", quietly = TRUE)) {
    expect_error(
      read_ftir_jdx(data.frame("testdata" = LETTERS)),
      regexp = "requires readJDX package installation for this function.",
      fixed = TRUE
    )
    testthat::skip("readJDX not available for testing interface")
  }
jdx_ftir <- read_ftir(system.file("extdata", "SBO.jdx", package = "readJDX"))
