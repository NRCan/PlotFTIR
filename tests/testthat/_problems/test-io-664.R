# Extracted from test-io.R:664

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "PlotFTIR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
test_that("parses RRUFF header metadata into attributes", {
    temp_file <- withr::local_tempfile(fileext = ".csv")
    tmppath <- dirname(temp_file)
    tmpfile <- basename(temp_file)
    
    raman_content <- c(
      "##FILETYPE=Raman",
      "##RAMAN WAVELENGTH=532.0",
      "##TITLE=Graphite spectrum",
      "100, 1000",
      "200, 800",
      "300, 600"
    )
    
    writeLines(raman_content, temp_file)
    
    result <- read_raman(path = tmppath, file = tmpfile)
    
    expect_equal(colnames(result), c("wavenumber", "intensity", "sample_id"))
    expect_true("PlotFTIR_data" %in% class(result))
    expect_equal(attr(result, "intensity"), "raman")
    expect_equal(length(unique(result$sample_id)), 1)
  })
