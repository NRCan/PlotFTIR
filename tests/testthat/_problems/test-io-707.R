# Extracted from test-io.R:707

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
test_that("reads two-column wavenumber/intensity data correctly", {
    temp_file <- withr::local_tempfile(fileext = ".csv")
    tmppath <- dirname(temp_file)
    tmpfile <- basename(temp_file)
    
    wn <- seq(100, 2000, by = 10)
    intensity <- 100 * exp(-(wn - 500)^2 / 5000) + 50
    data_lines <- paste(wn, ",", intensity, sep = "")
    
    raman_content <- c(
      "##FILETYPE=Raman",
      data_lines
    )
    
    writeLines(raman_content, temp_file)
    
    result <- read_raman(path = tmppath, file = tmpfile)
    
    expect_equal(nrow(result), length(wn))
    expect_equal(result$wavenumber, wn)
    expect_equal(round(result$intensity, 4), round(intensity, 4))
  })
test_that("sample_name parameter overrides filename-derived name", {
    temp_file <- withr::local_tempfile(fileext = ".csv")
    tmppath <- dirname(temp_file)
    tmpfile <- basename(temp_file)
    
    raman_content <- c(
      "##FILETYPE=Raman",
      "100, 1000"
    )
    
    writeLines(raman_content, temp_file)
    
    result <- read_raman(path = tmppath, file = tmpfile, sample_name = "custom_sample")
    
    expect_equal(result$sample_id[1], "custom_sample")
  })
