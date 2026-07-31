# Extracted from test-maths.R:1640

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "PlotFTIR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
test_that("vector normalization produces unit norm for each sample", {
    temp_file <- withr::local_tempfile(fileext = ".csv")
    tmppath <- dirname(temp_file)
    tmpfile <- basename(temp_file)
    
    wn <- seq(100, 2000, by = 10)
    intensity <- c(100, rep(50, length(wn) - 2), 100)
    
    raman_content <- c(
      "##FILETYPE=Raman",
      paste(wn, ",", intensity, sep = "")
    )
    
    writeLines(raman_content, temp_file)
    
    result <- read_raman(path = tmppath, file = tmpfile)
    
    normalized <- normalize_raman(result, method = "vector")
    
    sample_data <- normalized[normalized$sample_id == tools::file_path_sans_ext(tmpfile), ]
    norm_val <- sqrt(sum(sample_data$intensity^2))
    
    expect_equal(round(norm_val, 6), 1)
    expect_equal(attr(normalized, "intensity"), "normalized raman")
  })
