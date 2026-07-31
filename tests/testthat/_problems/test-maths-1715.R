# Extracted from test-maths.R:1715

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
test_that("max normalization sets max intensity to 1 for each sample", {
    temp_file <- withr::local_tempfile(fileext = ".csv")
    tmppath <- dirname(temp_file)
    tmpfile <- basename(temp_file)
    
    wn <- seq(100, 2000, by = 10)
    intensity <- c(50, rep(100, length(wn) - 2), 60)
    
    raman_content <- c(
      "##FILETYPE=Raman",
      paste(wn, ",", intensity, sep = "")
    )
    
    writeLines(raman_content, temp_file)
    
    result <- read_raman(path = tmppath, file = tmpfile)
    
    normalized <- normalize_raman(result, method = "max")
    
    max_intensity <- max(normalized$intensity)
    
    expect_equal(max_intensity, 1)
    expect_equal(attr(normalized, "intensity"), "normalized raman")
  })
test_that("normalization updates attribute to 'normalized raman'", {
    temp_file <- withr::local_tempfile(fileext = ".csv")
    tmppath <- dirname(temp_file)
    tmpfile <- basename(temp_file)
    
    wn <- seq(100, 2000, by = 10)
    intensity <- rep(100, length(wn))
    
    raman_content <- c(
      "##FILETYPE=Raman",
      paste(wn, ",", intensity, sep = "")
    )
    
    writeLines(raman_content, temp_file)
    
    result <- read_raman(path = tmppath, file = tmpfile)
    
    expect_equal(attr(result, "intensity"), "raman")
    
    normalized <- normalize_raman(result)
    
    expect_equal(attr(normalized, "intensity"), "normalized raman")
  })
test_that("normalize_raman validates method parameter", {
    temp_file <- withr::local_tempfile(fileext = ".csv")
    tmppath <- dirname(temp_file)
    tmpfile <- basename(temp_file)
    
    wn <- seq(100, 2000, by = 10)
    intensity <- rep(100, length(wn))
    
    raman_content <- c(
      "##FILETYPE=Raman",
      paste(wn, ",", intensity, sep = "")
    )
    
    writeLines(raman_content, temp_file)
    
    result <- read_raman(path = tmppath, file = tmpfile)
    
    expect_error(
      normalize_raman(result, method = "invalid"),
      regexp = "must be one of",
      fixed = TRUE
    )
  })
