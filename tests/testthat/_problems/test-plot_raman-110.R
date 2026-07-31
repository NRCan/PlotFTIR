# Extracted from test-plot_raman.R:110

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "PlotFTIR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing")
  }
temp_file1 <- withr::local_tempfile(fileext = ".csv")
temp_file2 <- withr::local_tempfile(fileext = ".csv")
tmppath <- dirname(temp_file1)
wn <- seq(100, 2000, by = 10)
intensity1 <- 100 * exp(-(wn - 500)^2 / 5000) + 50
intensity2 <- 80 * exp(-(wn - 800)^2 / 6000) + 40
raman_content1 <- c(
    "##FILETYPE=Raman",
    paste(wn, ",", intensity1, sep = "")
  )
raman_content2 <- c(
    "##FILETYPE=Raman",
    paste(wn, ",", intensity2, sep = "")
  )
writeLines(raman_content1, temp_file1)
writeLines(raman_content2, temp_file2)
result1 <- read_raman(path = tmppath, file = basename(temp_file1), sample_name = "sample1")
