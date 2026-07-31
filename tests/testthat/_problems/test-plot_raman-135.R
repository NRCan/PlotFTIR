# Extracted from test-plot_raman.R:135

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "PlotFTIR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
temp_file <- withr::local_tempfile(fileext = ".csv")
tmppath <- dirname(temp_file)
tmpfile <- basename(temp_file)
wn <- seq(100, 2000, by = 10)
intensity <- 100 * exp(-(wn - 500)^2 / 5000) + 50
raman_content <- c(
    "##FILETYPE=Raman",
    paste(wn, ",", intensity, sep = "")
  )
writeLines(raman_content, temp_file)
result <- read_raman(path = tmppath, file = tmpfile)
