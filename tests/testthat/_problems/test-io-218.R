# Extracted from test-io.R:218

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "PlotFTIR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data <- data.frame(
    wavenumber = 1000:1500,
    absorbance = biodiesel$absorbance[1:501]
  )
temp_file <- withr::local_tempfile(fileext = ".asp")
tmppath <- dirname(temp_file)
tmpfile <- basename(temp_file)
write(
    c(
      nrow(data),
      max(data$wavenumber),
      min(data$wavenumber),
      1,
      2,
      4,
      rev(data$absorbance)
    ),
    temp_file,
    ncolumns = 1
  )
expect_message(
    read_ftir(path = tmppath, file = tmpfile),
    regexp = "has deduced that input data",
    fixed = TRUE
  )
