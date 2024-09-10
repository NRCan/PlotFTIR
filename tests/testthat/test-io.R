test_that("reading csv works", {

  # Create a temporary CSV with wavenumber and absorbance columns
  data <- data.frame(wavenumber = 1000:1500, absorbance = biodiesel$absorbance[1:501])
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)
  write.csv(data, file = temp_file, row.names = FALSE)

  # Read the data using read_ftir
  result <- read_ftir(path = tmppath, file = tmpfile)

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile))
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$wavenumber)
  expect_equal(round(result$absorbance, 4), round(data$absorbance, 4))

  # Make sure single file paths work
  expect_equal(result, read_ftir(path = file.path(tmppath, tmpfile), file = NA))

  # Create a temporary CSV with misnamed wavenumber column
  data <- data.frame("row" = 1000:1500, absorbance = biodiesel$absorbance[1:501])
  write.csv(data, file = temp_file, row.names = FALSE)

  # Read the data using read_ftir
  expect_message(read_ftir(path = tmppath, file = tmpfile), regexp = "has deduced that input data", fixed = TRUE)
  suppressMessages(result <- read_ftir(path = tmppath, file = tmpfile))

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile))
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$row)
  expect_equal(round(result$absorbance, 2), round(data$absorbance, 2))

  #do it backwards
  data <- data.frame("absorbance" = data$absorbance, "row" = 1000:1500)
  write.csv(data, file = temp_file, row.names = FALSE)
  expect_equal(result$wavenumber, read_ftir(path = tmppath, file = tmpfile)$wavenumber)

  #do it with a name match
  data <- data.frame("energy" = 1000:1500, "absorbance" = data$absorbance)
  write.csv(data, file = temp_file, row.names = FALSE)
  expect_equal(result$wavenumber, read_ftir(path = tmppath, file = tmpfile)$wavenumber)

  # Create a temporary CSV with misnamed energy column (absorbance)
  data <- data.frame("wavenumber" = 1000:1500, "energy" = biodiesel$absorbance[1:501])
  write.csv(data, file = temp_file, row.names = FALSE)

  # Read the data using read_ftir
  expect_message(read_ftir(path = tmppath, file = tmpfile), regexp = "has deduced that input data", fixed = TRUE)
  suppressMessages(result <- read_ftir(path = tmppath, file = tmpfile))

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile))
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$wavenumber)
  expect_equal(round(result$absorbance, 4), round(data$energy, 4))

  # Create a temporary CSV with misnamed energy column (transmittance)
  data <- data.frame("wavenumber" = 1000:1500, "energy" = 100-(biodiesel$absorbance[1:501]*20))
  write.csv(data, file = temp_file, row.names = FALSE)

  # Read the data using read_ftir
  expect_message(read_ftir(path = tmppath, file = tmpfile), regexp = "has deduced that input data", fixed = TRUE)
  suppressMessages(result <- read_ftir(path = tmppath, file = tmpfile, sample_name = "test"))

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "transmittance", "sample_id"))
  expect_equal(result$sample_id[1], "test")
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$wavenumber)
  expect_equal(round(result$transmittance, 2), round(data$energy, 2))

  data <- data.frame("wavenumber" = 1000:1500, "absorbance" = biodiesel$absorbance[1:501], sample_id = "test")
  write.csv(data, file = temp_file, row.names = FALSE)
  expect_error(read_ftir(path = tmppath, file = tmpfile), regexp = "Input file has too many columns", fixed = TRUE)

  data <- data.frame("row" = 1000:1500, "col" = 2000:2500)
  write.csv(data, file = temp_file, row.names = FALSE)
  expect_error(read_ftir(path = tmppath, file = tmpfile), regexp = "Could not confidently determine which column contains wavenumber", fixed = TRUE)

})

test_that("read_ftir handles invalid arguments", {
  expect_error(read_ftir(path = NULL, file = "file.csv"), regexp = "must be a single string value", fixed = TRUE)
  expect_error(read_ftir(path = "path", file = NULL), regexp = "must be a single string value", fixed = TRUE)
  expect_error(read_ftir(path = c("path1", "path2"), file = "file.csv"), regexp = "must be a single string value", fixed = TRUE)
  expect_error(read_ftir(path = "path", file = c("file1.csv", "file2.csv")), regexp = "must be a single string value", fixed = TRUE)
  expect_error(read_ftir(path = ".", file = "file.csv", sample_name = c("name1", "name2")), regexp = "must be a single string value or single", fixed = TRUE)
  expect_error(read_ftir(path = ".", file = "file.csv", sample_name = 123), regexp = "must be a string value", fixed = TRUE)
  expect_error(read_ftir(path = ".", file = "nonexistent_file.csv"), regexp = 'nonexistent_file.csv" does not appear to exist', fixed = TRUE)
  tempfile <- withr::local_tempfile(fileext = ".docx")
  file.create(tempfile)
  expect_error(read_ftir(path = dirname(tempfile), file = basename(tempfile)), regexp = "could not be processed", fixed = TRUE)
  tempfile <- withr::local_tempfile(fileext = ".a2r")
  file.create(tempfile)
  expect_error(read_ftir(path = dirname(tempfile), file = basename(tempfile)), regexp = "PlotFTIR is not (yet) able to read .a2r files", fixed = TRUE)
  tempfile <- withr::local_tempfile(fileext = ".spc")
  file.create(tempfile)
  expect_error(read_ftir(path = dirname(tempfile), file = basename(tempfile)), regexp = "PlotFTIR is not (yet) able to read .spc files", fixed = TRUE)
})

test_that("reading asp works", {
  # create data and write file
  data <- data.frame(wavenumber = 1000:1500, absorbance = biodiesel$absorbance[1:501])
  temp_file <- withr::local_tempfile(fileext = ".asp")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)

  write(c(nrow(data), max(data$wavenumber), min(data$wavenumber), 1, 2, 4, rev(data$absorbance)), temp_file, ncolumns = 1)

  # Read the data using read_ftir
  expect_message(read_ftir(path = tmppath, file = tmpfile), regexp = "has deduced that input data", fixed = TRUE)
  suppressMessages(result <- read_ftir(path = tmppath, file = tmpfile))

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile))
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$wavenumber)
  expect_equal(round(result$absorbance, 2), round(data$absorbance, 2))

  # Create a temporary CSV with misnamed energy column (transmittance)
  data <- data.frame("wavenumber" = 1000:1500, "transmittance" = 100-(biodiesel$absorbance[1:501]*20))
  write(c(nrow(data), max(data$wavenumber), min(data$wavenumber), 1, 2, 4, rev(data$transmittance)), temp_file, ncolumns = 1)

  # Read the data using read_ftir
  expect_message(read_ftir(path = tmppath, file = tmpfile), regexp = "has deduced that input data", fixed = TRUE)
  suppressMessages(result <- read_ftir(path = tmppath, file = tmpfile, sample_name = "test"))

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "transmittance", "sample_id"))
  expect_equal(result$sample_id[1], "test")
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$wavenumber)
  expect_equal(round(result$transmittance, 2), round(data$transmittance, 2))
})

# Check reading multiple files
test_that("Reading multiple files works", {
  #Prep some files
  data <- data.frame(wavenumber = 1000:1500, absorbance = biodiesel$absorbance[1:501])
  tmppath <- withr::local_tempdir()
  temp_file1 <- withr::local_tempfile(tmpdir = tmppath, fileext = ".csv")
  temp_file2 <- withr::local_tempfile(tmpdir = tmppath, fileext = ".csv")
  tmpfile1 <- basename(temp_file1)
  tmpfile2 <- basename(temp_file2)
  write.csv(data, file = file.path(tmppath, tmpfile1), row.names = FALSE)
  write.csv(data, file = file.path(tmppath, tmpfile2), row.names = FALSE)

  # Read the data using read_ftir
  result <- read_ftir_directory(path = tmppath, files = c(tmpfile1, tmpfile2), sample_names = c("one", "two"))

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], "one")
  expect_equal(result$sample_id[nrow(result)], "two")
  expect_equal(nrow(result), nrow(data)*2)
  expect_equal(result$wavenumber, rep(data$wavenumber, 2))
  expect_equal(round(result$absorbance, 4), rep(round(data$absorbance, 4), 2))

  # Read the data using read_ftir
  result <- read_ftir_directory(path = tmppath, files = c(tmpfile1, tmpfile2))

  # Check the result (no sample names)
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile1))
  expect_equal(result$sample_id[nrow(result)], tools::file_path_sans_ext(tmpfile2))
  expect_equal(nrow(result), nrow(data)*2)
  expect_equal(result$wavenumber, rep(data$wavenumber, 2))
  expect_equal(round(result$absorbance, 4), rep(round(data$absorbance, 4), 2))


  # Checking for issues
  expect_error(suppressWarnings(read_ftir_directory(path = tmppath, files = c("fake.csv", "fake2.csv"))),
               regexp = "No spectral data was read from files", fixed = TRUE)
  expect_warning(read_ftir_directory(path = tmppath, files = c(tmpfile1, tmpfile2, "fake.csv")),
                 regexp = 'fake.csv" does not appear to exist', fixed = TRUE)
  suppressWarnings(result2 <- read_ftir_directory(path = tmppath, files = c(tmpfile1, tmpfile2, "fake.csv")))

  expect_equal(result, result2)

  expect_error(read_ftir_directory(path = tmppath, files = c(tmpfile1, tmpfile2), sample_names = c("One", "Two", "Extra")),
               regexp = "You provided 3 `sample_names` and 2 `files`", fixed = TRUE)

  expect_error(read_ftir_directory(path = c(tmppath, tmppath), files = c(tmpfile1, tmpfile2)),
               regexp = "must be a single string value", fixed = TRUE)
  expect_error(read_ftir_directory(path = tmppath, files = c(tmpfile1, as.data.frame(tmpfile2))),
               regexp = "must be a vector of string values", fixed = TRUE)
})

test_that("plot saves", {
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error(
      save_plot(123),
      "requires ggplot2 package installation",
      fixed = TRUE
    )
    testthat::skip("ggplot2 not available for testing file saving")
  }

  temp_file <- withr::local_tempfile(fileext = ".png")

  expect_false(file.exists(temp_file))
  save_plot(plot_ftir(biodiesel), filename = temp_file)
  expect_true(file.exists(temp_file))

  # test arg checks.
  expect_error(save_plot("abc", filename = temp_file),
               "`ftir_spectra_plot` must be a ggplot object. You provided a string",
               fixed = TRUE)
})
