test_that("reading csv works", {
  # Create a temporary CSV with wavenumber and absorbance columns
  data <- data.frame(wavenumber = 1000:1500, absorbance = runif(501))
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


  # Create a temporary CSV with misnamed wavenumber column
  data <- data.frame("row" = 1000:1500, absorbance = runif(501))
  write.csv(data, file = temp_file, row.names = FALSE)

  # Read the data using read_ftir
  expect_message(read_ftir(path = tmppath, file = tmpfile), regexp = "has deduced that input data", fixed = TRUE)
  suppressMessages(result <- read_ftir(path = tmppath, file = tmpfile))

  # Check the result
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile))
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$wavenumber, data$row)
  expect_equal(round(result$absorbance, 4), round(data$absorbance, 4))

  # Create a temporary CSV with misnamed energy column (absorbance)
  data <- data.frame("wavenumber" = 1000:1500, "energy" = runif(501))
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
  data <- data.frame("wavenumber" = 1000:1500, "energy" = runif(501, min = 80, max = 100))
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
})

test_that("read_ftir handles invalid arguments", {
  expect_error(read_ftir(path = NULL, file = "file.csv"), regexp = "must be a single string value", fixed = TRUE)
  expect_error(read_ftir(path = "path", file = NULL), regexp = "must be a single string value", fixed = TRUE)
  expect_error(read_ftir(path = c("path1", "path2"), file = "file.csv"), regexp = "must be a single string value", fixed = TRUE)
  expect_error(read_ftir(path = "path", file = c("file1.csv", "file2.csv")), regexp = "must be a single string value", fixed = TRUE)
  expect_error(read_ftir(path = ".", file = "file.csv", sample_name = c("name1", "name2")), regexp = "must be a single string value or single", fixed = TRUE)
  expect_error(read_ftir(path = ".", file = "file.csv", sample_name = 123), regexp = "must be a string value", fixed = TRUE)
  expect_error(read_ftir(path = ".", file = "nonexistent_file.csv"), regexp = "does not appear to exist", fixed = TRUE)
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
  data <- data.frame(wavenumber = 1000:1500, absorbance = runif(501))
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
  expect_equal(round(result$absorbance, 4), round(data$absorbance, 4))

  # Create a temporary CSV with misnamed energy column (transmittance)
  data <- data.frame("wavenumber" = 1000:1500, "transmittance" = runif(501, min = 80, max = 100))
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
