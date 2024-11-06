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

  # do it backwards
  data <- data.frame("absorbance" = data$absorbance, "row" = 1000:1500)
  write.csv(data, file = temp_file, row.names = FALSE)
  expect_equal(result$wavenumber, read_ftir(path = tmppath, file = tmpfile)$wavenumber)

  # do it with a name match
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
  data <- data.frame("wavenumber" = 1000:1500, "energy" = 100 - (biodiesel$absorbance[1:501] * 20))
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
  data <- data.frame("wavenumber" = 1000:1500, "transmittance" = 100 - (biodiesel$absorbance[1:501] * 20))
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
  # Prep some files
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
  expect_equal(nrow(result), nrow(data) * 2)
  expect_equal(result$wavenumber, rep(data$wavenumber, 2))
  expect_equal(round(result$absorbance, 4), rep(round(data$absorbance, 4), 2))

  # Read the data using read_ftir
  result <- read_ftir_directory(path = tmppath, files = c(tmpfile1, tmpfile2))

  # Check the result (no sample names)
  expect_equal(colnames(result), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(result$sample_id[1], tools::file_path_sans_ext(tmpfile1))
  expect_equal(result$sample_id[nrow(result)], tools::file_path_sans_ext(tmpfile2))
  expect_equal(nrow(result), nrow(data) * 2)
  expect_equal(result$wavenumber, rep(data$wavenumber, 2))
  expect_equal(round(result$absorbance, 4), rep(round(data$absorbance, 4), 2))


  # Checking for issues
  expect_error(suppressWarnings(read_ftir_directory(path = tmppath, files = c("fake.csv", "fake2.csv"))),
    regexp = "No spectral data was read from files", fixed = TRUE
  )
  expect_warning(read_ftir_directory(path = tmppath, files = c(tmpfile1, tmpfile2, "fake.csv")),
    regexp = 'fake.csv" does not appear to exist', fixed = TRUE
  )
  suppressWarnings(result2 <- read_ftir_directory(path = tmppath, files = c(tmpfile1, tmpfile2, "fake.csv")))

  expect_equal(result, result2)

  expect_error(read_ftir_directory(path = tmppath, files = c(tmpfile1, tmpfile2), sample_names = c("One", "Two", "Extra")),
    regexp = "You provided 3 `sample_names` and 2 `files`", fixed = TRUE
  )

  expect_error(read_ftir_directory(path = c(tmppath, tmppath), files = c(tmpfile1, tmpfile2)),
    regexp = "must be a single string value", fixed = TRUE
  )
  expect_error(read_ftir_directory(path = tmppath, files = c(tmpfile1, as.data.frame(tmpfile2))),
    regexp = "must be a vector of string values", fixed = TRUE
  )
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
    fixed = TRUE
  )
})

test_that("interface to ir is ok", {
  if (!requireNamespace("ir", quietly = TRUE)) {
    expect_error(ir_to_plotftir(data.frame("testdata" = LETTERS)), regexp = "requires ir package installation for this function.", fixed = TRUE)
    expect_error(ir_to_df(data.frame("testdata" = LETTERS)), regexp = "requires ir package installation for this function.", fixed = TRUE)
    expect_error(plotftir_to_ir(biodiesel), regexp = "requires ir package installation for this function.", fixed = TRUE)
    testthat::skip("ir not available for testing interface")
  }

  irdata <- ir::ir_sample_data
  # Param checks
  expect_error(ir_to_plotftir(biodiesel), regexp = "must be of class <ir>, produced by the ir package.", fixed = TRUE)
  expect_error(ir_to_plotftir(irdata, what = c(1, "two")), regexp = "must contain the row numbers of sample spectra to extract, or exact names matching what is in `ir_data$id_sample`", fixed = TRUE)
  expect_error(ir_to_plotftir(irdata, what = c(1, 1e6)), regexp = "must contain the row numbers of sample spectra to extract, or exact names matching what is in `ir_data$id_sample`", fixed = TRUE)

  allir <- ir_to_plotftir(irdata)
  expect_equal(length(unique(allir$sample_id)), nrow(irdata))
  expect_equal(colnames(allir), c("wavenumber", "absorbance", "sample_id"))

  irnum <- ir_to_plotftir(irdata, what = c(1:5))
  expect_equal(length(unique(irnum$sample_id)), 5)

  irname <- ir_to_plotftir(irdata, what = c("GN 11-389", "GN 11-400", "GN 11-407"))
  expect_equal(length(unique(irname$sample_id)), 3)

  plotir <- plotftir_to_ir(biodiesel)

  expect_equal(nrow(plotir), length(unique(biodiesel$sample_id)))

  plotirmeta <- plotftir_to_ir(biodiesel, data.frame("biodiesel_content" = c(0, 0.25, 0.5, 1, 2.5, 5, 7.5, 10, 0.5, 5, NA)))

  expect_equal(nrow(plotirmeta), length(unique(biodiesel$sample_id)))
  expect_true("biodiesel_content" %in% colnames(plotirmeta))

  expect_true("ggplot" %in% suppressWarnings(class(plot_ftir(irdata))))
})

test_that("Interface to ChemoSpec is ok", {
  if(!requireNamespace("R.utils", quietly = TRUE)) {
    expect_error(plotftir_to_chemospec(biodiesel), regexp = "requires R.utils package installation for this function.", fixed = TRUE)
    testthat::skip("R.utils not available for testing interface")
  }

  if (!requireNamespace("ChemoSpec", quietly = TRUE)) {
    expect_error(chemospec_to_plotftir(data.frame("testdata" = LETTERS)), regexp = "requires ChemoSpec package installation for this function.", fixed = TRUE)
    expect_error(plotftir_to_chemospec(biodiesel), regexp = "requires ChemoSpec package installation for this function.", fixed = TRUE)
    testthat::skip("ChemoSpec not available for testing interface")
  }

  data("SrE.IR", package = "ChemoSpec", envir = environment())
  data("SrE.NMR", package = "ChemoSpec", envir = environment())

  expect_error(chemospec_to_plotftir(SrE.NMR), regexp = "must be of IR spectra, this data appears to be from another instrument.", fixed = TRUE)
  expect_error(chemospec_to_plotftir(data.frame("A" = LETTERS)), regexp = "must be of class <Spectra>, produced by the ChemoSpec package. You provided ", fixed = TRUE)

  csftir <- chemospec_to_plotftir(SrE.IR)

  expect_equal(colnames(csftir), c("wavenumber", "absorbance", "sample_id"))
  expect_equal(length(unique(csftir$sample_id)), length(SrE.IR$names))

  expect_error(plotftir_to_chemospec(biodiesel, group_colours = "blue"), regexp = ", or a vector of the same length as group_crit", fixed = TRUE)
  expect_error(plotftir_to_chemospec(biodiesel, group_crit = c("biodiesel", "unknown"), group_colours = c("orange", "green", "blue")), regexp = ", or a vector of the same length as group_crit", fixed = TRUE)

  expect_message(plotftir_to_chemospec(biodiesel), regexp = " to ensure enough colours available for groups.", fixed = TRUE)
  expect_error(plotftir_to_chemospec(rbind(biodiesel, sample_spectra)), regexp = " has to make 12 or less groups for ChemoSpec to be happy", fixed = TRUE)

  csdata <- plotftir_to_chemospec(biodiesel, group_crit = c("biodiesel", "unknown"))

  expect_equal(class(csdata), "Spectra")
  expect_type(csdata$data, 'double')

  csdata2 <- chemospec_to_plotftir(csdata)

  expect_equal(csdata2[csdata2$sample_id == "biodiesel_0",], biodiesel[biodiesel$sample_id == "biodiesel_0",], ignore_attr = TRUE)

  expect_true("ggplot" %in% suppressWarnings(class(plot_ftir(SrE.IR))))
})
