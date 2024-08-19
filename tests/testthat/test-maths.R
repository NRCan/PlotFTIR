test_that("average_spectra works - balanced spectra", {
  # Mock data for FTIR spectra
  ftir_data <- data.frame(
    sample_id = c("A", "A", "B", "B", "C", "C"),
    wavenumber = c(1000, 1050, 1000, 1050, 1000, 1050),
    absorbance = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  )

  # Average spectra A and B
  average <- average_spectra(ftir_data, sample_ids = c("A", "B"), average_id = "average_AB")

  expect_equal(nrow(average), 2) # Expect 2 rows (wavenumbers)
  expect_equal(average$sample_id[1], "average_AB") # Expect new sample ID
  expect_equal(average$absorbance[average$wavenumber == 1000], 0.2) # Expect mean absorbance

  # Average only sample A
  average <- average_spectra(ftir_data, sample_ids = "A", average_id = "average_A")

  expect_equal(nrow(average), 2) # Expect 2 rows (wavenumbers)
  expect_equal(average$sample_id[1], "average_A") # Expect new sample ID
  expect_equal(average$absorbance, ftir_data[ftir_data$sample_id == "A", ]$absorbance) # Expect original data for A

  # Average all samples
  average <- average_spectra(ftir_data)

  expect_equal(nrow(average), 2) # Expect 3 rows (unique wavenumbers)
  expect_equal(average$sample_id[1], "averaged_spectra") # Expect default ID
  expect_equal(average$absorbance[average$wavenumber == 1000], 0.3) # Expect mean absorbance

  #Test Transmittance
  ftir_transmittance <- ftir_data
  colnames(ftir_transmittance)[3] <- 'transmittance'
  avg_trans <- average_spectra(ftir_transmittance)

  expect_equal(nrow(avg_trans), 2) # Expect 3 rows (unique wavenumbers)
  expect_equal(avg_trans$sample_id[1], "averaged_spectra") # Expect default ID
  expect_equal(avg_trans$transmittance[avg_trans$wavenumber == 1000], 0.3) # Expect mean absorbance
  expect_equal(average[,c('sample_id', 'wavenumber')], avg_trans[,c('sample_id', 'wavenumber')])
  expect_equal(average$absorbance, avg_trans$transmittance)

  expect_equal(nrow(average), 2) # Expect 3 rows (unique wavenumbers)
  expect_equal(average$sample_id[1], "averaged_spectra") # Expect default ID
  expect_equal(average$absorbance[average$wavenumber == 1000], 0.3) # Expect mean absorbance

  expect_error(average_spectra("not_a_data_frame"))

  expect_error(average_spectra(ftir_data[,c('wavenumber', 'absorbance')]), regexp = "is missing a column", fixed = TRUE)
  expect_error(average_spectra(ftir_data[,c('wavenumber', 'sample_id')]), regexp = "must have one of", fixed = TRUE)
  expect_error(average_spectra(ftir_data[,c('sample_id', 'absorbance')]), regexp = "is missing a column", fixed = TRUE)

  expect_error(average_spectra(ftir_data, sample_ids = "invalid_id"), regexp = "All provided", fixed = TRUE)

  expect_error(average_spectra(ftir_data, average_id = 10))

  ftir_data_mismatch <- ftir_data
  ftir_data_mismatch$wavenumber[ftir_data_mismatch$sample_id == "C"] <- c(1001, 1051)

  expect_warning(average_spectra(ftir_data_mismatch, sample_ids = c("A", "C")), regexp = "There is a mismatch in the wavenumber axis between sample_ids", fixed = TRUE)
})

test_that("average_spectra works unbalanced", {
  #Prep a non-balanced data set
  ftir_data <- biodiesel[biodiesel$sample_id %in% unique(biodiesel$sample_id)[1:3],]

  #sample 2 has different wavenumbers
  ftir_data[ftir_data$sample_id == unique(ftir_data$sample_id)[2],]$wavenumber <- ftir_data[ftir_data$sample_id == unique(ftir_data$sample_id)[2],]$wavenumber + 0.05

  # sample 3 has different wavenumber range
  ftir_data <- ftir_data[!(ftir_data$sample_id %in% unique(ftir_data$sample_id)[3] & ftir_data$wavenumber > 3500), ]
  ftir_data <- ftir_data[!(ftir_data$sample_id %in% unique(ftir_data$sample_id)[3] & ftir_data$wavenumber < 1000), ]

  expect_warning(average_spectra(ftir_data), regexp = "There is a mismatch in the wavenumber axis between sample_ids", fixed = TRUE)

  suppressWarnings(avg_123 <- average_spectra(ftir_data))
  suppressWarnings(avg_12 <- average_spectra(ftir_data, sample_ids = unique(ftir_data$sample_id)[1:2]))
  suppressWarnings(avg_13 <- average_spectra(ftir_data, sample_ids = unique(ftir_data$sample_id)[c(1,3)]))
  suppressWarnings(avg_23 <- average_spectra(ftir_data, sample_ids = unique(ftir_data$sample_id)[2:3]))
})
