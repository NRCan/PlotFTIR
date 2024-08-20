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

  # For now, just trying to test performance is consistent
  expect_equal(avg_123$wavenumber[1], 1000.7902)
  expect_equal(avg_12$wavenumber[1], 700.7895)
  expect_equal(tail(avg_12$wavenumber, 1), 3997.6198)
  expect_equal(tail(avg_123$wavenumber, 1), 3498.1066)

  expect_equal(nrow(avg_123), 1341)
  expect_equal(nrow(avg_12), 1770)
  expect_equal(nrow(avg_23), 1341)
  expect_equal(nrow(avg_13), 1340)

  expect_equal(mean(avg_12$absorbance), 0.04881436, tolerance = 1e-7)
  expect_equal(mean(avg_13$absorbance), 0.05684917, tolerance = 1e-7)
  expect_equal(mean(avg_23$absorbance), 0.05677168, tolerance = 1e-7)
  expect_equal(mean(avg_123$absorbance), 0.05678947, tolerance = 1e-7)

})

test_that("add_subtract_scalar_value works", {
  # Mock data for FTIR spectra
  ftir_data <- data.frame(
    sample_id = c("A", "A", "B", "B", "C"),
    wavenumber = c(1000, 1050, 1000, 1050, 1100),
    absorbance = c(0.1, 0.2, 0.3, 0.4, 0.5)
  )

  # Add 0.5 to absorbance of all samples
  modified_data <- add_scalar_value(ftir_data, value = 0.5)

  expect_equal(nrow(modified_data), nrow(ftir_data))  # Expect same number of rows
  expect_equal(modified_data$wavenumber, ftir_data$wavenumber)  # Expect unchanged wavenumbers
  expect_equal(modified_data$absorbance, c(0.6, 0.7, 0.8, 0.9, 1.0))  # Expect modified absorbance

  # Add 1.0 to absorbance of samples A and B
  modified_data <- add_scalar_value(ftir_data, value = 1.0, sample_ids = c("A", "B"))

  expect_equal(modified_data$absorbance[modified_data$sample_id == "A"], c(1.1, 1.2))  # Expect modified absorbance for A
  expect_equal(modified_data$absorbance[modified_data$sample_id == "B"], c(1.3, 1.4))  # Expect modified absorbance for B
  expect_equal(modified_data$absorbance[modified_data$sample_id == "C"], 0.5)  # Expect unchanged absorbance for C

  expect_error(add_scalar_value(ftir_data, value = "invalid"), "Provided `value` must be numeric", fixed = TRUE)

  expect_error(add_scalar_value(ftir_data, value = 0.5, sample_ids = "invalid_id"), "All provided `sample_ids` must be in `ftir` data", fixed = TRUE)

  # Modify ftir_data to have transmittance instead of absorbance
  ftir_data_transmittance <- ftir_data
  ftir_data_transmittance$absorbance <- NULL
  ftir_data_transmittance$transmittance <- c(0.9, 0.8, 0.7, 0.6, 0.5)

  modified_data <- add_scalar_value(ftir_data_transmittance, value = 0.1)

  expect_equal(modified_data$transmittance, c(1.0, 0.9, 0.8, 0.7, 0.6))  # Expect modified transmittance

  modified_data <- subtract_scalar_value(ftir_data_transmittance, value = 0.1)
  expect_equal(modified_data$transmittance, c(0.8, 0.7, 0.6, 0.5, 0.4))
})


test_that("Baseline error checking works", {

})

test_that("Baseline - average works", {
  ftir_data <- data.frame(
    sample_id = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
    wavenumber = c(1000, 1025, 1050, 1000, 1025, 1050, 1000, 1025, 1050),
    absorbance = c(0.1, 0.2, 0.3, 0.2, 0.3, 0.4, 0.3, 0.4, 0.5)
  )

  recalculated_ftir <- recalculate_baseline(ftir_data, method = 'average', individually = TRUE)
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(recalculated_ftir[recalculated_ftir$sample_id == "A",]$absorbance, c(-0.1, 0, 0.1))
  expect_equal(recalculated_ftir[recalculated_ftir$sample_id == "B",]$absorbance, c(-0.1, 0, 0.1))

  recalculated_ftir <- recalculate_baseline(ftir_data, method = 'average', individually = FALSE)
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(recalculated_ftir[recalculated_ftir$sample_id == "A",]$absorbance, c(-0.1, 0, 0.1))
  expect_equal(recalculated_ftir[recalculated_ftir$sample_id == "B",]$absorbance, c(0, 0.1, 0.2))

  recalculated_ftir <- recalculate_baseline(ftir_data, method = 'average', wavenumber_range = c(1000, 1025), individually = TRUE)
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(recalculated_ftir[recalculated_ftir$sample_id == "A",]$absorbance, c(-0.05, 0.05, 0.15))
  expect_equal(recalculated_ftir[recalculated_ftir$sample_id == "B",]$absorbance, c(-0.05, 0.05, 0.15))

  recalculated_ftir <- recalculate_baseline(ftir_data, method = 'average', wavenumber_range = c(1000, 1025), individually = FALSE)
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(recalculated_ftir[recalculated_ftir$sample_id == "A",]$absorbance, c(-0.05, 0.05, 0.15))
  expect_equal(recalculated_ftir[recalculated_ftir$sample_id == "B",]$absorbance, c(0.05, 0.15, 0.25))

  recalculate_baseline(ftir_data, sample_ids = "A", method = 'average', individually = TRUE)
  recalculate_baseline(ftir_data, sample_ids = "A", method = 'average', individually = FALSE)

  recalculate_baseline(ftir_data, sample_ids = c("A", "B"), method = "average", individually = TRUE)
  recalculate_baseline(ftir_data, sample_ids = c("A", "B"), method = "average", individually = FALSE)
})

test_that("Baseline - point works", {
  ftir_data <- data.frame(
    sample_id = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
    wavenumber = c(1000, 1025, 1050, 1000, 1025, 1050, 1000, 1025, 1050),
    absorbance = c(0.1, 0.2, 0.3, 0.2, 0.3, 0.4, 0.3, 0.4, 0.5)
  )
})

test_that("Baseline - minimum/maximum works", {
  ftir_data <- data.frame(
    sample_id = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
    wavenumber = c(1000, 1025, 1050, 1000, 1025, 1050, 1000, 1025, 1050),
    absorbance = c(0.1, 0.2, 0.3, 0.2, 0.3, 0.4, 0.3, 0.4, 0.5)
  )
})
