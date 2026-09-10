test_that("average_spectra works - balanced spectra", {
  # Mock data for FTIR spectra
  ftir_data <- data.frame(
    sample_id = c("A", "A", "B", "B", "C", "C"),
    wavenumber = c(1000, 1050, 1000, 1050, 1000, 1050),
    absorbance = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  )

  # Average spectra A and B
  average <- average_spectra(
    ftir_data,
    sample_ids = c("A", "B"),
    average_id = "average_AB"
  )
  expect_equal(nrow(average), 2) # Expect 2 rows (wavenumbers)
  expect_equal(average$sample_id[1], "average_AB") # Expect new sample ID
  expect_equal(average$absorbance[average$wavenumber == 1000], 0.2) # Expect mean absorbance
  expect_equal(attr(average, "intensity"), "absorbance")

  # Average only sample A
  average <- average_spectra(
    ftir_data,
    sample_ids = "A",
    average_id = "average_A"
  )
  expect_equal(nrow(average), 2) # Expect 2 rows (wavenumbers)
  expect_equal(average$sample_id[1], "average_A") # Expect new sample ID
  expect_equal(
    average$absorbance,
    ftir_data[ftir_data$sample_id == "A", ]$absorbance
  ) # Expect original data for A
  expect_equal(attr(average, "intensity"), "absorbance")

  # Average all samples
  average <- average_spectra(ftir_data)
  expect_equal(nrow(average), 2) # Expect 3 rows (unique wavenumbers)
  expect_equal(average$sample_id[1], "averaged_spectra") # Expect default ID
  expect_equal(average$absorbance[average$wavenumber == 1000], 0.3) # Expect mean absorbance
  expect_equal(attr(average, "intensity"), "absorbance")

  # Test Transmittance
  ftir_transmittance <- ftir_data
  colnames(ftir_transmittance)[3] <- "transmittance"
  avg_trans <- average_spectra(ftir_transmittance)

  expect_equal(nrow(avg_trans), 2) # Expect 3 rows (unique wavenumbers)
  expect_equal(avg_trans$sample_id[1], "averaged_spectra") # Expect default ID
  expect_equal(avg_trans$transmittance[avg_trans$wavenumber == 1000], 0.3) # Expect mean absorbance
  expect_equal(
    average[, c("sample_id", "wavenumber")],
    avg_trans[, c("sample_id", "wavenumber")]
  )
  expect_equal(average$absorbance, avg_trans$transmittance)
  expect_equal(attr(avg_trans, "intensity"), "transmittance")
  expect_equal(nrow(average), 2) # Expect 3 rows (unique wavenumbers)
  expect_equal(average$sample_id[1], "averaged_spectra") # Expect default ID
  expect_equal(average$absorbance[average$wavenumber == 1000], 0.3) # Expect mean absorbance

  expect_error_bilingual(
    average_spectra("not_a_data_frame"),
    en = "must be a data frame",
    fr = "doit être un data.frame"
  )
  expect_error_bilingual(
    average_spectra(ftir_data[, c("wavenumber", "absorbance")]),
    en = "is missing a column. It must contain a column named `sample_id`",
    fr = "ne contient pas une colonne. Il doit contenir une colonne nommée `sample_id`"
  )
  expect_error_bilingual(
    average_spectra(ftir_data[, c("wavenumber", "sample_id")]),
    en = "must have one of `absorbance`, `transmittance`, or `intensity` columns",
    fr = "doit contenir une des colonnes `absorbance`, `transmittance`, ou `intensity`"
  )
  expect_error_bilingual(
    average_spectra(ftir_data[, c("sample_id", "absorbance")]),
    en = "is missing a column. It must contain a column named `wavenumber`",
    fr = "ne contient pas une colonne. Il doit contenir une colonne nommée `wavenumber`"
  )

  expect_error_bilingual(
    average_spectra(ftir_data, sample_ids = "invalid_id"),
    en = "All provided `sample_ids` must be in `ftir` data.",
    fr = "Tous les `sample_ids` fournis doivent être dans les données `ftir`."
  )

  expect_error_bilingual(
    average_spectra(ftir_data, average_id = 10),
    en = "`average_id` must be a character value",
    fr = "`average_id` doit être une valeur de chaîne"
  )

  ftir_data_mismatch <- ftir_data
  ftir_data_mismatch$wavenumber[ftir_data_mismatch$sample_id == "C"] <- c(
    1001,
    1051
  )

  expect_warning_bilingual(
    average_spectra(ftir_data_mismatch, sample_ids = c("A", "C")),
    en = "There is a mismatch in the wavenumber axis between sample_ids.",
    fr = "Il y a un écart dans l'axe du nombre d'ondes entre les sample_ids."
  )
})

test_that("average_spectra works unbalanced", {
  # Prep a non-balanced data set
  ftir_data <- biodiesel[
    biodiesel$sample_id %in% unique(biodiesel$sample_id)[1:3],
  ]

  # sample 2 has different wavenumbers
  ftir_data[
    ftir_data$sample_id == unique(ftir_data$sample_id)[2],
  ]$wavenumber <- ftir_data[
    ftir_data$sample_id == unique(ftir_data$sample_id)[2],
  ]$wavenumber +
    0.05

  # sample 3 has different wavenumber range
  ftir_data <- ftir_data[
    !(ftir_data$sample_id %in%
      unique(ftir_data$sample_id)[3] &
      ftir_data$wavenumber > 3500),
  ]
  ftir_data <- ftir_data[
    !(ftir_data$sample_id %in%
      unique(ftir_data$sample_id)[3] &
      ftir_data$wavenumber < 1000),
  ]

  expect_warning_bilingual(
    average_spectra(ftir_data),
    en = "There is a mismatch in the wavenumber axis between sample_ids.",
    fr = "Il y a un écart dans l'axe du nombre d'ondes entre les sample_ids."
  )

  suppressWarnings(avg_123 <- average_spectra(ftir_data))
  suppressWarnings(
    avg_12 <- average_spectra(
      ftir_data,
      sample_ids = unique(ftir_data$sample_id)[1:2]
    )
  )
  suppressWarnings(
    avg_13 <- average_spectra(
      ftir_data,
      sample_ids = unique(ftir_data$sample_id)[c(1, 3)]
    )
  )
  suppressWarnings(
    avg_23 <- average_spectra(
      ftir_data,
      sample_ids = unique(ftir_data$sample_id)[2:3]
    )
  )

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

  expect_equal(attr(avg_12, "intensity"), "absorbance")
  expect_equal(attr(avg_13, "intensity"), "absorbance")
  expect_equal(attr(avg_23, "intensity"), "absorbance")
  expect_equal(attr(avg_123, "intensity"), "absorbance")
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

  expect_equal(nrow(modified_data), nrow(ftir_data)) # Expect same number of rows
  expect_equal(modified_data$wavenumber, ftir_data$wavenumber) # Expect unchanged wavenumbers
  expect_equal(modified_data$absorbance, c(0.6, 0.7, 0.8, 0.9, 1.0)) # Expect modified absorbance
  expect_equal(attr(modified_data, "intensity"), "absorbance")

  # Add 1.0 to absorbance of samples A and B
  modified_data <- add_scalar_value(
    ftir_data,
    value = 1.0,
    sample_ids = c("A", "B")
  )

  expect_equal(
    modified_data$absorbance[modified_data$sample_id == "A"],
    c(1.1, 1.2)
  ) # Expect modified absorbance for A
  expect_equal(
    modified_data$absorbance[modified_data$sample_id == "B"],
    c(1.3, 1.4)
  ) # Expect modified absorbance for B
  expect_equal(modified_data$absorbance[modified_data$sample_id == "C"], 0.5) # Expect unchanged absorbance for C

  expect_error_bilingual(
    add_scalar_value(ftir_data, value = "invalid"),
    en = "must be numeric",
    fr = "doit être numérique"
  )
  expect_error_bilingual(
    subtract_scalar_value(ftir_data, value = "invalid"),
    en = "must be numeric",
    fr = "doit être numérique"
  )

  expect_error_bilingual(
    add_scalar_value(ftir_data, value = 0.5, sample_ids = "invalid_id"),
    en = "All provided `sample_ids` must be in `ftir` data.",
    fr = "Tous les `sample_ids` fournis doivent être dans les données `ftir`."
  )

  expect_equal(attr(modified_data, "intensity"), "absorbance")

  # Modify ftir_data to have transmittance instead of absorbance
  ftir_data_transmittance <- ftir_data
  ftir_data_transmittance$absorbance <- NULL
  ftir_data_transmittance$transmittance <- c(90, 80, 70, 60, 50)

  modified_data <- add_scalar_value(ftir_data_transmittance, value = 1)

  expect_equal(modified_data$transmittance, c(91, 81, 71, 61, 51)) # Expect modified transmittance
  expect_equal(attr(modified_data, "intensity"), "transmittance")

  modified_data <- subtract_scalar_value(ftir_data_transmittance, value = 1)
  expect_equal(modified_data$transmittance, c(89, 79, 69, 59, 49))
  expect_equal(attr(modified_data, "intensity"), "transmittance")
})


test_that("Baseline error checking works", {
  expect_error_bilingual(
    recalculate_baseline("not_a_dataframe"),
    en = "must be a data frame",
    fr = "doit être un data.frame"
  )

  expect_error_bilingual(
    recalculate_baseline(biodiesel, method = "failure"),
    en = "`method` must be a string.",
    fr = "`method` doit être une chaîne de caractères."
  )

  expect_error_bilingual(
    recalculate_baseline(biodiesel, individually = "failure"),
    en = "`individually` must be a boolean value",
    fr = "`individually` doit être une valeur booléenne"
  )

  expect_error_bilingual(
    recalculate_baseline(biodiesel, sample_ids = "A"),
    en = "All provided `sample_ids` must be in `ftir` data.",
    fr = "Tous les `sample_ids` fournis doivent être dans les données `ftir`."
  )

  expect_error_bilingual(
    recalculate_baseline(biodiesel, wavenumber_range = c(1, 2, 3)),
    en = "`wavenumber_range` must be of length 1 or 2",
    fr = "`wavenumber_range` doit être d'une longueur de 1 ou 2"
  )

  expect_error_bilingual(
    recalculate_baseline(biodiesel, wavenumber_range = c("one", "two")),
    en = "`wavenumber_range` must be `numeric` or `NA`.",
    fr = "`wavenumber_range` doit être `numeric` ou `NA`."
  )

  expect_error_bilingual(
    recalculate_baseline(
      biodiesel,
      method = "point",
      wavenumber_range = c(1, 2)
    ),
    en = "`wavenumber_range` must be one numeric value if `method = 'point'`.",
    fr = "`wavenumber_range` doit être une valeur numérique si `method = 'point'`."
  )
  expect_error_bilingual(
    recalculate_baseline(biodiesel, method = "point", wavenumber_range = NA),
    en = "`wavenumber_range` must be a single numeric value.",
    fr = "`wavenumber_range` doit être une seule valeur numérique."
  )

  expect_error_bilingual(
    recalculate_baseline(biodiesel, method = "minimum", wavenumber_range = 1),
    en = "`wavenumber_range` must be `NA` or two numeric values if `method = 'minimum'`.",
    fr = "`wavenumber_range` doit être `NA` ou deux valeurs numériques si `method = 'minimum'`."
  )
  expect_error_bilingual(
    recalculate_baseline(biodiesel, method = "maximum", wavenumber_range = 1),
    en = "`wavenumber_range` must be `NA` or two numeric values if `method = 'maximum'`.",
    fr = "`wavenumber_range` doit être `NA` ou deux valeurs numériques si `method = 'maximum'`."
  )

  expect_error(
    recalculate_baseline(
      biodiesel,
      method = "average",
      wavenumber_range = 1500
    ),
    regexp = "must be two numeric values",
    fixed = TRUE
  )
})

test_that("Baseline - average works", {
  ftir_data <- data.frame(
    sample_id = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
    wavenumber = c(1000, 1025, 1050, 1000, 1025, 1050, 1000, 1025, 1050),
    absorbance = c(0.1, 0.2, 0.3, 0.2, 0.3, 0.4, 0.3, 0.4, 0.5)
  )

  expect_warning_bilingual(
    recalculate_baseline(ftir_data, method = "average", individually = TRUE),
    en = "Adjusting spectra baseline by the average of all values is not analytically useful",
    fr = "Ajuster la ligne de base du spectre par la moyenne de toutes les valeurs n'est pas analytiquement utile."
  )
  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      method = "average",
      individually = TRUE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(-0.1, 0, 0.1)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(-0.1, 0, 0.1)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      method = "average",
      individually = FALSE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(-0.1, 0, 0.1)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "average",
    wavenumber_range = c(1000, 1025),
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(-0.05, 0.05, 0.15)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(-0.05, 0.05, 0.15)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$absorbance,
    c(-0.05, 0.05, 0.15)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "average",
    wavenumber_range = c(1000, 1025),
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(-0.05, 0.05, 0.15)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.05, 0.15, 0.25)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$absorbance,
    c(0.15, 0.25, 0.35)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "average",
      individually = TRUE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(-0.1, 0.00, 0.1)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.2, 0.3, 0.4)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "average",
      individually = FALSE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(-0.1, 0.0, 0.1)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.2, 0.3, 0.4)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "average",
      individually = TRUE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(-0.1, 0.0, 0.1)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(-0.1, 0.0, 0.1)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$absorbance,
    c(0.3, 0.4, 0.5)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "average",
      individually = FALSE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(-0.1, 0.0, 0.1)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$absorbance,
    c(0.3, 0.4, 0.5)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  ftir_data$transmittance <- c(100, 90, 80, 90, 80, 70, 80, 70, 60)
  ftir_data$absorbance <- NULL

  expect_warning_bilingual(
    recalculate_baseline(ftir_data, method = "average", individually = TRUE),
    en = "Adjusting spectra baseline by the average of all values is not analytically useful",
    fr = "Ajuster la ligne de base du spectre par la moyenne de toutes les valeurs n'est pas analytiquement utile."
  )
  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      method = "average",
      individually = TRUE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(110, 100, 90)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(110, 100, 90)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      method = "average",
      individually = FALSE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(110, 100, 90)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "average",
    wavenumber_range = c(1000, 1025),
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(105, 95, 85)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(105, 95, 85)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$transmittance,
    c(105, 95, 85)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "average",
    wavenumber_range = c(1000, 1025),
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(105, 95, 85)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(95, 85, 75)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$transmittance,
    c(85, 75, 65)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "average",
      individually = TRUE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(110, 100, 90)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(90, 80, 70)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "average",
      individually = FALSE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(110, 100, 90)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(90, 80, 70)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "average",
      individually = TRUE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(110, 100, 90)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(110, 100, 90)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$transmittance,
    c(80, 70, 60)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  suppressWarnings(
    recalculated_ftir <- recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "average",
      individually = FALSE
    )
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(110, 100, 90)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$transmittance,
    c(80, 70, 60)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")
})

test_that("Baseline - point works", {
  ftir_data <- data.frame(
    sample_id = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
    wavenumber = c(1000, 1025, 1050, 1000, 1025, 1050, 1000, 1025, 1050),
    absorbance = c(0.1, 0.2, 0.3, 0.2, 0.3, 0.4, 0.3, 0.4, 0.5)
  )

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "point",
    wavenumber_range = 1000,
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  suppressWarnings(expect_warning_bilingual(
    recalculate_baseline(
      ftir_data,
      method = "point",
      wavenumber_range = 500,
      individually = TRUE
    ),
    en = "Provided wavenumber is not within spectral range.",
    fr = "La fréquence fournie n'est pas comprise dans la plage spectrale."
  ))

  suppressWarnings(expect_warning_bilingual(
    recalculate_baseline(
      ftir_data,
      method = "point",
      wavenumber_range = 1012.5,
      individually = TRUE
    ),
    en = "No wavenumber values in spectra within 10 cm-1 of supplied point.",
    fr = "Aucune valeur de fréquence dans le spectre à 10 cm-1 du point fourni."
  ))

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "point",
    wavenumber_range = 1000,
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.1, 0.2, 0.3)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = "A",
    method = "point",
    wavenumber_range = 1000,
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.2, 0.3, 0.4)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = "A",
    method = "point",
    wavenumber_range = 1000,
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.2, 0.3, 0.4)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    method = "point",
    wavenumber_range = 1000,
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$absorbance,
    c(0.3, 0.4, 0.5)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    method = "point",
    wavenumber_range = 1000,
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.1, 0.2, 0.3)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$absorbance,
    c(0.3, 0.4, 0.5)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  ftir_data$transmittance <- c(90, 80, 70, 80, 70, 60, 70, 60, 50)
  ftir_data$absorbance <- NULL

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "point",
    wavenumber_range = 1000,
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "point",
    wavenumber_range = 1000,
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(90, 80, 70)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = "A",
    method = "point",
    wavenumber_range = 1000,
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(80, 70, 60)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = "A",
    method = "point",
    wavenumber_range = 1000,
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(80, 70, 60)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    method = "point",
    wavenumber_range = 1000,
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$transmittance,
    c(70, 60, 50)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    method = "point",
    wavenumber_range = 1000,
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(90, 80, 70)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$transmittance,
    c(70, 60, 50)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")
})

test_that("Baseline - minimum/maximum works", {
  ftir_data <- data.frame(
    sample_id = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
    wavenumber = c(1000, 1025, 1050, 1000, 1025, 1050, 1000, 1025, 1050),
    absorbance = c(0.1, 0.2, 0.3, 0.2, 0.3, 0.4, 0.3, 0.4, 0.5)
  )

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "minimum",
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculate_baseline(ftir_data, method = "minimum", individually = TRUE),
    recalculate_baseline(ftir_data, method = "maximum", individually = TRUE)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "minimum",
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.1, 0.2, 0.3)
  )
  expect_equal(
    recalculate_baseline(ftir_data, method = "minimum", individually = FALSE),
    recalculate_baseline(ftir_data, method = "maximum", individually = FALSE)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = "A",
    method = "minimum",
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.2, 0.3, 0.4)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "minimum",
      individually = TRUE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "maximum",
      individually = TRUE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = "A",
    method = "minimum",
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.2, 0.3, 0.4)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "minimum",
      individually = FALSE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "maximum",
      individually = FALSE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    method = "minimum",
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$absorbance,
    c(0.3, 0.4, 0.5)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "minimum",
      individually = TRUE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "maximum",
      individually = TRUE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    method = "minimum",
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(0, 0.1, 0.2)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(0.1, 0.2, 0.3)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$absorbance,
    c(0.3, 0.4, 0.5)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "minimum",
      individually = FALSE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "maximum",
      individually = FALSE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    wavenumber_range = c(1030, 1050),
    method = "minimum",
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(-0.2, -0.1, 0.0)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(-0.2, -0.1, 0.0)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$absorbance,
    c(0.3, 0.4, 0.5)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "minimum",
      wavenumber_range = c(1030, 1050),
      individually = TRUE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "maximum",
      wavenumber_range = c(1030, 1050),
      individually = TRUE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    wavenumber_range = c(1030, 1050),
    method = "minimum",
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$absorbance,
    c(-0.2, -0.1, 0.0)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$absorbance,
    c(-0.1, 0.0, 0.1)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$absorbance,
    c(0.3, 0.4, 0.5)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "minimum",
      wavenumber_range = c(1030, 1050),
      individually = FALSE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "maximum",
      wavenumber_range = c(1030, 1050),
      individually = FALSE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "absorbance")

  ftir_data$transmittance <- c(90, 80, 70, 80, 70, 60, 70, 60, 50)
  ftir_data$absorbance <- NULL

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "minimum",
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculate_baseline(ftir_data, method = "minimum", individually = TRUE),
    recalculate_baseline(ftir_data, method = "maximum", individually = TRUE)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    method = "minimum",
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(90, 80, 70)
  )
  expect_equal(
    recalculate_baseline(ftir_data, method = "minimum", individually = FALSE),
    recalculate_baseline(ftir_data, method = "maximum", individually = FALSE)
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = "A",
    method = "minimum",
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(80, 70, 60)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "minimum",
      individually = TRUE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "maximum",
      individually = TRUE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = "A",
    method = "minimum",
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(80, 70, 60)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "minimum",
      individually = FALSE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = "A",
      method = "maximum",
      individually = FALSE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    method = "minimum",
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$transmittance,
    c(70, 60, 50)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "minimum",
      individually = TRUE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "maximum",
      individually = TRUE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    method = "minimum",
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(100, 90, 80)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(90, 80, 70)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$transmittance,
    c(70, 60, 50)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "minimum",
      individually = FALSE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "maximum",
      individually = FALSE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    wavenumber_range = c(1030, 1050),
    method = "minimum",
    individually = TRUE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(120, 110, 100)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(120, 110, 100)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$transmittance,
    c(70, 60, 50)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "minimum",
      wavenumber_range = c(1030, 1050),
      individually = TRUE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "maximum",
      wavenumber_range = c(1030, 1050),
      individually = TRUE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")

  recalculated_ftir <- recalculate_baseline(
    ftir_data,
    sample_ids = c("A", "B"),
    wavenumber_range = c(1030, 1050),
    method = "minimum",
    individually = FALSE
  )
  expect_equal(nrow(recalculated_ftir), nrow(ftir_data))
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "A", ]$transmittance,
    c(120, 110, 100)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "B", ]$transmittance,
    c(110, 100, 90)
  )
  expect_equal(
    recalculated_ftir[recalculated_ftir$sample_id == "C", ]$transmittance,
    c(70, 60, 50)
  )
  expect_equal(
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "minimum",
      wavenumber_range = c(1030, 1050),
      individually = FALSE
    ),
    recalculate_baseline(
      ftir_data,
      sample_ids = c("A", "B"),
      method = "maximum",
      wavenumber_range = c(1030, 1050),
      individually = FALSE
    )
  )
  expect_equal(attr(recalculated_ftir, "intensity"), "transmittance")
})


test_that("Normalization works", {
  expect_error_bilingual(
    normalize_spectra("not_a_dataframe"),
    en = "must be a data frame",
    fr = "doit être un data.frame"
  )
  expect_error_bilingual(
    normalize_spectra(biodiesel, sample_ids = "A"),
    en = "All provided `sample_ids` must be in `ftir` data.",
    fr = "Tous les `sample_ids` fournis doivent être dans les données `ftir`."
  )
  expect_error_bilingual(
    normalize_spectra(biodiesel, wavenumber_range = c(1, 2, 3)),
    en = "`wavenumber_range` must be of length 2",
    fr = "`wavenumber_range` doit être d'une longueur de 2"
  )
  expect_error_bilingual(
    normalize_spectra(biodiesel, wavenumber_range = c("one", "two")),
    en = "`wavenumber_range` must be `numeric` or `NA`.",
    fr = "`wavenumber_range` doit être `numeric` ou `NA`."
  )
  expect_error_bilingual(
    normalize_spectra(biodiesel, wavenumber_range = c(1, NA)),
    en = "`wavenumber_range` must be `numeric` or `NA`.",
    fr = "`wavenumber_range` doit être `numeric` ou `NA`."
  )
  expect_error_bilingual(
    normalize_spectra(biodiesel, wavenumber_range = 1500),
    en = "`wavenumber_range` must be of length 2",
    fr = "`wavenumber_range` doit être d'une longueur de 2"
  )
  expect_error_bilingual(
    normalize_spectra(absorbance_to_transmittance(biodiesel)),
    en = "Normalization of transmittance spectra not supported.",
    fr = "La normalisation des spectres de transmittance n'est pas prise en charge."
  )

  spectra <- data.frame(
    wavenumber = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    absorbance = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, 1.7, 1.9),
    sample_id = "test"
  )
  expect_equal(range(spectra$absorbance), c(0.1, 1.9))
  expect_equal(range(normalize_spectra(spectra)$absorbance), c(0.0, 1.0))
  expect_equal(
    range(normalize_spectra(spectra, wavenumber_range = c(1, 5))$absorbance),
    c(0.0, 2.25)
  )
  expect_equal(
    range(normalize_spectra(spectra, wavenumber_range = c(6, 10))$absorbance),
    c(-1.25, 1)
  )

  biodiesel_normal <- normalize_spectra(
    biodiesel,
    sample_ids = c("diesel_unknown")
  )

  expect_equal(
    range(biodiesel[biodiesel$sample_id == "biodiesel_0", "absorbance"]),
    range(biodiesel_normal[
      biodiesel_normal$sample_id == "biodiesel_0",
      "absorbance"
    ])
  )
  expect_equal(
    range(biodiesel_normal[
      biodiesel_normal$sample_id == "diesel_unknown",
      "absorbance"
    ]),
    c(0, 1)
  )
  expect_equal(attr(biodiesel_normal, "intensity"), "normalized absorbance")
})


test_that("conversion between units works", {
  biodiesel_transmittance <- absorbance_to_transmittance(biodiesel)
  biodiesel_absorbance <- transmittance_to_absorbance(biodiesel_transmittance)
  expect_named(
    biodiesel_transmittance,
    c("wavenumber", "transmittance", "sample_id")
  )
  expect_named(
    transmittance_to_absorbance(biodiesel_transmittance),
    c("wavenumber", "absorbance", "sample_id")
  )

  expect_equal(attr(biodiesel_transmittance, "intensity"), "transmittance")
  expect_equal(attr(biodiesel_absorbance, "intensity"), "absorbance")

  biodiesel_normal <- normalize_spectra(biodiesel)
  biodiesel_normal_trans <- absorbance_to_transmittance(biodiesel_normal)
  expect_equal(
    attr(biodiesel_normal_trans, "intensity"),
    "normalized transmittance"
  )
  expect_equal(
    attr(transmittance_to_absorbance(biodiesel_normal_trans), "intensity"),
    "normalized absorbance"
  )

  expect_error_bilingual(
    transmittance_to_absorbance(biodiesel),
    en = "must be transmittance data or contain a `transmittance` column",
    fr = "doit être des données de transmittance ou contenir une colonne `transmittance`"
  )
  expect_error_bilingual(
    absorbance_to_transmittance(absorbance_to_transmittance(biodiesel)),
    en = "must be absorbance data or contain a `absorbance` column",
    fr = "doit être des données d'absorbance ou contenir une colonne `absorbance`"
  )

  example_data <- data.frame(
    "wavenumber" = 1L,
    "absorbance" = c(0, .5, 1, 1.5, 2),
    "sample_id" = "test"
  )
  expect_equal(
    absorbance_to_transmittance(example_data)$transmittance,
    c(100, 31.62278, 10, 3.162278, 1),
    tolerance = 1e-4
  )

  example_data2 <- data.frame(
    "wavenumber" = 1L,
    "transmittance" = c(100, 50, 10, 5, 1),
    "sample_id" = "test"
  )
  expect_equal(
    transmittance_to_absorbance(example_data2)$absorbance,
    c(0, 0.30103, 1, 1.30103, 2),
    tolerance = 1e-4
  )

  example_data3 <- data.frame(
    "wavenumber" = 1L,
    "absorbance" = c(0, 0.5, 1, 1.5, 2),
    "sample_id" = "test",
    "transmittance" = c(100, 50, 10, 5, 1)
  )
  expect_error_bilingual(
    absorbance_to_transmittance(example_data3),
    en = "cannot contain more than one of `absorbance`, `transmittance`, or `intensity` columns",
    fr = "ne peut pas contenir plus d'une des colonnes `absorbance`, `transmittance`, ou `intensity`"
  )
  expect_error_bilingual(
    transmittance_to_absorbance(example_data3),
    en = "cannot contain more than one of `absorbance`, `transmittance`, or `intensity` columns",
    fr = "ne peut pas contenir plus d'une des colonnes `absorbance`, `transmittance`, ou `intensity`"
  )
})

test_that("Normalization carries thorugh other functions", {
  biodiesel_normal <- normalize_spectra(
    biodiesel,
    sample_ids = c("diesel_unknown")
  )

  expect_equal(
    range(biodiesel[biodiesel$sample_id == "biodiesel_0", "absorbance"]),
    range(biodiesel_normal[
      biodiesel_normal$sample_id == "biodiesel_0",
      "absorbance"
    ])
  )
  expect_equal(
    range(biodiesel_normal[
      biodiesel_normal$sample_id == "diesel_unknown",
      "absorbance"
    ]),
    c(0, 1)
  )
  expect_equal(attr(biodiesel_normal, "intensity"), "normalized absorbance")

  expect_equal(
    attr(add_scalar_value(biodiesel_normal, 1), "intensity"),
    "normalized absorbance"
  )
  expect_equal(
    attr(
      recalculate_baseline(
        biodiesel_normal,
        method = "point",
        wavenumber_range = 3900
      ),
      "intensity"
    ),
    "normalized absorbance"
  )

  expect_equal(
    attr(absorbance_to_transmittance(biodiesel_normal), "intensity"),
    "normalized transmittance"
  )
})


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

  sample_data <- normalized[
    normalized$sample_id == tools::file_path_sans_ext(tmpfile),
  ]
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

  expect_error_bilingual(
    normalize_raman(result, method = "invalid"),
    en = "`method` must be a string.",
    fr = "`method` doit être une chaîne de caractères."
  )
})

test_that("normalize_raman validates sample_ids parameter", {
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

  expect_error_bilingual(
    normalize_raman(result, sample_ids = "nonexistent"),
    en = "All provided `sample_ids` must be in `ftir` data.",
    fr = "Tous les `sample_ids` fournis doivent être dans les données `ftir`."
  )
})
