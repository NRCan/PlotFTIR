test_that("Plot SampleID extraction is ok", {
  # Test for ggplot2 else skip
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error_bilingual(
      get_plot_sample_ids(123),
      en = "requires ggplot2 package installation",
      fr = "nécessite l'installation du paquet ggplot2"
    )

    testthat::skip("ggplot2 not available for testing plot production")
  }

  p <- plot_ftir(biodiesel)

  expect_equal(get_plot_sample_ids(p), as.factor(unique(biodiesel$sample_id)))

  expect_error_bilingual(
    get_plot_sample_ids(biodiesel),
    en = "`ftir_spectra_plot` must be a ggplot object. You provided ",
    fr = "`ftir_spectra_plot` doit être un objet ggplot. Vous avez fourni "
  )
})

test_that("Intensity Typing works", {
  expect_equal(intensity_type(biodiesel), "absorbance")
  expect_equal(
    intensity_type(absorbance_to_transmittance(biodiesel)),
    "transmittance"
  )
  b2 <- biodiesel
  colnames(biodiesel)[colnames(biodiesel) == "absorbance"] <- "intensity"
  expect_equal(intensity_type(b2), "absorbance")
})

test_that("Checking FTIR data works", {
  # Most checks are validated in one way or another by the repeated calling of the check_ftir_data()
  # function in the other code, but we intentionally manually validate here.

  bad_ftir <- biodiesel
  attr(bad_ftir, "intensity") <- "test"
  expect_error_bilingual(
    check_ftir_data(bad_ftir),
    en = "has unexpected attributes.",
    fr = "a des attributs inattendus."
  )

  no_attr_ftir <- biodiesel
  attr(no_attr_ftir, "intensity") <- NULL
  expect_equal(attr(check_ftir_data(no_attr_ftir), "intensity"), "absorbance")
})

test_that("Print PlotFTIR data works in english", {
  withr::with_options(new = c(PlotFTIR.lang = 'en'), {
    output <- capture.output(print(check_ftir_data(biodiesel)))

    expect_true(any(grepl("PlotFTIR data:", output)))
    expect_true(any(grepl("Spectral range", output)))
    expect_true(any(grepl("Resolution", output)))
    expect_true(any(grepl("Intensity type", output)))
    expect_true(any(grepl("Number of samples", output)))
    expect_true(any(grepl("Sample IDs", output)))
  })
})

test_that("Print PlotFTIR data works in french", {
  withr::with_options(new = c(PlotFTIR.lang = 'fr'), {
    output <- capture.output(print(check_ftir_data(biodiesel)))

    expect_true(any(grepl("Donn", output)))
    expect_true(any(grepl("Plage spectrale", output)))
    expect_true(any(grepl("solution", output)))
    expect_true(any(grepl("Type d'intensit", output)))
    expect_true(any(grepl("Nombre d'", output)))
    expect_true(any(grepl("ID des ", output)))
  })
})
