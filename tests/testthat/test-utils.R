test_that("conversion between units works", {
  biodiesel_transmittance <- absorbance_to_transmittance(biodiesel)
  expect_named(
    biodiesel_transmittance,
    c("wavenumber", "transmittance", "sample_id")
  )
  expect_named(
    transmittance_to_absorbance(biodiesel_transmittance),
    c("wavenumber", "absorbance", "sample_id")
  )

  expect_error(transmittance_to_absorbance(biodiesel),
    "`ftir` must contain a `transmittance` column.",
    fixed = TRUE
  )
  expect_error(absorbance_to_transmittance(absorbance_to_transmittance(biodiesel)),
    "`ftir` must contain a `absorbance` column.",
    fixed = TRUE
  )

  example_data <- data.frame(
    "wavenumber" = 1L,
    "absorbance" = c(0, .5, 1, 1.5, 2),
    "sample_id" = "test"
  )
  expect_equal(absorbance_to_transmittance(example_data)$transmittance,
    c(100, 31.62278, 10, 3.162278, 1),
    tolerance = 1e-4
  )

  example_data2 <- data.frame(
    "wavenumber" = 1L,
    "transmittance" = c(100, 50, 10, 5, 1),
    "sample_id" = "test"
  )
  expect_equal(transmittance_to_absorbance(example_data2)$absorbance,
    c(0, 0.30103, 1, 1.30103, 2),
    tolerance = 1e-4
  )

  example_data3 <- data.frame(
    "wavenumber" = 1L,
    "absorbance" = c(0, 0.5, 1, 1.5, 2),
    "sample_id" = "test",
    "transmittance" = c(100, 50, 10, 5, 1)
  )
  expect_error(absorbance_to_transmittance(example_data3),
    "`ftir` cannot contain both `absorbance` and `transmittance` columns.",
    fixed = TRUE
  )
  expect_error(transmittance_to_absorbance(example_data3),
    "`ftir` cannot contain both `absorbance` and `transmittance` columns.",
    fixed = TRUE
  )
})

test_that("Plot SampleID extraction is ok", {
  # Test for ggplot2 else skip
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error(
      get_plot_sample_ids(123),
      "requires ggplot2 package installation",
      fixed = TRUE
    )

    testthat::skip("ggplot2 not available for testing plot production")
  }

  p <- plot_ftir(biodiesel)

  expect_equal(get_plot_sample_ids(p), as.factor(unique(biodiesel$sample_id)))

  expect_error(get_plot_sample_ids(biodiesel),
    "`ftir_spectra_plot` must be a ggplot object. You provided ",
    fixed = TRUE
  )
})

test_that("interface to ir is ok", {
  if (!requireNamespace("ir", quietly = TRUE)) {
    expect_error(ir_to_plotftir(data.frame("testdata" = LETTERS)), regexp = "requires `ir` package installation for this function.", fixed = TRUE)
    expect_error(ir_to_df(data.frame("testdata" = LETTERS)), regexp = "requires `ir` package installation for this function.", fixed = TRUE)
    expect_error(plotftir_to_ir(biodiesel), regexp = "requires `ir` package installation for this function.", fixed = TRUE)
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
})
