test_that("Plots are generated", {
  # Test for ggplot2 else skip
  if (!require("ggplot2", quietly = TRUE)) {
    expect_error(
      plot_ftir(biodiesel),
      "requires ggplot2 package installation",
      fixed = TRUE
    )

    testthat::skip("ggplot2 not available for testing plot production")
  }

  p1 <- plot_ftir(biodiesel)
  p2 <- plot_ftir_stacked(biodiesel)

  biodiesel_transmittance <- absorbance_to_transmittance(biodiesel)

  p3 <- plot_ftir(biodiesel_transmittance)
  p4 <- plot_ftir_stacked(biodiesel_transmittance)

  expect_true(ggplot2::is.ggplot(p1))
  expect_true(ggplot2::is.ggplot(p2))
  expect_equal(p1$labels$y, "Absorbance")
  expect_true(ggplot2::is.ggplot(p3))
  expect_true(ggplot2::is.ggplot(p4))
  expect_equal(p3$label$y, "% Transmittance")
})

test_that("data is checked correctly", {
  full_data_df <- data.frame(
    "sample_id" = LETTERS,
    "wavenumber" = seq_along(LETTERS),
    "absorbance" = runif(length(LETTERS)),
    "transmittance" = runif(length(LETTERS)) * 100
  )

  expect_error(
    plot_ftir(ftir = "abc"),
    "`ftir` must be a data frame. You provided a string."
  )
  expect_error(plot_ftir(ftir = data.frame("a" = 1:10)),
    "`ftir` is missing column(s)",
    fixed = TRUE
  )
  expect_error(plot_ftir(ftir = full_data_df[, c("sample_id", "wavenumber")]),
    "`ftir` must have one of `absorbance` or `transmittance` columns.",
    fixed = TRUE
  )
  expect_error(plot_ftir(ftir = full_data_df),
    "`ftir` cannot contain both `absorbance` and `transmittance` columns.",
    fixed = TRUE
  )

  expect_error(
    plot_ftir_stacked(ftir = "abc"),
    "`ftir` must be a data frame. You provided a string."
  )
  expect_error(plot_ftir_stacked(ftir = data.frame("a" = 1:10)),
    "`ftir` is missing a column",
    fixed = TRUE
  )
  expect_error(plot_ftir_stacked(ftir = full_data_df[, c("sample_id", "wavenumber")]),
    "`ftir` must have one of `absorbance` or `transmittance` columns.",
    fixed = TRUE
  )
  expect_error(plot_ftir_stacked(ftir = full_data_df),
    "`ftir` cannot contain both `absorbance` and `transmittance` columns.",
    fixed = TRUE
  )

  colnames(full_data_df)[4] <- "logabs"
  expect_error(plot_ftir(ftir = full_data_df),
    "`ftir` may only contain columns `sample_id`, `wavenumber`, and one of `absorbance` or `transmittance`.",
    fixed = TRUE
  )
  expect_error(plot_ftir(biodiesel, 1234),
    "`plot_title` must be a character string or vector of strings with length not more than two.",
    fixed = TRUE
  )
  expect_error(plot_ftir(biodiesel, c("My Plot", "My Subplot", "My Extrasubplot")),
    "`plot_title` must be a character string or vector of strings with length not more than two.",
    fixed = TRUE
  )
  expect_error(plot_ftir(biodiesel, legend_title = 1234),
    "`legend_title` must be a single character string.",
    fixed = TRUE
  )

  expect_error(plot_ftir_stacked(ftir = full_data_df),
    "`ftir` may only contain columns `sample_id`, `wavenumber`, and one of `absorbance` or `transmittance`.",
    fixed = TRUE
  )
  expect_error(plot_ftir_stacked(biodiesel, stack_offset = "abc"),
    "`stack_offset` must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(plot_ftir_stacked(biodiesel, stack_offset = -10),
    "`stack_offset` must be between 0 and 200.",
    fixed = TRUE
  )

  expect_error(plot_ftir(rbind(biodiesel, sample_spectra)),
    "The color palette in use works only with 12 or fewer unique samples in",
    fixed = TRUE
  )
})
