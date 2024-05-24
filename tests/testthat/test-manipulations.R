test_that("mainipulations works", {
  biodiesel_plot <- plot_ftir(biodiesel)

  # test arg checks.
  expect_error(zoom_in_on_range("abc"), "`ftir_spectra_plot` must be a ggplot object. You provided a string", fixed = TRUE)
  expect_error(compress_low_energy("abc"), "`ftir_spectra_plot` must be a ggplot object. You provided a string", fixed = TRUE)
  expect_error(add_wavenumber_marker("abc", 1500), "`ftir_spectra_plot` must be a ggplot object. You provided a string", fixed = TRUE)

  expect_error(zoom_in_on_range(biodiesel_plot, zoom_range = 100),
               "`zoom_range` must be a numeric vector of length two.", fixed = TRUE)
  expect_error(zoom_in_on_range(biodiesel_plot, zoom_range = c("a","b")),
               "`zoom_range` must be a numeric vector of length two.", fixed = TRUE)
  expect_error(zoom_in_on_range(biodiesel_plot, zoom_range = c(200, 2000)),
               "`zoom_range` must be values between 400 and 4000 cm^-1.", fixed = TRUE)

  expect_error(compress_low_energy(biodiesel_plot, cutoff = "bob"),
               "`cutoff` must be a numeric value. You provided a string.", fixed = TRUE)
  expect_error(compress_low_energy(biodiesel_plot, cutoff = 100),
               "`cutoff` must be a value between 400 and 4000 cm^-1.", fixed = TRUE)
  expect_error(compress_low_energy(biodiesel_plot, compression_ratio = "bob"),
               "`compression_ratio` must be a numeric value. You provided a string.", fixed = TRUE)
  expect_error(compress_low_energy(biodiesel_plot, cutoff = 2000, compression_ratio = 1000),
                 "`compression_ratio` must be a value between 0.01 and 100", fixed = TRUE)

  expect_error(add_wavenumber_marker(biodiesel_plot, wavenumber = "abc"),
               "`wavenumber` must be a numeric value. You provided a string.", fixed = TRUE)
  expect_error(add_wavenumber_marker(biodiesel_plot, wavenumber = 1000, text = mtcars),
               "`text` must be character or numeric, you provided a data frame.", fixed = TRUE)
  expect_error(add_wavenumber_marker(biodiesel_plot, wavenumber = 1000, text = c("This is", "too long")),
               "`text` should be character or numeric, but not a vector of length greater than one.", fixed = TRUE)
  expect_error(add_wavenumber_marker(biodiesel_plot, wavenumber = 1000, text = biodiesel_plot),
               "`text` must be character or numeric, you provided a <gg/ggplot> object.", fixed = TRUE)
  expect_error(add_wavenumber_marker(biodiesel_plot, wavenumber = 5000),
               "`wavenumber` must be a value between 400 and 4000 cm^-1.", fixed = TRUE)

  # Plots should come out mostly the same.
  zoomed_plot <- zoom_in_on_range(biodiesel_plot)
  compressed_plot <- compress_low_energy(biodiesel_plot)
  labelled_plot <- add_wavenumber_marker(biodiesel_plot, 1740, "CO Stretch")

  expect_equal(zoom_in_on_range(biodiesel_plot, c(1000, 1900)), zoom_in_on_range(biodiesel_plot, c(1900, 1000)))
  expect_equal(biodiesel_plot$labels$title, zoomed_plot$labels$title)
  expect_equal(biodiesel_plot$labels$title, compressed_plot$labels$title)
  expect_equal(biodiesel_plot$labels$title, labelled_plot$labels$title)

  expect_false(
    all(
      ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range ==
        ggplot2::ggplot_build(zoomed_plot)$layout$panel_params[[1]]$x.range
    )
  )

  expect_false(
    all(
      ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range ==
        ggplot2::ggplot_build(compressed_plot)$layout$panel_params[[1]]$x.range
    )
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range,
    ggplot2::ggplot_build(labelled_plot)$layout$panel_params[[1]]$x.range
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(zoomed_plot)$layout$panel_params[[1]]$y.range
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(compressed_plot)$layout$panel_params[[1]]$y.range
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(compressed_plot)$layout$panel_params[[1]]$y.range
  )
})

test_that("-.gg is ok", {
  biodiesel_plot <- plot_ftir(biodiesel)

  expect_error(biodiesel_plot - NULL, "Cannot use `-.gg()` with a single argument, ", fixed = TRUE)
  expect_error(4 - ggplot2::geom_vline(xintercept = 5), "You need to have a ggplot on the left side. You provided ", fixed = TRUE)
})
