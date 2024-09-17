test_that("zoom in is ok", {
  # Ensure caught failure if no ggplot2, then skip remainder of tests
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error(
      zoom_in_on_range(123),
      "requires ggplot2 package installation",
      fixed = TRUE
    )
    testthat::skip("ggplot2 not available for testing manipulations")
  }

  biodiesel_plot <- plot_ftir(biodiesel)

  # test arg checks.
  expect_error(zoom_in_on_range("abc"),
    "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fixed = TRUE
  )
  expect_error(zoom_in_on_range(biodiesel_plot, zoom_range = 100),
    "`zoom_range` must be a numeric vector of length two.",
    fixed = TRUE
  )
  expect_error(zoom_in_on_range(biodiesel_plot, zoom_range = c("a", "b")),
    "`zoom_range` must be a numeric vector of length two.",
    fixed = TRUE
  )
  expect_error(zoom_in_on_range(biodiesel_plot, zoom_range = c(200, 2000)),
    "`zoom_range` must be values between 400 and 4000 cm^-1.",
    fixed = TRUE
  )

  # Plots should come out mostly the same.
  zoomed_plot <- zoom_in_on_range(biodiesel_plot)

  expect_equal(
    zoom_in_on_range(biodiesel_plot, c(1000, 1900)),
    zoom_in_on_range(biodiesel_plot, c(1900, 1000))
  )
  expect_equal(biodiesel_plot$labels$title, zoomed_plot$labels$title)

  expect_false(
    all(
      ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range ==
        ggplot2::ggplot_build(zoomed_plot)$layout$panel_params[[1]]$x.range
    )
  )

  expect_false(
    all(
      ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range ==
        ggplot2::ggplot_build(zoomed_plot)$layout$panel_params[[1]]$y.range
    )
  )

  # Check that y range hasn't moved for transmittance plots
  transmittance_plot <- plot_ftir(absorbance_to_transmittance(biodiesel))
  zoomed_transmittance <- zoom_in_on_range(transmittance_plot, c(2000, 2600))

  expect_true(
    all(
      ggplot2::ggplot_build(transmittance_plot)$layout$panel_params[[1]]$y.range ==
        ggplot2::ggplot_build(zoomed_transmittance)$layout$panel_params[[1]]$y.range
    )
  )
})

test_that("compress region is ok", {
  # Ensure caught failure if no ggplot2, then skip remainder of tests
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error(
      compress_low_energy(123),
      "requires ggplot2 package installation",
      fixed = TRUE
    )

    testthat::skip("ggplot2 not available for testing manipulations")
  }

  biodiesel_plot <- plot_ftir(biodiesel)

  # test arg checks.

  expect_error(compress_low_energy("abc"),
    "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fixed = TRUE
  )

  expect_error(compress_low_energy(biodiesel_plot, cutoff = "bob"),
    "`cutoff` must be a numeric value. You provided a string.",
    fixed = TRUE
  )
  expect_error(compress_low_energy(biodiesel_plot, cutoff = 100),
    "`cutoff` must be a value between 400 and 4000 cm^-1.",
    fixed = TRUE
  )
  expect_error(compress_low_energy(biodiesel_plot, compression_ratio = "bob"),
    "`compression_ratio` must be a numeric value. You provided a string.",
    fixed = TRUE
  )
  expect_error(compress_low_energy(biodiesel_plot, cutoff = 2000, compression_ratio = 1000),
    "`compression_ratio` must be a value between 0.01 and 100",
    fixed = TRUE
  )

  # Plots should come out mostly the same.
  compressed_plot <- compress_low_energy(biodiesel_plot)

  expect_equal(biodiesel_plot$labels$title, compressed_plot$labels$title)

  expect_false(
    all(
      ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range ==
        ggplot2::ggplot_build(compressed_plot)$layout$panel_params[[1]]$x.range
    )
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(compressed_plot)$layout$panel_params[[1]]$y.range
  )
})

test_that("labelled plot is ok", {
  # Ensure caught failure if no ggplot2, then skip remainder of tests
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error(
      add_wavenumber_marker(123, 1740, "CO Stretch"),
      "requires ggplot2 package installation",
      fixed = TRUE
    )

    testthat::skip("ggplot2 not available for testing manipulations")
  }

  biodiesel_plot <- plot_ftir(biodiesel)

  # test arg checks.
  expect_error(add_wavenumber_marker("abc", 1500),
    "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fixed = TRUE
  )

  expect_error(
    add_wavenumber_marker(biodiesel_plot,
      wavenumber = "abc"
    ),
    "`wavenumber` must be a numeric value. You provided a string.",
    fixed = TRUE
  )
  expect_error(
    add_wavenumber_marker(biodiesel_plot,
      wavenumber = 1000,
      text = mtcars
    ),
    "`text` must be character or numeric, you provided a data frame.",
    fixed = TRUE
  )
  expect_error(
    add_wavenumber_marker(biodiesel_plot,
      wavenumber = 1000,
      text = c("This is", "too long")
    ),
    "`text` should be character or numeric, but not a vector of length greater than one.",
    fixed = TRUE
  )
  expect_error(
    add_wavenumber_marker(biodiesel_plot,
      wavenumber = 1000,
      text = biodiesel_plot
    ),
    "`text` must be character or numeric, you provided a <gg",
    fixed = TRUE
  )
  expect_error(add_wavenumber_marker(biodiesel_plot, wavenumber = 5000),
    "`wavenumber` must be a value between 400 and 4000 cm^-1.",
    fixed = TRUE
  )

  # Plots should come out mostly the same.
  labelled_plot <- add_wavenumber_marker(biodiesel_plot, 1740, "CO Stretch")

  expect_equal(biodiesel_plot$labels$title, labelled_plot$labels$title)

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range,
    ggplot2::ggplot_build(labelled_plot)$layout$panel_params[[1]]$x.range
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(labelled_plot)$layout$panel_params[[1]]$y.range
  )
})

test_that("-.gg is ok", {
  if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing -.gg.")
  }
  biodiesel_plot <- plot_ftir(biodiesel)

  expect_error(biodiesel_plot - NULL,
    "Cannot use `-.gg()` with a single argument, ",
    fixed = TRUE
  )
  expect_error(4 - ggplot2::geom_vline(xintercept = 5),
    "You need to have a ggplot on the left side. You provided ",
    fixed = TRUE
  )
})

test_that("rename is ok", {
  new_ids <- c(
    "toluene" = "Toluene", "heptanes" = "C7 Alkane", "isopropanol" = "IPA",
    "paper" = "White Paper", "polystyrene" = "PS Film"
  )

  # Test for ggplot2 else skip
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error(
      rename_plot_sample_ids(123, sample_ids = new_ids),
      "requires ggplot2 package installation",
      fixed = TRUE
    )

    testthat::skip("ggplot2 not available for testing renames")
  }

  p <- plot_ftir(sample_spectra)

  new_ids <- c(
    "Toluene" = "toluene", "C7 Alkane" = "heptanes", "IPA" = "isopropanol",
    "White Paper" = "paper", "PS Film" = "polystyrene"
  )

  rp <- rename_plot_sample_ids(p, new_ids)
  expect_true(ggplot2::is.ggplot(rp))
  expect_true("Toluene" %in% rp$scales$scales[[1]]$labels)
  expect_true("C7 Alkane" %in% rp$scales$scales[[1]]$labels)

  expect_error(rename_plot_sample_ids(sample_spectra, new_ids),
    "`ftir_spectra_plot` must be a ggplot object. You provided ",
    fixed = TRUE
  )

  expect_error(rename_plot_sample_ids(p, c(new_ids, "test" = "failure")),
    "All provided 'old names' must be in the `ftir_spectra_plot`.",
    fixed = TRUE
  )

  # check only partial names still makes a plot
  rp <- rename_plot_sample_ids(p, new_ids[1])
  expect_true(ggplot2::is.ggplot(rp))
  expect_true("Toluene" %in% rp$scales$scales[[1]]$labels)
  expect_false("C7 Alkane" %in% rp$scales$scales[[1]]$labels)
})

test_that("legend moving is ok", {
  # Ensure caught failure if no ggplot2, then skip remainder of tests
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.
    expect_error(
      move_plot_legend(123, position = "bottom"),
      "requires ggplot2 package installation",
      fixed = TRUE
    )

    testthat::skip("ggplot2 not available for testing manipulations")
  }

  biodiesel_plot <- plot_ftir(biodiesel)

  # test arg checks.

  expect_error(move_plot_legend("abc", position = "bottom"),
    "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fixed = TRUE
  )

  expect_error(move_plot_legend(biodiesel_plot, position = "bob"),
    "`position` must be one of ",
    fixed = TRUE
  )
  expect_error(move_plot_legend(biodiesel_plot, position = "bottom", justification = "bob"),
    "`justification` must be one of ",
    fixed = TRUE
  )
  expect_error(move_plot_legend(biodiesel_plot, direction = "bob"),
    "`direction` must be one of ",
    fixed = TRUE
  )
  expect_error(move_plot_legend(biodiesel_plot, legend_title_position = "bob"),
    "`legend_title_position` must be one of ",
    fixed = TRUE
  )

  # Plots should come out mostly the same.
  moved_legend_plot <- move_plot_legend(biodiesel_plot, position = "bottom", direction = "horizontal")

  expect_equal(biodiesel_plot$labels$title, moved_legend_plot$labels$title)

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range,
    ggplot2::ggplot_build(moved_legend_plot)$layout$panel_params[[1]]$x.range
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(moved_legend_plot)$layout$panel_params[[1]]$y.range
  )
})
