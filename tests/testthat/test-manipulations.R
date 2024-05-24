test_that("mainipulations works", {
  # Plots should come out mostly the same.

  biodiesel_plot <- plot_ftir(biodiesel)

  zoomed_plot <- zoom_in_on_range(biodiesel_plot)
  compressed_plot <- compress_low_energy(biodiesel_plot)
  labelled_plot <- add_wavenumber_marker(biodiesel_plot, 1740, "CO Stretch")

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
