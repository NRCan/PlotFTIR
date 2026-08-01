test_that("zoom in is ok", {
  # Ensure caught failure if no ggplot2, then skip remainder of tests
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error_bilingual(
      zoom_in_on_range(123),
      en = "requires ggplot2 package installation",
      fr = "nécessite l'installation du paquet ggplot2"
    )
    testthat::skip("ggplot2 not available for testing manipulations")
  }

  biodiesel_plot <- plot_ftir(biodiesel)

  # test arg checks.
  expect_error_bilingual(
    zoom_in_on_range("abc"),
    en = "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fr = "`ftir_spectra_plot` doit être un objet ggplot. Vous avez fourni a string"
  )
  expect_error_bilingual(
    zoom_in_on_range(biodiesel_plot, zoom_range = 100),
    en = "`zoom_range` must be a numeric vector of length two.",
    fr = "`zoom_range` doit être un vecteur numérique de longueur deux."
  )
  expect_error_bilingual(
    zoom_in_on_range(biodiesel_plot, zoom_range = c("a", "b")),
    en = "`zoom_range` must be a numeric vector of length two.",
    fr = "`zoom_range` doit être un vecteur numérique de longueur deux."
  )
  expect_error_bilingual(
    zoom_in_on_range(biodiesel_plot, zoom_range = c(200, 2000)),
    en = "`zoom_range` must be values between 701 and 3999 cm^-1.",
    fr = "`zoom_range` doit être des valeurs comprises entre 701 et 3999 cm^-1."
  )

  # Plots should come out mostly the same.
  zoomed_plot <- zoom_in_on_range(biodiesel_plot)

  expect_equal(
    zoom_in_on_range(biodiesel_plot, c(1000, 1900)),
    zoom_in_on_range(biodiesel_plot, c(1900, 1000))
  )

  bdlab <- ggplot2::get_labs(biodiesel_plot)
  zplab <- ggplot2::get_labs(zoomed_plot)
  expect_equal(bdlab$title, zplab$title)

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
      ggplot2::ggplot_build(transmittance_plot)$layout$panel_params[[
        1
      ]]$y.range ==
        ggplot2::ggplot_build(zoomed_transmittance)$layout$panel_params[[
          1
        ]]$y.range
    )
  )

  # Check that y range hasn't moved for stacked transmittance plots
  transmittance_stack_plot <- plot_ftir_stacked(absorbance_to_transmittance(
    biodiesel
  ))
  zoomed_transmittance_stack <- zoom_in_on_range(
    transmittance_stack_plot,
    c(2000, 2600)
  )

  expect_true(
    all(
      ggplot2::ggplot_build(transmittance_stack_plot)$layout$panel_params[[
        1
      ]]$y.range ==
        ggplot2::ggplot_build(zoomed_transmittance_stack)$layout$panel_params[[
          1
        ]]$y.range
    )
  )
})

test_that("compress region is ok", {
  # Ensure caught failure if no ggplot2, then skip remainder of tests
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error_bilingual(
      compress_low_energy(123),
      en = "requires ggplot2 package installation",
      fr = "nécessite l'installation du paquet ggplot2"
    )

    testthat::skip("ggplot2 not available for testing manipulations")
  }

  biodiesel_plot <- plot_ftir(biodiesel)

  # test arg checks.

  expect_error_bilingual(
    compress_low_energy("abc"),
    en = "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fr = "`ftir_spectra_plot` doit être un objet ggplot. Vous avez fourni a string"
  )

  expect_error_bilingual(
    compress_low_energy(biodiesel_plot, cutoff = "bob"),
    en = "`cutoff` must be a numeric value. You provided a string.",
    fr = "`cutoff` doit être une valeur numérique. Vous avez fourni a string."
  )
  expect_error_bilingual(
    compress_low_energy(biodiesel_plot, cutoff = 100),
    en = "`cutoff` must be a value between 701 and 3999 cm^-1.",
    fr = "`cutoff` doit être une valeur comprise entre 701 et 3999 cm^-1."
  )
  expect_error_bilingual(
    compress_low_energy(biodiesel_plot, compression_ratio = "bob"),
    en = "`compression_ratio` must be a numeric value. You provided a string.",
    fr = "`compression_ratio` doit être une valeur numérique. Vous avez fourni a string."
  )
  expect_error_bilingual(
    compress_low_energy(
      biodiesel_plot,
      cutoff = 2000,
      compression_ratio = 1000
    ),
    en = "`compression_ratio` must be a value between 0.01 and 100",
    fr = "`compression_ratio` doit être une valeur comprise entre 0.01 et 100"
  )

  # Plots should come out mostly the same.
  compressed_plot <- compress_low_energy(biodiesel_plot)

  bdlab <- ggplot2::get_labs(biodiesel_plot)
  cplab <- ggplot2::get_labs(compressed_plot)
  expect_equal(bdlab$title, cplab$title)

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

    expect_error_bilingual(
      add_wavenumber_marker(123, 1740, "CO Stretch"),
      en = "requires ggplot2 package installation",
      fr = "nécessite l'installation du paquet ggplot2"
    )

    testthat::skip("ggplot2 not available for testing manipulations")
  }

  biodiesel_plot <- plot_ftir(biodiesel)

  # test arg checks.
  expect_error_bilingual(
    add_wavenumber_marker("abc", 1500),
    en = "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fr = "`ftir_spectra_plot` doit être un objet ggplot. Vous avez fourni a string"
  )

  expect_error_bilingual(
    add_wavenumber_marker(biodiesel_plot, wavenumber = "abc"),
    en = "`wavenumber` must be a numeric value. You provided a string.",
    fr = "`wavenumber` doit être une valeur numérique. Vous avez fourni a string."
  )
  expect_error_bilingual(
    add_wavenumber_marker(biodiesel_plot, wavenumber = 1000, text = mtcars),
    en = "`text` must be character or numeric, you provided a data frame.",
    fr = "`text` doit être une chaîne de caractères ou numérique, vous avez fourni a data frame."
  )
  expect_error_bilingual(
    add_wavenumber_marker(
      biodiesel_plot,
      wavenumber = 1000,
      text = c("This is", "too long")
    ),
    en = "`text` should be character or numeric, but not a vector of length greater than one.",
    fr = "`text` doit être une chaîne de caractères ou numérique, mais pas un vecteur de longueur supérieure à un."
  )
  expect_error_bilingual(
    add_wavenumber_marker(
      biodiesel_plot,
      wavenumber = 1000,
      text = biodiesel_plot
    ),
    en = "`text` must be character or numeric, you provided a <gg",
    fr = "`text` doit être une chaîne de caractères ou numérique, vous avez fourni a <gg"
  )
  expect_error_bilingual(
    add_wavenumber_marker(biodiesel_plot, wavenumber = 5000),
    en = "`wavenumber` must be a value between 701 and 3999 cm^-1.",
    fr = "`wavenumber` doit être une valeur comprise entre 701 et 3999 cm^-1."
  )
  expect_error_bilingual(
    add_wavenumber_marker(
      biodiesel_plot,
      wavenumber = 1740,
      text = "CO Stretch",
      line_aesthetics = 'dashed'
    ),
    en = "`line_aesthetics` must be a named list. You provided",
    fr = "`line_aesthetics` doit être une liste nommée. Vous avez fourni"
  )

  expect_error_bilingual(
    add_wavenumber_marker(
      biodiesel_plot,
      wavenumber = 1740,
      text = "CO Stretch",
      label_aesthetics = 'bold'
    ),
    en = "`label_aesthetics` must be a named list. You provided",
    fr = "`label_aesthetics` doit être une liste nommée. Vous avez fourni"
  )

  # Plots should come out mostly the same.
  labelled_plot <- add_wavenumber_marker(biodiesel_plot, 1740, "CO Stretch")

  bdlab <- ggplot2::get_labs(biodiesel_plot)
  lplab <- ggplot2::get_labs(labelled_plot)
  expect_equal(bdlab$title, lplab$title)

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range,
    ggplot2::ggplot_build(labelled_plot)$layout$panel_params[[1]]$x.range
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(labelled_plot)$layout$panel_params[[1]]$y.range
  )
})

test_that("-.ggplot is ok", {
  if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing -.gg.")
  }
  biodiesel_plot <- plot_ftir(biodiesel)

  expect_error_bilingual(
    biodiesel_plot - NULL,
    en = "Cannot use `-.gg()` with a single argument, ",
    fr = "Impossible d'utiliser `-.gg()` avec un seul argument, "
  )
})

test_that("rename is ok", {

  # Test for ggplot2 else skip
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error_bilingual(
      rename_plot_sample_ids(123, sample_ids = new_ids),
      en = "requires ggplot2 package installation",
      fr = "nécessite l'installation du paquet ggplot2"
    )

    testthat::skip("ggplot2 not available for testing renames")
  }

  p <- plot_ftir(sample_spectra)

  new_ids <- c(
    "Toluene" = "toluene",
    "C7 Alkane" = "heptanes",
    "IPA" = "isopropanol",
    "White Paper" = "paper",
    "PS Film" = "polystyrene"
  )

  rp <- rename_plot_sample_ids(p, new_ids)
  expect_true(ggplot2::is_ggplot(rp))
  expect_true("Toluene" %in% rp$scales$scales[[1]]$labels)
  expect_true("C7 Alkane" %in% rp$scales$scales[[1]]$labels)

  expect_error_bilingual(
    rename_plot_sample_ids(sample_spectra, new_ids),
    en = "`ftir_spectra_plot` must be a ggplot object. You provided ",
    fr = "`ftir_spectra_plot` doit être un objet ggplot. Vous avez fourni "
  )

  expect_error_bilingual(
    rename_plot_sample_ids(p, c(new_ids, "test" = "failure")),
    en = "All provided `old names` must be in the `ftir_spectra_plot`.",
    fr = "Tous les `anciens noms` fournis doivent être dans le `ftir_spectra_plot`."
  )

  # check only partial names still makes a plot
  rp <- rename_plot_sample_ids(p, new_ids[1])
  expect_true(ggplot2::is_ggplot(rp))
  expect_true("Toluene" %in% rp$scales$scales[[1]]$labels)
  expect_false("C7 Alkane" %in% rp$scales$scales[[1]]$labels)
})

test_that("legend moving is ok", {
  # Ensure caught failure if no ggplot2, then skip remainder of tests
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.
    expect_error_bilingual(
      move_plot_legend(123, position = "bottom"),
      en = "requires ggplot2 package installation",
      fr = "nécessite l'installation du paquet ggplot2"
    )

    testthat::skip("ggplot2 not available for testing manipulations")
  }

  biodiesel_plot <- plot_ftir(biodiesel)

  # test arg checks.

  expect_error_bilingual(
    move_plot_legend("abc", position = "bottom"),
    en = "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fr = "`ftir_spectra_plot` doit être un objet ggplot. Vous avez fourni a string"
  )

  expect_error_bilingual(
    move_plot_legend(biodiesel_plot, position = "bob"),
    en = "`position` must be one of ",
    fr = "`position` doit être l'un des "
  )
  expect_error_bilingual(
    move_plot_legend(
      biodiesel_plot,
      position = "bottom",
      justification = "bob"
    ),
    en = "`justification` must be one of ",
    fr = "`justification` doit être l'un des "
  )
  expect_error_bilingual(
    move_plot_legend(biodiesel_plot, direction = "bob"),
    en = "`direction` must be one of ",
    fr = "`direction` doit être l'un des "
  )
  expect_error_bilingual(
    move_plot_legend(biodiesel_plot, legend_title_position = "bob"),
    en = "`legend_title_position` must be one of ",
    fr = "`legend_title_position` doit être l'un des "
  )

  # Plots should come out mostly the same.
  moved_legend_plot <- move_plot_legend(
    biodiesel_plot,
    position = "bottom",
    direction = "horizontal"
  )

  bdlab <- ggplot2::get_labs(biodiesel_plot)
  mlplab <- ggplot2::get_labs(moved_legend_plot)
  expect_equal(bdlab$title, mlplab$title)

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range,
    ggplot2::ggplot_build(moved_legend_plot)$layout$panel_params[[1]]$x.range
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(moved_legend_plot)$layout$panel_params[[1]]$y.range
  )
})

test_that("highlighting is ok", {
  # Ensure caught failure if no ggplot2, then skip remainder of tests
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.
    expect_error_bilingual(
      highlight_sample(123, "test"),
      en = "requires ggplot2 package installation",
      fr = "nécessite l'installation du paquet ggplot2"
    )

    testthat::skip("ggplot2 not available for testing manipulations")
  }

  biodiesel_plot <- plot_ftir(biodiesel)

  if (!require("gghighlight", quietly = TRUE)) {
    expect_error_bilingual(
      highlight_sample(biodiesel_plot, "test"),
      en = "requires gghighlight package installation",
      fr = "nécessite l'installation du paquet gghighlight"
    )

    testthat::skip("gghighlight not available for testing manipulations")
  }

  # test arg checks.

  expect_error_bilingual(
    highlight_sample("abc", "sample"),
    en = "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fr = "`ftir_spectra_plot` doit être un objet ggplot. Vous avez fourni a string"
  )

  expect_error_bilingual(
    highlight_sample(biodiesel_plot, "sample"),
    en = "All provided `sample_ids` must be in the `ftir_spectra_plot`.",
    fr = "Tous les `sample_ids` fournis doivent être dans le `ftir_spectra_plot`."
  )

  # Plots should come out mostly the same.
  highlighted_plot <- highlight_sample(biodiesel_plot, "diesel_unknown")

  bdlab <- ggplot2::get_labs(biodiesel_plot)
  hplab <- ggplot2::get_labs(highlighted_plot)
  expect_equal(bdlab$title, hplab$title)

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range,
    ggplot2::ggplot_build(highlighted_plot)$layout$panel_params[[1]]$x.range
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(highlighted_plot)$layout$panel_params[[1]]$y.range
  )
})

test_that("add_band is ok", {
  # Ensure caught failure if no ggplot2, then skip remainder of tests
  if (!require("ggplot2", quietly = TRUE)) {
    # Of course, we can't generate a plot to feed to the manipulations.
    # This means that we can pass any value, the `ggplot` presence is tested first.

    expect_error_bilingual(
      add_band(123),
      en = "requires ggplot2 package installation",
      fr = "nécessite l'installation du paquet ggplot2"
    )
    testthat::skip("ggplot2 not available for testing manipulations")
  }

  biodiesel_plot <- plot_ftir(biodiesel)

  # test arg checks.
  expect_error_bilingual(
    add_band("abc"),
    en = "`ftir_spectra_plot` must be a ggplot object. You provided a string",
    fr = "`ftir_spectra_plot` doit être un objet ggplot. Vous avez fourni a string"
  )
  expect_error_bilingual(
    add_band(biodiesel_plot, wavenumber_range = 100),
    en = "`wavenumber_range` must be a numeric vector of length two.",
    fr = "`wavenumber_range` doit être un vecteur numérique de longueur deux."
  )
  expect_error_bilingual(
    add_band(biodiesel_plot, wavenumber_range = c("a", "b")),
    en = "`wavenumber_range` must be a numeric vector of length two.",
    fr = "`wavenumber_range` doit être un vecteur numérique de longueur deux."
  )
  expect_error_bilingual(
    add_band(biodiesel_plot, wavenumber_range = c(200, 2000)),
    en = "`wavenumber_range` must be values between 701 and 3999 cm^-1.",
    fr = "`wavenumber_range` doit être des valeurs comprises entre 701 et 3999 cm^-1."
  )

  expect_error_bilingual(
    add_band(biodiesel_plot, wavenumber_range = c(1000, 2000), text = mtcars),
    en = "`text` must be character or numeric, you provided a data frame.",
    fr = "`text` doit être de type caractère ou numérique, vous avez fourni a data frame."
  )
  expect_error_bilingual(
    add_band(
      biodiesel_plot,
      wavenumber_range = c(1000, 2000),
      text = biodiesel_plot
    ),
    en = "`text` must be character or numeric, you provided a <gg",
    fr = "`text` doit être de type caractère ou numérique, vous avez fourni a <gg"
  )
  expect_error_bilingual(
    add_band(
      biodiesel_plot,
      wavenumber_range = c(1000, 2000),
      text = c("This is", "too long")
    ),
    en = "`text` should be character or numeric, but not a vector of length greater than one.",
    fr = "`text` doit être de type caractère ou numérique, mais pas un vecteur de longueur supérieure à un."
  )
  # Plots should come out mostly the same.
  banded_plot <- add_band(biodiesel_plot, c(1000, 2000))

  expect_equal(
    add_band(biodiesel_plot, c(1000, 1900)),
    add_band(biodiesel_plot, c(1900, 1000))
  )

  bdlab <- ggplot2::get_labs(biodiesel_plot)
  bplab <- ggplot2::get_labs(banded_plot)
  expect_equal(bdlab$title, bplab$title)

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$x.range,
    ggplot2::ggplot_build(banded_plot)$layout$panel_params[[1]]$x.range
  )

  expect_equal(
    ggplot2::ggplot_build(biodiesel_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(banded_plot)$layout$panel_params[[1]]$y.range
  )

  expect_equal(
    ggplot2::ggplot_build(banded_plot)$layout$panel_params[[1]]$y.range,
    ggplot2::ggplot_build(add_band(
      biodiesel_plot,
      c(1000, 1900),
      "Test Range"
    ))$layout$panel_params[[1]]$y.range
  )

  expect_equal(
    add_band(biodiesel_plot, c(1000, 1000), "test", "blue"),
    add_wavenumber_marker(
      biodiesel_plot,
      1000,
      "test",
      line_aesthetics = list(color = "blue")
    )
  )
})
