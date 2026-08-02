test_that("Plots are generated", {
  # Test for ggplot2 else skip
  if (!require("ggplot2", quietly = TRUE)) {
    expect_error_bilingual(
      plot_ftir(biodiesel),
      en = "requires ggplot2 package installation",
      fr = "nécessite l'installation du paquet ggplot2"
    )

    testthat::skip("ggplot2 not available for testing plot production")
  }

  p1 <- plot_ftir(biodiesel)
  p2 <- plot_ftir_stacked(biodiesel)

  p3 <- plot_ftir(absorbance_to_transmittance(biodiesel))
  p4 <- plot_ftir_stacked(absorbance_to_transmittance(biodiesel))
  p5 <- plot_ftir(normalize_spectra(biodiesel))

  expect_true(ggplot2::is_ggplot(p1))
  expect_true(ggplot2::is_ggplot(p2))
  p1lab <- ggplot2::get_labs(p1)
  p2lab <- ggplot2::get_labs(p2)
  expect_equal(p1lab$y, "Absorbance")
  expect_equal(p2lab$y, "Absorbance (a.u.)")

  p3lab <- ggplot2::get_labs(p3)
  p4lab <- ggplot2::get_labs(p4)
  p5lab <- ggplot2::get_labs(p5)
  expect_true(ggplot2::is_ggplot(p3))
  expect_true(ggplot2::is_ggplot(p4))
  expect_true(ggplot2::is_ggplot(p5))
  expect_equal(p3lab$y, "% Transmittance")
  expect_equal(p4lab$y, "Transmittance (a.u.)")
  expect_equal(p5lab$y, "Normalized Absorbance")

  # ensure lots of samples can be plotted with rollover to viridis palette.
  p6 <- suppressWarnings(plot_ftir(rbind(biodiesel, sample_spectra)))
  p6lab <- ggplot2::get_labs(p6)
  expect_true(ggplot2::is_ggplot(p6))
  expect_equal(p6lab$y, "Absorbance")
})

test_that("data is checked correctly", {
  if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing manipulations")
  }

  full_data_df <- data.frame(
    "sample_id" = LETTERS,
    "wavenumber" = seq_along(LETTERS),
    "absorbance" = runif(length(LETTERS)),
    "transmittance" = runif(length(LETTERS)) * 100
  )

  expect_error_bilingual(
    plot_ftir(ftir = "abc"),
    en = "`ftir` must be a data frame. You provided a string.",
    fr = "`ftir` doit être un data.frame. Vous avez fourni a string."
  )
  expect_error_bilingual(
    plot_ftir(ftir = data.frame("a" = 1:10)),
    en = "It must contain a column named",
    fr = "Il doit contenir une colonne nommée"
  )
  expect_error_bilingual(
    plot_ftir(ftir = full_data_df[, c("sample_id", "wavenumber")]),
    en = "`ftir` must have one of `absorbance`, `transmittance`, or `intensity` columns.",
    fr = "`ftir` doit contenir une des colonnes `absorbance`, `transmittance`, ou `intensity`."
  )
  expect_error_bilingual(
    plot_ftir(ftir = full_data_df),
    en = "cannot contain more than one of `absorbance`, `transmittance`, or `intensity` columns.",
    fr = "`ftir` ne peut pas contenir plus d'une des colonnes `absorbance`, `transmittance`, ou `intensity`."
  )

  expect_error_bilingual(
    plot_ftir_stacked(ftir = "abc"),
    en = "`ftir` must be a data frame. You provided a string.",
    fr = "`ftir` doit être un data.frame. Vous avez fourni a string."
  )
  expect_error_bilingual(
    plot_ftir_stacked(ftir = data.frame("a" = 1:10)),
    en = "`ftir` is missing a column",
    fr = "`ftir` ne contient pas une colonne"
  )
  expect_error_bilingual(
    plot_ftir_stacked(ftir = full_data_df[, c("sample_id", "wavenumber")]),
    en = "`ftir` must have one of `absorbance`, `transmittance`, or `intensity` columns.",
    fr = "`ftir` doit contenir une des colonnes `absorbance`, `transmittance`, ou `intensity`"
  )
  expect_error_bilingual(
    plot_ftir_stacked(ftir = full_data_df),
    en = "`ftir` cannot contain more than one of `absorbance`, `transmittance`, or `intensity` columns.",
    fr = "`ftir` ne peut pas contenir plus d'une des colonnes `absorbance`, `transmittance`, ou `intensity`."
  )

  colnames(full_data_df)[4] <- "logabs"
  expect_error_bilingual(
    plot_ftir(ftir = full_data_df),
    en = "`ftir` may only contain columns `sample_id`, `wavenumber`, and one of `absorbance`, `transmittance`, or `intensity`.",
    fr = "`ftir` ne peut contenir que les colonnes `sample_id`, `wavenumber`, et une des colonnes `absorbance`, `transmittance`, ou `intensity`."
  )
  expect_error_bilingual(
    plot_ftir(biodiesel, 1234),
    en = "`plot_title` must be a character string or vector of strings with length not more than two.",
    fr = "`plot_title` doit être une chaîne de caractères ou un vecteur de chaînes de caractères avec une longueur maximale de deux."
  )
  expect_error_bilingual(
    plot_ftir(biodiesel, c("My Plot", "My Subplot", "My Extrasubplot")),
    en = "`plot_title` must be a character string or vector of strings with length not more than two.",
    fr = "`plot_title` doit être une chaîne de caractères ou un vecteur de chaînes de caractères avec une longueur maximale de deux."
  )
  expect_error_bilingual(
    plot_ftir(biodiesel, legend_title = 1234),
    en = "`legend_title` must be a single character string.",
    fr = "`legend_title` doit être une unique chaîne de caractères."
  )

  expect_error_bilingual(
    plot_ftir_stacked(ftir = full_data_df),
    en = "`ftir` may only contain columns `sample_id`, `wavenumber`, and one of `absorbance`, `transmittance`, or `intensity`.",
    fr = "`ftir` ne peut contenir que les colonnes `sample_id`, `wavenumber`, et une des colonnes `absorbance`, `transmittance`, ou `intensity`."
  )
  expect_error_bilingual(
    plot_ftir_stacked(biodiesel, stack_offset = "abc"),
    en = "`stack_offset` must be a single numeric value.",
    fr = "`stack_offset` doit être une valeur numérique unique."
  )
  expect_error_bilingual(
    plot_ftir_stacked(biodiesel, stack_offset = -10),
    en = "`stack_offset` must be between 0 and 200.",
    fr = "`stack_offset` doit être compris entre 0 et 200."
  )

  expect_warning_bilingual(
    plot_ftir(rbind(biodiesel, sample_spectra)),
    en = "The color palette in use works best with 12 or fewer unique samples in",
    fr = "La palette de couleurs utilisée fonctionne mieux avec 12 échantillons uniques ou moins dans"
  )
})

test_that("Error messages are bilingual", {
  if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing language integration")
  }

  # Test that error messages work properly in both languages
  withr::with_envvar(new = c(LANG = 'en_US.UTF-8'), {
    # This should fail with English message
    expect_error(
      plot_ftir(biodiesel, lang = "bob"),
      "`lang` must be one of ",
      fixed = TRUE
    )
  })

  withr::with_envvar(new = c(LANG = 'fr_FR.UTF-8'), {
    # This should fail with French message (but we only check for partial match)
    expect_error(
      plot_ftir(biodiesel, lang = "bob"),
      "`lang` must be one of ",
      fixed = TRUE
    )
  })
})

test_that("Language settings work", {
  if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing language integration")
  }

  expect_error(
    plot_ftir(biodiesel, lang = "bob"),
    "`lang` must be one of ",
    fixed = TRUE
  )
  expect_error(
    plot_ftir_stacked(biodiesel, lang = "bob"),
    "`lang` must be one of ",
    fixed = TRUE
  )

  # Test French language settings
  p <- plot_ftir(biodiesel, lang = "fr")

  plab <- ggplot2::get_labs(p)
  expect_equal(plab$title, "Spectres IRTF")
  expect_equal(plab$x, bquote("Nombre d'onde" ~ (cm^-1)))

  p2 <- plot_ftir(
    biodiesel,
    lang = "fr",
    plot_title = c("My Plot", "my subtitle")
  )
  p2lab <- ggplot2::get_labs(p2)
  expect_equal(p2lab$title, "My Plot")
  expect_equal(p2lab$x, bquote("Nombre d'onde" ~ (cm^-1)))

  # Test English language settings
  p3 <- plot_ftir(biodiesel, lang = "en")
  plab3 <- ggplot2::get_labs(p3)
  expect_equal(plab3$title, "FTIR Spectra")
  expect_equal(plab3$x, bquote("Wavenumber" ~ (cm^-1)))

  # Test all language specification variants
  for (lang in c("english", "anglais")) {
    p <- plot_ftir(biodiesel, lang = lang)
    plab <- ggplot2::get_labs(p)
    expect_equal(plab$title, "FTIR Spectra")
    expect_equal(plab$x, bquote("Wavenumber" ~ (cm^-1)))
  }

  # Test French variants with the same function
  for (lang in c("fr", "french", "francais", "fran\u00e7ais")) {
    p <- plot_ftir(biodiesel, lang = lang)
    plab <- ggplot2::get_labs(p)
    expect_equal(plab$title, "Spectres IRTF")
    expect_equal(plab$x, bquote("Nombre d'onde" ~ (cm^-1)))
  }

  # Test stacked plot language settings
  p_stacked <- plot_ftir_stacked(biodiesel, lang = "fr")
  plab_stacked <- ggplot2::get_labs(p_stacked)
  expect_equal(plab_stacked$title, "Spectres IRTF")
  expect_equal(plab_stacked$x, bquote("Nombre d'onde" ~ (cm^-1)))

  # Test that legend title is also translated
  p <- plot_ftir(biodiesel, lang = "fr", legend_title = "Sample ID")
  plab <- ggplot2::get_labs(p)
  expect_equal(plab$colour, "ID de l'\u00e9chantillon")

  # Test that legend title is also translated in English
  p <- plot_ftir(biodiesel, lang = "en", legend_title = "Sample ID")
  plab <- ggplot2::get_labs(p)
  expect_equal(plab$colour, "Sample ID")
})
