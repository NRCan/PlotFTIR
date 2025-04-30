test_that("find_ftir_peaks handles input errors ok", {
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = seq(4000, 400, length.out = 100),
    absorbance = rnorm(100)
  )
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  expect_error(find_ftir_peaks(ftir), NA) # No error expected
  expect_error(
    find_ftir_peaks(ftir, zero_norm = "non-numeric"),
    "`zero_norm` must be numeric"
  ) # incorrect argument
  expect_error(
    find_ftir_peaks(ftir, zero_deriv = "non-numeric"),
    "`zero_deriv` must be numeric"
  ) # incorrect argumenet

  # Multiple sample spectra passed in
  ftir <- data.frame(
    sample_id = c(rep("sample1", 50), rep("sample2", 50)),
    wavenumber = seq(4000, 400, length.out = 100),
    absorbance = rnorm(100)
  )
  expect_error(find_ftir_peaks(ftir), "must only contain one sample spectra")

  # Transmission spectra passed in
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = seq(4000, 400, length.out = 100),
    transmittance = runif(100, min = 10, max = 100)
  )
  expect_error(find_ftir_peaks(ftir), NA) # should be no error
})


test_that("find_ftir_peaks returns sorted peaks", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = seq(4000, 400, length.out = 100),
    absorbance = rnorm(100)
  )
  peaks <- find_ftir_peaks(ftir)
  expect_equal(peaks, sort(peaks))
})

test_that("find_ftir_peaks returns correct peaks", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 10)
  )
  peaks <- find_ftir_peaks(
    ftir,
    sg_p_deriv = 3,
    sg_n_deriv = 7,
    sg_p_norm = 3,
    sg_n_norm = 7,
    window_norm = 50,
    window_deriv = 50
  )
  expect_equal(length(peaks), 10)
  expect_equal(
    peaks,
    c(545, 909, 1273, 1636, 2000, 2364, 2727, 3091, 3455, 3818)
  )
})

test_that("Fixed Peak Locations don't move", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]
  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  peaklist <- c(
    1040,
    1100,
    1130,
    1160,
    1190,
    1220,
    1260,
    1300,
    1340,
    1380,
    1410,
    1460,
    1560,
    1750,
    1900,
    1970
  )

  gmm_loose <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = F,
    method = "g"
  )

  lmm_loose <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = F,
    method = "l"
  )
  pvmm_loose <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = F,
    method = "pv"
  )
  dsgmm_loose <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = F,
    method = "dsg"
  )

  expect_false(all(gmm_loose$mu == peaklist))
  expect_false(all(lmm_loose$mu == peaklist))
  expect_false(all(pvmm_loose$mu == peaklist))
  expect_false(all(dsgmm_loose$mu == peaklist))

  gmm_fixed <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = TRUE,
    method = "g"
  )

  lmm_fixed <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = TRUE,
    method = "l"
  )
  pvmm_fixed <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = TRUE,
    method = "pv"
  )
  dsgmm_fixed <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = TRUE,
    method = "dsg"
  )

  expect_equal(gmm_fixed$mu, peaklist)
  expect_equal(lmm_fixed$mu, peaklist)
  expect_equal(pvmm_fixed$mu, peaklist)
  expect_equal(dsgmm_fixed$mu, peaklist)
})

test_that("zero_normalization and zero_deriv check ok", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 10)
  )
  peaks <- find_ftir_peaks(
    ftir,
    sg_p_deriv = 3,
    sg_n_deriv = 7,
    sg_p_norm = 3,
    sg_n_norm = 7,
    window_norm = 50,
    window_deriv = 50
  )

  expect_error(
    peaks <- find_ftir_peaks(
      ftir,
      sg_p_deriv = 3,
      sg_n_deriv = 7,
      sg_p_norm = 3,
      sg_n_norm = 7,
      window_norm = 50,
      window_deriv = 50,
      zero_norm = 100
    ),
    "is larger than the highest point in the spectra."
  )

  expect_error(
    peaks <- find_ftir_peaks(
      ftir,
      sg_p_deriv = 3,
      sg_n_deriv = 7,
      sg_p_norm = 3,
      sg_n_norm = 7,
      window_norm = 50,
      window_deriv = 50,
      zero_deriv = 100
    ),
    "is larger than the highest point in the derivative spectra."
  )
})

test_that("maxima function detects local maximas", {
  x <- c(1, 2, 3, 4, 5, 4, 3, 2, 1)
  expect_equal(maxima(x), 5)
  x <- c(1, 2, 3, 4, 5, 3, 4, 5, 6, 5, 4, 3, 2, 1)
  expect_equal(maxima(x, window = 2), c(5, 9))
})

test_that("minima function detects local minimas", {
  x <- c(1, 2, 3, 4, 5, 4, 3, 2, 1)
  expect_equal(minima(x), c(1, 9))
  x <- c(3, 2, 3, 4, 5, 3, 4, 5, 6, 5, 4, 3, 2, 1)
  expect_equal(minima(x), c(2, 6, 14))
  x <- c(1, 2, 3, 4, 5, 4, 5, 4, 5, 4, 5, 6, 5, 3, 5, 4, 3, 2, 1)
  expect_equal(minima(x), c(1, 6, 8, 10, 14, 19))
  expect_equal(minima(x, window = 2), c(1, 14, 19))
})

test_that("zero_threshold sets to zero values below threshold", {
  x <- c(1, 0.01, 0.001, 0.0001)
  expect_equal(zero_threshold(x, threshold = 1e-3), c(1, 0.01, 0.001, 0))
  x <- c(-1, -0.01, -0.001, -0.0001)
  expect_equal(zero_threshold(x, threshold = 1e-2), c(-1, -0.01, 0, 0))
  x <- c(1, -0.01, 0.001, -0.001)
  expect_equal(zero_threshold(x, threshold = 1e-2), c(1, -0.01, 0, 0))
})

test_that("fit_peaks (voigt) returns correct results", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 1, 0), 10)
  )
  fitted_peaks <- fit_peaks(ftir, method = "voigt")
  expect_equal(fitted_peaks$method, "voigt")
  expect_equal(length(fitted_peaks$mu), 10)
  expect_equal(
    round(fitted_peaks$mu),
    c(545, 909, 1273, 1636, 2000, 2364, 2727, 3091, 3455, 3818)
  )

  fitted_peaks$method <- NULL
  expect_warning(
    fittype <- get_fit_method(fitted_peaks),
    "should be generated with"
  )
  expect_equal(fittype, "voigt")
})

test_that("fit_peaks (gaussian) returns correct results", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 1, 0), 10)
  )
  fitted_peaks <- fit_peaks(ftir, method = "gaussian")
  expect_equal(fitted_peaks$method, "gauss")
  expect_equal(length(fitted_peaks$mu), 10)
  expect_equal(
    round(fitted_peaks$mu),
    c(545, 909, 1273, 1636, 2000, 2364, 2727, 3091, 3455, 3818)
  )

  fitted_peaks$method <- NULL
  expect_warning(
    fittype <- get_fit_method(fitted_peaks),
    "should be generated with"
  )
  expect_equal(fittype, "gauss")
})

test_that("fit_peaks (lorentz) returns correct results", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 1, 1, 2, 3, 10, 3, 2, 1, 1), 10)
  )
  fitted_peaks <- fit_peaks(ftir, method = "lorentz")
  expect_equal(fitted_peaks$method, "lorentz")
  expect_equal(length(fitted_peaks$mu), 10)
  expect_equal(
    round(fitted_peaks$mu),
    c(545, 909, 1273, 1636, 2000, 2364, 2727, 3091, 3455, 3819)
  )

  fitted_peaks$method <- NULL
  expect_warning(
    fittype <- get_fit_method(fitted_peaks),
    "should be generated with"
  )
  expect_equal(fittype, "lorentz")
})

test_that("fit_peaks (dsg) returns correct results", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 1, 0), 10)
  )
  fitted_peaks <- fit_peaks(ftir, method = "dsg")
  expect_equal(fitted_peaks$method, "doniach-šunjić-gauss")
  expect_equal(length(fitted_peaks$mu), 10)
  expect_equal(
    round(fitted_peaks$mu),
    c(545, 909, 1273, 1636, 2000, 2364, 2727, 3091, 3455, 3818)
  )

  fitted_peaks$method <- NULL
  expect_warning(
    fittype <- get_fit_method(fitted_peaks),
    "should be generated with"
  )
  expect_equal(fittype, "doniach-šunjić-gauss")
})

test_that("fit_peaks error checks are ok", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  expect_error(
    fit_peaks(absorbance_to_transmittance(ftir)),
    "must be supplied in absorbance units"
  )

  expect_error(
    fit_peaks(sample_spectra),
    "must only contain one sample spectra"
  )

  expect_error(
    fit_peaks(ftir, method = "bad_method"),
    "must be one of `voigt`, `lorentz`, `gauss` or `dsg`"
  )
})

test_that("Peak data.frame is created ok", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  peaksdf <- fit_peak_df(fitpeaks)

  expect_equal(
    colnames(peaksdf),
    c(
      "sample_id",
      "peak",
      "wavenumber",
      "sigma",
      "eta",
      "mix_ratio",
      "peak_shape"
    )
  )

  expect_equal(
    colnames(fit_peak_df(fit_peaks(ftir, method = "gauss"))),
    c("sample_id", "peak", "wavenumber", "sigma", "mix_ratio", "peak_shape")
  )
  expect_equal(
    colnames(fit_peak_df(fit_peaks(ftir, method = "lorentz"))),
    c("sample_id", "peak", "wavenumber", "gam", "mix_ratio", "peak_shape")
  )
  expect_equal(
    colnames(fit_peak_df(fit_peaks(ftir, method = "dsg"))),
    c(
      "sample_id",
      "peak",
      "wavenumber",
      "sigma",
      "eta",
      "alpha",
      "mix_ratio",
      "peak_shape"
    )
  )
})

test_that("get_fit_spectra works ok", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitg <- fit_peaks(ftir, method = "gauss")
  fitv <- fit_peaks(ftir, method = "voigt")
  fitl <- fit_peaks(ftir, method = "lorentz")
  fitd <- fit_peaks(ftir, method = "dsg")

  expect_equal(length(get_fit_spectra(ftir, fitg)), length(ftir$wavenumber))
  expect_equal(length(get_fit_spectra(ftir, fitv)), length(ftir$wavenumber))
  expect_equal(length(get_fit_spectra(ftir, fitl)), length(ftir$wavenumber))
  expect_equal(length(get_fit_spectra(ftir, fitd)), length(ftir$wavenumber))

  expect_equal(length(get_fit_spectra(ftir, fitg, 3)), length(ftir$wavenumber))
  expect_equal(length(get_fit_spectra(ftir, fitv, 3)), length(ftir$wavenumber))
  expect_equal(length(get_fit_spectra(ftir, fitl, 3)), length(ftir$wavenumber))
  expect_equal(length(get_fit_spectra(ftir, fitd, 3)), length(ftir$wavenumber))
})

test_that("get_fit_spectra checks are ok", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  expect_error(
    get_fit_spectra(ftir, fitpeaks, peak = "all"),
    "requested peak must be an integer value"
  )
  expect_error(
    get_fit_spectra(ftir, fitpeaks, peak = 1.5),
    "requested peak must be an integer value"
  )
  expect_error(
    get_fit_spectra(ftir, fitpeaks, peak = 100),
    "requested peak 100 is out of range"
  )
  expect_error(
    get_fit_spectra(ftir, fitpeaks, peak = -1),
    "requested peak -1 is out of range"
  )
})


test_that("plot_fit_ftir_peaks work", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  if (!require("ggplot2", quietly = TRUE)) {
    expect_error(
      plot_fit_ftir_peaks(ftir, fitpeaks),
      "requires ggplot2 package installation",
      fixed = TRUE
    )

    testthat::skip("ggplot2 not available for testing peak fit plot production")
  }

  p <- plot_fit_ftir_peaks(ftir, fitpeaks)
  expect_true(ggplot2::is_ggplot(p))
  expect_equal(p$labels$title, "Fitted FTIR Plot")
  expect_equal(
    p$labels$subtitle,
    "Showing as-analyzed spectra and sum of Voigt fitted peaks"
  )

  p2 <- plot_fit_ftir_peaks(
    ftir,
    fitpeaks,
    plot_title = c("Test Plot", "Test Subtitle")
  )
  expect_equal(p2$labels$title, "Test Plot")
  expect_equal(p2$labels$subtitle, "Test Subtitle")
})

test_that("plot_fit_residuals work", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  if (!require("ggplot2", quietly = TRUE)) {
    expect_error(
      plot_fit_residuals(ftir, fitpeaks),
      "requires ggplot2 package installation",
      fixed = TRUE
    )

    testthat::skip("ggplot2 not available for testing residual plot production")
  }

  p <- plot_fit_residuals(ftir, fitpeaks)
  expect_true(ggplot2::is_ggplot(p))
  expect_equal(p$labels$title, "Residual Plot")
  expect_equal(
    p$labels$subtitle,
    "Residual of Voigt fitted peaks and isopropanol"
  )

  p2 <- plot_fit_residuals(
    ftir,
    fitpeaks,
    plot_title = c("Test Plot", "Test Subtitle")
  )
  expect_equal(p2$labels$title, "Test Plot")
  expect_equal(p2$labels$subtitle, "Test Subtitle")
})

test_that("plot_components work", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]
  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  if (!require("ggplot2", quietly = TRUE)) {
    expect_error(
      plot_components(ftir, fitpeaks),
      "requires ggplot2 package installation",
      fixed = TRUE
    )

    testthat::skip(
      "ggplot2 not available for testing component plot production"
    )
  }

  if (!require("gghighlight", quietly = TRUE)) {
    expect_error(
      plot_components(ftir, fitpeaks),
      "requires gghighlight package installation",
      fixed = TRUE
    )

    testthat::skip(
      "gghighlight not available for testing component plot production"
    )
  }

  p <- plot_components(ftir, fitpeaks)

  expect_true(ggplot2::is_ggplot(p))
  expect_equal(p$labels$title, "Fitted FTIR Plot")
  expect_equal(
    p$labels$subtitle,
    "Showing as-analyzed spectra and components of Voigt fitted peaks"
  )

  p2 <- plot_components(ftir, fitpeaks, plot_fit = TRUE)
  p3 <- plot_fit_ftir_peaks(ftir, fitpeaks, plot_components = TRUE)
  expect_equal(p2, p3)
  p4 <- plot_components(
    ftir,
    fitpeaks,
    plot_title = c("Test Plot", "Test Subtitle")
  )
  expect_equal(p4$labels$title, "Test Plot")
  expect_equal(p4$labels$subtitle, "Test Subtitle")
})

test_that("plot_fit_ftir_peaks error checks are ok", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing fit peak plot production")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  ftir_trans <- absorbance_to_transmittance(ftir)

  expect_error(
    plot_fit_ftir_peaks(ftir_trans, fitpeaks),
    "must be supplied in absorbance units"
  )
  expect_error(
    plot_fit_ftir_peaks(sample_spectra, fitpeaks),
    "must only contain one sample spectra"
  )
  expect_warning(
    plot_fit_ftir_peaks(
      sample_spectra[
        sample_spectra$sample_id == "toluene",
      ],
      fitpeaks
    ),
    "does not contain fit peaks that match the ftir sample provided"
  )
  expect_error(
    plot_fit_ftir_peaks(ftir, fitpeaks, extra_arg = "ok"),
    "Supplied 1 unused argument: extra_arg"
  )
  fitpeaks$sample_id <- NULL
  expect_warning(
    plot_fit_ftir_peaks(ftir, fitpeaks),
    "should be generated with"
  )
})

test_that("plot_fit_residuals error checks are ok", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip(
      "ggplot2 not available for testing fit residual plot production"
    )
  }

  if (!require("gghighlight", quietly = TRUE)) {
    testthat::skip(
      "ggplot2 not available for testing fit residual plot production"
    )
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  ftir_trans <- absorbance_to_transmittance(ftir)

  expect_error(
    plot_fit_residuals(ftir_trans, fitpeaks),
    "must be supplied in absorbance units"
  )
  expect_error(
    plot_fit_residuals(sample_spectra, fitpeaks),
    "must only contain one sample spectra"
  )
  expect_warning(
    plot_fit_residuals(
      sample_spectra[
        sample_spectra$sample_id == "toluene",
      ],
      fitpeaks
    ),
    "does not contain fit peaks that match the ftir sample provided"
  )
  fitpeaks$sample_id <- NULL
  expect_warning(plot_fit_residuals(ftir, fitpeaks), "should be generated with")
  expect_error(
    plot_fit_residuals(ftir, fitpeaks, extra_arg = "ok"),
    "Supplied 1 unused argument: extra_arg"
  )
})

test_that("plot_components error checks are ok", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip(
      "ggplot2 not available for testing fit component plot production"
    )
  }

  if (!require("gghighlight", quietly = TRUE)) {
    testthat::skip(
      "gghighlight not available for testing fit component plot production"
    )
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  ftir_trans <- absorbance_to_transmittance(ftir)

  expect_error(
    plot_components(ftir_trans, fitpeaks),
    "must be supplied in absorbance units"
  )
  expect_error(
    plot_components(sample_spectra, fitpeaks),
    "must only contain one sample spectra"
  )
  expect_warning(
    plot_components(
      sample_spectra[
        sample_spectra$sample_id == "toluene",
      ],
      fitpeaks
    ),
    "does not contain fit peaks that match the ftir sample provided"
  )
  fitpeaks$sample_id <- NULL
  expect_warning(plot_components(ftir, fitpeaks), "should be generated with")
  expect_error(
    plot_components(ftir, fitpeaks, extra_arg = "ok"),
    "Supplied 1 unused argument: extra_arg"
  )
})

test_that("Languages are handled properly", {
  if (!requireNamespace('signal', quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip(
      "ggplot2 not available for testing fit component plot production"
    )
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  p <- plot_fit_ftir_peaks(ftir, fitpeaks)
  expect_equal(p$labels$title, "Fitted FTIR Plot")
  expect_equal(
    p$labels$subtitle,
    "Showing as-analyzed spectra and sum of Voigt fitted peaks"
  )
  expect_equal(p$plot_env$legend_title, "Sample ID")

  p <- plot_fit_residuals(ftir, fitpeaks)
  expect_equal(p$labels$title, "Residual Plot")
  expect_equal(
    p$labels$subtitle,
    "Residual of Voigt fitted peaks and isopropanol"
  )

  if (!requireNamespace('gghighlight')) {
    testthat::skip("gghighlight not available for testing")
  }

  p <- plot_components(ftir, fitpeaks)
  expect_equal(p$labels$title, "Fitted FTIR Plot")
  expect_equal(
    p$labels$subtitle,
    "Showing as-analyzed spectra and components of Voigt fitted peaks"
  )
  expect_equal(p$plot_env$legend_title, "Sample ID")

  p <- plot_fit_ftir_peaks(ftir, fitpeaks, plot_components = TRUE)
  expect_equal(p$labels$title, "Fitted FTIR Plot")
  expect_equal(
    p$labels$subtitle,
    "Showing as-analyzed spectra and components of Voigt fitted peaks"
  )
  expect_equal(p$plot_env$legend_title, "Sample ID")

  p <- plot_fit_ftir_peaks(ftir, fitpeaks, lang = "fr")
  expect_equal(p$labels$title, "Trac\u00e9 IRTF ajust\u00e9")
  expect_equal(
    p$labels$subtitle,
    "Montrer les spectres et de la somme des pics ajust\u00e9s par la m\u00e9thode Voigt"
  )
  expect_equal(p$plot_env$legend_title, "ID de l'\u00e9chantillon")

  p <- plot_fit_residuals(ftir, fitpeaks, lang = "fr")
  expect_equal(p$labels$title, "Trac\u00e9 des r\u00e9sidus")
  expect_equal(
    p$labels$subtitle,
    "R\u00e9sidu de Voigt pics ajust\u00e9s et isopropanol"
  )

  p <- plot_components(ftir, fitpeaks, lang = "fr")
  expect_equal(p$labels$title, "Trac\u00e9 IRTF ajust\u00e9")
  expect_equal(
    p$labels$subtitle,
    "Montrer les spectres et es composants analys\u00e9s de pics ajust\u00e9 par la m\u00e9thode Voigt"
  )
  expect_equal(p$plot_env$legend_title, "ID de l'\u00e9chantillon")

  p <- plot_fit_ftir_peaks(ftir, fitpeaks, plot_components = TRUE, lang = "fr")
  expect_equal(p$labels$title, "Trac\u00e9 IRTF ajust\u00e9")
  expect_equal(
    p$labels$subtitle,
    "Montrer les spectres et es composants analys\u00e9s de pics ajust\u00e9 par la m\u00e9thode Voigt"
  )
  expect_equal(p$plot_env$legend_title, "ID de l'\u00e9chantillon")

  expect_warning(
    plot_components(ftir, fitpeaks, lang = "test"),
    "language must be one of 'en', 'english', anglais', 'fr', 'french', 'francais' or"
  )
})


#component-optimization is extnsively tested in peak-fit code as well.

test_that("component-optimization dsgmm error checking is ok", {
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 10)
  )

  expect_error(
    spect_em_dsgmm(
      x = c(4001, ftir$wavenumber),
      y = ftir$absorbance,
      mu = runif(10)
    ),
    "Provided x and y vectors must be of the same length"
  )
  expect_error(
    spect_em_dsgmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      eta = runif(11)
    ),
    "All of mu, sigma, alpha, eta and mix_ratio must be of the same length"
  )
  expect_error(
    spect_em_dsgmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      maxit = 1
    ),
    "must be greater than 1 to perform optimization"
  )
})

test_that("component-optimization gmm error checking is ok", {
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 10)
  )

  expect_error(
    spect_em_gmm(
      x = c(4001, ftir$wavenumber),
      y = ftir$absorbance,
      mu = runif(10)
    ),
    "Provided x and y vectors must be of the same length"
  )
  expect_error(
    spect_em_gmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      sigma = runif(11)
    ),
    "All of mu, sigma, and mix_ratio must be of the same length"
  )
  expect_error(
    spect_em_gmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      maxit = 1
    ),
    "must be greater than 1 to perform optimization"
  )
})

test_that("component-optimization lmm error checking is ok", {
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 10)
  )

  expect_error(
    spect_em_lmm(
      x = c(4001, ftir$wavenumber),
      y = ftir$absorbance,
      mu = runif(10),
      gam = rep(10, 10)
    ),
    "Provided x and y vectors must be of the same length"
  )
  expect_error(
    spect_em_lmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      gam = runif(11)
    ),
    "All of mu, gam, and mix_ratio must be of the same length"
  )
  expect_error(
    spect_em_lmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      maxit = 1
    ),
    "must be greater than 1 to perform optimization"
  )
})

test_that("component-optimization pvmm error checking is ok", {
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 10)
  )

  expect_error(
    spect_em_pvmm(
      x = c(4001, ftir$wavenumber),
      y = ftir$absorbance,
      mu = runif(10)
    ),
    "Provided x and y vectors must be of the same length"
  )
  expect_error(
    spect_em_pvmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      eta = runif(11)
    ),
    "All of mu, sigma, eta, and mix_ratio must be of the same length"
  )
  expect_error(
    spect_em_pvmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      maxit = 1
    ),
    "must be greater than 1 to perform optimization"
  )
})

test_that("component-optimization verbose calls are ok", {
  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]
  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  mu_list <- c(
    1041,
    1104,
    1129,
    1159,
    1188,
    1222,
    1263,
    1304,
    1340,
    1375,
    1410,
    1462,
    1559,
    1752,
    1896,
    1972
  )

  expect_message(
    spect_em_gmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = mu_list,
      verbose = TRUE
    ),
    "Converged in "
  )
  expect_message(
    spect_em_lmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = mu_list,
      verbose = TRUE
    ),
    "Converged in "
  )
  expect_message(
    spect_em_pvmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = mu_list,
      verbose = TRUE
    ),
    "Converged in "
  )
  expect_message(
    spect_em_dsgmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = mu_list,
      verbose = TRUE
    ),
    "Converged in "
  )
})

test_that("component-optimization fixed-mu is ok", {
  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]
  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  mu_list_rounded <- c(
    1040,
    1100,
    1130,
    1160,
    1190,
    1220,
    1260,
    1300,
    1340,
    1380,
    1410,
    1460,
    1560,
    1750,
    1900,
    1970
  )

  gmm_loose <- spect_em_gmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded
  )
  lmm_loose <- spect_em_lmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded
  )
  pvmm_loose <- spect_em_pvmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded
  )
  dsgmm_loose <- spect_em_dsgmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded
  )

  expect_false(all(gmm_loose$mu == mu_list_rounded))
  expect_false(all(lmm_loose$mu == mu_list_rounded))
  expect_false(all(pvmm_loose$mu == mu_list_rounded))
  expect_false(all(dsgmm_loose$mu == mu_list_rounded))

  gmm_fixed <- spect_em_gmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded,
    fixed_mu = TRUE
  )
  lmm_fixed <- spect_em_lmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded,
    fixed_mu = TRUE
  )
  pvmm_fixed <- spect_em_pvmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded,
    fixed_mu = TRUE
  )
  dsgmm_fixed <- spect_em_dsgmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded,
    fixed_mu = TRUE
  )

  expect_equal(gmm_fixed$mu, mu_list_rounded)
  expect_equal(lmm_fixed$mu, mu_list_rounded)
  expect_equal(pvmm_fixed$mu, mu_list_rounded)
  expect_equal(dsgmm_fixed$mu, mu_list_rounded)
})
