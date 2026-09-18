# === Section 1: find_ftir_peaks() Core Tests ===

test_that("find_ftir_peaks handles input errors ok", {
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = seq(4000, 400, length.out = 100),
    absorbance = rnorm(100)
  )
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  expect_silent(find_ftir_peaks(ftir))
  expect_error_bilingual(
    find_ftir_peaks(ftir, zero_norm = "non-numeric"),
    en = "`zero_norm` must be numeric.",
    fr = "`zero_norm` doit être numérique."
  )
  expect_error_bilingual(
    find_ftir_peaks(ftir, zero_deriv = "non-numeric"),
    en = "`zero_deriv` must be numeric.",
    fr = "`zero_deriv` doit être numérique."
  )

  # Multiple sample spectra passed in
  ftir <- data.frame(
    sample_id = c(rep("sample1", 50), rep("sample2", 50)),
    wavenumber = seq(4000, 400, length.out = 100),
    absorbance = rnorm(100)
  )
  expect_error_bilingual(
    find_ftir_peaks(ftir),
    en = "must only contain one sample spectra.",
    fr = "ne doit contenir qu'un seul spectre d'échantillon."
  )
  # Transmission spectra passed in
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = seq(4000, 400, length.out = 100),
    transmittance = runif(100, min = 10, max = 100)
  )
})

test_that("find_ftir_peaks returns sorted peaks", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
  if (!requireNamespace("signal", quietly = TRUE)) {
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
  expect_length(peaks, 10)
  expect_equal(
    round(peaks),
    c(545, 909, 1273, 1636, 2000, 2364, 2727, 3091, 3455, 3818),
    tolerance = 1e-10
  )
})

# === Section 2: Hybrid Peak Detection Tests ===

test_that("find_ftir_peaks rejects bad window_merge parameter", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = seq(4000, 400, length.out = 100),
    absorbance = rnorm(100)
  )

  expect_error(
    find_ftir_peaks(ftir, window_merge = "non-numeric"),
    "`window_merge` must be numeric"
  )
  expect_error(
    find_ftir_peaks(ftir, window_merge = -1),
    "`window_merge` must be positive"
  )
})

test_that("find_ftir_peaks merges close peaks using representative approach", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 200)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 20)
  )

  peaks_default <- find_ftir_peaks(
    ftir,
    sg_p_deriv = 3,
    sg_n_deriv = 7,
    sg_p_norm = 3,
    sg_n_norm = 7,
    window_norm = 50,
    window_deriv = 50
  )

  peaks_wider_merge <- find_ftir_peaks(
    ftir,
    sg_p_deriv = 3,
    sg_n_deriv = 7,
    sg_p_norm = 3,
    sg_n_norm = 7,
    window_norm = 50,
    window_deriv = 50,
    window_merge = 10
  )

  expect_length(peaks_default, 20)
  expect_type(peaks_wider_merge, "double")
})

test_that("find_ftir_peaks finds flat-topped peaks via first derivative zero-crossing (#33)", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 200)),
    absorbance = c(
      rep(0.1, 40),
      seq(0.1, 3, length.out = 40), # flat-topped rising edge
      rep(3, 40), # flat top
      seq(3, 0.1, length.out = 40), # flat-topped falling edge
      rep(0.1, 40)
    )
  )
  flat_top_indices <- which(ftir$absorbance == max(ftir$absorbance))
  expected_peak <- ftir$wavenumber[round(mean(flat_top_indices))]
  peak_tolerance <- ceiling(abs(mean(diff(ftir$wavenumber))))

  peaks_broad <- find_ftir_peaks(
    ftir,
    sg_p_deriv = 2,
    sg_n_deriv = 11,
    sg_p_norm = 2,
    sg_n_norm = 11,
    window_norm = 30,
    window_deriv = 30
  )

  expect_true(any(abs(peaks_broad - expected_peak) <= peak_tolerance))
})

test_that(".merge_peak_candidates respects merge precedence for nearby candidates (#noissue)", {
  expect_equal(
    .merge_peak_candidates(c(100.5), c(101), window_merge = 1),
    100.5
  )
  expect_equal(
    .merge_peak_candidates(
      c(100),
      c(100.5),
      window_merge = 1,
      prefer = "candidate"
    ),
    100.5
  )
  expect_equal(
    .merge_peak_candidates(c(100.5), c(102), window_merge = 1),
    c(100.5, 102)
  )
})

test_that("peak candidate merging keeps derivative-centered locations over nearby raw maxima (#noissue)", {
  peaks <- .merge_peak_candidates(
    c(100),
    c(100.5),
    window_merge = 1,
    prefer = "candidate"
  )
  peaks <- .merge_peak_candidates(
    peaks,
    c(101),
    window_merge = 1,
    prefer = "existing"
  )

  expect_equal(peaks, 100.5)
})

# === Section 3: fit_peaks() Core Tests ===

test_that("fit_peaks (voigt) returns correct results", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 1, 0), 10)
  )
  fitted_peaks <- fit_peaks(ftir, method = "voigt")
  expect_false(is.null(fitted_peaks$method))
  expect_length(fitted_peaks$mu, 10)
  expect_equal(
    round(fitted_peaks$mu),
    c(545, 909, 1273, 1636, 2000, 2364, 2727, 3091, 3455, 3818)
  )

  fitted_peaks$method <- NULL
  expect_false("method" %in% names(fitted_peaks))
})

test_that("fit_peaks (gaussian) returns correct results", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 1, 0), 10)
  )
  fitted_peaks <- fit_peaks(ftir, method = "gaussian")
  expect_equal(fitted_peaks$method, "gauss")
  expect_length(fitted_peaks$mu, 10)
  expect_equal(
    round(fitted_peaks$mu),
    c(545, 909, 1273, 1636, 2000, 2364, 2727, 3091, 3455, 3818)
  )

  fitted_peaks$method <- NULL
  expect_false("method" %in% names(fitted_peaks))
})

test_that("fit_peaks (lorentz) returns correct results", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 1, 1, 2, 3, 10, 3, 2, 1, 1), 10)
  )
  fitted_peaks <- fit_peaks(ftir, method = "lorentz")
  expect_equal(fitted_peaks$method, "lorentz")
  expect_length(fitted_peaks$mu, 10)
  expect_equal(
    round(fitted_peaks$mu),
    c(545, 909, 1273, 1636, 2000, 2364, 2727, 3091, 3455, 3818),
    tolerance = 1.5
  )

  fitted_peaks$method <- NULL
  expect_false("method" %in% names(fitted_peaks))
})

test_that("fit_peaks (dsg) returns correct results", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 1, 0), 10)
  )
  fitted_peaks <- fit_peaks(ftir, method = "dsg")
  expect_equal(fitted_peaks$method, "doniach-sunjic-gauss")
  expect_length(fitted_peaks$mu, 10)
  expect_equal(
    round(fitted_peaks$mu),
    c(545, 909, 1273, 1636, 2000, 2364, 2727, 3091, 3455, 3818)
  )

  expect_false(is.null(fitted_peaks$method))
})

# === Section 3: Parameter Validation Tests (NEW for integration) ===

test_that("find_ftir_peaks validates parameter types", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = seq(4000, 400, length.out = 100),
    absorbance = rnorm(100)
  )

  expect_error(
    find_ftir_peaks(ftir, sg_p_norm = "invalid"),
    "`sg_p_norm` must be numeric"
  )
})

test_that("find_ftir_peaks handles even sg_n_norm values", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = seq(4000, 400, length.out = 100),
    absorbance = rnorm(100)
  )

  # Test with even sg_n_norm (should fail)
  expect_error(
    find_ftir_peaks(ftir, sg_n_norm = 12),
    "must be an odd integer >= 3"
  )
})

test_that("fit_peaks validates method parameter", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = seq(4000, 400, length.out = 100),
    absorbance = rnorm(100)
  )

  expect_error_bilingual(
    fit_peaks(ftir, method = "invalid_method"),
    en = "must be one of",
    fr = "doit être l'une des valeurs"
  )
})

test_that("fit_peaks accepts explicit fitting controls and separate peak-finder tuning (#noissue)", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 1, 0), 10)
  )
  explicit_peaks <- c(545, 909, 1273)

  fitted_peaks <- fit_peaks(
    ftir,
    peaklist = explicit_peaks,
    method = "voigt",
    sigma = rep(8, length(explicit_peaks)),
    eta = rep(0.4, length(explicit_peaks)),
    mix_ratio = rep(1 / length(explicit_peaks), length(explicit_peaks)),
    conv_cri = 1e-3,
    maxit = 1500
  )

  expect_equal(fitted_peaks$method, "voigt")
  expect_length(fitted_peaks$mu, length(explicit_peaks))
  expect_length(fitted_peaks$sigma, length(explicit_peaks))
  expect_length(fitted_peaks$eta, length(explicit_peaks))

  auto_peaks_fit <- fit_peaks(
    ftir,
    method = "gauss",
    sigma = rep(8, 10),
    conv_cri = 1e-3,
    maxit = 1500,
    window_norm = 50,
    window_deriv = 50,
    sg_n_norm = 7,
    sg_n_deriv = 7
  )

  expect_equal(auto_peaks_fit$method, "gauss")
  expect_length(auto_peaks_fit$mu, 10)
})

# === Section 4: Data Validation Pattern Tests (NEW for integration) ===

test_that("All functions validate ftir data structure", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  expect_error_bilingual(
    find_ftir_peaks(data.frame(a = 1, b = 2)),
    en = "must contain a column named `sample_id`.",
    fr = "doit contenir une colonne nommée `sample_id`."
  )
})

# === Section 5: Helper Function Tests ===

test_that("find_peak_maxima detects local maxima", {
  raman <- data.frame(
    sample_id = "sample1",
    wavenumber = 1:9,
    intensity = c(1, 2, 3, 4, 5, 4, 3, 2, 1)
  )
  attr(raman, "intensity") <- "raman"

  expect_equal(find_peak_maxima(raman)$wavenumber, 5)
})


# === Section 6: Output Generation Tests ===

test_that("Peak data.frame is created ok", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
      "amplitude",
      "mix_ratio",
      "peak_shape"
    )
  )

  expect_equal(
    colnames(fit_peak_df(fit_peaks(ftir, method = "gauss"))),
    c(
      "sample_id",
      "peak",
      "wavenumber",
      "sigma",
      "amplitude",
      "mix_ratio",
      "peak_shape"
    )
  )
  expect_equal(
    colnames(fit_peak_df(fit_peaks(ftir, method = "lorentz"))),
    c(
      "sample_id",
      "peak",
      "wavenumber",
      "gam",
      "amplitude",
      "mix_ratio",
      "peak_shape"
    )
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
      "amplitude",
      "mix_ratio",
      "peak_shape"
    )
  )
})

test_that("fit_peaks stores explicit amplitudes that sum to shifted signal mass (#noissue)", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]
  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  shifted_mass <- sum(
    ftir$absorbance - min(ftir$absorbance, na.rm = TRUE),
    na.rm = TRUE
  )

  fitted <- list(
    fit_peaks(ftir, method = "gauss"),
    fit_peaks(ftir, method = "voigt"),
    fit_peaks(ftir, method = "lorentz"),
    fit_peaks(ftir, method = "dsg")
  )

  for (fit in fitted) {
    expect_true("amplitude" %in% names(fit))
    expect_length(fit$amplitude, length(fit$mu))
    expect_equal(sum(fit$amplitude), shifted_mass, tolerance = 1e-6)
    expect_equal(
      fit$amplitude,
      fit$mix_ratio * shifted_mass,
      tolerance = 1e-6
    )
  }
})

test_that("get_fit_spectra works ok", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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

  expect_length(
    PlotFTIR:::.get_fit_spectra(ftir, fitg),
    length(ftir$wavenumber)
  )
  expect_length(
    PlotFTIR:::.get_fit_spectra(ftir, fitv),
    length(ftir$wavenumber)
  )
  expect_length(
    PlotFTIR:::.get_fit_spectra(ftir, fitl),
    length(ftir$wavenumber)
  )
  expect_length(
    PlotFTIR:::.get_fit_spectra(ftir, fitd),
    length(ftir$wavenumber)
  )

  expect_length(
    PlotFTIR:::.get_fit_spectra(ftir, fitg, 3),
    length(ftir$wavenumber)
  )
  expect_length(
    PlotFTIR:::.get_fit_spectra(ftir, fitv, 3),
    length(ftir$wavenumber)
  )
  expect_length(
    PlotFTIR:::.get_fit_spectra(ftir, fitl, 3),
    length(ftir$wavenumber)
  )
  expect_length(
    PlotFTIR:::.get_fit_spectra(ftir, fitd, 3),
    length(ftir$wavenumber)
  )
})

test_that("get_fit_spectra preserves baseline-shifted signal mass (#noissue)", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]
  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  ftir$absorbance <- ftir$absorbance - min(ftir$absorbance, na.rm = TRUE)

  fitted <- list(
    fit_peaks(ftir, method = "gauss"),
    fit_peaks(ftir, method = "voigt"),
    fit_peaks(ftir, method = "lorentz"),
    fit_peaks(ftir, method = "dsg")
  )

  for (fit in fitted) {
    reconstructed <- PlotFTIR:::.get_fit_spectra(ftir, fit)
    legacy_fit <- fit
    legacy_fit$amplitude <- NULL
    components <- vapply(
      seq_along(fit$mu),
      function(i) sum(PlotFTIR:::.get_fit_spectra(ftir, fit, i), na.rm = TRUE),
      numeric(1)
    )

    expect_equal(
      sum(reconstructed, na.rm = TRUE),
      sum(ftir$absorbance, na.rm = TRUE),
      tolerance = 1e-6
    )
    expect_equal(
      sum(PlotFTIR:::.get_fit_spectra(ftir, legacy_fit), na.rm = TRUE),
      sum(ftir$absorbance, na.rm = TRUE),
      tolerance = 1e-6
    )
    expect_equal(components, fit$amplitude, tolerance = 1e-6)
    expect_equal(sum(components), sum(reconstructed), tolerance = 1e-6)
  }
})

test_that("get_fit_spectra reconstruction is numerically faithful (#36)", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]
  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  ftir$absorbance <- ftir$absorbance - min(ftir$absorbance, na.rm = TRUE)

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
  total_ss <- sum(ftir$absorbance^2)

  for (method in c("gauss", "voigt", "lorentz", "dsg")) {
    fit <- fit_peaks(ftir, peaklist = peaklist, method = method)
    fitted_curve <- PlotFTIR:::.get_fit_spectra(ftir, fit)

    # Initialization uses the starting peak list and default shape parameters,
    # so a converged fit must not be worse than where the optimizer started.
    init <- fit
    init$mu <- peaklist
    init$mix_ratio <- rep(1 / length(peaklist), length(peaklist))
    init$amplitude <- init$mix_ratio * sum(ftir$absorbance)
    init$sigma <- rep(10, length(peaklist))
    init$gam <- rep(10, length(peaklist))
    init$eta <- rep(0.5, length(peaklist))
    init$alpha <- rep(1e-4, length(peaklist))
    init_curve <- PlotFTIR:::.get_fit_spectra(ftir, init)

    rss_fit <- sum((ftir$absorbance - fitted_curve)^2)
    rss_init <- sum((ftir$absorbance - init_curve)^2)

    expect_lte(rss_fit, rss_init)
    # Relative residual: the fit must explain the bulk of the signal energy.
    expect_lt(rss_fit / total_ss, 0.15)
    # Area preservation on the baseline-shifted scale.
    expect_equal(sum(fitted_curve), sum(ftir$absorbance), tolerance = 1e-6)
    expect_true(all(fitted_curve >= 0))
    expect_false(anyNA(fitted_curve))

    # Components must sum exactly to the full reconstruction, point by point.
    component_sum <- Reduce(
      "+",
      lapply(
        seq_along(fit$mu),
        function(i) PlotFTIR:::.get_fit_spectra(ftir, fit, i)
      )
    )
    expect_equal(component_sum, fitted_curve, tolerance = 1e-8)

    # Halving a component's amplitude must change the reconstruction by
    # exactly half that component, guarding against post hoc rescaling.
    scaled <- fit
    scaled$amplitude[1] <- scaled$amplitude[1] / 2
    expect_equal(
      PlotFTIR:::.get_fit_spectra(ftir, scaled),
      fitted_curve - PlotFTIR:::.get_fit_spectra(ftir, fit, 1) / 2,
      tolerance = 1e-8
    )
  }
})

test_that("get_fit_spectra recovers a known synthetic spectrum exactly (#36)", {
  x <- seq(1000, 1300, by = 1)
  mu <- c(1080, 1200)
  sigma <- c(10, 12)
  amplitude <- c(30, 20)
  absorbance <- Reduce(
    "+",
    Map(
      function(m, s, a) a * PlotFTIR:::.truncated_g(x, mu = m, sigma = s),
      mu,
      sigma,
      amplitude
    )
  )
  ftir <- data.frame(
    sample_id = "synthetic",
    wavenumber = x,
    absorbance = absorbance
  )

  known_fit <- list(
    mu = mu,
    sigma = sigma,
    mix_ratio = amplitude / sum(amplitude),
    amplitude = amplitude,
    method = "gauss"
  )

  expect_equal(
    PlotFTIR:::.get_fit_spectra(ftir, known_fit),
    absorbance,
    tolerance = 1e-10
  )
})

test_that("truncated gaussian matches kernel normalization used by other fits (#noissue)", {
  x <- seq(1000, 1100, by = 1)
  kernel <- PlotFTIR:::.truncated_g(x, mu = 1050, sigma = 8)

  expect_equal(sum(kernel), 1, tolerance = 1e-10)
})

test_that("get_fit_spectra checks are ok", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]

  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  expect_error(
    PlotFTIR:::.get_fit_spectra(ftir, fitpeaks, peak = "all"),
    "must be a positive integer"
  )
  expect_error(
    PlotFTIR:::.get_fit_spectra(ftir, fitpeaks, peak = 1.5),
    "must be a positive integer"
  )
  expect_error(
    PlotFTIR:::.get_fit_spectra(ftir, fitpeaks, peak = 100),
    "is out of range"
  )
  expect_error(
    PlotFTIR:::.get_fit_spectra(ftir, fitpeaks, peak = -1),
    "is out of range"
  )
})

# === Section 7: Plot Functionality Tests ===

test_that("plot_fit_ftir_peaks work", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
  # Instead of direct title checks, check that title exists and is not empty
  expect_false(is.null(p$labels$title))
  expect_false(nchar(p$labels$title) == 0)

  p2 <- plot_fit_ftir_peaks(
    ftir,
    fitpeaks,
    plot_title = c("Test Plot", "Test Subtitle")
  )
  expect_equal(p2$labels$title, "Test Plot")
  expect_equal(p2$labels$subtitle, "Test Subtitle")
})

test_that("plot_fit_residuals work", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
  # Instead of direct title checks, check that title exists and is not empty
  expect_false(is.null(p$labels$title))
  expect_false(nchar(p$labels$title) == 0)

  p2 <- plot_fit_residuals(
    ftir,
    fitpeaks,
    plot_title = c("Test Plot", "Test Subtitle")
  )
  expect_equal(p2$labels$title, "Test Plot")
  expect_equal(p2$labels$subtitle, "Test Subtitle")
})

test_that("plot_components work", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
  # Instead of direct title checks, check that title exists and is not empty
  expect_false(is.null(p$labels$title))
  expect_false(nchar(p$labels$title) == 0)

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

test_that("plot_components suppresses duplicate colour-scale messages (#36)", {
  if (!requireNamespace("signal", quietly = TRUE)) {
    testthat::skip("signal not available for testing")
  }

  ftir <- sample_spectra[
    sample_spectra$sample_id == "isopropanol",
  ]
  ftir <- ftir[ftir$wavenumber > 1000 & ftir$wavenumber < 2000, ]
  fitpeaks <- fit_peaks(ftir)

  if (!require("ggplot2", quietly = TRUE)) {
    testthat::skip(
      "ggplot2 not available for testing component plot production"
    )
  }

  if (!require("gghighlight", quietly = TRUE)) {
    testthat::skip(
      "gghighlight not available for testing component plot production"
    )
  }

  expect_no_message(plot_components(ftir, fitpeaks))
  expect_no_message(plot_components(ftir, fitpeaks, plot_fit = TRUE))
})

# === Section 8: Error Handling Tests ===

test_that("fit_peaks error checks are ok", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
    "must be one of"
  )
})

test_that("plot_fit_ftir_peaks error checks are ok", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
    "does not contain fit peaks that match the ftir sample provided",
    fixed = TRUE
  )
  expect_error(
    plot_fit_ftir_peaks(ftir, fitpeaks, extra_arg = "ok"),
    "unrecognized argument"
  )
  fitpeaks$sample_id <- NULL
  expect_warning(
    plot_fit_ftir_peaks(ftir, fitpeaks),
    "should be generated with",
    fixed = TRUE
  )
})

test_that("plot_fit_residuals error checks are ok", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
    "does not contain fit peaks that match the ftir sample provided",
    fixed = TRUE
  )
  fitpeaks$sample_id <- NULL
  expect_warning(
    plot_fit_residuals(ftir, fitpeaks),
    "should be generated with",
    fixed = TRUE
  )
  suppressWarnings(
    expect_error(
      plot_fit_residuals(ftir, fitpeaks, extra_arg = "ok"),
      "unrecognized argument"
    )
  )
})

test_that("plot_components error checks are ok", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
    "does not contain fit peaks that match the ftir sample provided",
    fixed = TRUE
  )
  fitpeaks$sample_id <- NULL
  expect_warning(
    plot_components(ftir, fitpeaks),
    "should be generated with",
    fixed = TRUE
  )
  expect_error(
    plot_components(ftir, fitpeaks, extra_arg = "ok"),
    "unrecognized argument"
  )
})

# === Section 9: Language Handling Tests ===

test_that("Languages are handled properly", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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

  if (!requireNamespace("gghighlight")) {
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

  expect_warning_bilingual(
    plot_components(ftir, fitpeaks, lang = "test"),
    en = "language must be one of 'en', 'english', 'anglais', 'fr', 'french', 'francais' or 'fran\u00e7ais', not 'test'. Use default.",
    fr = "la langue doit \u00eatre l'une des suivantes : 'en', 'english', 'anglais', 'fr', 'french', 'francais' ou 'fran\u00e7ais', et non 'test'. La valeur par d\u00e9faut sera utilis\u00e9e."
  )
})

# === Section 10: Component Optimization Tests (Internal) ===

test_that("component-optimization dsgmm error checking is ok", {
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 10)
  )

  expect_error(
    .spect_em_dsgmm(
      x = c(4001, ftir$wavenumber),
      y = ftir$absorbance,
      mu = runif(10)
    ),
    "vectors must be of the same length"
  )
  expect_error(
    .spect_em_dsgmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      eta = runif(11)
    ),
    "must be of the same length"
  )
  expect_error(
    .spect_em_dsgmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      maxit = 1
    ),
    'must be greater than "1" to perform optimization'
  )
})

test_that("component-optimization gmm error checking is ok", {
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 10)
  )

  expect_error(
    .spect_em_gmm(
      x = c(4001, ftir$wavenumber),
      y = ftir$absorbance,
      mu = runif(10)
    ),
    "vectors must be of the same length"
  )
  expect_error(
    .spect_em_gmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      sigma = runif(11)
    ),
    "must be of the same length"
  )
  expect_error(
    .spect_em_gmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      maxit = 1
    ),
    'must be greater than "1" to perform optimization'
  )
})

test_that("component-optimization lmm error checking is ok", {
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 10)
  )

  expect_error(
    .spect_em_lmm(
      x = c(4001, ftir$wavenumber),
      y = ftir$absorbance,
      mu = runif(10),
      gam = rep(10, 10)
    ),
    "vectors must be of the same length"
  )
  expect_error(
    .spect_em_lmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      gam = runif(11)
    ),
    "must be of the same length"
  )
  expect_error(
    .spect_em_lmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      maxit = 1
    ),
    'must be greater than "1" to perform optimization'
  )
})

test_that("component-optimization pvmm error checking is ok", {
  ftir <- data.frame(
    sample_id = "sample1",
    wavenumber = round(seq(4000, 400, length.out = 100)),
    absorbance = rep(c(0, 0, 1, 2, 3, 5, 3, 2, 0, 0), 10)
  )

  expect_error(
    .spect_em_pvmm(
      x = c(4001, ftir$wavenumber),
      y = ftir$absorbance,
      mu = runif(10)
    ),
    "vectors must be of the same length"
  )
  expect_error(
    .spect_em_pvmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      eta = runif(11)
    ),
    "must be of the same length"
  )
  expect_error(
    .spect_em_pvmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = runif(10),
      maxit = 1
    ),
    'must be greater than "1" to perform optimization'
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
    .spect_em_gmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = mu_list,
      verbose = TRUE
    ),
    "Converged in "
  )
  expect_message(
    .spect_em_lmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = mu_list,
      verbose = TRUE
    ),
    "Converged in "
  )
  expect_message(
    .spect_em_pvmm(
      x = ftir$wavenumber,
      y = ftir$absorbance,
      mu = mu_list,
      verbose = TRUE
    ),
    "Converged in "
  )
  expect_message(
    .spect_em_dsgmm(
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

  gmm_loose <- .spect_em_gmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded
  )
  lmm_loose <- .spect_em_lmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded
  )
  pvmm_loose <- .spect_em_pvmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded
  )
  dsgmm_loose <- .spect_em_dsgmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded
  )

  expect_false(all(gmm_loose$mu == mu_list_rounded))
  expect_false(all(lmm_loose$mu == mu_list_rounded))
  expect_false(all(pvmm_loose$mu == mu_list_rounded))
  expect_false(all(dsgmm_loose$mu == mu_list_rounded))

  gmm_fixed <- .spect_em_gmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded,
    fixed_mu = TRUE
  )
  lmm_fixed <- .spect_em_lmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded,
    fixed_mu = TRUE
  )
  pvmm_fixed <- .spect_em_pvmm(
    x = ftir$wavenumber,
    y = ftir$absorbance,
    mu = mu_list_rounded,
    fixed_mu = TRUE
  )
  dsgmm_fixed <- .spect_em_dsgmm(
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

# === Section 11: Fixed Peak Behavior Tests (Existing) ===

test_that("Fixed Peak Locations don't move", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
    fixed_peaks = FALSE,
    method = "g"
  )

  lmm_loose <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = FALSE,
    method = "l"
  )
  pvmm_loose <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = FALSE,
    method = "pv"
  )
  dsgmm_loose <- fit_peaks(
    ftir,
    peaklist = peaklist,
    fixed_peaks = FALSE,
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

# === Section 12: Difficult Peak-Separation Stress Tests ===

# Build a synthetic spectrum from known components using the same truncated
# kernels the optimizers target, so recovery can be checked against truth.
.synth_spectrum <- function(x, components, shape) {
  kernel <- switch(
    shape,
    gauss = function(p) {
      p$amplitude * PlotFTIR:::.truncated_g(x, mu = p$mu, sigma = p$sigma)
    },
    lorentz = function(p) {
      p$amplitude * PlotFTIR:::.truncated_l(x, mu = p$mu, gam = p$gam)
    },
    voigt = function(p) {
      p$amplitude *
        PlotFTIR:::.truncated_pv(x, mu = p$mu, sigma = p$sigma, eta = p$eta)
    },
    dsg = function(p) {
      p$amplitude *
        PlotFTIR:::.truncated_dsg(
          x,
          mu = p$mu,
          sigma = p$sigma,
          alpha = p$alpha,
          eta = p$eta
        )
    }
  )
  data.frame(
    sample_id = "synthetic",
    wavenumber = x,
    absorbance = Reduce("+", lapply(components, kernel))
  )
}

.relative_rss <- function(ftir, fit) {
  y <- ftir$absorbance - min(ftir$absorbance, na.rm = TRUE)
  sum((y - PlotFTIR:::.get_fit_spectra(ftir, fit))^2) / sum(y^2)
}

test_that("optimizer recovers well-separated synthetic gaussian peaks (#36)", {
  x <- seq(1000, 1300, by = 1)
  ftir <- .synth_spectrum(
    x,
    list(
      list(mu = 1080, sigma = 10, amplitude = 30),
      list(mu = 1200, sigma = 12, amplitude = 20)
    ),
    "gauss"
  )

  fit <- fit_peaks(
    ftir,
    peaklist = c(1075, 1205),
    method = "gauss",
    conv_cri = 1e-6,
    maxit = 3000
  )

  expect_equal(fit$mu, c(1080, 1200), tolerance = 0.5)
  expect_equal(fit$sigma, c(10, 12), tolerance = 0.5)
  expect_equal(fit$amplitude, c(30, 20), tolerance = 0.5)
  expect_lt(.relative_rss(ftir, fit), 1e-6)
})

test_that("optimizer separates strongly overlapping gaussian bands (#36)", {
  # Centres separated by ~2 sigma: heavily overlapping but still resolvable.
  x <- seq(1000, 1300, by = 1)
  ftir <- .synth_spectrum(
    x,
    list(
      list(mu = 1140, sigma = 12, amplitude = 25),
      list(mu = 1165, sigma = 12, amplitude = 25)
    ),
    "gauss"
  )

  fit <- fit_peaks(
    ftir,
    peaklist = c(1135, 1170),
    method = "gauss",
    conv_cri = 1e-6,
    maxit = 3000
  )

  expect_equal(sort(fit$mu), c(1140, 1165), tolerance = 1)
  expect_equal(fit$sigma, c(12, 12), tolerance = 1)
  expect_equal(fit$amplitude, c(25, 25), tolerance = 1)
  # Peaks must not collapse onto one another.
  expect_gt(diff(sort(fit$mu)), 15)
  expect_lt(.relative_rss(ftir, fit), 1e-4)
})

test_that("optimizer resolves a narrow shoulder on a broad band (#36)", {
  x <- seq(1000, 1300, by = 1)
  ftir <- .synth_spectrum(
    x,
    list(
      list(mu = 1150, sigma = 45, amplitude = 40),
      list(mu = 1180, sigma = 8, amplitude = 10)
    ),
    "gauss"
  )

  fit <- fit_peaks(
    ftir,
    peaklist = c(1145, 1182),
    method = "gauss",
    conv_cri = 1e-6,
    maxit = 3000
  )

  expect_equal(fit$mu, c(1150, 1180), tolerance = 1)
  # Widths must stay distinct: the broad band should not shrink to the shoulder.
  expect_gt(max(fit$sigma) / min(fit$sigma), 3)
  expect_equal(fit$amplitude, c(40, 10), tolerance = 1)
  expect_lt(.relative_rss(ftir, fit), 1e-3)
})

test_that("optimizer recovers overlapping lorentzian bands with broad wings (#36)", {
  x <- seq(1000, 1300, by = 1)
  ftir <- .synth_spectrum(
    x,
    list(
      list(mu = 1140, gam = 10, amplitude = 30),
      list(mu = 1168, gam = 10, amplitude = 20)
    ),
    "lorentz"
  )

  fit <- fit_peaks(
    ftir,
    peaklist = c(1134, 1174),
    method = "lorentz",
    conv_cri = 1e-6,
    maxit = 3000
  )

  expect_equal(fit$mu, c(1140, 1168), tolerance = 1)
  # Lorentzian wings are truncated by the finite window, so widths and
  # amplitudes recover with a looser tolerance than the gaussian cases.
  expect_equal(fit$gam, c(10, 10), tolerance = 1.5)
  expect_equal(fit$amplitude, c(30, 20), tolerance = 2)
  expect_lt(.relative_rss(ftir, fit), 1e-2)
})

test_that("optimizer recovers overlapping pseudo-voigt bands (#36)", {
  x <- seq(1000, 1300, by = 1)
  ftir <- .synth_spectrum(
    x,
    list(
      list(mu = 1140, sigma = 12, eta = 0.4, amplitude = 30),
      list(mu = 1162, sigma = 12, eta = 0.4, amplitude = 25)
    ),
    "voigt"
  )

  fit <- fit_peaks(
    ftir,
    peaklist = c(1136, 1166),
    method = "voigt",
    conv_cri = 1e-6,
    maxit = 3000
  )

  expect_equal(fit$mu, c(1140, 1162), tolerance = 1)
  expect_true(all(fit$eta >= 0 & fit$eta <= 1))
  expect_equal(fit$amplitude, c(30, 25), tolerance = 2)
  expect_lt(.relative_rss(ftir, fit), 1e-2)
})

test_that("optimizer reconstructs an asymmetric doniach-sunjic band (#36)", {
  x <- seq(1000, 1300, by = 1)
  ftir <- .synth_spectrum(
    x,
    list(list(mu = 1150, sigma = 12, alpha = 0.2, eta = 0.5, amplitude = 40)),
    "dsg"
  )

  fit <- fit_peaks(
    ftir,
    peaklist = 1150,
    method = "dsg",
    conv_cri = 1e-6,
    maxit = 3000
  )

  expect_equal(fit$mu, 1150, tolerance = 1)
  # sigma/alpha/eta trade off against each other in the DSG shape, so only the
  # peak position and the reconstruction are checked quantitatively.
  expect_gt(fit$alpha, 0)
  expect_lt(.relative_rss(ftir, fit), 1e-2)
})

test_that("optimizer log-likelihood improves monotonically on hard cases (#36)", {
  x <- seq(1000, 1300, by = 1)
  ftir <- .synth_spectrum(
    x,
    list(
      list(mu = 1140, sigma = 12, amplitude = 25),
      list(mu = 1165, sigma = 12, amplitude = 25)
    ),
    "gauss"
  )

  fits <- suppressMessages(list(
    gauss = fit_peaks(ftir, peaklist = c(1135, 1170), method = "gauss"),
    voigt = fit_peaks(ftir, peaklist = c(1135, 1170), method = "voigt"),
    lorentz = fit_peaks(ftir, peaklist = c(1135, 1170), method = "lorentz"),
    dsg = fit_peaks(ftir, peaklist = c(1135, 1170), method = "dsg")
  ))

  for (fit in fits) {
    expect_gt(length(fit$LL), 1)
    expect_true(all(diff(fit$LL) >= -1e-8))
  }
})

test_that("zero_normalization and zero_deriv check ok", {
  if (!requireNamespace("signal", quietly = TRUE)) {
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
