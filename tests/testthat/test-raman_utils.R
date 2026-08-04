test_that("identifies known peaks in synthetic data with clear maxima", {
  set.seed(42)

  wn <- seq(100, 2000, by = 5)
  peak1 <- 100 * exp(-(wn - 500)^2 / 5000)
  peak2 <- 50 * exp(-(wn - 1000)^2 / 8000)
  baseline <- 5
  noise <- rnorm(length(wn), 0, 2)

  intensity <- peak1 + peak2 + baseline + noise

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = intensity,
    sample_id = "test"
  )

  attr(raman_data, "intensity") <- "raman"

  peaks <- find_peak_maxima(raman_data, height = 10)

  expect_s3_class(peaks, "data.frame")
  expect_true("sample_id" %in% colnames(peaks))
  expect_true("wavenumber" %in% colnames(peaks))
  expect_true("intensity" %in% colnames(peaks))
  expect_false("fwhm" %in% colnames(peaks))

  expect_gt(nrow(peaks), 0)

  found_peak1 <- any(abs(peaks$wavenumber - 500) < 20)
  found_peak2 <- any(abs(peaks$wavenumber - 1000) < 20)

  expect_true(found_peak1 || found_peak2)
})

test_that("respects height threshold parameter", {
  wn <- seq(100, 2000, by = 5)
  high_peak <- 200 * exp(-(wn - 600)^2 / 3000)
  low_peak <- 10 * exp(-(wn - 1500)^2 / 4000)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = high_peak + low_peak,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks_strict <- find_peak_maxima(raman_data, height = 50)
  peaks_relaxed <- find_peak_maxima(raman_data, height = 5)

  expect_gt(nrow(peaks_relaxed), nrow(peaks_strict))
})

test_that("enforces minimum distance between detected peaks", {
  wn <- seq(100, 2000, by = 1)
  peak1 <- 100 * exp(-(wn - 500)^2 / 500)
  peak2 <- 100 * exp(-(wn - 540)^2 / 500)
  peak3 <- 80 * exp(-(wn - 590)^2 / 500)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = peak1 + peak2 + peak3,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks_close <- find_peak_maxima(raman_data, distance = 20)
  peaks_far <- find_peak_maxima(raman_data, distance = 80)

  expect_gt(nrow(peaks_close), nrow(peaks_far))
})

test_that("compute_fwhm = TRUE returns fwhm column", {
  wn <- seq(100, 2000, by = 5)
  signal <- 100 * exp(-(wn - 500)^2 / 5000)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)

  expect_true("fwhm" %in% colnames(peaks_fwhm))
  expect_true(all(!is.na(peaks_fwhm$fwhm)))
  expect_true(all(peaks_fwhm$fwhm > 0))
})

test_that("compute_fwhm = FALSE does not include fwhm column", {
  wn <- seq(100, 2000, by = 5)
  signal <- 100 * exp(-(wn - 500)^2 / 5000)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks_no_fwhm <- find_peak_maxima(raman_data, compute_fwhm = FALSE)

  expect_false("fwhm" %in% colnames(peaks_no_fwhm))
})

test_that("errors when intensity attribute is not raman or normalized raman", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  wrong_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(wrong_data, "intensity") <- "absorbance"

  expect_error_bilingual(
    find_peak_maxima(wrong_data),
    en = "intensity attribute not set",
    fr = "d'intensité n'est pas défini"
  )
})

test_that("errors when invalid sample_ids are provided", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  expect_error_bilingual(
    find_peak_maxima(raman_data, sample_ids = "nonexistent"),
    en = "must be in",
    fr = "doivent être dans"
  )
})

test_that("find_peak_maxima errors when height is not numeric", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  expect_error_bilingual(
    find_peak_maxima(raman_data, height = "high"),
    en = "must be a single numeric value or NULL",
    fr = "doit être une valeur numérique unique ou NULL"
  )

  expect_error_bilingual(
    find_peak_maxima(raman_data, height = c(10, 20)),
    en = "must be a single numeric value or NULL",
    fr = "doit être une valeur numérique unique ou NULL"
  )
})

test_that("find_peak_maxima errors when distance is not positive", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  expect_error_bilingual(
    find_peak_maxima(raman_data, distance = -5),
    en = "must be a positive numeric value or NULL",
    fr = "doit être une valeur numérique positive ou NULL"
  )

  expect_error_bilingual(
    find_peak_maxima(raman_data, distance = 0),
    en = "must be a positive numeric value or NULL",
    fr = "doit être une valeur numérique positive ou NULL"
  )

  expect_error_bilingual(
    find_peak_maxima(raman_data, distance = "far"),
    en = "must be a positive numeric value or NULL",
    fr = "doit être une valeur numérique positive ou NULL"
  )
})

test_that("find_peak_maxima errors when compute_fwhm is not logical", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  expect_error_bilingual(
    find_peak_maxima(raman_data, compute_fwhm = "yes"),
    en = "must be a logical value",
    fr = "doit être une valeur booléenne"
  )

  expect_error_bilingual(
    find_peak_maxima(raman_data, compute_fwhm = c(TRUE, FALSE)),
    en = "must be a logical value",
    fr = "doit être une valeur booléenne"
  )
})

test_that("find_peak_maxima returns empty data.frame when no peaks above threshold (without FWHM)", {
  wn <- seq(100, 2000, by = 5)
  intensity <- rep(1, length(wn))

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = intensity,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks <- find_peak_maxima(raman_data, height = 100)

  expect_s3_class(peaks, "data.frame")
  expect_equal(nrow(peaks), 0)
  expect_false("fwhm" %in% colnames(peaks))
  expect_true(all(
    c("sample_id", "wavenumber", "intensity") %in% colnames(peaks)
  ))
})

test_that("find_peak_maxima returns empty data.frame with fwhm column when no peaks above threshold (with FWHM)", {
  wn <- seq(100, 2000, by = 5)
  intensity <- rep(1, length(wn))

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = intensity,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks <- find_peak_maxima(raman_data, height = 100, compute_fwhm = TRUE)

  expect_s3_class(peaks, "data.frame")
  expect_equal(nrow(peaks), 0)
  expect_true("fwhm" %in% colnames(peaks))
  expect_true(all(
    c("sample_id", "wavenumber", "intensity", "fwhm") %in% colnames(peaks)
  ))
})

test_that("smooth_spectra errors when signal package is not available", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  if (!requireNamespace("signal", quietly = TRUE)) {
    expect_error_bilingual(
      smooth_spectra(raman_data),
      en = "requires signal package installation",
      fr = "nécessite l'installation du paquet signal"
    )
  } else {
    skip("signal package is available")
  }
})

test_that("smooth_spectra errors when window_length is invalid", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  if (requireNamespace("signal", quietly = TRUE)) {
    expect_error_bilingual(
      smooth_spectra(raman_data, window_length = -1),
      en = "must be a positive integer",
      fr = "doit être un entier positif"
    )

    expect_error_bilingual(
      smooth_spectra(raman_data, window_length = 0),
      en = "must be a positive integer",
      fr = "doit être un entier positif"
    )

    expect_error_bilingual(
      smooth_spectra(raman_data, window_length = 3.5),
      en = "must be an integer",
      fr = "doit être un entier"
    )

    expect_error_bilingual(
      smooth_spectra(raman_data, window_length = "seven"),
      en = "must be a positive integer",
      fr = "doit être un entier positif"
    )
  } else {
    skip("signal package is available")
  }
})

test_that("smooth_spectra errors when polyorder is invalid", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  if (requireNamespace("signal", quietly = TRUE)) {
    expect_error_bilingual(
      smooth_spectra(raman_data, polyorder = -1),
      en = "must be a non-negative integer",
      fr = "doit être un entier non négatif"
    )

    expect_error_bilingual(
      smooth_spectra(raman_data, polyorder = 7),
      en = "less than",
      fr = "inférieur à"
    )

    expect_error_bilingual(
      smooth_spectra(raman_data, polyorder = "two"),
      en = "must be a non-negative integer",
      fr = "doit être un entier non négatif"
    )
  } else {
    skip("signal package is available")
  }
})

test_that("baseline_correct errors when baseline package is not available", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  if (!requireNamespace("baseline", quietly = TRUE)) {
    expect_error_bilingual(
      baseline_correct(raman_data),
      en = "requires baseline package installation",
      fr = "nécessite l'installation du paquet baseline"
    )
  } else {
    skip("baseline package is available")
  }
})

test_that("baseline_correct errors when lambda is invalid", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  if (requireNamespace("baseline", quietly = TRUE)) {
    expect_error_bilingual(
      baseline_correct(raman_data, lambda = -1),
      en = "must be a positive numeric value",
      fr = "doit être une valeur numérique positive"
    )

    expect_error_bilingual(
      baseline_correct(raman_data, lambda = 0),
      en = "must be a positive numeric value",
      fr = "doit être une valeur numérique positive"
    )

    expect_error_bilingual(
      baseline_correct(raman_data, lambda = "high"),
      en = "must be a positive numeric value",
      fr = "doit être une valeur numérique positive"
    )
  } else {
    skip("baseline package is available")
  }
})

test_that("baseline_correct errors when p is invalid", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  if (requireNamespace("baseline", quietly = TRUE)) {
    expect_error_bilingual(
      baseline_correct(raman_data, p = -0.1),
      en = "must be a numeric value in (0, 0.5]",
      fr = "doit être une valeur numérique dans"
    )

    expect_error_bilingual(
      baseline_correct(raman_data, p = 0),
      en = "must be a numeric value in (0, 0.5]",
      fr = "doit être une valeur numérique dans"
    )

    expect_error_bilingual(
      baseline_correct(raman_data, p = 0.6),
      en = "must be a numeric value in (0, 0.5]",
      fr = "doit être une valeur numérique dans"
    )

    expect_error_bilingual(
      baseline_correct(raman_data, p = "high"),
      en = "must be a numeric value in (0, 0.5]",
      fr = "doit être une valeur numérique dans"
    )
  } else {
    skip("baseline package is available")
  }
})

test_that("smooth_spectra errors when intensity attribute is not raman or normalized raman", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  wrong_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(wrong_data, "intensity") <- "intensity"

  if (requireNamespace("signal", quietly = TRUE)) {
    expect_error_bilingual(
      smooth_spectra(wrong_data),
      en = "intensity attribute not set",
      fr = "d'intensité n'est pas défini"
    )
  } else {
    skip("signal package is available")
  }
})

test_that("smooth_spectra errors when invalid sample_ids are provided", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  if (requireNamespace("signal", quietly = TRUE)) {
    expect_error_bilingual(
      smooth_spectra(raman_data, sample_ids = "nonexistent"),
      en = "must be in",
      fr = "doivent être dans"
    )
  } else {
    skip("signal package is available")
  }
})

test_that("smooth_spectra warns and auto-corrects even window_length to odd", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  if (requireNamespace("signal", quietly = TRUE)) {
    expect_warning_bilingual(
      smooth_spectra(raman_data, window_length = 6),
      en = "auto-corrected",
      fr = "a automatiquement corrigé"
    )
  } else {
    skip("signal package is available")
  }
})

test_that("baseline_correct errors when intensity attribute is not raman or normalized raman", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  wrong_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(wrong_data, "intensity") <- "intensity"

  if (requireNamespace("baseline", quietly = TRUE)) {
    expect_error_bilingual(
      baseline_correct(wrong_data),
      en = "intensity attribute not set",
      fr = "d'intensité n'est pas défini"
    )
  } else {
    skip("baseline package is available")
  }
})

test_that("baseline_correct errors when invalid sample_ids are provided", {
  wn <- seq(100, 2000, by = 5)
  signal <- exp(-((wn - 1000) / 500)^2)

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "test"
  )
  attr(raman_data, "intensity") <- "raman"

  if (requireNamespace("baseline", quietly = TRUE)) {
    expect_error_bilingual(
      baseline_correct(raman_data, sample_ids = "nonexistent"),
      en = "must be in",
      fr = "doivent être dans"
    )
  } else {
    skip("baseline package is available")
  }
})
