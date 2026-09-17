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

test_that("FWHM is correct for asymmetric (non-Gaussian) peak shapes", {
  # Deliberately asymmetric triangular peak: steep rise, slow decay.
  # Symmetric Gaussian peaks mask the right-interpolation sign bug because
  # both flanks share identical slope magnitude; an asymmetric shape exposes it.
  wn <- seq(400, 600, by = 0.5)

  # Triangular peak: base at WN=470 to WN=530, apex at WN=500 with height 100
  # Left flank (WN=470->500): slope = +100/30 = 3.333; half_max crossing at 485
  # Right flank (WN=500->530): slope = -100/30 = -3.333; half_max crossing at 515
  # Expected FWHM = 515 - 485 = 30 cm^-1 exactly
  signal <- ifelse(
    wn < 470 | wn > 530,
    0,
    ifelse(wn <= 500, 100 * (wn - 470) / 30, 100 * (530 - wn) / 30)
  )

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "asym"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)

  expect_true("fwhm" %in% colnames(peaks_fwhm))
  expect_gte(nrow(peaks_fwhm), 1)

  fwhm_val <- peaks_fwhm$fwhm[which.max(peaks_fwhm$intensity)]

  # Expected FWHM = 30; allow 2-point tolerance for discrete sampling
  expect_lt(abs(fwhm_val - 30), 2.5)

  # Verify left and right flank crossings are on correct sides of the peak:
  # Left crossing must be < peak WN, right crossing must be > peak WN.
  peak_wn <- peaks_fwhm$wavenumber[which.max(peaks_fwhm$intensity)]
  half_max <- 50

  # Reconstruct crossings from data to confirm directionality
  left_cross <- NA_real_
  for (j in seq.int(which(wn == peak_wn)[1L], 2L, by = -1L)) {
    if (signal[j] > half_max && signal[j - 1] <= half_max) {
      denom <- signal[j] - signal[j - 1]
      if (denom != 0) {
        left_cross <- wn[j - 1] +
          (wn[j] - wn[j - 1]) * (half_max - signal[j - 1]) / denom
      } else {
        left_cross <- wn[j]
      }
      break
    }
  }

  right_cross <- NA_real_
  for (j in seq.int(which(wn == peak_wn)[1L], length(wn) - 1L, by = 1L)) {
    if (signal[j] > half_max && signal[j + 1] <= half_max) {
      denom <- signal[j] - signal[j + 1]
      if (denom != 0) {
        right_cross <- wn[j] +
          (wn[j + 1] - wn[j]) * (signal[j] - half_max) / denom
      } else {
        right_cross <- wn[j + 1]
      }
      break
    }
  }

  expect_true(!is.na(left_cross) && !is.na(right_cross))
  expect_true(left_cross < peak_wn)
  expect_true(right_cross > peak_wn)
  expect_lt(abs((right_cross - left_cross) - 30), 2.5)
})

test_that("FWHM computation is directionally correct for asymmetric peaks", {
  # Regression test: the right-interpolation formula had a sign error
  # (half_max - intensity[j]) instead of (intensity[j] - half_max).
  # For fine-sampled data this produces errors of ~1 WN unit; to expose it,
  # we use coarse sampling that straddles the half-max crossing on the right flank.
  wn_left <- seq(490, 520, by = 1) # peak at WN=500, fine on left side
  wn_gap <- c(600, 700) # coarse gap straddling half-max crossing (~WN=675)
  wn_right <- seq(800, 900, by = 1) # fine beyond gap
  wn <- sort(c(wn_left, wn_gap, wn_right))

  signal <- numeric(length(wn))
  for (i in seq_along(wn)) {
    w <- wn[i]
    if (w < 490 | w > 850) {
      signal[i] <- 0
    } else if (w <= 500) {
      # steep rise: slope=+10
      signal[i] <- (w - 490) * 100 / 10
    } else {
      signal[i] <- max(0, 100 - (w - 500) * 100 / 350)
    } # shallow fall: ~-0.286
  }

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "dir"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)
  expect_true("fwhm" %in% colnames(peaks_fwhm))
  expect_gte(nrow(peaks_fwhm), 1)

  peak_wn <- peaks_fwhm$wavenumber[which.max(peaks_fwhm$intensity)]

  # Compute left and right crossings independently from the data to verify direction.
  half_max <- max(peaks_fwhm$intensity) / 2
  peak_idx <- which(wn == peak_wn)[1L]

  left_cross <- NA_real_
  for (j in seq.int(peak_idx, 2L, by = -1L)) {
    if (signal[j] > half_max && signal[j - 1] <= half_max) {
      denom <- signal[j] - signal[j - 1]
      left_cross <- wn[j - 1] +
        (wn[j] - wn[j - 1]) *
          (half_max - signal[j - 1]) /
          pmax(denom, .Machine$double.eps)
      break
    }
  }

  right_cross <- NA_real_
  for (j in seq.int(peak_idx, length(wn) - 1L, by = 1L)) {
    if (signal[j] > half_max && signal[j + 1] <= half_max) {
      denom <- signal[j] - signal[j + 1]
      right_cross <- wn[j] +
        (wn[j + 1] - wn[j]) *
          (signal[j] - half_max) /
          pmax(denom, .Machine$double.eps)
      break
    }
  }

  # Both crossings must exist and be on the correct side of the peak.
  expect_true(!is.na(left_cross) && !is.na(right_cross))
  expect_true(left_cross < peak_wn)
  expect_true(right_cross > peak_wn)

  # FWHM from independent computation must match the reported value within tolerance.
  expected_fwhm <- abs(right_cross - left_cross)
  fwhm_val <- peaks_fwhm$fwhm[which.max(peaks_fwhm$intensity)]
  expect_lt(abs(fwhm_val - expected_fwhm), 1.0)
})

test_that("compute_fwhm gives correct FWHM for multiple peaks", {
  wn <- seq(100, 2000, by = 3)

  # Two well-separated Gaussian peaks:
  # Peak 1 at WN=500 with sigma~40 -> FWHM ~ 2*sqrt(2*log(2))*40 ~= 94.2
  peak1 <- 100 * exp(-(wn - 500)^2 / (2 * 40^2))
  # Peak 2 at WN=1200 with sigma~60 -> FWHM ~ 2*sqrt(2*log(2))*60 ~= 141.3
  peak2 <- 80 * exp(-(wn - 1200)^2 / (2 * 60^2))

  set.seed(42)
  signal <- peak1 + peak2 + 5
  noise <- rnorm(length(wn), 0, 1)
  intensity <- signal + noise

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = intensity,
    sample_id = "multi"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)

  expect_true("fwhm" %in% colnames(peaks_fwhm))
  expect_gte(nrow(peaks_fwhm), 2)

  # Find the two main detected peaks near expected positions
  peak1_detected <- peaks_fwhm[
    peaks_fwhm$wavenumber >= 470 &
      peaks_fwhm$wavenumber <= 530,
  ]
  peak2_detected <- peaks_fwhm[
    peaks_fwhm$wavenumber >= 1150 &
      peaks_fwhm$wavenumber <= 1250,
  ]

  expect_gte(nrow(peak1_detected), 1)
  expect_gte(nrow(peak2_detected), 1)

  # Use the highest-intensity detected peak near each expected position
  fwhm_1 <- peak1_detected$fwhm[which.max(peak1_detected$intensity)]
  fwhm_2 <- peak2_detected$fwhm[which.max(peak2_detected$intensity)]

  # Each peak's FWHM should be within ~15% of theoretical value
  expected_fwhm_1 <- 2 * sqrt(2 * log(2)) * 40
  expected_fwhm_2 <- 2 * sqrt(2 * log(2)) * 60

  expect_lt(abs(fwhm_1 - expected_fwhm_1) / expected_fwhm_1, 0.15)
  expect_lt(abs(fwhm_2 - expected_fwhm_2) / expected_fwhm_2, 0.15)

  # FWHM values must be positive and finite for main peaks
  expect_true(fwhm_1 > 0 && is.finite(fwhm_1))
  expect_true(fwhm_2 > 0 && is.finite(fwhm_2))

  # The two detected peaks should have different FWHMs (different widths)
  expect_false(abs(fwhm_1 - fwhm_2) < min(fwhm_1, fwhm_2) * 0.1)
})

test_that("FWHM is measured at half prominence above the local base", {
  # Gaussian (sigma = 40, amplitude 100) on a flat baseline of 30.
  # SciPy's peak_widths (rel_height = 0.5) evaluates at level
  # base + (peak - base)/2 = 80, i.e. half the prominence above the base,
  # giving the theoretical FWHM = 2*sqrt(2*log(2))*40 ~= 94.19.
  # The old raw max/2 rule evaluated at level 65 and overestimated by ~23%.
  wn <- seq(0, 1000, by = 0.1)
  sigma <- 40
  signal <- 30 + 100 * exp(-(wn - 500)^2 / (2 * sigma^2))

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = signal,
    sample_id = "base"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)
  expect_gte(nrow(peaks_fwhm), 1)
  fwhm_val <- peaks_fwhm$fwhm[which.max(peaks_fwhm$intensity)]

  expected <- 2 * sqrt(2 * log(2)) * sigma
  expect_lt(abs(fwhm_val - expected), 0.5)
})

test_that("FWHM uses the higher flank minimum as base for interfered peaks", {
  # Peak of height 100 at wn = 50. Left background is a linear rise from 2,
  # with no local minimum (base at the spectral edge). The right side falls to
  # a valley of exactly 11 at wn = 75 before rising toward an interfering
  # neighbor peak (height 60, lower than our peak).
  # SciPy convention: base level = max(2, 11) = 11, so the evaluation level is
  # h - 0.5 * (h - base) = 55.5 -- NOT half the raw maximum (50), NOT an
  # average of the minima (6).
  n <- 100
  wn <- seq_len(n)
  x <- numeric(n)
  for (i in 1:49) {
    x[i] <- 2 * i # linear rise to x[49] = 98
  }
  x[50] <- 100
  right_slope <- 87 / 24
  for (i in 51:75) {
    x[i] <- 98 - right_slope * (i - 51) # descent to valley x[75] = 11
  }
  neighbor_slope <- 49 / 25
  for (i in 76:n) {
    x[i] <- 11 + neighbor_slope * (i - 75) # rise toward neighbor, x[100] = 60
  }

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = x,
    sample_id = "interfered"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)
  expect_gte(nrow(peaks_fwhm), 1)
  fwhm_val <- peaks_fwhm$fwhm[which.max(peaks_fwhm$intensity)]

  # Analytic crossings at level 55.5 on the piecewise-linear signal:
  # left flank x[i] = 2i crosses at wn = 55.5 / 2 = 27.75;
  # right descent x[s] = 98 - (87/24)(s - 51) crosses at s = 51 + 42.5/(87/24).
  eval_level <- 100 - 0.5 * (100 - max(2, 11))
  expected_left <- eval_level / 2
  expected_right <- 51 + (98 - eval_level) / right_slope
  expected_fwhm <- expected_right - expected_left

  expect_lt(abs(fwhm_val - expected_fwhm), 1e-4)
})

test_that("FWHM prominence is bounded by a strictly higher neighboring peak", {
  # Main peak of height 60 at wn = 40 over flat background 1. The right side
  # falls to a valley of 2 at wn = 60, then rises toward a HIGHER neighbor
  # (height 90). Per the SciPy prominence definition, the search area on the
  # right stops at the first strictly higher sample, and the base is the
  # minimum within that bounded interval: max(1, 2) = 2 -> level 60 - 29 = 31.
  n <- 81
  wn <- seq_len(n)
  x <- numeric(n)
  x[1:19] <- 1 # flat background
  left_slope <- 59 / 20
  for (i in 20:40) {
    x[i] <- 1 + left_slope * (i - 20) # linear rise to peak x[40] = 60
  }
  descent_slope <- 56 / 19
  for (i in 41:60) {
    x[i] <- 58 - descent_slope * (i - 41) # descent to valley x[60] = 2
  }
  rise_slope <- 88 / 20
  for (i in 61:n) {
    x[i] <- 2 + rise_slope * (i - 61) # rise toward higher neighbor, x[81] = 90
  }

  raman_data <- data.frame(
    wavenumber = wn,
    intensity = x,
    sample_id = "higher_neighbor"
  )
  attr(raman_data, "intensity") <- "raman"

  peaks_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)
  expect_gte(nrow(peaks_fwhm), 1)

  # Both the main peak (60) and the higher neighbor (90) are local maxima;
  # select the main peak by position.
  main_row <- which(abs(peaks_fwhm$wavenumber - 40) < 1e-6)[1L]
  fwhm_val <- peaks_fwhm$fwhm[main_row]

  eval_level <- 60 - 0.5 * (60 - max(1, 2)) # = 31
  expected_left <- 20 + (eval_level - 1) / left_slope
  expected_right <- 41 + (58 - eval_level) / descent_slope
  expected_fwhm <- expected_right - expected_left

  expect_lt(abs(fwhm_val - expected_fwhm), 1e-4)
})

test_that("FWHM handles a peak next to the left spectral edge without erroring", {
  # Regression test: `seq.int(a, b)` / `seq.int(a, b, by = -1)` do not yield an
  # empty sequence when the bounds are already inverted (unlike `seq_len()`) --
  # they either walk backward past valid indices or error with "wrong sign in
  # 'by' argument". A peak detected at index 2 (one sample from the left edge)
  # made the left-border search collapse to `seq.int(1, 2, by = -1L)`, an
  # ascending range paired with a descending step, which errored outright.
  n <- 40
  x <- rep(1, n)
  x[2] <- 50 # peak one sample from the left edge

  raman_data <- data.frame(
    wavenumber = seq_len(n),
    intensity = x,
    sample_id = "left_edge_peak"
  )
  attr(raman_data, "intensity") <- "raman"

  expect_no_error({
    peaks_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)
  })

  # No sample is available between the peak and the wavenumber-1 edge, so
  # left_border stays at the edge (1) and the left flank base search range is
  # empty; prominence is measured against the flat background (1) alone, and
  # both crossings fall exactly on samples 1 and 3.
  main_row <- which(abs(peaks_fwhm$wavenumber - 2) < 1e-6)[1L]
  expect_false(is.na(peaks_fwhm$fwhm[main_row]))
  expect_equal(peaks_fwhm$fwhm[main_row], 1, tolerance = 1e-6)
})

test_that("FWHM handles a peak next to the right spectral edge without erroring", {
  # Mirrors the left-edge regression above, for the right-border/right-base
  # search: a peak detected at index n - 1 made `seq.int(peak_idx + 1L, n - 1L)`
  # collapse to a single-past-the-end call whose downstream base search range
  # was likewise inverted, walking past index n and returning `NA`, which broke
  # the flank-crossing comparisons.
  n <- 40
  x <- rep(1, n)
  x[n - 1] <- 50 # peak one sample from the right edge

  raman_data <- data.frame(
    wavenumber = seq_len(n),
    intensity = x,
    sample_id = "right_edge_peak"
  )
  attr(raman_data, "intensity") <- "raman"

  expect_no_error({
    peaks_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)
  })

  main_row <- which(abs(peaks_fwhm$wavenumber - (n - 1)) < 1e-6)[1L]
  expect_false(is.na(peaks_fwhm$fwhm[main_row]))
  expect_equal(peaks_fwhm$fwhm[main_row], 1, tolerance = 1e-6)
})

test_that("FWHM handles two closely-spaced peaks separated by a dip without erroring", {
  # A pair of peaks only two samples apart (separated by a single dip sample)
  # remains a realistic case the SciPy-comparison fuzz test exercised
  # extensively; confirm it still runs cleanly and produces finite, sane
  # widths for both peaks after the `seq.int` guard fix.
  n <- 40
  x <- rep(1, n)
  x[15] <- 50 # first peak
  x[16] <- 30 # dip between peaks
  x[17] <- 60 # second, taller, closely-spaced peak

  raman_data <- data.frame(
    wavenumber = seq_len(n),
    intensity = x,
    sample_id = "close_peaks"
  )
  attr(raman_data, "intensity") <- "raman"

  expect_no_error({
    peaks_fwhm <- find_peak_maxima(raman_data, compute_fwhm = TRUE)
  })

  expect_equal(nrow(peaks_fwhm), 2)
  expect_false(any(is.na(peaks_fwhm$fwhm)))
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
