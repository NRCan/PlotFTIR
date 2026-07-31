describe("find_peak_maxima", {
  
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
    peak2 <- 90 * exp(-(wn - 503)^2 / 500)
    peak3 <- 80 * exp(-(wn - 506)^2 / 500)
    
    raman_data <- data.frame(
      wavenumber = wn,
      intensity = peak1 + peak2 + peak3,
      sample_id = "test"
    )
    attr(raman_data, "intensity") <- "raman"
    
    peaks_close <- find_peak_maxima(raman_data, distance = 5)
    peaks_far <- find_peak_maxima(raman_data, distance = 20)
    
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
    
    expect_error(
      find_peak_maxima(wrong_data),
      "intensity attribute not set",
      fixed = TRUE
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
    
    expect_error(
      find_peak_maxima(raman_data, sample_ids = "nonexistent"),
      "must be in",
      fixed = TRUE
    )
  })
})
