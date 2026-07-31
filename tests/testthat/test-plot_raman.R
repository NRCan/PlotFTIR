test_that("plot_raman uses non-reversed x-axis", {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing")
  }
  
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)
  
  wn <- seq(100, 2000, by = 10)
  intensity <- 100 * exp(-(wn - 500)^2 / 5000) + 50
  
  raman_content <- c(
    "##FILETYPE=Raman",
    paste(wn, ",", intensity, sep = "")
  )
  
  writeLines(raman_content, temp_file)
  
  result <- read_raman(path = tmppath, file = tmpfile)
  
  p <- plot_raman(result)
  
  expect_true("ggplot" %in% class(p))
  
  x_scales <- p$scales$scales
  x_scale <- x_scales[[1]]
  
  expect_false(inherits(x_scale$trans, "reverse"))
})

test_that("plot_raman labels axes correctly for Raman data (EN)", {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing")
  }
  
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
    tmpfile <- basename(temp_file)
  
  wn <- seq(100, 2000, by = 10)
  intensity <- 100 * exp(-(wn - 500)^2 / 5000) + 50
  
  raman_content <- c(
    "##FILETYPE=Raman",
    paste(wn, ",", intensity, sep = "")
  )
  
  writeLines(raman_content, temp_file)
  
  result <- read_raman(path = tmppath, file = tmpfile)
  
  p <- plot_raman(result, lang = "en")
  
  expect_true("Raman shift" %in% ggplot2::labels(p)$x)
})

test_that("plot_raman labels axes correctly for Raman data (FR)", {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing")
  }
  
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)
  
  wn <- seq(100, 2000, by = 10)
  intensity <- 100 * exp(-(wn - 500)^2 / 5000) + 50
  
  raman_content <- c(
    "##FILETYPE=Raman",
    paste(wn, ",", intensity, sep = "")
  )
  
  writeLines(raman_content, temp_file)
  
  result <- read_raman(path = tmppath, file = tmpfile)
  
  p <- plot_raman(result, lang = "fr")
  
  expect_true("D\u00e9calage Raman" %in% ggplot2::labels(p)$x)
})

test_that("plot_raman_stacked() offsets samples without y-axis labels", {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    testthat::skip("ggplot2 not available for testing")
  }
  
  temp_file1 <- withr::local_tempfile(fileext = ".csv")
  temp_file2 <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file1)
  
  wn <- seq(100, 2000, by = 10)
  intensity1 <- 100 * exp(-(wn - 500)^2 / 5000) + 50
  intensity2 <- 80 * exp(-(wn - 800)^2 / 6000) + 40
  
  raman_content1 <- c(
    "##FILETYPE=Raman",
    paste(wn, ",", intensity1, sep = "")
  )
  
  raman_content2 <- c(
    "##FILETYPE=Raman",
    paste(wn, ",", intensity2, sep = "")
  )
  
  writeLines(raman_content1, temp_file1)
  writeLines(raman_content2, temp_file2)
  
  result1 <- read_raman(path = tmppath, file = basename(temp_file1), sample_name = "sample1")
  result2 <- read_raman(path = tmppath, file = basename(temp_file2), sample_name = "sample2")
  
  combined <- rbind(result1, result2)
  
  p <- plot_raman_stacked(combined, stack_offset = 50)
  
  expect_true("ggplot" %in% class(p))
})

test_that("plot_raman() gates on ggplot2 availability", {
  temp_file <- withr::local_tempfile(fileext = ".csv")
  tmppath <- dirname(temp_file)
  tmpfile <- basename(temp_file)
  
  wn <- seq(100, 2000, by = 10)
  intensity <- 100 * exp(-(wn - 500)^2 / 5000) + 50
  
  raman_content <- c(
    "##FILETYPE=Raman",
    paste(wn, ",", intensity, sep = "")
  )
  
  writeLines(raman_content, temp_file)
  
  result <- read_raman(path = tmppath, file = tmpfile)
  
  with_mocked_bindings({
    ggplot2 <- NULL
    expect_error(
      plot_raman(result),
      regexp = "requires ggplot2 package installation",
      fixed = TRUE
    )
  })
})
