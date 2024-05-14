test_that("Plots are generated", {
  p1 <- plot_ftir(biodiesel)
  p2 <- plot_ftir_stacked(biodiesel)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})
