test_that("test measurement timing", {
  skip_on_cran()

  cdm <- mockMeasurementDiagnostics()
  result <- summariseMeasurementUse(
                cdm = cdm,
                codes = list("test_codelist" = c(3001467L, 45875977L)))

  expect_no_error(boxplot1 <- plotMeasurementTimings(result,
                                              facet = "cdm_name",
                                              colour = NULL))

  expect_true(all(c("gg", "ggplot") %in% class(boxplot1)))

  expect_error(plotMeasurementTimings(result, facet = "h"))
  expect_error(plotMeasurementTimings(result, colour = "h"))
  expect_error(plotMeasurementTimings(result, timeScale = "h"))
  expect_error(plotMeasurementTimings(result, plotType =  "h"))
  })
